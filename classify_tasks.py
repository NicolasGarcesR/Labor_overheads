"""
classify_tasks.py
=================
Reads DATA/Tasks/Task Statements.xlsx, classifies every O*NET "Core" task
statement as CORE or OVERHEAD using rule-based text analysis grounded in the
thesis framework, then aggregates to the occupation level via mode.

Criterion (from thesis slide 5):
  CORE     = task that directly produces the firm's output.
             Removing the worker creates a TECHNOLOGICAL bottleneck.
  OVERHEAD = task that manages the firm's institutional environment.
             Removing the worker creates an INSTITUTIONAL bottleneck (at most).

Output: DATA/Tasks/Task Statements_classified.xlsx
  - All original columns preserved
  - task_class : "CORE" | "OVERHEAD" | NA  (NA for non-Core task type rows)
  - occ_class  : "CORE" | "OVERHEAD"       (mode over Core-typed tasks for that occupation)
  - occ_oh_share: fraction of Core tasks classified OVERHEAD (useful for a continuous measure)
"""

import re
import sys
import math
import collections
import openpyxl
from openpyxl.styles import PatternFill, Font, Alignment, Border, Side
from openpyxl.utils import get_column_letter

sys.stdout.reconfigure(encoding="utf-8", errors="replace")

# ──────────────────────────────────────────────────────────────────────────────
# 1.  PATTERN LIBRARY
# Each entry is (regex_pattern, score, label) where label is "OVERHEAD" or "CORE".
# Patterns are applied in order; scores accumulate.  Final label = argmax.
# Ties are broken by occupation SOC prefix heuristic (conservative: OVERHEAD).
# ──────────────────────────────────────────────────────────────────────────────

# ── OVERHEAD patterns ──────────────────────────────────────────────────────────
# "Remove this worker → output may fall, but through an INSTITUTIONAL bottleneck"

OH_PATTERNS = [

    # ── A. Managing / supervising / directing people ──────────────────────────
    # These manage human capital allocation, not production directly.
    # Strategy: specific context = high weight (3-4); generic fallback = low weight (1).
    # Generic patterns alone cannot flip a task to OVERHEAD (need OH > CR sum).
    (r"\bsupervis(e|es|ing|ion)\s+(of\s+)?(staff|employee|worker|personnel|"
     r"subordinate|team|department|clerk|assistant|volunteer|intern)\b", 3),
    (r"\bsupervis(e|es|ing|ion)\b", 1),    # generic fallback (catches "supervise operations")
    (r"\boversee(ing|s)?\s+(staff|employee|worker|operation|department|team|"
     r"program|project|budget|compliance|contract)\b", 3),
    (r"\bappoint(ing)?\s+(manager|director|officer|head|chief|supervisor|"
     r"staff|employee|worker)s?\b", 3),
    (r"\b(delegat(e|es|ing))\s+(task|dut|responsibilit|work|authorit)\b", 3),
    (r"\b(assign(ing|s|ed)?)\s+(duties|tasks|responsibilities|work|shift)\s+"
     r"(to\s+)?(staff|employee|worker|team)\b", 3),
    (r"\b(manag(e|es|ing|er|ers)?)\s+(staff|employee|worker|personnel|team|"
     r"subordinate|department|unit)\b", 3),
    (r"\b(direct(ing|s)?)\s+(staff|employee|worker|personnel|human resource|"
     r"team|department|division|unit)\b", 3),
    # Generic "direct/manage" org activities at medium weight
    (r"\b(direct|manag(e|es|ing))\s+(activit|operation|function|program|"
     r"effort)s?\s+(of|for|across|within)\b", 2),
    (r"\b(hire|hiring|recruit(ing|ment)?|onboard(ing)?)\s+(staff|employee|"
     r"worker|personnel|candidate|new hire|talent)\b", 3),
    (r"\b(hire|hiring|recruit(ing|ment)?)\b", 1),    # generic fallback
    (r"\b(terminat(e|ing|ion)?)\s+(employee|worker|staff|employment)\b", 4),
    (r"\b(disciplin(e|ary|ing)?)\s+(employee|worker|staff|action)\b", 4),
    (r"\bperformance (review|apprais|evaluat|assessment)\b", 2),
    (r"\bperformance (review|apprais|evaluat|assessment)\s+(of|for|with)?\s*"
     r"(staff|employee|worker)\b", 4),
    # Coordinate — specific context (high weight)
    (r"\bcoordinat(e|es|ing|ion)\s+(of\s+)?(activities|work|effort|operation|"
     r"duty|task|function)\s+(of|among|between)\s+(staff|employee|worker|"
     r"team|department|unit|division|contractor)\b", 3),
    (r"\bcoordinat(e|es|ing|ion)\s+(staff|employee|personnel|volunteer|intern)\b", 3),
    (r"\bcoordinat(e|es|ing|ion)\s+(scheduling|hiring|payroll|budget|compliance|"
     r"policy|procurement|vendor|contract)\b", 3),
    (r"\bcoordinat(e|ing)\s+with\s+(board|executive|director|investor|"
     r"regulator|authority|government)\b", 3),
    # Coordinate — generic fallback (low weight)
    (r"\bcoordinat(e|es|ing|ion)\b", 1),

    # ── B. Financial administration / budgeting ───────────────────────────────
    (r"\b(budget(ing|ary|s)?)\b", 3),
    (r"\b(fiscal (plan|responsib|year))\b", 3),
    (r"\b(account(ing|s|ant))\b", 2),
    (r"\b(reconcil(e|ing|iation))\b", 3),
    (r"\b(payroll|pay (employee|worker|staff))\b", 4),
    (r"\b(expense (report|account|tracking|reimbursement))\b", 4),
    (r"\b(financial (activit|statement|report|plan|oversight|management|"
     r"control))\b", 3),
    (r"\b(fund(ing)? (operation|activit|request|allocation))\b", 2),
    (r"\b(cost (control|reduction|management|accounting))\b", 3),
    (r"\b(invoic(e|es|ing)|billing (client|customer|department))\b", 2),
    (r"\binvestment decision\b", 3),

    # ── C. Compliance / regulatory / legal administration ─────────────────────
    (r"\b(comply|complian(ce|t))\b", 3),
    (r"\b(regulat(ion|ory|e|ing))\b", 2),
    (r"\b(legal (requirement|standard|obligation|guideline|compliance|issue))\b", 3),
    (r"\b(licens(e|ing|es)|permit(s|ting)?|certifi(cation|ed|cate))\b", 1),
    (r"\b(accreditat(ion|ed))\b", 3),
    (r"\b(audit(ing|or)?)\b", 2),
    (r"\b(enforce (regulation|rule|policy|safety|standard|ordinance|code))\b", 3),
    (r"\b(adhere to|ensure compliance|in accordance with|(following|applies to) "
     r"(law|regulation|standard|code|ordinance))\b", 3),
    (r"\b(sanction|violation|penalty|grievance (procedure|process))\b", 2),

    # ── D. Reporting / documentation (administrative) ─────────────────────────
    # Distinguish from *writing as output* (journalism, technical writing)
    # and from clinical documentation (part of care output).
    # Specific context = high weight; generic prep/document = medium weight.
    (r"\bprepar(e|ing|ed)?\s+(report|budget|memo|briefing|grant proposal|"
     r"policy document|minutes|agenda)\s+(for|to)\s+(management|board|"
     r"director|executive|leadership|committee|stakeholder|regulator)\b", 3),
    (r"\bprepar(e|ing|ed)?\s+(budget|financial report|expense report|cost report|"
     r"payroll|tax return|regulatory filing|compliance report)\b", 3),
    (r"\bsubmit(ting)?\s+(report|form|filing|application)\s+(to|with)\s+"
     r"(management|board|regulator|agency|authority|government)\b", 3),
    # Generic "prepare report/plan/memo/budget" at medium weight
    (r"\bprepar(e|ing|ed)?\s+(report|budget|plan|memo|proposal|agenda|minutes|\s+"
     r"briefing)\b", 2),
    (r"\brecord.keeping\b", 3),
    (r"\b(maintain|keep)\s+(administrative|financial|personnel|payroll|"
     r"budget|compliance|legal)\s+(record|file|log|register|database)\b", 3),
    (r"\bdocument(ing|ation)?\s+(administrative|organizational|policy|"
     r"compliance|financial|personnel|program|departmental)\b", 2),
    # Generic document/record at low weight
    (r"\b(document|record)\s+(finding|outcome|result|progress|activit)\b", 1),
    (r"\bfill\s+(out|in|and submit)\s+(administrative|HR|tax|payroll|"
     r"compliance|regulatory)\s+(form|report|application)\b", 3),
    (r"\bfile\s+(claim|regulatory|tax|compliance|legal)\s+(report|document|"
     r"form|return|petition)\b", 3),
    (r"\bcompose\s+(memo|memorandum|correspondence|official letter|business letter)\b", 2),
    (r"\b(administrative\s+)?(correspondence|memorandum)\b", 1),

    # ── E. Policy / procedure development ────────────────────────────────────
    (r"\b(develop|implement|establish|formulat(e|ing)|draft)\s+"
     r"(polic(y|ies)|procedure|protocol|guideline|standard|rule)(s|s?)\b", 4),
    (r"\b(revise|update|review)\s+(polic(y|ies)|procedure|protocol|guideline|"
     r"standard|manual|handbook)s?\b", 3),
    (r"\b(polic(y|ies)|procedure|protocol)\s+(development|change|changes|"
     r"implementation|compliance|adherence|enforcement)\b", 3),
    # Looser: implement/establish procedures/policies even with intervening words
    (r"\b(implement|establish|develop|formulate)\b.{0,40}?\b(polic(y|ies)|"
     r"procedure|protocol|standard|rule)s?\b", 2),

    # ── F. External stakeholder / institutional communication ─────────────────
    (r"\b(board (of director|members?))\b", 4),
    (r"\b(investor|shareholder|public relation|investor relation|media relation|"
     r"press release)\b", 4),
    (r"\b(confer(ring)? with|brief(ing)?|present(ing)? to)\s+"
     r"(board|executive|director|management|official|authority)\b", 3),
    (r"\b(lobbying|lobby|legislative)\b", 4),
    (r"\b(government (official|agenc|regulat|authorit))\b", 2),

    # ── G. Strategic planning / organizational objectives ─────────────────────
    (r"\b(strategic (plan|goal|objective|initiative|direction))s?\b", 4),
    (r"\b(business (plan|objective|goal|strategy|development plan))\b", 3),
    (r"\b(organizational (goal|objective|mission|vision|structure))\b", 3),
    (r"\b(prioritiz(e|ing)) (work|task|project|activit|initiative)s?\b", 3),
    # "Set/establish goals/objectives/targets" with plural support
    (r"\b(set(ting)?|establish(ing)?|define)\s+(goals?|objectives?|priorities?|"
     r"targets?|benchmarks?|deadlines?)\s+(for|of|across)\b", 3),
    (r"\b(set(ting)?|establish(ing)?)\s+(goals?|objectives?|priorities?|targets?)\b", 2),

    # ── H. Scheduling / administrative support ────────────────────────────────
    (r"\b(schedul(e|ing|ed)?)\s+(meeting|appointment|staff|employee|shift|"
     r"work|interview|review|event|conference)s?\b", 3),
    (r"\b(administrative (task|activit|function|support|dut|work|process))\b", 4),
    (r"\b(clerical|bookkeeping|secretarial)\b", 4),
    (r"\b(attend (meeting|conference|seminar|workshop|briefing))\b", 3),
    (r"\b(facilitate|chair|run|lead)\s+(meeting|conference|workshop|briefing|"
     r"session)s?\b", 3),

    # ── I. HR/people-management processes ──────────────────────────────────────
    (r"\b(train(ing|s)?)\s+(staff|employee|worker|personnel|new hire|team)(s)?\b", 3),
    (r"\b(onboard(ing)? (new |staff|employee|worker|hire))\b", 4),
    (r"\b(compens(ate|ation)|benefit(s)? (package|plan|administration)|"
     r"wage (determination|scale))\b", 4),
    (r"\b(staffing (level|plan|need|requirement|agency))\b", 4),
    (r"\b(workforce (plan|development|management|need|strateg))\b", 4),
    (r"\b(employee (relation|engagement|satisfaction|retention|health))\b", 4),

    # ── J. Negotiation of organizational/contractual terms ────────────────────
    # (selling/price negotiation is CORE for sales occupations; caught below)
    (r"\b(negotiat(e|ing|ion))\s+(contract|agreement|settlement|term|lease|"
     r"collective|union|deal)s?\b", 2),

    # ── K. Monitoring organizational performance ───────────────────────────────
    # Specific financial/HR monitoring = high weight; generic = low weight.
    (r"\bmonitor(ing)?\s+(staff|employee|worker|vendor|contractor|budget|"
     r"expenditure|organizational|program performance|financial|fiscal|"
     r"compliance|policy)\b", 3),
    # Generic monitor/evaluate at low weight
    (r"\bmonitor(ing)?\s+(progress|compliance|adherence|performance)\b", 1),
    (r"\bevaluat(e|ing|ion)\s+(performance|effectiveness)\s+(of|for)\s+"
     r"(staff|employee|program|department|organization|policy|initiative)\b", 3),
    (r"\bevaluat(e|ing|ion)\s+(performance|effectiveness|efficiency)\b", 1),
    (r"\banalyz(e|ing)\s+(organizational|staffing|labor|personnel|budget|"
     r"financial|cost|expenditure|fiscal|revenue|profitability|overhead)\b", 3),
    # Analyze processes/operations for management/improvement purposes
    (r"\banalyz(e|ing)\b.{0,50}?\b(process|operation|activit)e?s?\b.{0,40}?\b"
     r"(recommend|improve|evaluat|assess|identify|determin)\b", 2),
    # Review operations / operational reports (e.g. "prepare and review reports")
    (r"\b(review|prepare).{0,30}?\b(operational report|report|schedule|plan|"
     r"budget|memo)s?\b.{0,20}?\b(accura|effici|compli|deadlin)\b", 2),
    (r"\breview\s+(operational|administrative|financial|budget|compliance|"
     r"personnel|staffing)\b", 2),
]

# ── CORE patterns ──────────────────────────────────────────────────────────────
# "Remove this worker → output falls through a TECHNOLOGICAL bottleneck"

CR_PATTERNS = [

    # ── A. Physical/technical production ────────────────────────────────────────
    (r"\b(manufactur(e|ing|ed)?|assembl(e|ing|y)|fabricat(e|ing|ion))\b", 3),
    (r"\b(construct(ing|ion)?|build(ing|s)?|erect(ing)?|install(ing|ation)?)\b", 3),
    (r"\b(mold|cast|forge|weld(ing)?|stamp|mach(ine|ining)|"
     r"cut(ting)?|drill(ing)?|grind(ing)?|sand(ing)?|polish(ing)?\b)", 3),
    (r"\b(pour (concrete|cement)|lay (brick|pipe|cable|floor|tile|foundation))\b", 4),
    (r"\b(plumb(ing)?|wire (building|structure|circuit|electrical))\b", 3),
    (r"\b(excavat(e|ing|ion)|demol(ish|ition))\b", 3),
    (r"\b(harvest(ing)?|plant(ing)?|cultivat(e|ing)|irrigat(e|ing))\b", 3),
    (r"\b(mine|mining|drill(ing)? (well|oil|gas)|extract(ing|ion)?)\b", 3),
    (r"\b(logg(ing|er)?|fell(ing)? (tree|timber))\b", 3),

    # ── B. Medical / healthcare delivery ─────────────────────────────────────────
    (r"\b(treat(ing|ment)?|diagnos(e|ing|is)|prescrib(e|ing)|diagnos)\b", 4),
    (r"\b(surgery|surgic(al)?|operat(e|ing) on (patient|person|individual))\b", 5),
    (r"\b(patient care|bedside|administer (medication|treatment|therapy|"
     r"vaccine|injection|intravenous|IV))\b", 4),
    (r"\b(perform (surgery|examination|procedure|intervention|assessment|"
     r"evaluation|test) (on|for) (patient|person|individual|client))\b", 4),
    (r"\b(assess(ing|ment)? (patient|client|resident|individual))\b", 4),
    (r"\b(provide (nursing|medical|clinical|direct|patient|health) care)\b", 4),
    (r"\b(monitor(ing)? (patient|vital|condition|symptom|health status))\b", 4),
    (r"\b(rehabilitat(e|ing|ion)|physical therapy|occupational therapy|"
     r"speech therapy)\b", 4),
    (r"\b(dental|periodontal|oral (hygiene|health|surgery|exam))\b", 4),
    (r"\b(chiropractic|optometric|optometry|audiolog)\b", 4),
    (r"\b(dispens(e|ing) (medication|prescription|drug)|fill prescription)\b", 4),

    # ── C. Education — teaching students/trainees directly ──────────────────────
    (r"\b(teach(ing)? (class|lesson|course|student|pupil|subject|skill))\b", 4),
    (r"\b(instruct(ing|ion)? (student|pupil|class|player|athlete|learner))\b", 4),
    (r"\b(tutor(ing)?|grade (paper|exam|assignment|test|student work))\b", 4),
    (r"\b(develop (curriculum|lesson plan|course material|educational material))\b", 3),
    (r"\b(conduct (class|lecture|lab|practicum|tutorial|training session "
     r"for student))\b", 4),
    (r"\b(coach (player|athlete|team|student))\b", 3),

    # ── D. Technical design / engineering / software development ────────────────
    (r"\b(design (product|circuit|component|prototype|structure|device|"
     r"equipment|machine|blueprint|schematic|layout|network|"
     r"algorithm|architecture))\b", 4),
    (r"\b(develop (software|application|app|code|program|prototype|product|"
     r"algorithm|model|system|tool|framework|database))\b", 4),
    (r"\b(write (software|code|program|script|algorithm|query))\b", 4),
    (r"\b(program(ming|s)?|debug(ging)?|test(ing)? (code|software|application|"
     r"system|program|function))\b", 4),
    (r"\b(architect(ing|ure|ural)? (system|solution|platform|network))\b", 3),
    (r"\b(engineer(ing|ed)? (solution|system|component|product|structure|"
     r"design))\b", 3),
    (r"\b(draw (plan|blueprint|schematic|technical drawing|diagram))\b", 3),
    (r"\b(draft (blueprint|technical plan|specification|engineering drawing))\b", 3),

    # ── E. Repair / maintenance of equipment / physical systems ─────────────────
    (r"\b(repair(ing|s)?|maintain(ing|ance)?|service(ing)?) "
     r"(machine|equipment|vehicle|engine|electrical|electronic|system|"
     r"structure|pipeline|aircraft|vessel|appliance)\b", 4),
    (r"\b(troubleshoot(ing)?|diagnos(e|ing)) (problem|fault|error|"
     r"malfunction|issue) (in|of|with|on) (machine|equipment|system|"
     r"vehicle|engine|electrical|electronic)\b", 4),
    (r"\b(replac(e|ing) (part|component|unit|filter|belt|bearing|seal|line))\b", 3),
    (r"\b(overhaul(ing)?|rebuild(ing)?|restore (equipment|vehicle|machine))\b", 4),

    # ── F. Operating machinery / vehicles / transport ────────────────────────────
    (r"\b(operat(e|es|ing)) (machine|equipment|vehicle|aircraft|vessel|"
     r"crane|forklift|truck|train|press|lathe|drill|pump|"
     r"conveyor|boiler|reactor|plant)\b", 4),
    (r"\b(drive|pilot|fly|navigate|captain|steer) (vehicle|aircraft|vessel|"
     r"truck|bus|train|car|ship|helicopter|plane)\b", 4),
    (r"\b(control (machine|process|equipment|system|flow|temperature|pressure|"
     r"speed|valve|switch|circuit))\b", 3),
    (r"\b(monitor(ing)? (equipment|machine|process|vessel|aircraft|system|"
     r"gauge|instrument|sensor) (operation|performance|status|reading))\b", 3),
    (r"\b(load(ing)?|unload(ing)?|move|transport|deliver) (cargo|freight|"
     r"goods|material|shipment|package)\b", 2),

    # ── G. Creative / content production (as primary output) ───────────────────
    (r"\b(write (article|story|book|novel|script|speech|editorial|column|"
     r"report content|creative|copy|text|content|narrative|broadcast))\b", 4),
    (r"\b(compos(e|ing) (music|song|score|piece|symphony))\b", 4),
    (r"\b(perform(ing)? (music|concert|show|play|act|routine|stunt))\b", 4),
    (r"\b(edit(ing)? (film|video|audio|photograph|article|manuscript|content|"
     r"broadcast))\b", 3),
    (r"\b(draw|sketch|illustrat(e|ing|ion)|render|paint(ing)?|sculpt(ing)?)\b", 3),
    (r"\b(photograph|film(ing)?|record (video|audio|performance|broadcast))\b", 3),
    (r"\b(animat(e|ing|ion)|creat(e|ing) (visual|graphic|multimedia|film|"
     r"animation))\b", 3),

    # ── H. Scientific research and laboratory work ──────────────────────────────
    (r"\b(conduct (research|experiment|study|clinical trial|lab test|"
     r"field study|survey|investigation))\b", 4),
    (r"\b(analyz(e|ing) (specimen|sample|data|result|measurement|tissue|"
     r"blood|soil|chemical|environmental|geological|biological))\b", 4),
    (r"\b(collect(ing)? (sample|specimen|data|measurement|reading|"
     r"observation))\b", 3),
    (r"\b(measur(e|ing)|calculat(e|ing)|quantif(y|ying)) "
     r"(sample|material|property|parameter|level|concentration)\b", 3),
    (r"\b(test(ing)? (sample|material|product|chemical|biological|"
     r"physical|mechanical|structural))\b", 3),
    (r"\b(survey (land|property|terrain|archaeological|geological|"
     r"hydrological))\b", 4),

    # ── I. Food preparation / hospitality delivery ────────────────────────────────
    (r"\b(prepar(e|ing|ation of) (food|meal|dish|beverage|order|ingredient|"
     r"recipe))\b", 4),
    (r"\b(cook(ing)?|bake|baking|grill(ing)?|fry(ing)?|roast(ing)?|"
     r"mix(ing)? (ingredient|dough)|brew(ing)?)\b", 4),
    (r"\b(serve (food|beverage|meal|customer|guest|patron|diner))\b", 3),

    # ── J. Direct service delivery to clients/customers/patients ────────────────
    (r"\b(provide (direct|personal|on-site|hands-on|face-to-face) service)\b", 3),
    (r"\b(assist (patient|client|customer|guest|student|visitor) (with|in) "
     r"(receiv|complet|access|navigat|fill))\b", 3),
    (r"\b(care for (patient|client|resident|elder|child|individual|animal))\b", 4),
    (r"\b(cut (hair|nail)|style (hair)|groom(ing)?|mas(sage|saging))\b", 4),
    (r"\b(clean(ing)? (building|facility|room|surface|equipment|vehicle))\b", 3),
    (r"\b(landscap(e|ing)|mow(ing)?|trim(ming)?|prune|maintain (ground|lawn|"
     r"garden|landscape))\b", 3),
    (r"\b(guard|secur(e|ing|ity)?|patrol|protect (facility|asset|person|"
     r"area|border))\b", 3),
    (r"\b(respond to (emergency|call|incident|accident|fire|crime|"
     r"medical))\b", 4),
    (r"\b(fight(ing)? fire|extinguish)\b", 4),

    # ── K. Sales / direct revenue-generating activity ─────────────────────────────
    # Revenue-generating work = output of the firm in sales sectors
    (r"\b(sell(ing)?|solicit(ing)?) (product|service|merchandise|insurance|"
     r"subscription|contract|account)\b", 3),
    (r"\b(demonstrate (product|feature|capability|service))\b", 3),
    (r"\b(close (sale|deal|transaction))\b", 4),

    # ── L. Veterinary / animal care ───────────────────────────────────────────────
    (r"\b(treat(ing)? (animal|pet|livestock)|veterinar(y|ian)|"
     r"diagnos(e|ing) (animal|pet|livestock))\b", 4),
    (r"\b(perform (surgery|procedure|examination) on (animal|pet|livestock))\b", 5),

    # ── M. Legal practice (core when producing the legal output itself) ──────────
    # Distinguishes from compliance administration (OVERHEAD)
    (r"\b(represent (client|defendant|plaintiff|accused) "
     r"(in court|at trial|before tribunal))\b", 5),
    (r"\b(argue (case|motion|appeal|brief) (in court|before judge|tribunal))\b", 5),
    (r"\b(draft (contract|brief|motion|pleading|will|deed|legal document|"
     r"legislation))\b", 3),
    (r"\b(conduct (deposition|cross.examination|examination|hearing))\b", 4),

    # ── N. Financial product delivery (core when producing financial output) ────
    (r"\b(analyz(e|ing) (investment|portfolio|security|stock|bond|"
     r"asset|risk|return|financial model|valuation))\b", 3),
    (r"\b(execut(e|ing) (trade|transaction|order)|trading)\b", 4),
    (r"\b(underwrit(e|ing|ten)|issu(e|ing) (insurance|loan|bond|security))\b", 4),
    (r"\b(assess (risk|creditworthiness|insurance claim|loan application))\b", 3),
    (r"\b(structur(e|ing) (deal|transaction|loan|portfolio))\b", 3),

    # ── O. Computing / IT (when producing the technology as output) ──────────────
    (r"\b(deploy(ing)? (application|system|service|infrastructure|model))\b", 3),
    (r"\b(configure (system|network|server|database|router|switch|firewall))\b", 3),
    (r"\b(maintain(ing)? (system|server|network|database|infrastructure|"
     r"application))\b", 2),
    (r"\b(analyz(e|ing) (data|dataset|log|network traffic|performance "
     r"metric|algorithm output))\b", 3),
    (r"\b(develop (model|algorithm|pipeline|machine learning|"
     r"neural network|AI system))\b", 4),
]

# ──────────────────────────────────────────────────────────────────────────────
# 2.  SCORING FUNCTION
# ──────────────────────────────────────────────────────────────────────────────

def classify_task(text: str) -> tuple[str, int, int]:
    """
    Returns (label, oh_score, cr_score).
    label is "OVERHEAD" or "CORE".
    """
    if not text or not isinstance(text, str):
        return ("CORE", 0, 0)

    t = text.lower()

    oh_score = sum(
        w for pat, w in OH_PATTERNS
        if re.search(pat, t, re.IGNORECASE)
    )
    cr_score = sum(
        w for pat, w in CR_PATTERNS
        if re.search(pat, t, re.IGNORECASE)
    )

    # Tie-breaking: tie or zero → CORE (most tasks describe what workers DO)
    # OVERHEAD requires a net positive OH signal.
    label = "OVERHEAD" if oh_score > cr_score else "CORE"
    return (label, oh_score, cr_score)


# ──────────────────────────────────────────────────────────────────────────────
# 3.  OCCUPATION-LEVEL AGGREGATION
# ──────────────────────────────────────────────────────────────────────────────

def mode_label(labels: list[str]) -> str:
    """Return the modal label.
    OVERHEAD requires > 35% of Core-type tasks classified as OVERHEAD.
    This threshold captures genuine administrative occupations while
    allowing production workers to have some overhead tasks (documenting
    results, scheduling) without flipping their label.
    Interpretation: an occupation is labeled OVERHEAD if more than 1 in 3
    of its O*NET core task statements are institutional/administrative in
    nature, not output-producing.
    """
    if not labels:
        return "CORE"
    oh = labels.count("OVERHEAD")
    n  = len(labels)
    return "OVERHEAD" if (oh / n) > 0.25 and oh >= 2 else "CORE"


def oh_share(labels: list[str]) -> float:
    if not labels:
        return float("nan")
    return labels.count("OVERHEAD") / len(labels)


# ──────────────────────────────────────────────────────────────────────────────
# 4.  EXCEL PROCESSING
# ──────────────────────────────────────────────────────────────────────────────

INPUT_PATH  = ("C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/"
               "DATA/Tasks/Task Statements.xlsx")
OUTPUT_PATH = ("C:/Users/wb631291/OneDrive - WBG/Desktop/PhD/"
               "DATA/Tasks/Task Statements_classified.xlsx")

print("Loading Task Statements.xlsx ...")
wb_in = openpyxl.load_workbook(INPUT_PATH, read_only=True, data_only=True)
ws_in = wb_in.active
rows  = list(ws_in.iter_rows(values_only=True))
header = rows[0]
data   = rows[1:]

# Column indices (0-based)
COL_OCC   = list(header).index("O*NET-SOC Code")
COL_TITLE = list(header).index("Title")
COL_TASK  = list(header).index("Task")
COL_TYPE  = list(header).index("Task Type")

print(f"  {len(data):,} task rows across {len(set(r[COL_OCC] for r in data)):,} occupations")
print(f"  Core task rows: {sum(1 for r in data if r[COL_TYPE]=='Core'):,}")

# ── Pass 1: classify each Core task row ──────────────────────────────────────
classified = []   # list of (task_class, oh_score, cr_score) | None per row
occ_core_labels = collections.defaultdict(list)   # occ_code → [labels]

for row in data:
    task_type = row[COL_TYPE]
    if task_type == "Core":
        label, oh, cr = classify_task(row[COL_TASK])
        classified.append((label, oh, cr))
        occ_core_labels[row[COL_OCC]].append(label)
    else:
        classified.append(None)   # Supplemental / None rows: no assessment

# ── Pass 2: occupation-level aggregation ─────────────────────────────────────
occ_mode  = {occ: mode_label(labels) for occ, labels in occ_core_labels.items()}
occ_share = {occ: oh_share(labels)   for occ, labels in occ_core_labels.items()}

# ──────────────────────────────────────────────────────────────────────────────
# 5.  WRITE OUTPUT EXCEL
# ──────────────────────────────────────────────────────────────────────────────
print("Writing classified Excel ...")

wb_out = openpyxl.Workbook()
ws_out = wb_out.active
ws_out.title = "Task Statements Classified"

# ── Style helpers ────────────────────────────────────────────────────────────
FILL_OH   = PatternFill("solid", fgColor="FFDDC1")  # salmon – OVERHEAD
FILL_CR   = PatternFill("solid", fgColor="C8E6C9")  # green  – CORE
FILL_NA   = PatternFill("solid", fgColor="F5F5F5")  # light grey – non-Core
FILL_HDR  = PatternFill("solid", fgColor="1A237E")  # dark blue header
FONT_HDR  = Font(color="FFFFFF", bold=True, size=10)
FONT_NORM = Font(size=9)
ALIGN_CTR = Alignment(horizontal="center", vertical="center", wrap_text=False)
ALIGN_LFT = Alignment(horizontal="left",   vertical="center", wrap_text=True)
THIN      = Side(style="thin", color="BDBDBD")
BORDER    = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)

new_cols = ["task_class", "oh_score", "cr_score", "occ_class", "occ_oh_share"]
all_headers = list(header) + new_cols

# Header row
for col_idx, h in enumerate(all_headers, start=1):
    cell = ws_out.cell(row=1, column=col_idx, value=h)
    cell.fill   = FILL_HDR
    cell.font   = FONT_HDR
    cell.border = BORDER
    cell.alignment = ALIGN_CTR

# Freeze header
ws_out.freeze_panes = "A2"

# Data rows
for row_idx, (row, cls) in enumerate(zip(data, classified), start=2):
    occ  = row[COL_OCC]
    task_type = row[COL_TYPE]

    if cls is not None:
        task_class, oh, cr = cls
        occ_c = occ_mode.get(occ, "")
        occ_s = occ_share.get(occ, "")
        occ_s_str = f"{occ_s:.3f}" if isinstance(occ_s, float) and not math.isnan(occ_s) else ""
    else:
        task_class, oh, cr = "", "", ""
        occ_c = occ_mode.get(occ, "")
        occ_s = occ_share.get(occ, "")
        occ_s_str = f"{occ_s:.3f}" if isinstance(occ_s, float) and not math.isnan(occ_s) else ""

    # Determine row fill
    if task_type == "Core":
        row_fill = FILL_OH if task_class == "OVERHEAD" else FILL_CR
    else:
        row_fill = FILL_NA

    new_vals = [task_class, oh if oh != "" else None,
                cr if cr != "" else None, occ_c, occ_s_str]
    all_vals = list(row) + new_vals

    for col_idx, val in enumerate(all_vals, start=1):
        cell = ws_out.cell(row=row_idx, column=col_idx, value=val)
        cell.fill   = row_fill
        cell.font   = FONT_NORM
        cell.border = BORDER
        cell.alignment = ALIGN_LFT

# ── Column widths ─────────────────────────────────────────────────────────────
col_widths = {
    1: 14,   # O*NET Code
    2: 38,   # Title
    3: 10,   # Task ID
    4: 90,   # Task (wide – main content)
    5: 12,   # Task Type
    6: 14,   # Incumbents Responding
    7: 12,   # Date
    8: 14,   # Domain Source
    9: 12,   # task_class
    10: 9,   # oh_score
    11: 9,   # cr_score
    12: 12,  # occ_class
    13: 14,  # occ_oh_share
}
for col_i, width in col_widths.items():
    ws_out.column_dimensions[get_column_letter(col_i)].width = width

# Row heights
ws_out.row_dimensions[1].height = 20
for i in range(2, len(data) + 2):
    ws_out.row_dimensions[i].height = 30

# ── Auto-filter ───────────────────────────────────────────────────────────────
ws_out.auto_filter.ref = ws_out.dimensions

# ── Summary sheet ──────────────────────────────────────────────────────────────
ws_sum = wb_out.create_sheet("Occupation Summary")

sum_header = ["O*NET-SOC Code", "Title", "occ_class", "occ_oh_share",
              "n_core_tasks", "n_overhead_tasks", "n_core_prod_tasks",
              "soc_minor"]

for col_idx, h in enumerate(sum_header, start=1):
    cell = ws_sum.cell(row=1, column=col_idx, value=h)
    cell.fill = FILL_HDR
    cell.font = FONT_HDR
    cell.border = BORDER
    cell.alignment = ALIGN_CTR
ws_sum.freeze_panes = "A2"

# Build occupation-level info
occ_title = {}
for row in data:
    occ_title[row[COL_OCC]] = row[COL_TITLE]

for sum_row, occ in enumerate(sorted(occ_core_labels.keys()), start=2):
    labels = occ_core_labels[occ]
    n_oh = labels.count("OVERHEAD")
    n_cr = labels.count("CORE")
    occ_c = occ_mode[occ]
    occ_s = occ_share[occ]
    soc_minor = occ[:4] if occ else ""

    fill = FILL_OH if occ_c == "OVERHEAD" else FILL_CR
    vals = [occ, occ_title.get(occ, ""), occ_c,
            round(occ_s, 3) if not math.isnan(occ_s) else "",
            len(labels), n_oh, n_cr, soc_minor]

    for col_idx, val in enumerate(vals, start=1):
        cell = ws_sum.cell(row=sum_row, column=col_idx, value=val)
        cell.fill = fill
        cell.font = FONT_NORM
        cell.border = BORDER
        cell.alignment = ALIGN_CTR

# Summary column widths
for c, w in [(1,14),(2,42),(3,12),(4,14),(5,14),(6,16),(7,16),(8,10)]:
    ws_sum.column_dimensions[get_column_letter(c)].width = w

ws_sum.auto_filter.ref = ws_sum.dimensions

# ──────────────────────────────────────────────────────────────────────────────
# 6.  PRINT DIAGNOSTICS
# ──────────────────────────────────────────────────────────────────────────────

core_task_results = [c for c in classified if c is not None]
n_oh_tasks = sum(1 for c in core_task_results if c[0] == "OVERHEAD")
n_cr_tasks = sum(1 for c in core_task_results if c[0] == "CORE")

occ_oh_count = sum(1 for v in occ_mode.values() if v == "OVERHEAD")
occ_cr_count = sum(1 for v in occ_mode.values() if v == "CORE")

print()
print("=== Task-level classification (Core tasks only) ===")
print(f"  OVERHEAD tasks : {n_oh_tasks:,}  ({100*n_oh_tasks/len(core_task_results):.1f}%)")
print(f"  CORE tasks     : {n_cr_tasks:,}  ({100*n_cr_tasks/len(core_task_results):.1f}%)")
print()
print("=== Occupation-level classification (mode) ===")
print(f"  OVERHEAD occs  : {occ_oh_count:,}  ({100*occ_oh_count/(occ_oh_count+occ_cr_count):.1f}%)")
print(f"  CORE occs      : {occ_cr_count:,}  ({100*occ_cr_count/(occ_oh_count+occ_cr_count):.1f}%)")
print()

# Spot-check: top OVERHEAD scoring tasks
print("=== Top 10 highest-OH-score tasks ===")
scored = [(data[i][COL_OCC], data[i][COL_TITLE], data[i][COL_TASK],
           classified[i][1], classified[i][2], classified[i][0])
          for i in range(len(data)) if classified[i] is not None]
top_oh = sorted(scored, key=lambda x: -x[3])[:10]
for occ, title, task, oh, cr, lbl in top_oh:
    print(f"  [{lbl}] OH={oh} CR={cr} | {title}: {task[:80]}...")

print()
print("=== Top 10 highest-CR-score tasks ===")
top_cr = sorted(scored, key=lambda x: -x[4])[:10]
for occ, title, task, oh, cr, lbl in top_cr:
    print(f"  [{lbl}] OH={oh} CR={cr} | {title}: {task[:80]}...")

print()
print("=== Sample OVERHEAD occupations (by mode) ===")
oh_occs = [(occ, occ_title.get(occ,""), occ_share[occ])
           for occ in occ_mode if occ_mode[occ]=="OVERHEAD"]
oh_occs.sort(key=lambda x: -x[2])
for occ, title, s in oh_occs[:15]:
    print(f"  {occ}  {title:<45}  OH-share={s:.2f}")

print()
print("=== Sample CORE occupations (by mode) ===")
cr_occs = [(occ, occ_title.get(occ,""), occ_share[occ])
           for occ in occ_mode if occ_mode[occ]=="CORE"]
cr_occs.sort(key=lambda x: x[2])
for occ, title, s in cr_occs[:15]:
    print(f"  {occ}  {title:<45}  OH-share={s:.2f}")

print()
wb_out.save(OUTPUT_PATH)
print(f"Saved → {OUTPUT_PATH}")
