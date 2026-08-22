% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Technological-Determinist Reading: Print as Autonomous Cause of Reformation Success
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the question of what caused the Reformation's spread and durability; this
 *   file generates the technological_determinism reading: the printing press
 *   as an autonomous enabling technology whose affordances made vernacular
 *   scripture spread and Reformation success inevitable. Under this reading
 *   the technology is the fixed, lawlike term and human actors are downstream
 *   responders; the beneficiary structure of the resulting account is
 *   obscured, because inevitability converts contingent strategic gains into
 *   natural outcomes that no one chose and no one can be credited with. The
 *   constraint modeled here is the deterministic causal thesis AS IT OPERATES
 *   IN DISCOURSE — in survey textbooks, curricula, peer review, and popular
 *   history — where it functions as a maintained interpretive arrangement
 *   with real beneficiaries and real costs, not merely as a sentence in a
 *   monograph. The claim/metric gap is deliberate and is the measurement this
 *   corpus exists to take: the reading CLAIMS mountain (technology as natural
 *   force, per the reading's own lights), while the authored metrics describe
 *   an enforced, beneficiary-bearing canon whose persistence depends on
 *   active gatekeeping. The engine evaluates that divergence; the claim is
 *   not reconciled to the metrics. KEY AGENTS (by structural relationship): -
 *   textbook_canon_editors: agenda-setter (institutional/mobile) — selects
 *   and maintains the causal spine across editions -
 *   scholarly_journal_gatekeepers: co-agenda-setter
 *   (institutional/constrained) — enforces frame fit through review -
 *   media_determinist_scholarship: primary beneficiary
 *   (powerful/identity_locked) — collects authority, chairs, and citation
 *   flows; professionally fused with the frame - survey_textbook_publishers:
 *   beneficiary (institutional/arbitrage) — monetizes the frame's stability
 *   across markets - protestant_heritage_institutions: indirect beneficiary
 *   (institutional/constrained) — inherits legitimacy from the inevitability
 *   backdrop - agency_centered_historians: primary target
 *   (organized/constrained) — bears foreclosure of the survey-level
 *   interpretive space - history_students: target (powerless/trapped) — bears
 *   the flattened causal picture as settled fact - general_reading_public:
 *   diffuse target (powerless/mobile) — bears miscalibration about media and
 *   social change - sixteenth_century_print_entrepreneurs: excluded voice
 *   (dead; their letters, colophons, and account books show deliberate
 *   strategy) - historiography_observers: analytical observer — watches the
 *   frame's enforcement and erosion without taking a side in the causation
 *   dispute Family note: the colloquial label 'the printing press caused the
 *   Reformation' decomposes, per the epsilon-invariance principle, into three
 *   structurally distinct constraint stories sharing the
 *   press_reformation_causality kernel — this determinist reading, a
 *   strategic_deployment reading (reformers and printers weaponized print
 *   toward religious and commercial goals), and a co_constitution reading
 *   (print economy and religious controversy shaped each other through
 *   feedback loops). Each file carries its own epsilon, beneficiary set, and
 *   victim set; they are linked through network edges, not merged.
 *
 * KEY AGENTS:
 *   - textbook_canon_editors: agenda-setter (institutional/mobile) — decides which causal claims are presented as settled background
 *   - scholarly_journal_gatekeepers: co-agenda-setter (institutional/constrained) — enforces frame fit at the review stage
 *   - media_determinist_scholarship: primary beneficiary (powerful/identity_locked) — collects the frame's authority rents; exit would dissolve professional identity
 *   - survey_textbook_publishers: beneficiary (institutional/arbitrage) — profits from narrative stability and can rebrand if markets move
 *   - protestant_heritage_institutions: indirect beneficiary (institutional/constrained) — their founding reads as history's direction under the frame
 *   - agency_centered_historians: primary target (organized/constrained) — demoted to color commentary in surveys
 *   - history_students: target (powerless/trapped) — examined on the arrow as fact
 *   - general_reading_public: diffuse target (powerless/mobile) — supplied predominantly with the arrow
 *   - sixteenth_century_print_entrepreneurs: excluded voice (powerless/trapped) — archival testimony of strategy, structurally outside the conversation
 *   - historiography_observers: analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.58).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.42).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Technological-Determinist Reading: Print as Autonomous Cause of Reformation Success").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__technological_determinism).
domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'a1d9205b-4001-4564-bfee-dca34f916777').
narrative_ontology:cs_kernel_codification('a1d9205b-4001-4564-bfee-dca34f916777', formalized).
narrative_ontology:cs_authority_grounding('a1d9205b-4001-4564-bfee-dca34f916777', expertise).
narrative_ontology:cs_interpretation_layer_present('a1d9205b-4001-4564-bfee-dca34f916777').
narrative_ontology:cs_reading_relation('a1d9205b-4001-4564-bfee-dca34f916777', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_reading_relation('a1d9205b-4001-4564-bfee-dca34f916777', press_reformation_causality__strategic_deployment, influences).
narrative_ontology:cs_axiom('a1d9205b-4001-4564-bfee-dca34f916777', foundational, artifact_affordance_sufficiency).
narrative_ontology:cs_axiom_status(artifact_affordance_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a1d9205b-4001-4564-bfee-dca34f916777', artifact_affordance_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('a1d9205b-4001-4564-bfee-dca34f916777', foundational, human_agency_downstream_position).
narrative_ontology:cs_axiom_status(human_agency_downstream_position, holdable).
narrative_ontology:cs_axiom_grounding('a1d9205b-4001-4564-bfee-dca34f916777', human_agency_downstream_position, empirically_contingent).
narrative_ontology:cs_reference_frame('a1d9205b-4001-4564-bfee-dca34f916777', press_as_autonomous_causal_agent).
narrative_ontology:cs_drift_state('a1d9205b-4001-4564-bfee-dca34f916777', contemporary_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a1d9205b-4001-4564-bfee-dca34f916777', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, media_determinist_scholarship).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, survey_textbook_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_heritage_institutions).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, agency_centered_historians).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, history_students).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, general_reading_public).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_autonomy_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, medium_determinism_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, print_necessity_sufficiency_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commission, edit, and revise the survey textbooks and digital course packages through which most students meet the Reformation. They decide which causal claims appear as settled background and which appear as open questions. They can shift the framing between editions when scholarship or markets demand it, but each shift carries adoption risk across thousands of syllabi, so the spine tends to persist.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, textbook_canon_editors, agenda_setter,
    institutional, generational, mobile, global).

% Editors and referees at flagship history and media-studies journals. They accept or reject work partly on its fit with established causal frames, enforcing through review cycles and revision demands rather than edicts. Their careers ride on the journals' authority, which rests on the perceived settledness of the field's core narratives.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, scholarly_journal_gatekeepers, agenda_setter,
    institutional, biographical, constrained, continental).

% Senior scholars, endowed chairs, and research programs in the lineage running from mid-century communication theory through the 1970s print-history synthesis. They collect citations, prize-committee seats, and canonical status from the frame's dominance. Their professional self-concept is fused with the medium-first explanatory tradition: abandoning the frame would dissolve the identity their careers were built on, so they defend it even as the evidential ground shifts beneath it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, media_determinist_scholarship, beneficiary,
    powerful, generational, identity_locked, continental).

% Commercial publishers of survey texts and courseware. A single clean causal arrow lowers authoring, editing, and marketing costs and travels across national markets unchanged. They profit from the frame's stability and can rebrand quickly toward whatever narrative sells next; their loyalty is to the stability, not to the specific claim.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, survey_textbook_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Denominations, museums, anniversary commissions, and national-memory bodies whose founding appears, under the inevitability narrative, as history's direction rather than as a contingent political and religious choice. They fund documentaries, commemorations, and educational materials that retell the arrow, and their legitimacy claims quietly depend on the outcome having been unavoidable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_heritage_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Scholars of religious politics, princely strategy, censorship regimes, patronage, and printer-commercial calculation. Their findings appear in respected specialist monographs, but in surveys their work is demoted to color commentary on an outcome the medium has already decided. Exiting means building parallel survey infrastructure against an entrenched canon or drifting to the field's margins; staying means accepting subordinate billing for their central subject matter.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, agency_centered_historians, payer,
    organized, biographical, constrained, continental).

% Encounter the causal arrow as settled fact in required surveys and are examined on it. They bear the cognitive cost of a flattened picture of how media technology and social change interact, and they cannot opt out of the canonical frame without examination penalty. Most never learn that the arrow is contested.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, history_students, payer,
    powerless, immediate, trapped, global).

% Consume popular histories, documentaries, and anniversary programming structured by the arrow. They carry a durable miscalibration about the relationship between communication technology and social transformation. Nominally free to read otherwise, but the popular-history supply side is dominated by the frame, so their practical menu is narrow.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, general_reading_public, payer,
    powerless, immediate, mobile, global).

% The printers, patrons, colporteurs, and financier-backers whose surviving letters, colophons, privilege petitions, and account books show deliberate commercial and confessional calculation — edition choices timed to feast days, editions pitched to urban versus rural buyers, calculated risks under imperial censorship. Dead for centuries, they cannot object to being cast as mere transmission vectors of an autonomous process, and their archives sit in specialist repositories far from the survey conversation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, sixteenth_century_print_entrepreneurs, excluded,
    powerless, biographical, trapped, continental).

% Philosophers and sociologists of historical knowledge who study how causal frames stabilize, get enforced, and erode. They take no side in the causation dispute itself; they watch the gatekeeping, track the citation flows, and can see the full structure — including the gap between what the frame claims and what maintains it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historiography_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__technological_determinism, media_determinist_scholarship).
narrative_ontology:fixing_cost_class(press_reformation_causality__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Compresses a century of entangled religious, political, commercial, and technological development into a single teachable causal arrow, allowing survey courses, textbooks, documentaries, and popular accounts to coordinate on one shared explanatory spine instead of fragmenting into incompatible local narratives.
% TRANSFER_FUNCTION: Moves interpretive authority and canonical attention from agency-centered scholarship to medium-centered scholarship; moves rhetorical closure (inevitability) to the successor institutions of the era's winners; moves cognitive simplicity to students and general readers at the cost of causal granularity.
% ABSENT_VOICES: Sixteenth-century printers, patrons, and polemicists — whose surviving correspondence and account books show deliberate strategy — are dead and structurally outside the conversation; regionally minded revisionists and Catholic-print historians speak from specialist venues the survey apparatus does not reach. Their absence is what lets unanimity about the arrow look like settled knowledge rather than the product of a filtered room.
% DISAPPEARANCE_RATIONALE: If the deterministic frame vanished overnight, survey curricula, textbook causal spines, media-studies syllabi, and the popular-history supply chain would lose their organizing arrow and require reauthoring; the strategic-deployment and co-constitution research programs would move from corrective margins toward the explanatory center; heritage commemorations would lose the inevitability backdrop against which their legitimacy claims are staged.
% FOUNDING_PROBLEM: Early twentieth-century secularizing historiography needed a non-providential explanation for why the Reformation spread so quickly and proved irreversible; attributing the outcome to a material technology answered that need by making religious upheaval downstream of engineering.
% FOUNDING_PROBLEM_CORROBORATION: The underlying explanatory problem — why the Reformation succeeded — remains open and is attested from outside the frame's beneficiary set: Febvre and Martin's L'Apparition du livre (1958) posed the print question while explicitly denying technological sufficiency; Adrian Johns's The Nature of the Book (1998) and Andrew Pettegree's Brand Luther (2015) attest, from outside the benefiting parties, both that the problem is live and that the deterministic answer is insufficient; Catholic-Reformation print scholarship corroborates that the same presses served the opposite confessional outcome. No party outside the frame's beneficiaries attests that the deterministic answer settles the problem.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58 at interval end: the frame's costs are interpretive rather than monetary — agency-centered scholarship loses survey-level territory, contingency is converted into inevitability (which launders the winners' strategic gains), and attention and citation flows are channeled medium-ward. That places it well above a pure coordination arrangement but below coercive extraction, since nothing material is seized. Suppression is 0.42: enforcement is soft-power gatekeeping (referee reports, canon selection, syllabus inertia) that leaves rival programs alive in specialist venues while closing the survey-level space. Theater ratio is 0.48: a large share of the frame's discursive activity is ritual invocation — 'the print revolution changed everything' — which adds rhetorical closure without analytical content, while the necessity core (mass vernacular scripture required print) does real organizational work. Accessibility collapse is 0.6: once the frame is absorbed, alternative explanations register as noise around a signal, but the revisionist record keeps reopening the question, so alternatives never fully disappear. Resistance is 0.6: a sustained, organized revisionist program (from Febvre and Martin's original denial of sufficiency through Johns's critique of print-culture stability to Pettegree's printer-centered account) has contested the frame for decades. The temporal series run on one shared eight-point grid (1955-2025) so every tracked metric is authored at every examined time point; the trajectories show emergence (Febvre/Martin's question, McLuhan's popularization), canonization peaking around Eisenstein's 1979 synthesis, a plateau of peak enforcement around 1990, and partial erosion as revisionist and regional scholarship gained footholds. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change: hardening through canonization, then softening as the frame's monopoly eroded.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structural data. From the media_determinist_scholarship seat, the frame is transparent description — the technology really did have those affordances, so holding the view costs nothing and extracts nothing; that seat computes something close to a natural regularity. From the agency_centered_historians seat, the same frame operates as enforced foreclosure: their findings are real but structurally demoted, and the interpretive space they occupy shrinks to footnotes. Textbook editors experience the frame as product stability — a reliable spine that lowers authoring risk across editions. Students experience it as settled fact they are examined on. The engine derives these per-seat classifications from power, exit options, and declared position; the authored mountain claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Media_determinist_scholarship sits nearest the beneficiary end: the frame subsidizes its authority, and identity-lock means the subsidy persists even as the frame's evidential base erodes. Survey_textbook_publishers benefit through reduced narrative risk and arbitrage-grade ability to rebrand. Protestant_heritage_institutions benefit indirectly — inevitability converts their founding from a contingent political-religious choice into history's direction — a secondary effect the raw derivation may underweight. Agency_centered_historians sit near the target end: organized enough to sustain journals and conferences, but constrained in exit because the survey infrastructure they would need to build duplicates an entrenched canon. History_students are trapped targets: examination systems bind them to the frame regardless of preference. The general_reading_public bears diffuse costs with nominal mobility, but the supply side of popular history is frame-dominated, so mobility is weaker than it looks. The sixteenth-century entrepreneurs are excluded rather than targeted: the frame misdescribes them, and they cannot answer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the whole frame as pure extraction would erase its genuine coordination core: compressing a chaotic century into one teachable causal arrow solves a real pedagogical coordination problem, and the print-necessity half of the claim is solid. Accepting the mountain claim at face value would hide the enforced, beneficiary-bearing structure — which is exactly what the false-summit signature tests, since the reading declares beneficiaries while asserting naturality. The founding problem (a non-providential explanation for the Reformation's speed and irreversibility) is still live, so the frame is not yet mandatrophy-resolved; but the erosion path is visible in the measurements: if the sufficiency premise dies while inevitability invocations persist, the frame drifts toward inertial performance, with the theater-ratio series as the leading indicator. The recent dip in theater (0.51 to 0.48) suggests the frame is shedding some ritual weight as scholarship granularizes, which cuts against imminent piton decay — the honest reading is contested terrain, not settled decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_frame,
    'Is the press-to-Reformation causal link a genuine historical law comparable to a physical regularity, or a constructed interpretive frame whose persistence is maintained by disciplinary incentives, textbook economics, and canon formation?',
    'Comparative regional analysis correlating print-market density with confessional outcome across European polities, plus natural experiments such as the Ottoman print ban and Catholic exploitation of the same presses; if confessional outcomes vary widely at constant print density, the lawlike reading fails.',
    'If the link is constructed, the mountain claim is a false summit and the frame reclassifies toward a hybrid coordination/extraction structure with visible beneficiaries; if genuine, the frame''s extraction measures reflect only the ordinary cost of a true description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_frame, empirical, 'Whether the deterministic causal claim is natural law or maintained construct.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (technological_determinism) of the press_reformation_causality kernel; what would the sibling readings (strategic_deployment, co_constitution) change structurally, and where exactly is the disagreement located?',
    'Locate the disputed element: the locus of causal efficacy (artifact affordance versus human strategy versus mutual constitution). Each sibling file authors its own epsilon, beneficiary set, and victim set over the same historical material.',
    'Under strategic_deployment, printers and reformers become agenda-setters and the frame''s beneficiaries become strategic actors rather than passive collectors; under co_constitution, the victim set dissolves into feedback participants and measured extraction drops sharply. The disagreement is located in causal-direction commitment, not in the evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel, which reading, where the readings diverge.').

omega_variable(
    necessity_vs_sufficiency_conflation,
    'Does the historical evidence establish print''s necessity for mass vernacular scripture (widely granted) or its sufficiency for Reformation success (contested)? Does the deterministic frame trade rhetorically on conflating the two?',
    'Separate the claims analytically: test sufficiency by seeking cases where print capacity existed without confessional rupture, and necessity by seeking rupture without print capacity; measure how much of the frame''s persuasive force depends on sliding between them.',
    'If only necessity holds, the inevitability extension is rhetorical addition rather than established structure, raising the frame''s theatrical share and lowering the defensible core of its claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_sufficiency_conflation, empirical, 'Whether the frame conflates a solid necessity claim with a contested sufficiency claim.').

omega_variable(
    counterexample_regional_record,
    'Do Catholic regions with dense print markets (Antwerp, Paris, Cologne) and Protestant conversions in thin-print territories falsify the sufficiency premise, or are they absorbable as boundary conditions?',
    'Systematic cross-regional dataset joining print output, censorship regimes, and confessional outcomes; adversarial review of whether the frame''s holders treat disconfirming regions as exceptions or as refutations.',
    'If disconfirming regions accumulate faster than boundary-condition patches, the frame''s foundational premise is empirically overridden and its enforcement becomes visibly defensive rather than descriptive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterexample_regional_record, empirical, 'Status of the regional counterexample record against sufficiency.').

omega_variable(
    inevitability_as_legitimacy_laundering,
    'Does retroactive inevitability function as legitimacy laundering for contingent gains — converting printers'' commercial calculations, princes'' jurisdictional seizures, and denominational foundings into history''s predetermined direction?',
    'Trace how inevitability language is deployed in heritage commemorations, anniversary programming, and denominational self-description, and whether removing it changes the legitimacy claims those institutions make.',
    'If laundering is confirmed, the frame''s effective extraction exceeds the measured value because its beneficiaries include institutions whose founding legitimacy depends on the inevitability narrative remaining unquestioned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_as_legitimacy_laundering, conceptual, 'Whether inevitability framing serves present-day legitimacy interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1955, press_reformation_causality__technological_determinism, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1955, observed).
narrative_ontology:measurement(pres_tr_t1962, press_reformation_causality__technological_determinism, theater_ratio, 1962, 0.26).
narrative_ontology:measurement_basis(pres_tr_t1962, observed).
narrative_ontology:measurement(pres_tr_t1970, press_reformation_causality__technological_determinism, theater_ratio, 1970, 0.33).
narrative_ontology:measurement_basis(pres_tr_t1970, observed).
narrative_ontology:measurement(pres_tr_t1979, press_reformation_causality__technological_determinism, theater_ratio, 1979, 0.41).
narrative_ontology:measurement_basis(pres_tr_t1979, observed).
narrative_ontology:measurement(pres_tr_t1990, press_reformation_causality__technological_determinism, theater_ratio, 1990, 0.46).
narrative_ontology:measurement_basis(pres_tr_t1990, observed).
narrative_ontology:measurement(pres_tr_t2002, press_reformation_causality__technological_determinism, theater_ratio, 2002, 0.5).
narrative_ontology:measurement_basis(pres_tr_t2002, observed).
narrative_ontology:measurement(pres_tr_t2013, press_reformation_causality__technological_determinism, theater_ratio, 2013, 0.51).
narrative_ontology:measurement_basis(pres_tr_t2013, observed).
narrative_ontology:measurement(pres_tr_t2025, press_reformation_causality__technological_determinism, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(pres_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1955, press_reformation_causality__technological_determinism, base_extractiveness, 1955, 0.34).
narrative_ontology:measurement_basis(pres_be_t1955, observed).
narrative_ontology:measurement(pres_be_t1962, press_reformation_causality__technological_determinism, base_extractiveness, 1962, 0.46).
narrative_ontology:measurement_basis(pres_be_t1962, observed).
narrative_ontology:measurement(pres_be_t1970, press_reformation_causality__technological_determinism, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement_basis(pres_be_t1970, observed).
narrative_ontology:measurement(pres_be_t1979, press_reformation_causality__technological_determinism, base_extractiveness, 1979, 0.63).
narrative_ontology:measurement_basis(pres_be_t1979, observed).
narrative_ontology:measurement(pres_be_t1990, press_reformation_causality__technological_determinism, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement_basis(pres_be_t1990, observed).
narrative_ontology:measurement(pres_be_t2002, press_reformation_causality__technological_determinism, base_extractiveness, 2002, 0.64).
narrative_ontology:measurement_basis(pres_be_t2002, observed).
narrative_ontology:measurement(pres_be_t2013, press_reformation_causality__technological_determinism, base_extractiveness, 2013, 0.61).
narrative_ontology:measurement_basis(pres_be_t2013, observed).
narrative_ontology:measurement(pres_be_t2025, press_reformation_causality__technological_determinism, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(pres_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1955, press_reformation_causality__technological_determinism, suppression_requirement, 1955, 0.28).
narrative_ontology:measurement_basis(pres_su_t1955, observed).
narrative_ontology:measurement(pres_su_t1962, press_reformation_causality__technological_determinism, suppression_requirement, 1962, 0.38).
narrative_ontology:measurement_basis(pres_su_t1962, observed).
narrative_ontology:measurement(pres_su_t1970, press_reformation_causality__technological_determinism, suppression_requirement, 1970, 0.47).
narrative_ontology:measurement_basis(pres_su_t1970, observed).
narrative_ontology:measurement(pres_su_t1979, press_reformation_causality__technological_determinism, suppression_requirement, 1979, 0.56).
narrative_ontology:measurement_basis(pres_su_t1979, observed).
narrative_ontology:measurement(pres_su_t1990, press_reformation_causality__technological_determinism, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement_basis(pres_su_t1990, observed).
narrative_ontology:measurement(pres_su_t2002, press_reformation_causality__technological_determinism, suppression_requirement, 2002, 0.54).
narrative_ontology:measurement_basis(pres_su_t2002, observed).
narrative_ontology:measurement(pres_su_t2013, press_reformation_causality__technological_determinism, suppression_requirement, 2013, 0.47).
narrative_ontology:measurement_basis(pres_su_t2013, observed).
narrative_ontology:measurement(pres_su_t2025, press_reformation_causality__technological_determinism, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(pres_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The colloquial label 'the printing press caused the Reformation' fails the epsilon-invariance test: measuring it as a claim about artifact affordances yields one epsilon; measuring it as a claim about strategic human deployment yields another; measuring it as mutual constitution yields a third. Three files, one kernel. This determinist reading is the upstream canonical frame: its decades of dominance shaped the resource environment in which the strategic_deployment program operates (as corrective, marginally funded work) — hence the influences edge. Against co_constitution the relation is stronger: the strong determinist premise (unidirectional artifact-to-outcome causation with an inevitable result) and the co-constitutive premise (bidirectional shaping with contingent outcome) cannot both be core commitments of a single explanatory framework. Sibling files carry their own epsilon, beneficiaries, and victims; nothing here hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
