% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto — Institutional Pragmatism Reading (Survival Capitulation Legitimated as Revelation)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   On 25 September 1890 Wilford Woodruff issued the statement later
 *   canonized as Official Declaration 1, suspending the contraction of new
 *   plural marriages after nearly fifty years in which the practice had been
 *   preached as an everlasting covenant. The suspension followed a decade of
 *   escalating federal pressure: the Edmunds and Edmunds-Tucker Acts had
 *   imprisoned hundreds of practitioners, dissolved the church's corporate
 *   charter, seized its property, disenfranchised many members, and made Utah
 *   statehood contingent on abandonment of the practice. This story
 *   instantiates ONE reading of that event — the institutional-pragmatism
 *   reading — under which the revelation narrative functioned as legitimation
 *   for a survival-driven capitulation already determined by the balance of
 *   coercive power. Under this reading the standing arrangement (canonical
 *   doctrine unchanged, public practice suspended, a limited number of
 *   marriages privately authorized between 1890 and 1904, then repudiated
 *   under congressional pressure) carries BOTH a genuine survival function —
 *   a single simultaneous reversal that preserved the institutional vehicle
 *   and shielded the membership from destruction — AND asymmetric costs:
 *   polygamist families bore the reversal, converts and members absorbed a
 *   managed narrative, and the survival dividends accrued to the leadership
 *   that negotiated and framed it. The sibling readings of the same kernel
 *   (genuine prophetic reinterpretation; bare exogenous override) are
 *   separate constraints with their own epsilon values, linked here rather
 *   than averaged. Epsilon is authored for the standing Manifesto arrangement
 *   as this reading assesses it — never for any arrangement this reading
 *   would endorse.
 *
 * KEY AGENTS:
 *   - - lds_first_presidency_and_twelve: agenda-setting beneficiary (institutional/constrained) — administers the suspension and captures the survival dividend
 *   - - us_federal_government: external enforcer-collector (institutional/arbitrage) — set the coercive terms and collects compliance
 *   - - pre_manifesto_polygamist_families: primary target (powerless/trapped) — bears the covenant-reversal costs
 *   - - post_manifesto_secret_marriage_participants: primary target (powerless/trapped) — stranded when private authorization was withdrawn
 *   - - deceived_rank_and_file_membership: epistemic target (powerless/identity_locked) — bears the managed-narrative costs
 *   - - continuance_apostles: elite target (organized/constrained) — surrendered standing rather than repudiate families
 *   - - historians_of_mormonism: analytical observer (analytical/analytical) — reconstructs the decision sequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.7).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto — Institutional Pragmatism Reading (Survival Capitulation Legitimated as Revelation)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '5353cfe5-7c37-4955-8c45-35a18653fe9a').
narrative_ontology:cs_kernel_codification('5353cfe5-7c37-4955-8c45-35a18653fe9a', fixed_text).
narrative_ontology:cs_authority_grounding('5353cfe5-7c37-4955-8c45-35a18653fe9a', lineage).
narrative_ontology:cs_interpretation_layer_present('5353cfe5-7c37-4955-8c45-35a18653fe9a').
narrative_ontology:cs_reading_relation('5353cfe5-7c37-4955-8c45-35a18653fe9a', plural_marriage_mandate__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_reading_relation('5353cfe5-7c37-4955-8c45-35a18653fe9a', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5353cfe5-7c37-4955-8c45-35a18653fe9a', foundational, revelation_claims_track_institutional_necessity).
narrative_ontology:cs_axiom_status(revelation_claims_track_institutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5353cfe5-7c37-4955-8c45-35a18653fe9a', revelation_claims_track_institutional_necessity, empirically_contingent).
narrative_ontology:cs_axiom('5353cfe5-7c37-4955-8c45-35a18653fe9a', foundational, institutional_survival_precedes_doctrinal_consistency).
narrative_ontology:cs_axiom_status(institutional_survival_precedes_doctrinal_consistency, holdable).
narrative_ontology:cs_axiom_grounding('5353cfe5-7c37-4955-8c45-35a18653fe9a', institutional_survival_precedes_doctrinal_consistency, instrumental).
narrative_ontology:cs_reference_frame('5353cfe5-7c37-4955-8c45-35a18653fe9a', institutional_continuity_discretionary_adaptation).
narrative_ontology:cs_drift_state('5353cfe5-7c37-4955-8c45-35a18653fe9a', reed_smoot_hearing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5353cfe5-7c37-4955-8c45-35a18653fe9a', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_twelve).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, us_federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, pre_manifesto_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_secret_marriage_participants).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_rank_and_file_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, continuance_apostles).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_supersedes_commandment_claim).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, revelation_as_legitimation_vehicle_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the September 1890 statement suspending new plural marriages after nearly forty years in which the practice had been preached as an everlasting covenant, and framed the reversal as the fruit of a revelation to the church president. Administered what followed: publicly denied that any new plural marriages were being performed while privately authorizing a limited number through trusted subordinates in Mexico, Canada, and aboard ships; controlled what the general membership was told about these ceremonies; and after 1904 reversed course, retiring the apostles who had officiated and enforcing uniform compliance. Collected the survival of the corporate church, restored legal standing, and the political pathway that produced Utah statehood in 1896.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_twelve, agenda_setter,
    institutional, generational, constrained, continental).

% Drove the anti-polygamy campaign of the 1880s: imprisoned hundreds of practitioners, dissolved the church's corporate charter, seized its property, disenfranchised many members, and made Utah statehood conditional on abandoning the practice. Set the terms the church adapted to and collected the results: cessation of plural marriage, confirmation of federal supremacy over territorial religious governance, and closure of the Utah question without further confrontation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, us_federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, us_federal_government, agenda_setter).

% Households numbering in the thousands that entered plural marriage between the 1840s and 1890 as a religious duty, at the cost of poverty, prosecution, and hardship. After 1890 they were instructed to end cohabitation with plural wives; some dissolved their households, some continued in secret while federal prosecutions ran into the mid-1890s, and some moved to colonies in Mexico and Canada. Sacrifices made under the earlier teaching were voided without compensation, apology, or release ritual.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, pre_manifesto_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Couples sealed in plural marriages between 1890 and 1904 under private authorization from senior church figures, with ceremonies performed in northern Mexico, in Canadian settlements, and aboard ocean vessels, while the church's public position denied that any such marriages were taking place. When the 1904 Second Manifesto repudiated the practice, these families became liabilities: participants faced disciplinary scrutiny, the apostles who had officiated were pushed from their positions, and some households emigrated to remote colonies to keep their unions intact.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_secret_marriage_participants, payer,
    powerless, biographical, trapped, continental).

% The general membership, including European converts recruited by missionaries during the very years the private ceremonies continued, understood the 1890 statement as a permanent, divinely willed end to plural marriage. Many had given property, safety, and family ties for the principle and now accepted its reversal as the will of God. How much post-1890 practice had continued became widely known only when congressional hearings published testimony about it after 1904.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_rank_and_file_membership, payer,
    powerless, biographical, identity_locked, global).

% Senior church officers, several of them members of the Quorum of the Twelve, who performed or stood behind the post-1890 ceremonies believing them authorized. When congressional investigation hardened public policy, each had to choose between surrendering his position and repudiating the marriages he had blessed; two gave up their quorum seats rather than denounce the families involved.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, continuance_apostles, payer,
    organized, biographical, constrained, continental).

% Academic and independent researchers who reconstruct the decision sequence from diaries, private correspondence, temple and colony records, and hearing transcripts, weighing the revelation account against the documented trail of legal counsel, negotiation, and timing.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, historians_of_mormonism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_twelve).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a genuine collective-action problem: under existential external pressure, several thousand scattered households and a hierarchical church needed to reverse a core practice simultaneously and credibly enough to satisfy a hostile sovereign. A single authoritative statement, framed in the community's own theological idiom, moved the entire population onto one compliant posture at once — something no piecemeal household decision could have achieved.
% TRANSFER_FUNCTION: Covenant compliance and belief-alignment moved upward from the membership: polygamist households surrendered covenanted family arrangements, and converts and members surrendered the expectation of a principle they had sacrificed for. Institutional survival, restored legal standing, and the Utah statehood pathway moved outward from the leadership's negotiation to the whole community, with the leadership retaining discretionary control over which marriages remained privately recognized until 1904.
% ABSENT_VOICES: Rank-and-file members voted on nothing; plural wives were never asked to consent to the dissolution of their households; the apostles who opposed the hard line testified only when Congress subpoenaed them; European converts were recruited during the very years the private ceremonies continued, without disclosure. Unanimity inside the arrangement was produced partly by keeping these seats out of the room.
% DISAPPEARANCE_RATIONALE: Had the suspension vanished overnight — say, in 1893 — federal prosecutions would have resumed at full intensity, the church's remaining property and its temples stood forfeit, Utah statehood recedes indefinitely, and the community fractures between compliance and exile factions; the institutional vehicle that carried the community through the twentieth century does not exist in that branch.
% FOUNDING_PROBLEM: Existential coercion: by 1890 the federal government had imprisoned hundreds of polygamists, seized church assets, dissolved the church's charter, disenfranchised members, and withheld statehood; continued practice meant destruction of the institutional church.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: federal court and marshal records document the imprisonments and seizures; the Reed Smoot hearing transcripts (1904-1907) document both the coercion's history and the post-1890 practice from witnesses with no incentive to flatter the leadership; contemporaneous non-Mormon press and later academic histories converge on the coercion-and-resolution account. No source outside the beneficiary set attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70 at interval end) because the arrangement's costs fell on people with no voice in the reversal: families ordered to dissolve covenanted households, couples married under private authorization and then stranded by its repudiation, and a membership taught one story while another was practiced. Suppression (0.68) is authored as a raw structural property and is deliberately NOT scaled by power or scope: it reflects the internal enforcement machinery — disciplinary action against continuers, retirement of the apostles who officiated, and tight information control — that held the public-private alignment together. Theater (0.55) reflects the share of observable activity devoted to maintaining the revelation framing (epistles, testimonies, retrospective canonization in 1908) relative to the operational work of negotiation and compliance. Accessibility collapse is moderate (0.45) because alternatives did not vanish when the arrangement was understood — defiance, mass exile, and schism remained comprehensible and partially exercised (Mexico and Canada colonies, apostolic refusal, the later fundamentalist secessions) — they were priced, not erased; correspondingly the arrangement met real resistance (0.60). Coordination type is authored as enforcement_mechanism: the arrangement's dominant function was enforcing a synchronized reversal of communal practice under sovereign pressure, and its failure mode (any household continuing openly) destroyed the church outright. The temporal series run on one shared eight-point grid (1890-1910, three-year steps with 1904 pinned) so no metric borrows another's end-state values; extraction and suppression peak at the 1904 Second Manifesto, when the accumulated gap was forcibly closed and its participants stranded, then settle to a lower plateau as the arrangement shifts from survival mechanism to narrative maintenance. The shape is a two-phase step (accumulation to 1904, settlement after) rather than an oscillation, so the intermittent-reinforcement concern does not apply; the 1904 discontinuity is the interval's load-bearing event. Assumptions: the interval stops at 1910 to capture the settlement plateau without importing the 1920s-30s fundamentalist schisms, which belong to descendant constraints; measurement values are authored judgments from the documentary record, not instrument readings.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the leadership seat the arrangement is the survival mechanism that saved everything worth saving — the temples, the priesthood line, the community itself — and the revelation framing is the price of moving a people who could not have been moved by a legal notice. From the polygamist-family seats the same documents read as an order to unmake their households under a signature claiming divine authority for what the federal marshal had demanded. From the deceived-member seat the arrangement is an epistemic injury: years of sacrifice redirected by a narrative whose timing matched the district attorney's calendar more closely than any discernible revelatory cycle. The federal seat experiences neither the costs nor the survival function — only successful policy. Two structural contrasts sharpen the divergence: inter-institutionally, leadership and federal government hold similar nominal power but opposite exit profiles (the church's assets, temples, and anchored flock made its exit constrained; the government could calibrate enforcement intensity at will, an arbitrage position); at the same institutional level, the agenda-setting presidency and the continuance apostles sat in the same Quorum yet on opposite sides of the private authorizations, and the apostles' constrained exit (resignation at the cost of lifelong standing) is what finally enforced public-private alignment. Coalition potential among the powerless victim classes was structurally blocked: the deceived majority could not coordinate with the stranded minority until the hearings exposed the gap to both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation. The leadership sits near the beneficiary pole: it captures the survival dividend, the restored legal standing, and the statehood pathway, and it controls both the rules and the information environment. The federal government sits near the same pole: it collects compliance and closure without bearing the arrangement's internal costs. The four payer groups sit near the target pole, ordered by trapping depth. The post-Manifesto secret-marriage participants are the most thoroughly trapped — their marriages were valid only under an authorization that was then publicly withdrawn, so their exit collapsed retroactively. The pre-1890 polygamist families follow: covenant obligation, kinship structure, and community membership locked their exits, and the 1893 amnesty was conditioned on precisely the compliance they could not freely refuse. The deceived membership carries identity-lock amplification: the fusion is soteriological and ideological (exit meant losing both salvation framework and entire social world), which binds them nearer the full-target end than mobility alone would predict. The continuance apostles were organized enough to exercise a costly exit — two resigned rather than repudiate the families they had sealed — which moderates but does not remove their target position. Continental scope with ceremonies performed across three national jurisdictions is exactly the large-scope verification failure the scope modifier prices in: no single observer could audit what was being solemnized in Colonia Juarez, aboard transatlantic steamers, and in Salt Lake City at the same time, and the arrangement's persistence depended on that audit failure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — existential federal pressure — was dead by roughly 1907: statehood had been achieved in 1896, the Smoot hearings closed with compliance confirmed, and no further capitulation was ever demanded of the church. What persisted was the legitimation narrative, canonized in 1908 and maintained thereafter. Reading the arrangement as pure coordination would miss the costs (the stranded families, the managed membership); reading it as pure imposition would miss the genuine survival function that made the trade rational for nearly everyone who made it. The entanglement is the datum: the same document that preserved the institution redistributed its costs downward onto those least able to refuse. The dead-problem-plus-rearranging-world combination is flagged honestly here rather than laundered as enduring necessity — the arrangement outlived its function and persists as memory management, which is exactly the signature the mismatch consumer exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Which causal account of the 1890 suspension is structurally true — strategic capitulation wearing a revelation narrative (this reading), genuine prophetic reinterpretation (endogenous reading), or bare exogenous override with no doctrinal content (exogenous override reading)?',
    'Documentary sequencing: compare the dates of legal counsel, negotiation, and private deliberation against the dates and content of the revelation accounts, weighting private diaries and correspondence over public epistles.',
    'If the endogenous account is sustained, this story''s measured extraction falls sharply (the deception charge collapses) and the arrangement moves toward an adaptive profile with minimal asymmetry; if the exogenous account is sustained, the legitimation layer drops out and the arrangement reads as a purely imposed reversal. This reading''s dual structure survives only if both the survival function and the managed-belief costs are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the three-way contest over what the 1890 suspension actually was; this file instantiates one reading of the kernel plural_marriage_mandate and does not hedge across siblings.').

omega_variable(
    mset_gap_width_extraction_coupling,
    'How wide was the gap between official doctrine and authorized practice between 1890 and 1904, and does gap width track the measured extraction trajectory?',
    'Reconstruct the population of post-1890 plural marriages from Mexican colony records, temple records, ship manifests, and later witness testimony; plot the cumulative count against the measurement series.',
    'A wide gap supports this reading''s claim that the doctrine-practice divergence is the primary observable and sustains high extraction estimates; a negligible gap collapses the deception-based victim class and lowers the reading''s extraction toward the endogenous profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mset_gap_width_extraction_coupling, empirical, 'Width of the M-set gap (doctrine unchanged, practice suspended, secret continuations) as the primary observable of this reading.').

omega_variable(
    leader_sincerity_ambiguity,
    'Did the church president experience the revelation he reported, or was the revelation narrative constructed after the decision to capitulate had effectively been made?',
    'Diary and correspondence analysis surrounding September and October 1890, including private statements to associates about legal necessity versus divine initiative.',
    'A constructed narrative confirms this reading''s legitimation thesis at full strength; a sincere revelation experienced under duress softens the deception component and shifts weight toward the endogenous reading without eliminating the strategic element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leader_sincerity_ambiguity, empirical, 'Sincerity of the revelation framing versus its instrumental construction.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Were rank-and-file members genuinely deceived about the continuation of plural marriage after 1890, or was a don''t-ask culture of willful ignorance in place that redistributes responsibility?',
    'Period discourse analysis: what ordinary members, mission presidents, and local leaders said and printed about post-1890 marriages before the congressional hearings made the practice undeniable.',
    'If knowledge was widespread, the deceived-monogamist victim class shrinks, costs concentrate on the polygamist families, and the asymmetry becomes narrower and harsher; if knowledge was tightly held, the epistemic victim class stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, empirical, 'Boundary of the deceived-monogamist victim class.').

omega_variable(
    survival_justification_weight,
    'Does institutional survival justify reversing a practice the community had been taught was an everlasting covenant — and who is entitled to make that trade?',
    'Not resolvable by evidence; turns on prior commitments about institutional continuity versus covenant fidelity. Descendant communities (mainline membership, fundamentalist offshoots) weigh the trade retrospectively and disagree.',
    'A survival-first weighting ratifies the leadership''s trade and keeps the survival half of the arrangement load-bearing; a covenant-fidelity weighting condemns the trade and pushes the whole arrangement toward the extraction pole regardless of survival outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_justification_weight, preference, 'Value question underlying the legitimacy of the survival-versus-covenant trade.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmm_ipr_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1890, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1893, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1893, 0.54).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1893, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.58).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1896, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1899, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1899, 0.61).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1899, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.64).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1902, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.65).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1904, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1907, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1907, 0.6).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1907, observed).
narrative_ontology:measurement(pmm_ipr_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.55).
narrative_ontology:measurement_basis(pmm_ipr_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(pmm_ipr_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement_basis(pmm_ipr_be_t1890, observed).
narrative_ontology:measurement(pmm_ipr_be_t1893, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1893, 0.66).
narrative_ontology:measurement_basis(pmm_ipr_be_t1893, observed).
narrative_ontology:measurement(pmm_ipr_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.7).
narrative_ontology:measurement_basis(pmm_ipr_be_t1896, observed).
narrative_ontology:measurement(pmm_ipr_be_t1899, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1899, 0.72).
narrative_ontology:measurement_basis(pmm_ipr_be_t1899, observed).
narrative_ontology:measurement(pmm_ipr_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.75).
narrative_ontology:measurement_basis(pmm_ipr_be_t1902, observed).
narrative_ontology:measurement(pmm_ipr_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement_basis(pmm_ipr_be_t1904, observed).
narrative_ontology:measurement(pmm_ipr_be_t1907, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1907, 0.74).
narrative_ontology:measurement_basis(pmm_ipr_be_t1907, observed).
narrative_ontology:measurement(pmm_ipr_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.7).
narrative_ontology:measurement_basis(pmm_ipr_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(pmm_ipr_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement_basis(pmm_ipr_su_t1890, observed).
narrative_ontology:measurement(pmm_ipr_su_t1893, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1893, 0.7).
narrative_ontology:measurement_basis(pmm_ipr_su_t1893, observed).
narrative_ontology:measurement(pmm_ipr_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.72).
narrative_ontology:measurement_basis(pmm_ipr_su_t1896, observed).
narrative_ontology:measurement(pmm_ipr_su_t1899, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1899, 0.74).
narrative_ontology:measurement_basis(pmm_ipr_su_t1899, observed).
narrative_ontology:measurement(pmm_ipr_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.76).
narrative_ontology:measurement_basis(pmm_ipr_su_t1902, observed).
narrative_ontology:measurement(pmm_ipr_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.78).
narrative_ontology:measurement_basis(pmm_ipr_su_t1904, observed).
narrative_ontology:measurement(pmm_ipr_su_t1907, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1907, 0.72).
narrative_ontology:measurement_basis(pmm_ipr_su_t1907, observed).
narrative_ontology:measurement(pmm_ipr_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement_basis(pmm_ipr_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto' covers three structurally distinct causal claims; per the epsilon-invariance principle they are authored as three stories sharing the kernel plural_marriage_mandate. This file instantiates the institutional-pragmatism reading (revelation narrative as legitimation of strategic capitulation; epsilon 0.70; beneficiaries leadership and federal government; victims spanning three member classes plus dissenting apostles). The endogenous reading authors low extraction over the same referent (legitimate adaptation); the exogenous-override reading authors high extraction with the legitimation layer removed (pure imposed reversal). Upstream/downstream structure: the endogenous reading is the account the institution itself teaches and is cited against this reading's deception claims; the exogenous-override reading shares this reading's coercion premise and differs only on whether the narrative element adds a distinct cost layer. Edges here link siblings only; averaging across readings is prohibited.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
