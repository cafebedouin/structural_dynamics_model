% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Ceremonial Retention (Husk Reading)
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   After major disaster events expose gaps between certified preparedness
 *   and actual response competence, institutions formalize preparedness
 *   through visible programs: mandatory drills, systematic inspections,
 *   standardized certifications. Over time, these visible programs become the
 *   primary measure of preparedness itself. Resources shift from live skill
 *   transmission (apprenticeship, adaptive training, tacit knowledge
 *   transfer) to ceremonial compliance (drill scheduling, checklist
 *   completion, certification paperwork). The husk reading asserts this is a
 *   constraint in which the ceremonial apparatus persists not because it
 *   preserves competence but because it provides institutional legitimacy and
 *   because the actual competence gap is structurally invisible until
 *   disaster strikes. This reading contests the 'competence reading' (which
 *   holds that drills and inspections DO preserve competence) and competes
 *   with the 'hybrid reading' (which holds that specialized institutions
 *   retain competence while broader society becomes ceremonial). The husk
 *   reading's distinctive claim: preparedness IS performance—the
 *   competence-to-ceremony ratio is high, extraction is substantial, and the
 *   victim is actual response capacity.
 *
 * KEY AGENTS:
 *   - institutional_administrators: Set and defend the ceremonial apparatus as evidence of preparedness; benefit from visible compliance without resource burden of live competence maintenance
 *   - frontline_responders: Conduct ritual drills disconnected from real scenarios; bear the cost when actual competence fails during D5 events
 *   - emergency_management_planners: Design protocols that prioritize observable compliance over tacit skill retention; face institutional pressure to demonstrate quantitative performance
 *   - at_risk_populations: Depend entirely on responder competence during actual events; reassured by ceremonial indicators but discover the gap between appearance and reality too late
 *   - technical_specialists: Possess tacit expertise that cannot be certified through drills; excluded from planning that prioritizes measurable outputs over competence preservation
 *   - oversight_bodies: Benefit from institutional narrative that compliance metrics demonstrate preparedness; have structural incentive to accept ceremonial indicators
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.62).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.76).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Ceremonial Retention (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'a13c1e1c-23eb-43a3-b542-3064550651ba').
narrative_ontology:cs_kernel_codification('a13c1e1c-23eb-43a3-b542-3064550651ba', implicit).
narrative_ontology:cs_authority_grounding('a13c1e1c-23eb-43a3-b542-3064550651ba', extraction).
narrative_ontology:cs_interpretation_layer_present('a13c1e1c-23eb-43a3-b542-3064550651ba').
narrative_ontology:cs_reading_relation('a13c1e1c-23eb-43a3-b542-3064550651ba', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('a13c1e1c-23eb-43a3-b542-3064550651ba', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a13c1e1c-23eb-43a3-b542-3064550651ba', foundational, ceremony_substitutes_for_competence).
narrative_ontology:cs_axiom_status(ceremony_substitutes_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('a13c1e1c-23eb-43a3-b542-3064550651ba', ceremony_substitutes_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('a13c1e1c-23eb-43a3-b542-3064550651ba', foundational, institutional_legitimacy_extraction).
narrative_ontology:cs_axiom_status(institutional_legitimacy_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a13c1e1c-23eb-43a3-b542-3064550651ba', institutional_legitimacy_extraction, deontological).
narrative_ontology:cs_reference_frame('a13c1e1c-23eb-43a3-b542-3064550651ba', formal_ceremonial_preparedness).
narrative_ontology:cs_drift_state('a13c1e1c-23eb-43a3-b542-3064550651ba', contemporary_post_disaster_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a13c1e1c-23eb-43a3-b542-3064550651ba', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, at_risk_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The theater_ratio is exceptionally high (0.76 at interval end) and rising steeply from t=0 to t=25, then plateauing—this is the classic piton signature: performative maintenance maintaining the ceremony at a fixed, high level. The measurement grid shows theater rising faster than base_extractiveness, indicating the ratio of ceremonial to functional activity is increasing even as extraction holds relatively stable. This is Goodhart drift: the metric (drills conducted, certifications issued, checklists completed) has displaced the object (actual competence). Suppression_requirement rises from 0.42 to 0.62, indicating the constraint requires increasing active enforcement to suppress the growing gap between ceremony and competence—responders see the disconnect, technical specialists resist being excluded, at-risk populations (post-disaster) recognize the failure. The constraint persists because the administrator benefits from appearance without bearing the cost of competence (arbitrage exit), while responders and at-risk populations are trapped. The time-grid is shared across all three metrics, preventing the temporal alignment errors documented in OQ-105. The coercion grid shows differential pressure across levels: organizational and individual-level stakes_inflation rise faster than structural-level accessibility_collapse, indicating the competence gap is felt most acutely by those closest to response, while structural narratives insulate institutional legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional administrator seat, the constraint is a coordination function that preserves preparedness through systematized, repeatable protocols—visible compliance proves readiness. From the frontline responder and at-risk population seats, the same structure is pure extraction: resources flow away from competence-building into ceremonial display. From the technical specialist seat (structurally excluded), the constraint is obviously a snare—it uses the appearance of preparedness to suppress alternative voices and lock in the ceremonial apparatus. The engine should compute markedly different types at different seats: the administrator seat (beneficiary, institutional power, arbitrage exit) would compute toward piton or snare at low extraction; the responder seat (moderate power, constrained exit, direct exposure to competence gap) would compute toward snare; the technical specialist seat (excluded, moderate power, mobile exit—but structurally excluded from the conversation) would compute as analyst-observer to snare-perceiver. The authored metrics and claimed_type apply to the constraint as a SYSTEM, not to any single seat's experience of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional_legitimacy_apparatus is the beneficiary: it collects the legitimacy gains (institutional trust, regulatory compliance, public confidence) without bearing the resource cost of live competence maintenance. Actual_response_capacity is the victim: resources that could build deep competence are diverted to ceremony; when disaster strikes, the gap between certified readiness and actual competence becomes catastrophically visible. Frontline_responders and at_risk_populations also bear high costs: responders expend effort in ritual without developing adaptive judgment; at-risk populations depend on responder competence that the constraint actively starves. The directionality for each victim seat is high (toward 1.0 extraction) because exit is constrained (responders cannot refuse to participate in mandatory drills without losing employment; at-risk populations cannot exit the disaster) or trapped (actual_response_capacity is an emergent property, not an agent—it is starved by the constraint's operation). Institutional administrators have d near 0.0 (full beneficiary, arbitrage exit if the constraint becomes politically untenable). Technical_specialists have high d (toward extraction) despite their expertise because they are excluded from the decision-making that would use it—exclusion is itself a suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts classic mandatrophy: the founding problem (responders lack competence; institutions need to demonstrate preparedness) gave rise to a coordination solution (formalized drills and inspections). Over time, the means (ceremonial compliance) became decoupled from the end (live competence preservation). The constraint now extracts from competence-building to fund appearance-maintenance. The founding_problem_status is 'contested' because institutional administrators claim the founding problem is still live (disasters still happen, responders still need preparation) while technical specialists and post-disaster auditors claim the problem was misdiagnosed and the constraint now persists by inertia and institutional incentive rather than necessity. The theater_ratio evidence (0.76 and rising until plateau at t=25) supports the contested verdict: the constraint has become mostly theater, with residual coordination function (drills do teach SOME skills) but the ratio heavily weighted toward ceremony. A piton classification captures this: the constraint persists not because any seat genuinely benefits enough to maintain it (even institutional_legitimacy is theater-dependent and brittle), but because the cost to fix (dismantling the ceremonial apparatus and reorganizing around competence measurement) exceeds the clear benefit to any single agent. Institutional administrators could change it but don't (they benefit from appearance); responders cannot change it (constrained exit); oversight bodies have no incentive to change it (they benefit from the narrative that compliance = readiness). The resistance measurement (0.59 average, rising at organizational and class levels) shows real pushback—technical specialists resisting exclusion, responders complaining about ritual wastefulness, post-disaster auditors questioning the competence gap—but suppression (0.62 at interval end) is sufficient to maintain the ceremonial apparatus against this resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_competence_fungibility,
    'At what point does ceremonial drill participation cease to preserve competence and become purely theatrical? Is there a threshold separating drills that build skill from drills that merely perform readiness?',
    'Longitudinal competence assessment comparing responders trained via ceremonial drills with responders trained via immersive adaptive scenarios, measured against live disaster performance outcomes. Post-disaster debriefs examining whether responders who passed inspections lacked specific competencies.',
    'If ceremony and competence are fungible at some threshold (e.g., responders who drill quarterly maintain 70% of peak competence), the husk reading overstates extraction and the competence reading gains ground. If the threshold is zero (drills preserve almost no adaptive competence), the husk reading is strengthened and the competence reading fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_fungibility, empirical, 'Whether ceremonial drills preserve any measurable live competence or are pure theater.').

omega_variable(
    institutional_legitimacy_beneficiary_identity,
    'Is the beneficiary of the preparedness ceremonial apparatus truly ''institutional legitimacy'' (an abstract good) or specific institutional actors (administrators, oversight bodies) who capture legitimacy gains? Does institutional legitimacy distribute or concentrate?',
    'Trace resource flows from preparedness budgets to specific actors and institutions. Examine institutional budget allocation before and after major disasters: does legitimacy-restoration funding flow to competence-building or to expanded ceremonial programs?',
    'If legitimacy gains concentrate on institutional administrators and oversight bodies, they should be listed as beneficiaries rather than ''institutional_legitimacy_apparatus'' (a non-agent entity). If legitimacy distributes diffusely, the constraint is harder to classify (diffuse beneficiary, concentrated victim) and may reframe as snare rather than piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_legitimacy_beneficiary_identity, empirical, 'Whether institutional legitimacy is the true beneficiary or a cover story for concentrated institutional benefits.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the high suppression (0.62 at interval end) structural (legal requirement to participate in drills, employer enforcement of certification) or internalized (responders have accepted the ceremonial logic and suppress their own doubts about competence preservation)?',
    'Post-disaster exit analysis: if responders withdraw from preparedness systems after major events, suppression is primarily structural. If they rationalize the system despite failures, suppression is internalized. Qualitative interviews with responders about their frame for interpreting preparedness effectiveness.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests—responders carry the suppression with them even after leaving the preparedness system. This would increase the extraction assessment and support the husk reading. If structural, the constraint is more brittle—withdrawal and replacement of personnel could destabilize it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized in responder frames.').

omega_variable(
    hybrid_reading_incompatibility,
    'Does the husk reading''s core premise (preparedness is ceremony, competence is lacking) structurally coexist with the hybrid reading''s claim (specialized technical institutions retain competence while broader society becomes ceremonial), or does the presence of technical competence anywhere contradict the husk claim?',
    'Examine whether specialized institutions (water boards, search-rescue teams) operate under different measurement and training regimes than general preparedness frameworks. If they do, the hybrid reading is live and the husk reading applies only to non-specialized sectors. If all preparedness—specialized and general—uses the same ceremonial measurement, the husk and hybrid readings foreclose each other.',
    'If hybrid and husk readings coexist (different sectors, different competence profiles), the constraint_family needs two separate stories. If they foreclose, the reading_relations should use ''forecloses'' rather than ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_reading_incompatibility, conceptual, 'Whether the husk reading and hybrid reading can both be structurally true in the same system or whether one foreclosed the other.').

omega_variable(
    disaster_timing_bias,
    'Does the husk reading''s claim rest on unmeasured, low-frequency events (major disasters are rare)? Is the competence gap visible only when a disaster exceeds the threshold that drills prepared for, making competence assessment contingent on disaster rarity?',
    'Measure responder competence not only during major disasters but in medium-scale exercises or regional events where the constraint still governs response but disaster outcomes are more frequent. Compare competence across frequency distributions.',
    'If competence gaps emerge consistently across all disaster scales (not just catastrophic outliers), the husk reading is robust. If gaps appear only for rare, maximum-scale events that drills never prepared for, the husk reading overstates extraction—the constraint may be rope or tangled_rope at typical scales. This would support the competence reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disaster_timing_bias, empirical, 'Whether the husk reading''s competence gap is an artifact of rare, maximum-scale disaster measurement or a genuine feature at typical disaster scales.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__husk_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__husk_reading, theater_ratio, 10, 0.66).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__husk_reading, theater_ratio, 15, 0.7).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.73).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__husk_reading, theater_ratio, 25, 0.75).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__husk_reading, theater_ratio, 30, 0.76).
narrative_ontology:measurement(prep_tr_t35, preparedness_retention__husk_reading, theater_ratio, 35, 0.76).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__husk_reading, theater_ratio, 40, 0.76).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__husk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__husk_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__husk_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__husk_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__husk_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(prep_be_t35, preparedness_retention__husk_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__husk_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__husk_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__husk_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__husk_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__husk_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__husk_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__husk_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(prep_su_t35, preparedness_retention__husk_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__husk_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel is decomposed into three constraint stories corresponding to three live readings: the competence reading (drills preserve competence), the husk reading (drills are ceremonial, competence is lacking), and the hybrid reading (technical sectors retain competence, general populace becomes ceremonial). The husk reading presented here asserts that institutional preparedness is substantially extractive (ε=0.68) and highly theatrical (theater_ratio=0.76). The competence reading would assert lower extraction (coordination-heavy, beneficiary widely distributed, victims absent or minimal). The hybrid reading would show stratified extraction (high in general preparedness sectors, low in specialized technical sectors) with different ε and theater_ratio values. The three readings share a contested kernel (what preparedness means and how it is verified) but instantiate different constraints with different structural properties and different measured types. All three stories must be authored for the family to be complete; they are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_retention__husk_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
