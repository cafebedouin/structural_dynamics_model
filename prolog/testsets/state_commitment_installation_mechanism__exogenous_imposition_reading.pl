% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_exogenous, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State Authority Top-Down Commitment Installation via Transformation Mandate
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   A state authority holding a transformation mandate (revolutionary
 *   government, colonial administration, modernizing absolute monarch)
 *   decrees establishment of a new institutional commitment
 *   framework—standardized education system, unified legal code, centralized
 *   religious authority, or labor discipline regime. The commitment is
 *   presented as fulfilling the mandate and modernizing society. No
 *   grassroots validation precedes it; installation is by decree and enforced
 *   by bureaucratic and coercive apparatus. Institutional conservatives who
 *   held prior authority lose status and control. Grassroots practitioners
 *   bear compliance costs but have no voice in design. The constraint is
 *   claimed as tangled_rope (coordination benefit + asymmetric extraction +
 *   active enforcement); the metric profile describes substantial extraction
 *   that rises sharply in the installation phase then stabilizes.
 *
 * KEY AGENTS:
 *   - state_authority_apparatus: holds transformation mandate, issues decree, enforces compliance
 *   - transformation_mandate_holders: technical cadres appointed to oversee installation and validation
 *   - institutional_conservatives: lose prior authority under old frameworks
 *   - grassroots_practitioners: bear compliance costs, structurally excluded from design
 *   - competing_transformation_programs: alternative reform visions barred from trial
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.76).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State Authority Top-Down Commitment Installation via Transformation Mandate").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '67384249-5616-4c5b-b596-297acf039c49').
narrative_ontology:cs_kernel_codification('67384249-5616-4c5b-b596-297acf039c49', formalized).
narrative_ontology:cs_authority_grounding('67384249-5616-4c5b-b596-297acf039c49', extraction).
narrative_ontology:cs_interpretation_layer_present('67384249-5616-4c5b-b596-297acf039c49').
narrative_ontology:cs_reading_relation('67384249-5616-4c5b-b596-297acf039c49', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('67384249-5616-4c5b-b596-297acf039c49', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('67384249-5616-4c5b-b596-297acf039c49', foundational, state_mandate_legitimacy_sufficient).
narrative_ontology:cs_axiom_status(state_mandate_legitimacy_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('67384249-5616-4c5b-b596-297acf039c49', state_mandate_legitimacy_sufficient, deontological).
narrative_ontology:cs_axiom('67384249-5616-4c5b-b596-297acf039c49', secondary, grassroots_validation_unnecessary_at_scale).
narrative_ontology:cs_axiom_status(grassroots_validation_unnecessary_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('67384249-5616-4c5b-b596-297acf039c49', grassroots_validation_unnecessary_at_scale, instrumental).
narrative_ontology:cs_reference_frame('67384249-5616-4c5b-b596-297acf039c49', mandated_state_authority_apex).
narrative_ontology:cs_drift_state('67384249-5616-4c5b-b596-297acf039c49', post_stabilization_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('67384249-5616-4c5b-b596-297acf039c49', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_mandate_holders).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional_conservatives).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_practitioners).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, localized_alternative_frameworks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at decree to 0.68 at stabilization as the state apparatus captures interpretive authority and enforces the framework against resistance. Suppression is high and stable (0.62→0.76, then holding at 0.76) because the constraint's persistence depends entirely on coercive enforcement—without active suppression of competing frameworks and resistance, the installed commitment collapses. Theater ratio falls (0.55→0.42) because in the early phase the state justifies installation as fulfilling mandate (performative framing); as the framework stabilizes, enforcement becomes more routine and less theatrical. The measurements on one shared time grid show the characteristic extraction accumulation and suppression intensification of exogenous imposition: rapid rise in early years (0–15), stabilization by year 25–40 as the installed commitment becomes normalized and alternatives are largely foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and mandate holders perceive the constraint as successful coordination installation—a real problem (fragmentation) solved by a real mechanism (unified framework). Institutional conservatives and grassroots practitioners perceive the same structure as imposed extraction—their prior authority dismantled, their practice controlled, their voice excluded. From the state seat, suppression is 'compliance enforcement' (legitimate). From the victim seats, suppression is coercion. The engine computes both perception sets from the structural data; divergence at the payer and victim seats from the beneficiary seats is the measure of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.1 (beneficiary, high power, arbitrage exit—can change the framework at will; extraction is negative for them). Mandate holders: d ≈ 0.2 (beneficiary, institutional power, mobile exit—they can be reassigned). Institutional conservatives: d ≈ 0.85 (victim, powerful power but constrained exit—they can resist quietly but formal exit is branded as backwardness or treason). Grassroots practitioners: d ≈ 0.90 (victim, powerless, trapped exit—they bear the compliance costs and cannot leave). Competing programs: d ≈ 0.95 (victim, institutional power but trapped exit—they are barred from being tried). The directionality spans the full range because the constraint concentrates benefits at the apex and diffuses costs across the base.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of unified framework for state-scale coordination) is real and genuine. The constraint solves it—a single commitment framework does enable uniform compliance measurement and central extraction. The tangled_rope classification holds: there is real coordination benefit (unified standard) AND asymmetric extraction (authority transferred to state apparatus, alternative frameworks foreclosed, grassroots practice controlled). The key is that WITHOUT active enforcement and suppression of competing frameworks, the installed commitment would collapse—this proves it is not natural law (mountain) but maintained structure. A snare reading (pure extraction with coordination cover) would be inaccurate because the coordination function is not merely a cover story; the state genuinely needs the unified framework to operate at scale. But the coordination function does NOT justify the asymmetric distribution of benefits and costs, nor the exclusion of grassroots voices from design. Tangled rope captures both truths: real coordination need, real extraction asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_vs_seizure_ambiguity,
    'Does the state authority actually hold a legitimate transformation mandate, or has it seized power and retroactively invented a mandate to justify decree?',
    'Genealogical analysis of how the mandate was claimed (constitutional, revolutionary, hereditary, etc.) and whether pre-seizure documents or prior consensus supported the transformation scope, or whether mandate was crafted after taking power.',
    'If mandate is legitimate, the extraction is payment for coordination service under crisis conditions; if mandate is seized authority, the extraction is pure coercion dressed as transformation. The constraint''s classification remains tangled_rope in either case, but the degree to which extraction is defensible changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_seizure_ambiguity, empirical, 'Whether the transformation mandate is genuinely held authority or ex-post justification for seizure.').

omega_variable(
    alternative_framework_viability,
    'Would a competing transformation program (decentralized, grassroots-validated, regionally adapted) have solved the coordination problem equally well or better?',
    'Comparative historical analysis of parallel cases where alternative programs were tried (different revolutionary movements, colonial comparisons, regional experimentation where the state apparatus permitted it). Anthropological documentation of whether prior local frameworks achieved coordination despite appearance of fragmentation.',
    'If viable alternatives existed, the state''s exclusive installation is unnecessarily extractive and the suppression of competing programs is surplus extraction. If no viable alternative existed, the asymmetric distribution of benefits is a necessary cost of coordination. Either way, the constraint remains tangled_rope but the extractiveness assessment becomes conditional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_framework_viability, conceptual, 'Whether exogenous imposition was necessary for coordination or merely convenient for state authority.').

omega_variable(
    institutional_conservative_capacity,
    'Could institutional conservatives (prior authority holders) have gradually evolved and modernized their own frameworks without state imposition, or were they structurally locked into pre-modern arrangements?',
    'Historical record of whether conservatives were capable of learning, adapting, and incorporating new practices when not actively suppressed. Documentation of cases where prior institutions successfully modernized themselves (guilds that became professional associations, regional councils that developed coordinating mechanisms).',
    'If conservatives could have evolved their own frameworks, treating them as victims of necessary suppression is overstated and their exclusion from design becomes indefensible. If they were locked into pre-modern arrangements, suppression accelerates necessary change but extraction remains extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_conservative_capacity, empirical, 'Whether institutional conservatives were incapable of self-reform or merely unwilling.').

omega_variable(
    grassroots_voice_integration_feasibility,
    'Would including grassroots practitioners in commitment design have been feasible given the scale and timeframe of the transformation mandate, or is top-down installation structurally necessary at that scale?',
    'Case studies where grassroots input was solicited during institutional installation (local committees, experimental pilots, adaptation procedures) and whether doing so slowed, accelerated, or had no effect on implementation timelines and adoption outcomes.',
    'If grassroots inclusion was feasible, exclusion is pure extraction and suppression is surplus. If top-down imposition was necessary for speed and scale, the asymmetry is a cost of crisis management, not malice. The constraint remains tangled_rope but the defensibility of extraction changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grassroots_voice_integration_feasibility, empirical, 'Whether grassroots exclusion was operationally necessary or politically chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.18).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Part of the state_commitment_installation_mechanism constraint family. Three structurally distinct readings: exogenous_imposition_reading (this constraint: top-down decree, state as beneficiary, grassroots excluded), endogenous_climb_reading (fringe-to-center climb, demonstrated superiority, no single beneficiary), and hybrid_cascade_reading (apex installation requires fringe validation, mixed benefit distribution). Each reading has distinct ε values, beneficiary sets, and timeframes. The family decomposition follows the ε-invariance principle: the 'commitment installation mechanism' concept conflates three mechanisms with different extraction profiles, and authoring them as one constraint would fabricate verdicts. The ε-invariance test: measuring via different readings yields meaningfully different extractiveness and beneficiary structures, so the readings are separate constraints linked by mechanism, not perspectives on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
