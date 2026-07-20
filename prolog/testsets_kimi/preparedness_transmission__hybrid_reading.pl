% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission â Hybrid Reading
 *   domain: disaster risk management / institutional memory / civil defense systems
 *
 * SUMMARY:
 *   This constraint story captures the hybrid reading of the
 *   preparedness_transmission kernel: the institutional system that transmits
 *   disaster-preparedness capability across generations is stratified, with
 *   physical infrastructure competence remaining high while civilian
 *   coordination knowledge decays. The constraint operates through budget
 *   allocation, professional training pipelines, and doctrinal emphasis that
 *   privileges engineering and capital-intensive readiness over social
 *   coordination and civilian mutual-aid capacity. Under stress, shelters and
 *   communications hardware perform, but evacuation and decentralized
 *   coordination fail because the knowledge layer has atrophied.
 *
 * KEY AGENTS:
 *   - national_preparedness_authority: Agenda setter (institutional/constrained) â administers doctrine and budget allocation
 *   - engineering_agencies: Primary beneficiary (organized/mobile) â maintain capital-intensive readiness, collect preparedness contracts
 *   - infrastructure_operators: Secondary beneficiary (institutional/constrained) â run physical systems that perform under stress
 *   - civilian_populations: Primary target (powerless/trapped) â bear uncompensated risk from coordination decay
 *   - local_emergency_managers: Secondary target (moderate/constrained) â manage the gap between working hardware and uncoordinated public
 *   - community_resilience_groups: Excluded voice (moderate/constrained) â advocate for civilian coordination, marginalized in budgets
 *   - disaster_researchers: Analytical observer (analytical/analytical) â document stratification and post-event gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission â Hybrid Reading").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster risk management / institutional memory / civil defense systems").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'a3c01228-f073-4b51-87a3-964843716de5').
narrative_ontology:cs_kernel_codification('a3c01228-f073-4b51-87a3-964843716de5', distributed).
narrative_ontology:cs_authority_grounding('a3c01228-f073-4b51-87a3-964843716de5', practice).
narrative_ontology:cs_interpretation_layer_present('a3c01228-f073-4b51-87a3-964843716de5').
narrative_ontology:cs_reading_relation('a3c01228-f073-4b51-87a3-964843716de5', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3c01228-f073-4b51-87a3-964843716de5', preparedness_transmission__husk_reading, influences).
narrative_ontology:cs_axiom('a3c01228-f073-4b51-87a3-964843716de5', foundational, preparedness_layers_are_separable).
narrative_ontology:cs_axiom_status(preparedness_layers_are_separable, holdable).
narrative_ontology:cs_axiom_grounding('a3c01228-f073-4b51-87a3-964843716de5', preparedness_layers_are_separable, empirically_contingent).
narrative_ontology:cs_axiom('a3c01228-f073-4b51-87a3-964843716de5', secondary, infrastructure_competence_preserves_legitimacy).
narrative_ontology:cs_axiom_status(infrastructure_competence_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a3c01228-f073-4b51-87a3-964843716de5', infrastructure_competence_preserves_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('a3c01228-f073-4b51-87a3-964843716de5', integrated_civil_defense_system).
narrative_ontology:cs_drift_state('a3c01228-f073-4b51-87a3-964843716de5', contemporary_after_action_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3c01228-f073-4b51-87a3-964843716de5', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_operators).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_populations).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_emergency_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts national preparedness doctrine, allocates budgets across programs, and certifies state and local plans. Has formal authority to set training mandates but faces political and institutional pressure to fund visible capital infrastructure over intangible civilian coordination programs.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, national_preparedness_authority, agenda_setter,
    institutional, generational, constrained, national).

% Design, build, and maintain disaster-resilient infrastructure. Receive sustained federal and state contracts for shelters, hardened communications, and logistics hardware. Their technical competence is high and continuously exercised through engineering review and certification cycles.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_agencies, beneficiary,
    organized, biographical, mobile, national).

% Staff and operate physical preparedness assets such as emergency operations centers, shelter networks, and warning systems. Their maintenance budgets are protected during appropriations while civilian preparedness and coordination grants are repeatedly cut or held flat.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_operators, beneficiary,
    institutional, generational, constrained, national).

% Live in designated hazard zones and rely on official warning and evacuation systems. Have not received systematic civilian coordination or mutual-aid training in decades. During events they are instructed to await official direction rather than self-organize.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_populations, payer,
    powerless, immediate, trapped, national).

% Coordinate between state mandate and local implementation. Administer drills that test agency command response but rarely test community self-organization. Aware of coordination knowledge gaps through exercise failures and after-action reports, but lack funding and doctrinal support to close them.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_emergency_managers, payer,
    moderate, biographical, constrained, regional).

% Run volunteer neighborhood preparedness programs, mutual-aid training, and local communication networks. Compete for small grants and are rarely integrated into official drills, doctrine, or after-action review processes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, community_resilience_groups, excluded,
    moderate, biographical, constrained, local).

% Study post-disaster after-action reports, survey preparedness knowledge across populations, and publish findings documenting the divergence between infrastructure performance and social coordination outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_researchers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains physical infrastructure readiness for disaster response through professionalized engineering maintenance cycles, capital allocation, and hardware inspection regimes.
% TRANSFER_FUNCTION: Moves funding, institutional attention, and doctrinal legitimacy from civilian coordination and mutual-aid programs to physical infrastructure and engineering agencies, while transferring residual disaster risk to unprepared civilian populations and under-resourced local managers.
% ABSENT_VOICES: Community-based disaster preparedness organizations, sociologists of disaster coordination, and local emergency managers who prioritize civilian mutual-aid training are structurally underrepresented in budget allocations and doctrine-setting.
% DISAPPEARANCE_RATIONALE: If the stratified transmission constraint vanished, resources and training attention would rebalance toward civilian coordination; emergency response would shift from infrastructure-dependent to community-activated; the institutional equilibrium between engineering agencies and civilian populations would reorganize around integrated readiness.
% FOUNDING_PROBLEM: How to maintain societal disaster-response capability across generational turnover and institutional memory loss without continuous live events to validate the full system.
% FOUNDING_PROBLEM_CORROBORATION: Engineering agencies and infrastructure operators attest the problem remains live and is solved by maintained hardware. Disaster sociologists, inspector-general audits, and post-event after-action reports from outside the engineering-beneficiary set attest the founding problem is only half-solved and the coordination component has been systematically neglected.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored as substantial because the constraint systematically underinvests in civilian coordination while maintaining capital-intensive infrastructure; this transfers risk to populations and extracts public resources for engineering constituencies. Suppression (0.45) reflects institutional marginalization of alternatives rather than violent coercion: community-based preparedness models are excluded through funding mechanisms and doctrinal omission. Theater_ratio (0.42) captures the growing share of drills that test infrastructure response and agency command while treating civilian behavior as a passive input â satisfying accountability without validating coordination. Accessibility_collapse (0.48) is moderate because alternatives exist in theory but lack institutional pathways to scale; once inside the official system, physical preparedness is the only visible option. Resistance (0.38) is modest: local managers and community groups push back but lack budget authority, while post-disaster after-action reports surface the gap but are absorbed by the interpretive layer.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (national authority) experiences the constraint as successful coordination because infrastructure inspections pass, engineering certifications renew, and capital budgets are obligated. The payer seats (civilians, local managers) experience the same constraint as a liability: when the event arrives, coordination scripts are missing and physical systems operate in a vacuum. The divergence is structural â both assessments are true to their respective data layers.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering agencies and infrastructure operators are structural beneficiaries: the constraint subsidizes their budgets and legitimates their centrality, producing low directionality and damped effective extraction. Civilian populations are structural targets: they pay through uncompensated risk and the absence of coordination capacity, producing high directionality and amplified extraction. Local emergency managers sit between â structurally payers with constrained exit, but their institutional proximity to the agenda setter moderates amplification relative to civilians.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining disaster-response capability across generational turnover â is live in the physical layer and dead in the coordination layer. The hybrid reading prevents mislabeling by decomposing the kernel: the competence reading would wrongly classify the whole system as a rope; the husk reading would wrongly classify it as a piton; the hybrid reading isolates the stratification so that the physical layer's genuine coordination is not used to launder the coordination layer's decay. The R5 genealogy interview flags this mismatch (status contested, disappearance rearranges), directing the engine to treat the coordination component as a captured or zombie function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_mechanism_ambiguity,
    'Is the coordination decay a result of natural institutional entropy, or an active reallocation of resources to physical infrastructure?',
    'Historical budget analysis and documentary evidence of deliberate trade-offs versus passive neglect in preparedness program authorizations.',
    'If active reallocation, extraction is higher and the constraint trends snare-like; if passive entropy, the constraint trends piton-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_mechanism_ambiguity, empirical, 'Whether decay is intentional reallocation or institutional forgetting').

omega_variable(
    layer_coupling_under_stress,
    'Do physical infrastructure and civilian coordination function independently under disaster conditions, or does infrastructure effectiveness depend on coordination quality?',
    'Comparative case studies of disasters where infrastructure was intact but coordination failed versus succeeded.',
    'If coupled, the stratified reading understates systemic vulnerability; if independent, the hybrid frame is analytically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_coupling_under_stress, empirical, 'Whether physical and coordination layers are operationally separable').

omega_variable(
    suppression_of_alternatives,
    'Is the marginalization of community-based preparedness structural (budget exclusion) or cultural (professional identity favoring engineering)?',
    'Tracking community-preparedness program funding and procurement rules versus interviewing institutional actors about doctrine formation.',
    'Determines whether suppression is a raw structural property or an identity-locked phenomenon that persists after barrier removal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternatives, conceptual, 'Structural versus cultural suppression mechanism').

omega_variable(
    kernel_decomposition_authority,
    'Does the hybrid reading''s layer decomposition derive from the kernel''s own structure, or is it an external analytic imposition?',
    'Archival analysis of whether the original preparedness doctrine distinguishes physical and social layers or treats preparedness as a unified competence.',
    'If the kernel is unified, hybrid is a reformulation; if inherently layered, hybrid is a direct reading. This affects whether the sibling readings are competing interpretations or true alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_authority, conceptual, 'Whether layer decomposition is endogenous to the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__hybrid_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__hybrid_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(prep_tr_t25, preparedness_transmission__hybrid_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__hybrid_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__hybrid_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(prep_be_t25, preparedness_transmission__hybrid_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__hybrid_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__hybrid_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(prep_su_t25, preparedness_transmission__hybrid_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_transmission kernel, decomposed per the Îµ-invariance principle. The hybrid reading isolates the stratified-layer claim; the husk reading isolates universal atrophy; the competence reading isolates live-practice success. Each carries a distinct Îµ, stakeholder structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
