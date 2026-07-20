% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the survival_competence_reading of the
 *   catastrophe_memory_preservation kernel. It holds that ritual practice
 *   does not merely symbolize or mourn catastrophe, but functionally encodes
 *   operational threat-recognition drills that transmit genuine survival
 *   competence across generations. The constraint coordinates a genuine
 *   collective-action problemâgenerational memory loss between
 *   low-frequency disastersâwhile asymmetrically extracting present
 *   autonomy and resources from the living generation for the benefit of
 *   descendants who cannot consent or opt out. The claim is tangled_rope
 *   because the coordination function and the extraction are structurally
 *   inseparable: the same ritual that encodes competence also enforces
 *   participation and subordinates present autonomy.
 *
 * KEY AGENTS:
 *   - future_generations (beneficiary, powerless/trapped): Receive inherited competence without cost but without choice.
 *   - present_generation_community (payer, moderate/identity_locked): Bear ritual costs and autonomy loss; exit fractures identity.
 *   - ritual_authority (agenda_setter, organized/constrained): Administers and enforces the ritual frame; bound by role dependency.
 *   - secular_institutions (excluded, institutional/mobile): Alternative competence-providers structurally barred from the ritual frame.
 *   - ritual_studies_scholars (observer, analytical): External analytical seat debating functional vs. symbolic readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, 'a3964ed0-4df9-4d45-83a7-1fd7b78328a2').
narrative_ontology:cs_kernel_codification('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', implicit).
narrative_ontology:cs_authority_grounding('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', practice).
narrative_ontology:cs_interpretation_layer_present('a3964ed0-4df9-4d45-83a7-1fd7b78328a2').
narrative_ontology:cs_reading_relation('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', foundational, ritual_transmits_operational_competence).
narrative_ontology:cs_axiom_status(ritual_transmits_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', ritual_transmits_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', foundational, generational_obligation_over_present_autonomy).
narrative_ontology:cs_axiom_status(generational_obligation_over_present_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', generational_obligation_over_present_autonomy, deontological).
narrative_ontology:cs_reference_frame('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', operative_ritual_competence).
narrative_ontology:cs_drift_state('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', modern_institutional_alternatives_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3964ed0-4df9-4d45-83a7-1fd7b78328a2', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive inherited operational competence for recognizing and responding to catastrophic threats, transmitted through ritual practice they did not choose and cannot opt out of inheriting. They bear no direct cost but are the named justification for present sacrifice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Bear the costs of repeated, demanding ritual participation that encodes threat-recognition drills. Their autonomy, time, and psychological resources are subordinated to a future survival benefit they will not personally receive. Exit means breaking with communal identity and risking ostracism or loss of symbolic standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_community, payer,
    moderate, biographical, identity_locked, regional).

% Administer ritual protocols, enforce participation norms, and interpret catastrophic events through the ritual frame. Their authority derives from preserving the competence claim against competing modern explanations. They are bound to the ritual system by identity and role dependency.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_authority, agenda_setter,
    organized, generational, constrained, regional).

% Would offer secular disaster-preparedness education, infrastructure, and institutional memory as alternative threat-recognition systems, but are structurally excluded from the ritual framing of competence transmission. Their absence naturalizes the ritual as the only viable mnemonic technology.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, secular_institutions, excluded,
    institutional, generational, mobile, global).

% Observe the ritual and debate whether it transmits genuine operational competence or merely symbolic identity. Their analytical frame is external to the ritual's self-understanding and is not bound by its participation demands.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmit operational threat-recognition competence across generational gaps without relying on written records or institutional specialization, using embodied repetition and affective conditioning to maintain readiness for low-frequency high-severity catastrophes.
% TRANSFER_FUNCTION: Moves cognitive and behavioral preparation for threat recognition from the living generation to future generations, extracting present autonomy, time, and psychological resources in exchange for a projected future survival benefit that the payers will not observe.
% ABSENT_VOICES: Secular disaster-management institutions and individual autonomy advocates are excluded; they would argue that operational competence is better preserved through explicit education, distributed institutional memory, and physical infrastructure rather than ritual obligation.
% DISAPPEARANCE_RATIONALE: If the ritual constraint vanished, the community would lose its primary mechanism for embedding threat-recognition in muscle memory and collective affect. Alternative institutional memory would need to be constructed from scratch; the generational contract would shift from obligatory participation to optional education, and the community's catastrophic readiness would reorganize around secular expertise.
% FOUNDING_PROBLEM: Catastrophic threats such as flood, famine, invasion, and plague occur at intervals longer than individual memory; without a reliable transmission mechanism, communities forget warning signs, response protocols, and survival behaviors between events.
% FOUNDING_PROBLEM_CORROBORATION: Archaeologists and disaster-studies researchers corroborate the reality of low-frequency catastrophic risk and institutional memory loss, but they dispute that ritual is the necessary or best solution. Ritual authorities attest from within the benefiting frame; independent ethnographers and secular emergency-management researchers provide partial external corroboration of the risk while rejecting the ritual mechanism as uniquely viable.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because present participants pay real, ongoing costsâtime, autonomy, and psychological burdenâfor a future benefit they will not personally observe. Suppression is moderate (0.62) because persistence depends on social enforcement and identity-lock rather than physical coercion; the community must actively suppress exit and alternative explanations to maintain participation as modern secular alternatives become visible. Theater ratio is moderate (0.45) because some ritual performance is necessary for embodied transmission, but an increasing share of ritual activity under modernity serves to naturalize the practice against competing institutional-memory technologies rather than to drill operational competence. Accessibility collapse (0.55) is incomplete because secular alternatives exist but are delegitimized within the ritual frame; resistance (0.50) reflects the friction between present-generation autonomy and generational obligation.
 *
 * PERSPECTIVAL GAP:
 *   The future-generation seat should compute as low-directionality beneficiary: they are subsidized by the constraint, paying nothing, receiving projected survival benefit. The present-generation seat should compute as high-directionality target: they bear the full cost, have identity-locked exit, and receive no personal survival benefit. The ritual_authority seat sits ambiguouslyâadministering enforcement while also identity-boundâproducing a directionality near symmetric or slightly target-leaning. The divergence between the beneficiary and payer seats is the central structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are structural beneficiaries (d near 0.0): the constraint is justified in their name and they receive the coordination benefit without bearing extraction. Present generation community are structural victims/payers (d near 1.0): they suffer the extraction directly and have the least exit. Ritual authorities derive mixed directionality because they enforce the constraint but are themselves constrained by it; they do not personally capture the extracted autonomy, though they accrue status and role security.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both a genuine coordination function (cross-generational competence transfer) and asymmetric extraction (present autonomy subordinated to future survival). If only the coordination function were observed, the constraint might be misread as a rope; if only the extraction were observed, it might be misread as a snare. The tangled_rope classification captures that the ritual genuinely solves a coordination problem while simultaneously extracting from one generation for another, and that it requires active enforcement (social and identity-based) to hold the asymmetry in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_competence_reality,
    'Does the ritual actually transmit operational threat-recognition competence, or only symbolic fidelity and group cohesion?',
    'Comparative ethnography and disaster-outcome studies measuring catastrophic-response performance in communities with high versus low ritual intensity, controlling for secular infrastructure and education levels.',
    'If no competence is transferred, this reading collapses toward mourning_practice_reading or hybrid_atrophy_reading, and the constraint reclassifies as rope, scaffold, or piton depending on enforcement levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_competence_reality, empirical, 'Whether ritual encodes genuine operational competence or only symbolic continuity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social sanction, ostracism) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: if suppression metrics fall after an individual leaves the community, the mechanism was structural; if suppression persists via guilt, shame, or identity loss, it was internalized.',
    'If internalized, effective extraction exceeds the structural measure because the target carries the suppression after exit; the constraint shifts toward identity-locked snare dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism in ritual participation').

omega_variable(
    kernel_reading_indeterminacy,
    'Which reading of the catastrophe_memory_preservation kernel is structurally trueâsurvival competence, symbolic mourning, or atrophied hybrid?',
    'Operational tests of competence transfer versus symbolic function; historical analysis of ritual change and functional drift under modernity.',
    'Determines whether this constraint is a tangled_rope (this reading), a rope (mourning_practice_reading), or a piton (hybrid_atrophy_reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Structural indeterminacy between competing kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.73).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_preservation kernel, decomposed from the colloquial label 'ritual preserves memory' into structurally distinct claims per the Îµ-invariance principle. This reading posits ongoing operational competence transfer; sibling readings posit symbolic continuity and historical atrophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
