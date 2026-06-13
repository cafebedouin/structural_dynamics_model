% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Encoding of Intergenerational Trauma as Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual practices encode and transmit
 *   intergenerational trauma, functioning as a warning system for future
 *   generations. The core mechanism is the imposition of psychological burden
 *   on descendants (victims) to maintain collective threat-vigilance
 *   (beneficiary). It is claimed as a Tangled Rope because it genuinely
 *   coordinates collective memory and survival strategies, but does so
 *   through an asymmetric extraction of psychological well-being. The metrics
 *   reflect a moderate-to-high extractiveness and suppression, as the
 *   perpetuation of trauma requires active social enforcement and limits
 *   individual agency.
 *
 * KEY AGENTS:
 *   - founding_generation_survivors: Agenda-setter (institutional/identity_locked) — established the trauma-encoding rituals.
 *   - descendants_psychological_burden: Payer (powerless/identity_locked) — bears the psychological costs of inherited trauma.
 *   - future_generations_threat_vigilance: Beneficiary (organized/constrained) — the collective capacity for threat detection.
 *   - ritual_leaders_interpreters: Agenda-setter (organized/constrained) — maintain and enforce the rituals.
 *   - external_psychological_support: Excluded (moderate/mobile) — offers alternative trauma processing, but is often rejected.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Encoding of Intergenerational Trauma as Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '7320a88f-0d71-4883-b2e8-11ba4ea47ccc').
narrative_ontology:cs_kernel_codification('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', implicit).
narrative_ontology:cs_authority_grounding('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', lineage).
narrative_ontology:cs_interpretation_layer_present('7320a88f-0d71-4883-b2e8-11ba4ea47ccc').
narrative_ontology:cs_reading_relation('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', foundational, trauma_as_essential_warning).
narrative_ontology:cs_axiom_status(trauma_as_essential_warning, holdable).
narrative_ontology:cs_axiom_grounding('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', trauma_as_essential_warning, instrumental).
narrative_ontology:cs_axiom('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', secondary, collective_memory_requires_visceral_transmission).
narrative_ontology:cs_axiom_status(collective_memory_requires_visceral_transmission, holdable).
narrative_ontology:cs_axiom_grounding('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', collective_memory_requires_visceral_transmission, conventional).
narrative_ontology:cs_reference_frame('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', perpetual_vigilance_paradigm).
narrative_ontology:cs_drift_state('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', contemporary_psychological_awareness, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7320a88f-0d71-4883-b2e8-11ba4ea47ccc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_threat_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendants_psychological_burden).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the psychological cost to descendants is substantial and persistent, often manifesting as chronic anxiety or hypervigilance. Suppression (0.75) is also high, driven by strong social pressure, identity fusion, and the perceived existential necessity of the rituals for group survival; questioning the ritual is often seen as betraying the group or endangering its future. Theater ratio is low (0.20) because the rituals are genuinely believed to serve a vital function, even if the efficacy of trauma-encoding as a warning system is debatable. The increasing extractiveness and suppression over time reflect the hardening of the ritual's enforcement and the deepening of the psychological burden as the original catastrophe recedes into the past, making the 'warning' less direct but the 'cost' more ingrained.
 *
 * PERSPECTIVAL GAP:
 *   The founding generation and ritual leaders perceive the constraint as a necessary Rope or even a Mountain (a natural consequence of their history), essential for group survival. Descendants, however, experience it as a Snare or Tangled Rope, feeling trapped by the psychological burden and the lack of alternatives for processing their inherited trauma. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding generation and ritual leaders are agenda-setters, benefiting from the perceived stability and survival of the group (d near 0.0-0.2). Future generations' threat vigilance is a collective beneficiary (d near 0.2-0.4). Descendants bearing the psychological burden are clear targets (d near 0.8-1.0), as the constraint directly extracts from their well-being. External psychological support is excluded, meaning the constraint actively suppresses their alternative approaches.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (group survival through vigilance) is still 'live' according to its proponents, preventing a clear mandatrophy resolution. However, the rising extractiveness and suppression suggest a drift towards a Snare, where the 'warning system' function becomes a cover for perpetuating psychological control and burden. The classification as Tangled Rope captures this hybrid nature, acknowledging the coordination function while highlighting the asymmetric extraction. If the founding problem were deemed 'dead' (i.e., the threat no longer exists), the constraint would likely reclassify as a Snare, as its coordination function would have atrophied, leaving only extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_trauma_encoding,
    'Does the ritual encoding of intergenerational trauma genuinely enhance collective threat-detection and survival capacity, or does it primarily perpetuate psychological burden?',
    'Longitudinal studies comparing survival outcomes and psychological well-being in groups with and without such trauma-encoding rituals, controlling for actual threat levels.',
    'If efficacy is low, the ''beneficiary'' aspect (threat-vigilance) is largely theatrical, pushing the constraint closer to a Snare. If efficacy is high, the coordination function is stronger, supporting the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_trauma_encoding, empirical, 'Empirical validation of the ritual''s claimed warning system function.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social pressure, lack of alternatives) or internalized (identity fusion, belief in necessity)?',
    'Post-exit suppression trajectory: if psychological burden and resistance to alternative processing persist after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — descendants carry the suppression with them after exit, making the constraint more resilient and extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in perpetuating trauma.').

omega_variable(
    intergenerational_ethics_of_burden,
    'Is it ethically justifiable to impose psychological burden on future generations for the sake of collective survival, particularly when the original threat has receded?',
    'Philosophical and ethical deliberation within the community, potentially leading to a re-evaluation of the ritual''s purpose and form.',
    'A shift in ethical consensus could lead to a re-framing of the constraint, potentially reducing its perceived legitimacy and driving efforts to mitigate its extractive aspects, pushing it towards a Scaffold or even a Rope if the burden is actively addressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_ethics_of_burden, preference, 'Ethical evaluation of intergenerational trauma transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 1.0).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_kernel', focusing on the transmission of trauma as a warning system. It is linked to other readings that emphasize symbolic continuity, survival competence, and boundary maintenance, as these functions are often intertwined in collective memory rituals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
