% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Intergenerational Trauma Encoding as Collective Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A community preserves historical trauma through ritual, narrative, and
 *   collective mourning practice. The stated function is collective survival:
 *   ritual encodes ancestral threat-recognition into descendants'
 *   psychological and social structure so that the group remains vigilant
 *   against historical repetition. This constraint is ONE reading of a
 *   contested kernel about how catastrophe memory functions in ritual
 *   systems. The trauma-encoding reading treats ritual as a transmission
 *   mechanism that extracts psychological burden from descendants as the
 *   price of their inherited warning capacity. Sibling readings frame the
 *   same ritual as boundary-maintenance, survival-competence encoding, or
 *   symbolic continuity—each of which produces structurally different
 *   constraints and different extracted costs. This story instantiates ONLY
 *   the trauma-encoding reading: ritual as a mechanism that encodes
 *   intergenerational trauma as warning, benefiting collective
 *   threat-vigilance while extracting psychological weight from descendants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Intergenerational Trauma Encoding as Collective Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'f7d0457f-2d04-4606-ae48-4560c26cab93').
narrative_ontology:cs_kernel_codification('f7d0457f-2d04-4606-ae48-4560c26cab93', distributed).
narrative_ontology:cs_authority_grounding('f7d0457f-2d04-4606-ae48-4560c26cab93', lineage).
narrative_ontology:cs_interpretation_layer_present('f7d0457f-2d04-4606-ae48-4560c26cab93').
narrative_ontology:cs_reading_relation('f7d0457f-2d04-4606-ae48-4560c26cab93', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7d0457f-2d04-4606-ae48-4560c26cab93', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7d0457f-2d04-4606-ae48-4560c26cab93', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('f7d0457f-2d04-4606-ae48-4560c26cab93', foundational, trauma_encodes_threat_recognition).
narrative_ontology:cs_axiom_status(trauma_encodes_threat_recognition, holdable).
narrative_ontology:cs_axiom_grounding('f7d0457f-2d04-4606-ae48-4560c26cab93', trauma_encodes_threat_recognition, empirically_contingent).
narrative_ontology:cs_axiom('f7d0457f-2d04-4606-ae48-4560c26cab93', foundational, descendants_must_carry_ancestral_burden).
narrative_ontology:cs_axiom_status(descendants_must_carry_ancestral_burden, holdable).
narrative_ontology:cs_axiom_grounding('f7d0457f-2d04-4606-ae48-4560c26cab93', descendants_must_carry_ancestral_burden, deontological).
narrative_ontology:cs_reference_frame('f7d0457f-2d04-4606-ae48-4560c26cab93', trauma_as_collective_survival_mechanism).
narrative_ontology:cs_drift_state('f7d0457f-2d04-4606-ae48-4560c26cab93', contemporary_psychological_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7d0457f-2d04-4606-ae48-4560c26cab93', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, group_survival_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness is moderate-to-high (0.68) because the constraint imposes psychological burden on powerless descendants (anxiety, hypervigilance, trauma markers) to sustain collective threat-awareness. Suppression is moderate (0.45) because the constraint relies less on coercive barriers than on identity-fusion: descendants internalize the necessity of carrying trauma as part of being group members. Theater ratio is elevated (0.52) and rising over time because as direct threat recedes and descendants have no lived experience of the founding catastrophe, the maintenance of trauma-narratives becomes increasingly performative—the function shifts from 'remember to survive' to 'remember to remember.' The measurement series captures this drift: extractiveness peaks at t=60 (when therapeutic alternatives emerge most strongly) and stabilizes at t=80+, while theater ratio continues rising, indicating that the constraint's real function (threat-warning) degrades while its performative function (memory maintenance) intensifies. Suppression is relatively low because descendants are not coerced by external force but by identity-lock—they choose to carry the trauma because rejecting it means rejecting group identity.
 *
 * PERSPECTIVAL GAP:
 *   Ritual practitioners (agenda-setters) and community elders (beneficiaries) compute this constraint as rope or even mountain—natural law of survival, necessary coordination for collective defense. They have mobile exit and powerful voice. Descendant generations (payers) compute it as snare or tangled rope—they bear extraction (psychological weight, hypervigilance) with constrained exit and no voice in the design. The engine computes per-seat classifications from power, exit, and beneficiary/victim structure; the authored metrics describe extractiveness from the payer seat (descendants), not the beneficiary seat (collective threat-vigilance). This divergence is the diagnostic point.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendants are structural targets: they bear costs (psychological burden, hypervigilance, trauma markers, constrained life-planning due to threat-fixation) without meaningful voice in the design, with identity-locked exit, and with no alternative coordination mechanism available. Collective threat-vigilance (beneficiary non-agent) accrues the benefit—it gets sustained early-warning capacity without having to require survivors' grandchildren to re-learn threats directly. Practitioners are asymmetric: they administer the constraint (agenda-setter role) and benefit from authority (elder status), but many are also descendants carrying the burden themselves. This dual-positioning creates inter-seat asymmetry that the engine captures through directionality overrides if needed; here, the structural data (beneficiaries = threat-vigilance; victims = descendant_generations) makes the directionality derivation clear.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the boundary between genuine coordination and extraction. The founding problem (how to preserve survivors' threat-recognition for descendants who won't experience the threat directly) is live—communities with cyclical persecution actually do face this problem. The founding_problem_status = 'live' marks that the original coordination need persists. But the theater_ratio trajectory (rising from 0.35 to 0.52+ over the interval) signals that as descendants distance from the founding catastrophe, the ritual's function increasingly shifts from 'actual threat-transmission' to 'performance of memory.' This is not mandatrophy in the classical sense (a constraint whose founding problem has died but persists from institutional inertia), because the threat-vigilance function never fully atrophies—the community still faces real threats of persecution or cyclical catastrophe in many cases. Instead, this is a constraint in which the real function (coordination for survival) persists but is increasingly wrapped in performative function (memory-maintenance ritual). The distinction matters: if the founding problem is contested (some say the threat is gone, others say it still looms), the constraint's classification depends on which reading one accepts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_coexistence,
    'Which reading of the catastrophe_memory_kernel is the ''correct'' or ''primary'' function of this ritual?',
    'Ethnographic analysis of how the community explicitly frames its ritual (through teaching, justification, and emphasis). Different communities may emphasize different functions. Discourse analysis of how ritual is defended and preserved.',
    'Different readings entail different type classifications and different extraction profiles. A community that emphasizes trauma-encoding pays higher extractive costs on descendants; a community emphasizing boundary-maintenance or symbolic continuity may show lower descendant-burden and higher beneficiary-diffusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether this ritual''s primary function is trauma-encoding, boundary-maintenance, survival-competence, or symbolic continuity—or a composite of several.').

omega_variable(
    threat_recurrence_assumption,
    'Is the collective threat the ritual encodes still live, or is it a historical fact that will not recur?',
    'Analysis of actual threat recurrence patterns (do cyclical catastrophes continue for this community, or has the threat landscape fundamentally changed?). Comparison with communities that faced one-time catastrophes and communities with ongoing persecution patterns.',
    'If the threat is dead, the founding_problem_status shifts from ''live'' to ''dead,'' triggering mandatrophy assessment. The extractiveness of the ritual would be re-classified as zombification rather than functional coordination. If the threat is live, extractiveness remains justified by the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_recurrence_assumption, empirical, 'Whether the historical threat the ritual encodes continues or has been resolved.').

omega_variable(
    trauma_resolution_suppression,
    'Is the measured suppression (0.45) structural (legal/economic barriers to leaving, social ostracism) or internalized (descendants have internalized the necessity of carrying trauma and feel they cannot leave without losing identity)?',
    'Post-exit analysis: do descendants who leave the community and the ritual framework show rapid reductions in hypervigilance and trauma-markers (indicating suppression was structural and its removal allows recovery), or do they continue to carry the burden even without community enforcement (indicating suppression is internalized)?',
    'If suppression is primarily internalized, effective extraction is higher than the metric suggests—the burden persists even without enforcement. If structural, exit offers genuine relief. This distinction affects whether therapeutic intervention could reduce extractiveness without community action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_resolution_suppression, empirical, 'Whether suppression in this constraint is primarily structural or internalized.').

omega_variable(
    adaptive_value_contested,
    'Does the encoded trauma actually produce the claimed warning function, or does it produce pathology without demonstrable adaptive benefit?',
    'Comparison of threat-detection rates and response timing in descendants vs. non-descendants; psychological outcome measurement (anxiety, PTSD, functional impairment); assessment of whether hypervigilance improves or degrades collective threat-response.',
    'If the warning function is demonstrable and effective, extractiveness is justified by the coordination benefit. If hypervigilance produces pathology without improving threat-detection, extractiveness becomes undefended extraction. This is the central empirical question for classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_value_contested, empirical, 'Whether intergenerational trauma encoding produces an actual warning/survival function or just pathology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.54).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel, which decomposes into four structurally distinct constraints based on what ritual encodes and transmits. The trauma-encoding reading focuses on psychological burden imposed on descendants as the price of collective threat-vigilance. Sibling readings frame the same ritual as maintaining group boundaries, encoding survival techniques, or preserving symbolic coherence. Each reading has different extracted costs, different type classifications, and different beneficiary/victim structures. All four readings coexist as live positions in the scholarly and community literature; none rules out the others within a single framework, though they do create structural pressure on each other (a community emphasizing trauma-encoding may suppress the boundary-maintenance or symbolic-continuity readings to maintain focus on threat-vigilance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
