% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual — Hybrid Atrophy Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the hybrid_atrophy_reading of the
 *   catastrophe_memory_preservation kernel. The ritual began as a genuine
 *   survival-competence preservation system (coordination function: teaching
 *   each generation how to recognize and survive the specific catastrophe
 *   that nearly ended the group). Under modernity — state disaster
 *   management, scientific early-warning systems, geographic mobility, and
 *   secularization — the survival function became obsolete. The ritual
 *   persisted, but its content shifted from procedural instruction to
 *   symbolic mourning and identity affirmation. The present generation
 *   inherits the full cost structure (time, resources, emotional labor)
 *   without the adaptive payoff. The constraint is a piton: the original
 *   coordination function has atrophied, but the practice continues through
 *   institutional inertia and identity-theater maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.35).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual — Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '1fef71f8-0ee9-427d-8e07-0564bcff2528').
narrative_ontology:cs_kernel_codification('1fef71f8-0ee9-427d-8e07-0564bcff2528', distributed).
narrative_ontology:cs_authority_grounding('1fef71f8-0ee9-427d-8e07-0564bcff2528', practice).
narrative_ontology:cs_interpretation_layer_present('1fef71f8-0ee9-427d-8e07-0564bcff2528').
narrative_ontology:cs_reading_relation('1fef71f8-0ee9-427d-8e07-0564bcff2528', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fef71f8-0ee9-427d-8e07-0564bcff2528', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('1fef71f8-0ee9-427d-8e07-0564bcff2528', foundational, ritual_atrophied_from_survival_to_mourning).
narrative_ontology:cs_axiom_status(ritual_atrophied_from_survival_to_mourning, holdable).
narrative_ontology:cs_axiom_grounding('1fef71f8-0ee9-427d-8e07-0564bcff2528', ritual_atrophied_from_survival_to_mourning, empirically_contingent).
narrative_ontology:cs_axiom('1fef71f8-0ee9-427d-8e07-0564bcff2528', foundational, present_generation_bears_cost_without_adaptive_payoff).
narrative_ontology:cs_axiom_status(present_generation_bears_cost_without_adaptive_payoff, holdable).
narrative_ontology:cs_axiom_grounding('1fef71f8-0ee9-427d-8e07-0564bcff2528', present_generation_bears_cost_without_adaptive_payoff, empirically_contingent).
narrative_ontology:cs_reference_frame('1fef71f8-0ee9-427d-8e07-0564bcff2528', pre_modern_survival_ritual).
narrative_ontology:cs_drift_state('1fef71f8-0ee9-427d-8e07-0564bcff2528', modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1fef71f8-0ee9-427d-8e07-0564bcff2528', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, non_practicing_descendants).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_preserves_symbolic_continuity).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, collective_identity_requires_commemorative_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform the commemorative ritual regularly; gain in-group identity, social cohesion, and moral standing within the community. Bear significant time, resource, and emotional costs (elaborate preparations, fasting, travel, psychological burden of re-enacting trauma). Exit would mean loss of community membership and identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_practitioners, payer).

% Descendants of the catastrophe-affected group who do not actively practice the ritual but face implicit social pressure to participate or support it financially. Bear reputational and occasional material costs without receiving the identity benefits that practitioners report. Can exit by relocating or disaffiliating, but at cost of family/community ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, non_practicing_descendants, payer,
    moderate, biographical, mobile, regional).

% Study the ritual as a case of cultural atrophy: a practice that historically encoded survival-relevant threat recognition but now functions primarily as identity theater. Their analysis does not affect the constraint's operation but provides the external observational baseline for this classification.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, cultural_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically: coordinated intergenerational threat recognition and survival behavior (evacuation routes, resource caching, social solidarity under crisis). Presently: coordinates collective identity affirmation and symbolic continuity with the past.
% TRANSFER_FUNCTION: Transfers time, labor, emotional energy, and material resources from the present generation (both practitioners and non-practicing descendants) to the maintenance of the ritual apparatus (specialists, venues, paraphernalia) and the symbolic capital of the community.
% ABSENT_VOICES: The pre-modern survivors for whom the ritual was a living survival technology — they cannot testify whether the current form resembles the functional original. Also, potential reformers within the community who would simplify or abandon the ritual but are silenced by identity-policing.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose its primary structured practice of collective catastrophe memory. Identity cohesion would degrade, intergenerational transmission of the catastrophe narrative would become haphazard, and the community's distinctive moral claim (we remember, therefore we are) would weaken. However, no physical survival capacity would be lost — the adaptive function atrophied generations ago.
% FOUNDING_PROBLEM: The ritual was founded to preserve operational threat-recognition competence across generations after a catastrophic event (e.g., flood, invasion, famine) that nearly destroyed the group. It encoded specific survival behaviors: recognizing precursor signs, coordinating evacuation, allocating scarce resources, maintaining group cohesion under panic.
% FOUNDING_PROBLEM_CORROBORATION: Historical ethnographies and oral histories collected by early anthropologists (e.g., Boas, Malinowski) document the ritual's explicit instructional content — survival procedures taught through re-enactment. Contemporary practitioners and community elders acknowledge the survival content has been lost; the ritual is now described as 'honoring the ancestors' and 'keeping our identity alive.' No living practitioner claims the ritual teaches actionable survival skills.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.62) when the ritual still carried residual survival-relevant information, then declines as that content is lost and only the identity-theater remains. Theater ratio rises steadily from 0.15 to 0.72, tracking the replacement of functional content with performative elaboration. Suppression requirement remains low and flat — the ritual persists through social expectation and identity pressure, not active coercion. Accessibility collapse is low (0.32) because alternatives exist (secular commemoration, historical study, non-participation) but are socially costly. Resistance is moderate (0.41) — some descendants quietly drop out, but open criticism is policed by identity-policing.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the ritual feels like meaningful identity work (low effective extraction). From the non-practicing descendant seat, it feels like a tax on their heritage (higher effective extraction). The analytical observer sees the structural atrophy: the constraint's original justification is dead, but the cost structure persists. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual practitioners are both beneficiaries (identity, cohesion) and payers (costs of performance). Their directionality is near-symmetric (d ≈ 0.5) — they choose to stay because the identity benefit offsets the cost, but exit is constrained by community ties. Non-practicing descendants are payers with mobile exit — they bear reputational costs but can leave. The historical survivors (the original beneficiaries) are absent; their survival was the original coordination function's payoff. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (survival competence) is dead — modern disaster infrastructure has superseded the ritual's operational role. Yet the ritual persists because it now serves a different function: identity maintenance. This is classic mandatrophy: the mandate (preserve survival competence) has been replaced by a new, undeclared mandate (preserve group identity) that the constraint's administrators (ritual specialists) benefit from. The constraint is not a snare because the practitioners genuinely value the identity function; it is a piton because the original coordination apparatus is maintained theatrically after its functional death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a structurally distinct reading of the catastrophe_memory_preservation kernel, or does it collapse into one of the sibling readings?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, temporal profiles, and claimed coordination functions. If the survival_competence_reading and mourning_practice_reading are limiting cases of this reading''s trajectory, they are not independent constraints.',
    'If not distinct, this story duplicates another reading and should be merged. Distinctness validates the kernel decomposition approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this reading''s structural independence from sibling readings.').

omega_variable(
    historical_survival_function,
    'Did the ritual ever genuinely preserve survival-competence, or is that a retrospective narrative constructed by the current identity-maintenance function?',
    'Cross-cultural and historical analysis of ritual content correlated with documented survival outcomes. Ethnographic records from pre-modern periods, archaeological evidence of coordinated evacuation behavior, and comparative study of similar rituals in groups that faced recurrent catastrophes.',
    'If the survival function is a retrospective narrative, the piton classification (atrophied from genuine coordination) is a false summit — the constraint never had the coordination function it claims to have atrophied from. It would be a snare or rope from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_survival_function, empirical, 'Whether the atrophied coordination function was real or a founding myth.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression requirement structural (weak enforcement machinery) or internalized (participants police themselves through identity fusion)?',
    'Post-exit suppression trajectory: track individuals who leave the community — do they continue to feel compelled to perform the ritual? If suppression persists after exit, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the piton classification more extractive than metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in identity-locked constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_mem_hybrid_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_mem_hybrid_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cata_mem_hybrid_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(cata_mem_hybrid_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(cata_mem_hybrid_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.68).
narrative_ontology:measurement(cata_mem_hybrid_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(cata_mem_hybrid_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cata_mem_hybrid_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_mem_hybrid_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(cata_mem_hybrid_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(cata_mem_hybrid_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(cata_mem_hybrid_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cata_mem_hybrid_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_mem_hybrid_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_mem_hybrid_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cata_mem_hybrid_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(cata_mem_hybrid_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement(cata_mem_hybrid_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'catastrophe memory ritual' into three structurally distinct claims: (1) survival_competence_reading — the ritual still teaches actionable survival skills (Mountain if true, low extraction); (2) hybrid_atrophy_reading — the ritual once did but has atrophied to identity theater (Piton, moderate declining extraction); (3) mourning_practice_reading — the ritual was always symbolic, never operational (Rope or Tangled Rope depending on whether identity coordination is genuine). Each reading has different ε, different beneficiaries/victims, and different temporal profiles. They are linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
