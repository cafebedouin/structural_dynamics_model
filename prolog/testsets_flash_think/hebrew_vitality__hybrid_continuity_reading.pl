% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents an analytical synthesis, a 'hybrid continuity'
 *   reading of Hebrew vitality. It posits that liturgical preservation
 *   provided a necessary substrate, but vernacular revival required active
 *   reconstruction. It is a framework for understanding, not an actionable
 *   rule. The low extractiveness and suppression reflect its nature as a
 *   descriptive, rather than coercive, constraint. Its primary function is to
 *   coordinate understanding among scholars and practitioners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.1).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, 'adc36ff6-6207-4507-8eb3-06d66d43867d').
narrative_ontology:cs_kernel_codification('adc36ff6-6207-4507-8eb3-06d66d43867d', distributed).
narrative_ontology:cs_authority_grounding('adc36ff6-6207-4507-8eb3-06d66d43867d', expertise).
narrative_ontology:cs_interpretation_layer_present('adc36ff6-6207-4507-8eb3-06d66d43867d').
narrative_ontology:cs_reading_relation('adc36ff6-6207-4507-8eb3-06d66d43867d', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('adc36ff6-6207-4507-8eb3-06d66d43867d', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('adc36ff6-6207-4507-8eb3-06d66d43867d', foundational, vitality_requires_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('adc36ff6-6207-4507-8eb3-06d66d43867d', vitality_requires_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('adc36ff6-6207-4507-8eb3-06d66d43867d', foundational, vitality_is_multidimensional_process).
narrative_ontology:cs_axiom_status(vitality_is_multidimensional_process, holdable).
narrative_ontology:cs_axiom_grounding('adc36ff6-6207-4507-8eb3-06d66d43867d', vitality_is_multidimensional_process, empirically_contingent).
narrative_ontology:cs_reference_frame('adc36ff6-6207-4507-8eb3-06d66d43867d', multidimensional_language_ecology).
narrative_ontology:cs_drift_state('adc36ff6-6207-4507-8eb3-06d66d43867d', contemporary_sociolinguistic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('adc36ff6-6207-4507-8eb3-06d66d43867d', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_revitalization_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, native_speakers_of_hebrew).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and refine theoretical frameworks for language vitality, benefiting from a more nuanced understanding of Hebrew's history. Their work shapes the discourse and informs practical efforts.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_revitalization_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Benefits from a comprehensive analytical framework that guides strategic planning for language promotion and education, avoiding monocausal pitfalls. Their efforts are informed by this understanding.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_revitalization_movement, beneficiary,
    organized, biographical, constrained, national).

% Their historical practices are recognized as a necessary substrate for Hebrew's revival, validating their role in continuity. However, this reading also implies their practices alone are insufficient for full vernacular vitality, which might challenge their self-perception.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_communities, payer,
    moderate, generational, identity_locked, global).

% Represent the ultimate goal of vernacular revival. This reading provides a historical and theoretical context for their existence, affirming the complex path that led to a living, spoken Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_speakers_of_hebrew, beneficiary,
    moderate, biographical, mobile, national).

% Analyze the case of Hebrew within broader theories of language death and revival, using this reading as a complex empirical example. They are not directly affected by the constraint but contribute to its intellectual context.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, linguistic_theorists, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding among scholars and practitioners regarding the complex, multi-stage process of language revitalization, emphasizing the interplay of historical preservation and active reconstruction.
% TRANSFER_FUNCTION: Transfers analytical clarity and strategic guidance from sociolinguistic scholarship to language revitalization practitioners; transfers recognition of historical continuity to communities maintaining liturgical use.
% ABSENT_VOICES: Proponents of monocausal theories of language vitality (e.g., those who believe only liturgical use matters, or only native generation matters) might object to the complexity and nuanced interplay proposed by this hybrid reading.
% DISAPPEARANCE_RATIONALE: If this analytical framework vanished, the understanding of Hebrew's revival would fragment into simpler, potentially misleading narratives, hindering effective strategies for other language revitalization efforts and obscuring the historical dynamics of Hebrew itself.
% FOUNDING_PROBLEM: The historical and theoretical challenge of explaining how Hebrew transitioned from a primarily liturgical language to a modern, spoken vernacular, and identifying the necessary and sufficient conditions for this unique revitalization.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic studies of language contact and shift, historical analyses of the Zionist movement and its language policies, and comparative studies of other language revitalization efforts, all from independent academic sources, corroborate the complexity of the problem.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it coordinates understanding and provides a beneficial framework for analyzing language revitalization, with minimal extraction or suppression. Extractiveness is low (0.1) as it's an analytical tool, not a mechanism for rent collection. Suppression is low (0.05) as it doesn't actively coerce behavior, though it may intellectually challenge simpler views. Theater ratio is low (0.05) as its function is primarily intellectual and descriptive. Accessibility collapse is moderate (0.7) because adopting this nuanced framework makes monocausal explanations less credible. Resistance is low (0.2) as it's an academic synthesis, not a policy mandate.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for a comprehensive understanding, proponents of purely liturgical or purely native-generation views might perceive it differently. The engine's classification as a Rope reflects its coordinating function for those who adopt it, but those holding simpler views might see it as an unnecessary complication or even a challenge to their established narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   Language revitalization scholars and the Hebrew revitalization movement are beneficiaries, gaining clarity and strategic guidance. Liturgical communities are payers in the sense that their historical role is re-evaluated as 'necessary but insufficient,' which might require adjusting their self-understanding. Native speakers of Hebrew are beneficiaries as their existence is contextualized by this complex historical process. Linguistic theorists are observers, using this case to refine broader theories.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine analytical synthesis, or does it implicitly favor one aspect (liturgical vs. vernacular) over the other?',
    'Comparative analysis with other language revitalization cases to test the generalizability and balance of the ''hybrid continuity'' model.',
    'If found to implicitly favor one aspect, its classification might shift towards a more extractive type if that bias serves a particular agenda, or its coordination function might be seen as less pure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''hybrid_continuity_reading'' of the ''hebrew_vitality'' kernel, which attempts to integrate ''liturgical_reading'' and ''native_daily_reading''.').

omega_variable(
    empirical_sufficiency_of_substrate,
    'To what extent was liturgical preservation truly a ''necessary enabler'' for vernacular revival, and could other forms of textual or cultural continuity have served a similar function?',
    'Detailed historical and comparative sociolinguistic research into other language revival movements lacking a strong liturgical tradition.',
    'If other forms of continuity are found to be equally effective, the ''necessity'' claim of liturgical preservation might be weakened, potentially altering the perceived balance of the hybrid continuity model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_sufficiency_of_substrate, empirical, 'Assessing the empirical necessity of liturgical preservation as a substrate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(hebr_be_t1940, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(hebr_be_t1980, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(hebr_su_t1940, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1940, 0.05).
narrative_ontology:measurement(hebr_su_t1980, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
