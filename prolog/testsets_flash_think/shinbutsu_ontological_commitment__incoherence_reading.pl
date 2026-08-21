% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shūgō: Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the state of 'shinbutsu-shūgō' (syncretism of
 *   kami and buddhas) in pre-Meiji Japan, specifically from the perspective
 *   that it represented an institutionally tolerated ontological incoherence
 *   rather than a unified system. The constraint is the *tolerance* of this
 *   ambiguity, which allowed diverse religious practices to coexist without
 *   requiring a stable, coherent metaphysical framework. This reading
 *   emphasizes the lack of a definitive commitment, which ultimately
 *   facilitated the ease of separation during the Meiji Restoration. The
 *   claimed type is Piton because the 'function' of providing a coherent
 *   framework atrophied into mere inertial tolerance, which was easily
 *   dismantled when a new agenda arose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.3).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.2).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shūgō: Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, 'f3fae6c5-521c-47ab-ab38-6eed54c3ae1a').
narrative_ontology:cs_kernel_codification('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', implicit).
narrative_ontology:cs_authority_grounding('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', practice).
narrative_ontology:cs_interpretation_layer_present('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a').
narrative_ontology:cs_reading_relation('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_axiom('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', foundational, ontological_ambiguity_is_functional).
narrative_ontology:cs_axiom_status(ontological_ambiguity_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', ontological_ambiguity_is_functional, conventional).
narrative_ontology:cs_axiom('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', foundational, no_unified_metaphysical_system).
narrative_ontology:cs_axiom_status(no_unified_metaphysical_system, holdable).
narrative_ontology:cs_axiom_grounding('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', no_unified_metaphysical_system, empirically_contingent).
narrative_ontology:cs_reference_frame('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', pre_meiji_tolerated_ambiguity).
narrative_ontology:cs_drift_state('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f3fae6c5-521c-47ab-ab38-6eed54c3ae1a', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, syncretic_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_communities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_pre_meiji).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, buddhist_monks_pre_meiji).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, scholars_seeking_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious institutions that integrated Shinto and Buddhist elements, benefiting from the flexibility and lack of strict ontological boundaries, allowing them to serve diverse community needs without doctrinal conflict. Later, these institutions were targeted for dissolution during the Meiji separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, syncretic_institutions, beneficiary,
    organized, generational, constrained, local).

% Practiced a blend of Shinto and Buddhist rituals and beliefs, finding practical utility and spiritual meaning in the fluid boundaries. The incoherence allowed for local adaptation and synthesis without external theological imposition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_communities, beneficiary,
    moderate, biographical, mobile, local).

% Operated within a system where the distinction between kami and buddhas was often blurred, allowing for flexible ritual practice and patronage without needing to reconcile deep ontological differences. Their professional identity was often intertwined with this ambiguity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shinto_priests_pre_meiji, beneficiary,
    moderate, biographical, constrained, local).

% Similarly benefited from the institutional tolerance of incoherence, allowing for the integration of local kami worship into Buddhist frameworks (e.g., honji-suijaku theory) without requiring a definitive, exclusive ontological commitment.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_monks_pre_meiji, beneficiary,
    moderate, biographical, constrained, local).

% Intellectuals and theologians who sought a clear, unified, or logically consistent ontological framework for Japanese religious practice. They bore the cost of intellectual frustration and the inability to establish a stable, coherent system due to the prevailing institutional tolerance of ambiguity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, scholars_seeking_coherence, payer,
    powerless, biographical, constrained, national).

% The new political elite of the Meiji Restoration who sought to establish Shinto as the state religion and separate it from Buddhism (Shinbutsu-bunri). They benefited from the prior ontological incoherence, as it made the separation easier to implement due to the lack of a deeply integrated, coherent system to resist their reforms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed diverse religious practices and institutions (Shinto shrines, Buddhist temples, local cults) to coexist and intermingle without requiring a unified theological or ontological framework, thereby preventing internal doctrinal conflicts and facilitating flexible local adaptation.
% TRANSFER_FUNCTION: Transferred the burden of ontological reconciliation from central institutions to individual practitioners or left it unresolved, effectively trading intellectual clarity for practical coexistence and institutional flexibility.
% ABSENT_VOICES: Theologians or intellectual reformers who might have pushed for a clear, unified, or logically consistent ontological system were not institutionally empowered to enforce such a system, remaining largely on the periphery of mainstream religious practice.
% DISAPPEARANCE_RATIONALE: The Meiji government's forced separation of Shinto and Buddhism (Shinbutsu-bunri) in 1868 demonstrated that the prior state of institutional tolerance for ontological incoherence was not a robust, self-sustaining system. Once this tolerance was removed, the religious landscape of Japan underwent a dramatic reorganization, with syncretic institutions being dismantled and new, distinct Shinto and Buddhist identities being enforced.
% FOUNDING_PROBLEM: To manage the coexistence of indigenous Shinto practices and imported Buddhism without requiring a definitive, unified theological system, thereby avoiding doctrinal conflict and allowing for flexible local adaptation and synthesis.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the Meiji Restoration and its religious policies, as well as scholarly analyses of pre-Meiji religious practice, corroborate that the problem of managing coexistence without strict ontological commitment was a defining feature of the era, and that the Meiji state actively sought to resolve it by imposing separation. This corroboration comes from historical records and academic scholarship, not solely from the beneficiaries of the prior system.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low-moderate (0.3) as the incoherence primarily extracted intellectual clarity and prevented the development of a unified religious identity, rather than material resources. Suppression is low (0.2) because the state was one of tolerance and inertia, not active enforcement of ambiguity. The theater ratio is low (0.1) as the incoherence was a genuine, lived reality, not a performance. Accessibility collapse and resistance are also low, as conceptual alternatives for coherence or separation always existed, even if not institutionally dominant. The constraint persisted due to institutional inertia, making it a Piton that was easily disrupted by the Meiji state's reforms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of syncretic institutions and local communities, the tolerance of incoherence was a functional arrangement that allowed for rich, adaptable religious life. From the perspective of scholars seeking coherence, it was a frustrating lack of intellectual rigor. The Meiji state viewed it as an obstacle to national unity, but one whose inherent fragility made it susceptible to rapid reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic institutions, local communities, and pre-Meiji Shinto priests and Buddhist monks were beneficiaries, as the tolerance of incoherence allowed them flexibility and avoided doctrinal conflict. Scholars seeking coherence were victims, bearing the intellectual cost of ambiguity. The Meiji state builders, while ultimately dismantling the constraint, benefited from its inherent weakness and lack of deep structural coherence, which made the separation easier to implement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_syncretism_definition,
    'Is the pre-Meiji shinbutsu-shūgō best characterized as an ''incoherence'' (lack of stable commitment) or a ''syncretism'' (unified cosmological order)?',
    'Detailed textual analysis of primary religious doctrines and institutional records, focusing on explicit statements of ontological relationship versus practical coexistence without theoretical reconciliation.',
    'If truly syncretic, the constraint would be a Rope (coordinating a unified system) with higher extractiveness (cost of maintaining the unified system). If incoherent, it remains a Piton, highlighting its fragility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incoherence_vs_syncretism_definition, conceptual, 'Distinguishing between a lack of commitment and a unified, albeit complex, commitment.').

omega_variable(
    separation_ease_causality,
    'To what extent did the ontological incoherence directly cause the ease of the Meiji Shinbutsu-bunri, versus other political or social factors?',
    'Comparative historical analysis with other cases of religious separation, controlling for political centralization and social cohesion, to isolate the impact of ontological clarity.',
    'If incoherence was a primary cause, it reinforces the Piton classification by highlighting its structural weakness. If other factors were dominant, the constraint''s fragility might be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_ease_causality, empirical, 'Assessing the causal role of incoherence in the Meiji separation.').

omega_variable(
    kernel_reading_incoherence,
    'This constraint is the ''incoherence_reading'' of the ''shinbutsu_ontological_commitment'' kernel. What structural elements would change if a sibling reading were adopted?',
    'Analyzing the core axioms of the ''syncretic_reading'' (unified cosmological order) or ''partition_reading'' (separate domains) and their implications for institutional structure, power dynamics, and enforcement mechanisms.',
    'The ''syncretic_reading'' would imply a stronger, more actively coordinated constraint (likely a Rope or Tangled Rope) with higher extractiveness (cost of maintaining the unified system). The ''partition_reading'' would imply a constraint that actively maintained boundaries (likely a Rope or Snare) with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incoherence, conceptual, 'Structural implications of alternative readings of the shinbutsu ontological commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1000, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1000, 0.25).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1400, 0.3).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.3).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1000, 0.15).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1200, 0.18).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1400, 0.2).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
