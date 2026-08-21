% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story describes the 'hybrid pragmatic reading' of the
 *   Deferential Realism ontology, which posits a typology with a fixed core
 *   (mountains, ropes) grounded in physical and coordination constraints, but
 *   a contested periphery (tangled_ropes, snares) where classification
 *   depends on normative judgments about legitimate beneficiaries. This
 *   reading acknowledges the irreducible role of values and interpretation in
 *   understanding social constraints, bridging purely empirical and purely
 *   rhetorical approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.65).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '09e7f523-2ab5-4824-91a2-535a249bbf9f').
narrative_ontology:cs_kernel_codification('09e7f523-2ab5-4824-91a2-535a249bbf9f', formalized).
narrative_ontology:cs_authority_grounding('09e7f523-2ab5-4824-91a2-535a249bbf9f', expertise).
narrative_ontology:cs_interpretation_layer_present('09e7f523-2ab5-4824-91a2-535a249bbf9f').
narrative_ontology:cs_reading_relation('09e7f523-2ab5-4824-91a2-535a249bbf9f', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e7f523-2ab5-4824-91a2-535a249bbf9f', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('09e7f523-2ab5-4824-91a2-535a249bbf9f', foundational, classification_is_hybrid_empirical_normative).
narrative_ontology:cs_axiom_status(classification_is_hybrid_empirical_normative, holdable).
narrative_ontology:cs_axiom_grounding('09e7f523-2ab5-4824-91a2-535a249bbf9f', classification_is_hybrid_empirical_normative, conventional).
narrative_ontology:cs_axiom('09e7f523-2ab5-4824-91a2-535a249bbf9f', foundational, epistemic_pluralism_is_necessary).
narrative_ontology:cs_axiom_status(epistemic_pluralism_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('09e7f523-2ab5-4824-91a2-535a249bbf9f', epistemic_pluralism_is_necessary, deontological).
narrative_ontology:cs_reference_frame('09e7f523-2ab5-4824-91a2-535a249bbf9f', integrated_epistemic_pluralism).
narrative_ontology:cs_drift_state('09e7f523-2ab5-4824-91a2-535a249bbf9f', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('09e7f523-2ab5-4824-91a2-535a249bbf9f', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, purist_diagnosticians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the scholars and practitioners who apply and refine the Deferential Realism typology. They benefit from a framework that can bridge empirical observation and normative judgment, allowing for a more nuanced understanding of complex social phenomena. They actively shape the interpretation of the typology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers, agenda_setter,
    institutional, generational, analytical, universal).

% These actors use the hybrid pragmatic reading of the typology to inform the design and critique of institutions. They benefit from its flexibility in addressing real-world problems that involve both objective constraints and value-laden choices, finding it a useful tool for navigating complexity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers, beneficiary,
    organized, biographical, mobile, global).

% These are scholars or practitioners who seek a purely objective, value-neutral classification of constraints. They find the 'contested periphery' and the reliance on 'normative judgments' to be a cost, as it complicates their pursuit of an empirically verifiable, observer-independent typology. They bear the burden of this ambiguity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, purist_diagnosticians, payer,
    moderate, biographical, constrained, global).

% These critics view any constraint typology primarily as a rhetorical tool for persuasion or legitimation, rather than a diagnostic instrument. They are largely excluded from the core conversation of this reading, which seeks to ground its classifications in a hybrid reality, not just persuasive power.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_critics, excluded,
    moderate, biographical, mobile, global).

% These are the aspects of reality or social arrangements that resist easy categorization within the typology, particularly in its contested periphery. They are 'excluded' in the sense of not fitting neatly, forcing the typology to adapt or acknowledge its limits.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, unclassified_phenomena, excluded,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(deferential_realism_ontology__hybrid_pragmatic_reading, unclassified_phenomena).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared conceptual framework for understanding and classifying constraints, enabling interdisciplinary dialogue by acknowledging both fixed empirical realities and the role of normative judgment in social constructs.
% TRANSFER_FUNCTION: Transfers the burden of classification from a purely observational task to a hybrid one, distributing the 'cost' of ambiguity and contestation across the analytical community, while providing a flexible lens for institutional critique.
% ABSENT_VOICES: Those who insist on a purely objective, value-free classification system (e.g., extreme positivists) or those who view all classification as purely rhetorical and ungrounded (e.g., extreme relativists) are largely absent from the core discourse of this hybrid reading.
% DISAPPEARANCE_RATIONALE: If this hybrid pragmatic reading vanished, the discourse around constraints would likely polarize. Analysts would either retreat to a purely empirical, often insufficient, diagnostic approach or a purely rhetorical, ungrounded critique, losing the capacity to bridge these perspectives and engage with the full complexity of social reality.
% FOUNDING_PROBLEM: The need for a robust analytical framework that could account for both the objective, 'mountain-like' aspects of reality and the constructed, value-laden nature of social constraints, without collapsing into either naive realism or pure relativism.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of science, sociologists of knowledge, and practitioners in fields like institutional economics and political theory corroborate this problem, recognizing the inherent hybridity of social reality and the limitations of purely monistic analytical approaches. This is attested in academic literature and interdisciplinary conferences.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.65) reflects the 'cost' of navigating the contested periphery, the intellectual effort required for normative judgments, and the potential for some classifications to be imposed. `suppression` (0.55) is medium, representing the implicit pressure to conform to the hybrid framework while still allowing for contestation in the periphery. `theater_ratio` (0.25) is low-to-moderate, as the pragmatic approach values genuine analysis over pure performance, but still involves some 'performance' of objectivity in contested areas. `accessibility_collapse` (0.5) is moderate: alternatives for core classifications are collapsed, but for peripheral ones, they remain open. `resistance` (0.7) is high, reflecting the 'openly contested' nature of peripheral classifications.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observers and institutional designers experience this framework as a valuable, enabling tool for understanding complex reality. In contrast, purist diagnosticians perceive it as a compromise that introduces unwanted subjectivity and ambiguity, making their analytical task more difficult. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytical observers and institutional designers are beneficiaries; they gain a flexible and powerful framework for analysis and design. Purist diagnosticians are targets/payers, as their preferred mode of purely objective analysis is challenged, and they bear the cost of the framework's inherent ambiguity. Rhetorical critics and unclassified phenomena are excluded, as their perspectives are not central to this reading's core function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Deferential Realism ontology, or merely a nuanced application of a single, underlying framework?',
    'Analysis of the logical coherence and internal consistency of each reading''s core axioms and their implications for classification. If the axioms lead to fundamentally different, irreconcilable classification outcomes, they are distinct readings.',
    'If it''s a distinct reading, its classification stands as an independent analysis. If it''s merely a nuanced application, the core ontology might be a single, more fundamental constraint, and this would be a sub-component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s status as a distinct reading of the DR ontology kernel.').

omega_variable(
    normative_empirical_boundary,
    'Where precisely does the ''fixed core'' (empirical observation) end and the ''contested periphery'' (normative judgment) begin in practice?',
    'Case studies and detailed empirical analysis of specific constraints, tracing the points at which observational data becomes insufficient and normative arguments become decisive for classification.',
    'A clearer boundary would reduce the perceived extractiveness and suppression for purist diagnosticians, potentially shifting the classification towards a more ''rope-like'' function. An ambiguous boundary reinforces the ''tangled_rope'' nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_empirical_boundary, empirical, 'Ambiguity of the boundary between empirical and normative classification.').

omega_variable(
    classification_legitimacy_source,
    'For classifications in the ''contested periphery'', what is the ultimate source of their legitimacy: consensus among analysts, persuasive power, or a deeper, albeit contested, normative grounding?',
    'Sociological study of scientific and policy communities, analyzing how classifications are adopted, challenged, and stabilized in practice, and the arguments used to defend them.',
    'If legitimacy primarily derives from consensus or persuasion, the ''theater_ratio'' might be higher, and the ''tangled_rope'' aspect more pronounced due to the active maintenance of agreement. If it''s deeper normative grounding, the ''extractiveness'' might be more inherent to the intellectual work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_legitimacy_source, conceptual, 'Source of legitimacy for contested classifications in the periphery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(defe_tr_t50, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(defe_be_t50, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(defe_su_t50, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'deferential_realism_ontology' kernel. Each reading offers a different structural interpretation of the typology's nature and function, leading to different epsilon values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
