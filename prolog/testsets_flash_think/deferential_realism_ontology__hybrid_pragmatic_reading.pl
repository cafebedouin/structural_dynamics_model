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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid_pragmatic_reading' of the
 *   'deferential_realism_ontology' kernel. This reading acknowledges a fixed,
 *   observational core for natural and pure coordination constraints
 *   (Mountains, Ropes), but asserts that classifications for extractive
 *   constraints (Tangled Ropes, Snares) necessarily involve normative
 *   judgments about legitimate beneficiaries. Sibling readings include the
 *   'immutable_diagnostic_reading' (which posits a purely observational,
 *   value-neutral typology) and the 'rhetorical_scaffold_reading' (which
 *   views the typology primarily as a normative vocabulary for policy
 *   critique).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.6).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '5aff41ba-7f82-48b2-bb99-674e790e9ace').
narrative_ontology:cs_kernel_codification('5aff41ba-7f82-48b2-bb99-674e790e9ace', formalized).
narrative_ontology:cs_authority_grounding('5aff41ba-7f82-48b2-bb99-674e790e9ace', expertise).
narrative_ontology:cs_interpretation_layer_present('5aff41ba-7f82-48b2-bb99-674e790e9ace').
narrative_ontology:cs_reading_relation('5aff41ba-7f82-48b2-bb99-674e790e9ace', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aff41ba-7f82-48b2-bb99-674e790e9ace', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('5aff41ba-7f82-48b2-bb99-674e790e9ace', foundational, normative_judgment_is_irreducible_for_extraction).
narrative_ontology:cs_axiom_status(normative_judgment_is_irreducible_for_extraction, holdable).
narrative_ontology:cs_axiom_grounding('5aff41ba-7f82-48b2-bb99-674e790e9ace', normative_judgment_is_irreducible_for_extraction, deontological).
narrative_ontology:cs_axiom('5aff41ba-7f82-48b2-bb99-674e790e9ace', foundational, observational_core_is_stable).
narrative_ontology:cs_axiom_status(observational_core_is_stable, holdable).
narrative_ontology:cs_axiom_grounding('5aff41ba-7f82-48b2-bb99-674e790e9ace', observational_core_is_stable, empirically_contingent).
narrative_ontology:cs_reference_frame('5aff41ba-7f82-48b2-bb99-674e790e9ace', nuanced_classification_framework).
narrative_ontology:cs_drift_state('5aff41ba-7f82-48b2-bb99-674e790e9ace', contemporary_interdisciplinary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5aff41ba-7f82-48b2-bb99-674e790e9ace', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_users).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, normative_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, strict_empiricists).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, unaligned_institutional_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively apply and promote this hybrid reading of the DR framework, benefiting from its analytical flexibility and capacity to address complex socio-technical systems. They shape the discourse and interpretive norms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_users, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_users, beneficiary).

% Their expertise in ethical and political philosophy is central to classifying the 'contested periphery' of the typology, as this reading explicitly incorporates normative judgments about legitimate beneficiaries. They gain intellectual authority and relevance.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, normative_theorists, beneficiary,
    powerful, biographical, mobile, global).

% Bear the cost of their preferred purely observational approach being deemed insufficient for the 'contested periphery' of the typology. They are often excluded from the full interpretive authority for these classifications, as their methods are seen as incomplete.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, strict_empiricists, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, strict_empiricists, excluded).

% Bear the cost of their preferred purely rhetorical approach being deemed insufficient for the 'fixed core' of the typology. They are excluded from the full interpretive authority for these classifications, as their methods are seen as lacking ontological grounding.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_critics, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_critics, excluded).

% These are real-world entities (e.g., corporations, governments) whose arrangements might be classified as 'snares' or 'tangled_ropes' by this framework. They bear the 'cost' of this classification in terms of legitimacy challenges and increased scrutiny.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, unaligned_institutional_actors, payer,
    powerful, biographical, constrained, global).

% Study the meta-level debate surrounding the DR ontology, analyzing its internal coherence, external validity, and impact on interdisciplinary discourse. They are not directly subject to its classifications but observe its effects.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, diffuse).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a coherent and flexible framework for classifying constraints, enabling interdisciplinary dialogue and policy analysis by distinguishing between fixed, natural limits and human-made, potentially extractive arrangements, while acknowledging the role of normative judgment.
% TRANSFER_FUNCTION: Transfers interpretive authority for the 'contested periphery' of constraint classification from purely empirical observation to a combination of empirical data and normative judgment, thereby elevating the role of normative theory in institutional analysis. It also transfers the burden of justification to those whose arrangements are classified as extractive.
% ABSENT_VOICES: Proponents of purely observational or purely rhetorical approaches to constraint classification, who would argue for a simpler, less hybrid ontology, are structurally marginalized in the application of this reading.
% DISAPPEARANCE_RATIONALE: Without this hybrid reading, the Deferential Realism framework would either collapse into a purely observational tool (missing the normative dimension of extraction) or a purely rhetorical one (losing its grounding in physical/coordination realities), significantly altering its utility for institutional design and critique across various disciplines.
% FOUNDING_PROBLEM: The inadequacy of existing analytical tools to consistently classify constraints that exhibit both natural-like persistence and human-driven extraction, leading to confusion in policy debates and a lack of a common language for critique that could bridge empirical and normative concerns.
% FOUNDING_PROBLEM_CORROBORATION: Scholars in institutional economics, political science, and philosophy of science find the framework's nuanced approach essential for understanding complex socio-technical systems. Practitioners in regulatory bodies also use such hybrid tools to identify and address rent-seeking behavior, corroborating the ongoing relevance of the problem.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` (0.55) reflects the 'cost' of the interpretive work and the contestation inherent in applying normative judgments, as well as the impact on those whose arrangements are classified as extractive. `Suppression` (0.60) is moderate, indicating active intellectual and institutional effort to maintain this hybrid view against alternative framings. `Theater_ratio` (0.20) is low because the framework is actively used for analysis and debate, not merely performed. The `claimed_type` of 'tangled_rope' reflects its dual function: it coordinates understanding (a genuine function) but also involves asymmetric extraction (the 'cost' of normative judgment, the suppression of alternative views, and the impact of its classifications).
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading experience it as a necessary and robust analytical tool, while those advocating for purely empirical or purely rhetorical ontologies perceive it as an over-complicated or ideologically biased construct. The engine's classification captures this tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   DR framework users and normative theorists are beneficiaries, gaining analytical utility and intellectual authority. Strict empiricists and rhetorical critics are payers, as their preferred approaches are deemed insufficient for parts of the typology. Unaligned institutional actors are also payers, as their real-world arrangements may be subject to critical classification by this reading. The analytical observers maintain a detached perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_ambiguity,
    'Is this constraint a genuine hybrid ontology, or is its ''hybridity'' a rhetorical device to manage internal tensions within the Deferential Realism framework?',
    'Analysis of the framework''s application in novel, high-stakes policy domains: if the hybrid classification consistently yields actionable insights and is adopted by diverse, unaligned actors, it supports genuine hybridity. If it primarily serves to reconcile internal theoretical disagreements, it suggests a rhetorical function.',
    'If rhetorical, the constraint''s `theater_ratio` would be higher, and its `claimed_type` might shift towards `piton` or `scaffold` (as a temporary theoretical construct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Ambiguity regarding the ontological status of the hybrid pragmatic reading.').

omega_variable(
    immutable_diagnostic_reading_delta,
    'How would the classification of specific real-world constraints change if the ''immutable_diagnostic_reading'' (purely observational) were adopted instead of this hybrid reading?',
    'Apply both readings to a set of contested real-world constraints (e.g., intellectual property law, carbon markets) and compare the resulting classifications and their justifications.',
    'The immutable diagnostic reading would likely classify more ''tangled_ropes'' and ''snares'' as ''ropes'' or even ''mountains'' by focusing solely on observable coordination functions and ignoring normative questions of legitimate beneficiaries, leading to lower `extractiveness` and `suppression` scores for those constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutable_diagnostic_reading_delta, conceptual, 'Structural delta if the purely observational reading were adopted.').

omega_variable(
    rhetorical_scaffold_reading_delta,
    'How would the classification of specific real-world constraints change if the ''rhetorical_scaffold_reading'' (purely normative/persuasive) were adopted instead of this hybrid reading?',
    'Analyze how the framework is used in advocacy and policy debates under the rhetorical reading: if its primary function is to label and persuade rather than diagnose, it supports the rhetorical view.',
    'The rhetorical scaffold reading would likely treat all classifications as instrumental for persuasion, potentially leading to higher `theater_ratio` and `resistance` (from those being persuaded) for the framework itself, and a shift in the `claimed_type` of the framework towards `scaffold` (as a temporary persuasive tool).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rhetorical_scaffold_reading_delta, conceptual, 'Structural delta if the purely rhetorical reading were adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'deferential_realism_ontology' kernel, which also includes 'immutable_diagnostic_reading' and 'rhetorical_scaffold_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
