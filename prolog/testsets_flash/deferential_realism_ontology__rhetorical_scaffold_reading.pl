% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Ontology as Rhetorical Scaffold
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint models the Deferential Realism typology as a 'rhetorical
 *   scaffold'—a temporary support structure for normative critique. In this
 *   reading, the classification of a mechanism as a 'snare' or 'tangled rope'
 *   is not a discovery of an objective fact, but a declaration driven by
 *   normative judgment about legitimate beneficiaries. Its value lies in its
 *   persuasive power to mobilize action against perceived injustices, rather
 *   than its diagnostic accuracy. The constraint is claimed as a scaffold
 *   because its utility is transitional, aiming to shift discourse and
 *   policy, after which its specific rhetorical function might sunset.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Ontology as Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '9ab46c6e-a0a3-437b-9f17-d665d0259308').
narrative_ontology:cs_kernel_codification('9ab46c6e-a0a3-437b-9f17-d665d0259308', distributed).
narrative_ontology:cs_authority_grounding('9ab46c6e-a0a3-437b-9f17-d665d0259308', practice).
narrative_ontology:cs_interpretation_layer_present('9ab46c6e-a0a3-437b-9f17-d665d0259308').
narrative_ontology:cs_reading_relation('9ab46c6e-a0a3-437b-9f17-d665d0259308', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ab46c6e-a0a3-437b-9f17-d665d0259308', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('9ab46c6e-a0a3-437b-9f17-d665d0259308', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('9ab46c6e-a0a3-437b-9f17-d665d0259308', classification_is_normative_declaration, deontological).
narrative_ontology:cs_axiom('9ab46c6e-a0a3-437b-9f17-d665d0259308', foundational, framework_value_is_persuasive_power).
narrative_ontology:cs_axiom_status(framework_value_is_persuasive_power, holdable).
narrative_ontology:cs_axiom_grounding('9ab46c6e-a0a3-437b-9f17-d665d0259308', framework_value_is_persuasive_power, instrumental).
narrative_ontology:cs_reference_frame('9ab46c6e-a0a3-437b-9f17-d665d0259308', critical_theory_advocacy).
narrative_ontology:cs_drift_state('9ab46c6e-a0a3-437b-9f17-d665d0259308', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9ab46c6e-a0a3-437b-9f17-d665d0259308', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, critical_theory_efficacy).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_framing_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize the typology as a tool to frame policy mechanisms as 'snares' or 'tangled ropes,' thereby mobilizing public opinion and political action against them. They benefit from the persuasive power of the framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    moderate, biographical, mobile, global).

% Adopt the typology to articulate their critiques of existing institutions, finding it effective in communicating the extractive nature of certain arrangements to a broader audience. The framework provides a common language for their advocacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Develop and refine the typology, emphasizing its role in normative critique and its capacity to expose hidden power dynamics. They shape the discourse around the framework's application and interpretation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, academic_theorists, agenda_setter,
    institutional, generational, constrained, global).

% Are the targets of critiques framed by the typology. They bear the cost of defending policies labeled as 'snares' or 'tangled ropes,' facing public pressure and demands for reform. Their legitimacy is challenged by the framework's application.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers, payer,
    powerful, immediate, constrained, national).

% Believe the typology should be an objective diagnostic tool, not a rhetorical one. They are excluded from the 'rhetorical scaffold' framing, as their emphasis on objective measurement is sidelined by the focus on normative critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_proponents, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, accessible vocabulary for diverse critics and advocacy groups to coordinate their critiques of policy mechanisms, focusing on their extractive or coercive aspects.
% TRANSFER_FUNCTION: Transfers persuasive power and legitimacy to policy critics and advocacy groups, enabling them to reframe existing arrangements as illegitimate, thereby shifting public discourse and political pressure onto policymakers.
% ABSENT_VOICES: Proponents of an 'immutable diagnostic' reading of the typology are largely absent from this rhetorical framing; they would argue for objective, measurable criteria over normative judgment, but their perspective is not central to this reading's function.
% DISAPPEARANCE_RATIONALE: If this rhetorical framing of the typology vanished, policy critique would lose a powerful, widely understood vocabulary for delegitimizing extractive mechanisms. Critics would need to develop new frameworks, and the current momentum for certain policy reforms might dissipate, leading to a rearrangement of advocacy strategies.
% FOUNDING_PROBLEM: The difficulty of effectively communicating complex critiques of institutional extraction and coercion to a broad public, and the need for a framework that could mobilize normative judgment against perceived injustices.
% FOUNDING_PROBLEM_CORROBORATION: Academic theorists and advocacy groups consistently attest to the ongoing challenge of effective policy critique and the utility of the typology in this regard. Independent media analysis of public discourse and legislative debates corroborates the framework's impact on framing policy discussions, supporting the claim that the problem remains live and the framework addresses it.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the framework itself doesn't directly extract resources, but rather facilitates extraction of legitimacy from existing policies. Suppression is low (0.1) as this reading doesn't actively suppress alternative framings, but rather prioritizes its own rhetorical utility. Theater ratio is high (0.6) because the 'objective' appearance of the typology often masks its underlying normative and persuasive intent. Accessibility collapse is low (0.3) as alternative critical frameworks remain available, and resistance is minimal (0.05) because the framework is a tool for critique, not a target of it in this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy critics, the typology is a highly effective and legitimate tool for social change. From the perspective of policymakers, it is a rhetorical weapon that unfairly delegitimizes their efforts. The 'immutable diagnostic' proponents see it as a misapplication of a potentially objective tool. This divergence is central to the constraint's function as a rhetorical scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics and advocacy groups are beneficiaries (d near 0.0) as they gain a powerful tool for their work. Academic theorists act as agenda-setters (d near 0.5), shaping the framework's application. Policymakers are payers (d near 1.0) as they bear the cost of defending against critiques framed by the typology. Proponents of an immutable diagnostic reading are excluded, as their perspective is not central to this reading's function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_empirical_classification,
    'To what extent are classifications within the Deferential Realism typology (e.g., ''snare'') genuinely empirical observations versus normative declarations driven by a desired policy outcome?',
    'Analysis of classification disputes: if resolution consistently depends on re-evaluating normative claims about legitimacy rather than new empirical data, it supports the ''rhetorical scaffold'' reading.',
    'If classification is primarily normative, the framework''s utility shifts from objective diagnosis to a tool for advocacy, potentially altering its perceived legitimacy and application in policy debates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_vs_empirical_classification, conceptual, 'Ambiguity between descriptive and prescriptive use of the typology.').

omega_variable(
    persuasive_power_durability,
    'How durable is the persuasive power of the ''rhetorical scaffold'' reading of the typology, particularly if its normative underpinnings become widely acknowledged?',
    'Longitudinal study of policy discourse: track the typology''s influence in public and political debates over time, especially after critical analyses expose its rhetorical nature.',
    'If its persuasive power diminishes upon exposure of its normative basis, the ''scaffold'' function might collapse, requiring new rhetorical tools for critique. If it persists, it suggests a deeper resonance of its normative claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasive_power_durability, empirical, 'The lifespan and resilience of the framework''s rhetorical efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
