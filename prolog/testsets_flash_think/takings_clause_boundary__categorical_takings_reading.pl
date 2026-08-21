% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Rule (Takings Clause Boundary Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint defines the boundary of the Takings Clause, establishing
 *   that permanent physical occupations and regulations causing total value
 *   elimination are 'per se' takings requiring compensation. All other
 *   regulations are evaluated under the multi-factor Penn Central balancing
 *   test. This reading attempts to provide clear, predictable rules for
 *   extreme cases while maintaining flexibility for government regulation in
 *   the vast middle ground. It is one specific interpretation of the broader
 *   'takings_clause_boundary' kernel.
 *
 * KEY AGENTS:
 *   - Property Owners: Primary beneficiaries in per se cases, payers in Penn Central failures.
 *   - Government Regulators: Agenda-setters for regulation, payers in per se cases, beneficiaries in Penn Central successes.
 *   - Courts: Primary enforcers and interpreters of the rule.
 *   - Public Interest Advocates: Observers and litigators, influencing the application of the rule.
 *   - Developers and Investors: Bear costs and seek benefits within the regulatory framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.6).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.7).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Rule (Takings Clause Boundary Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '9f1f3199-5093-4249-9d19-dc49191d6964').
narrative_ontology:cs_kernel_codification('9f1f3199-5093-4249-9d19-dc49191d6964', formalized).
narrative_ontology:cs_authority_grounding('9f1f3199-5093-4249-9d19-dc49191d6964', lineage).
narrative_ontology:cs_interpretation_layer_present('9f1f3199-5093-4249-9d19-dc49191d6964').
narrative_ontology:cs_reading_relation('9f1f3199-5093-4249-9d19-dc49191d6964', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('9f1f3199-5093-4249-9d19-dc49191d6964', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('9f1f3199-5093-4249-9d19-dc49191d6964', foundational, private_property_fundamental_right).
narrative_ontology:cs_axiom_status(private_property_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('9f1f3199-5093-4249-9d19-dc49191d6964', private_property_fundamental_right, deontological).
narrative_ontology:cs_axiom('9f1f3199-5093-4249-9d19-dc49191d6964', foundational, government_power_to_regulate_for_public_good).
narrative_ontology:cs_axiom_status(government_power_to_regulate_for_public_good, holdable).
narrative_ontology:cs_axiom_grounding('9f1f3199-5093-4249-9d19-dc49191d6964', government_power_to_regulate_for_public_good, deontological).
narrative_ontology:cs_reference_frame('9f1f3199-5093-4249-9d19-dc49191d6964', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('9f1f3199-5093-4249-9d19-dc49191d6964', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9f1f3199-5093-4249-9d19-dc49191d6964', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, developers_and_investors).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, developers_and_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear protection against permanent physical occupations and total value elimination, ensuring compensation in these extreme cases. However, they bear the cost of regulations that do not meet these thresholds, subject to the less predictable Penn Central balancing test, where their claims often fail.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, property_owners, payer).

% Benefit from the flexibility to enact a wide range of regulations under the Penn Central test without triggering compensation. They bear the cost of compensation when their actions constitute a per se taking (physical occupation or total value elimination), which limits their regulatory power in those specific areas.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, government_regulators, payer).

% Interpret and apply the Takings Clause, defining the boundaries of this rule. They are responsible for enforcing the per se rules and conducting the Penn Central balancing test, shaping the practical impact of the constraint on both property owners and regulators.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Monitor and litigate cases under this rule, advocating for either stronger property rights protections or greater government regulatory flexibility, depending on their specific mission (e.g., environmental protection, affordable housing, economic development).
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, public_interest_advocates, observer,
    organized, generational, analytical, national).

% Navigate the regulatory landscape shaped by this rule. They benefit from the predictability of the per se rules for extreme cases, but face uncertainty and potential costs from regulations evaluated under the Penn Central factors, which can impact project viability and investment decisions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, developers_and_investors, payer,
    powerful, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, developers_and_investors, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for balancing private property rights with the government's power to regulate for the public good, offering clear boundaries for extreme cases and a flexible balancing test for others, thereby coordinating expectations for both property owners and regulators.
% TRANSFER_FUNCTION: Transfers the financial burden of certain severe regulatory impacts (permanent physical occupations, total value eliminations) from private property owners to the public (via government compensation), while allowing other regulatory burdens to remain with property owners.
% ABSENT_VOICES: Advocates for absolute property rights (who would argue against any regulation diminishing value without compensation) and proponents of unfettered government regulatory power (who would argue against any compensation requirement for public welfare regulations).
% DISAPPEARANCE_RATIONALE: Without this rule, the fundamental constitutional boundary between legitimate regulation and unconstitutional taking would vanish, leading to pervasive uncertainty, endless litigation, and potentially arbitrary confiscation or paralysis of public welfare regulation. The entire system of land use, environmental protection, and economic development would need to be re-established.
% FOUNDING_PROBLEM: To define the constitutional boundary between legitimate government regulation of private property and an unconstitutional 'taking' that requires just compensation, ensuring fairness and preventing arbitrary confiscation while preserving public welfare.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, constitutional law experts, property rights organizations, and government agencies all acknowledge the ongoing tension and the necessity of such a boundary, even if they dispute its precise application and interpretation. Supreme Court cases continue to refine its contours.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (providing a framework for property rights and regulatory power) but also involves asymmetric extraction. In per se takings, the government's regulatory power is extracted, requiring compensation. In Penn Central cases where no taking is found, property owners bear the cost of regulation without compensation, representing extraction from them. Active enforcement by courts is required to maintain this balance. Extractiveness is moderate-high (0.60) due to the significant costs imposed on one party or the other depending on the outcome. Suppression (0.70) reflects the active judicial enforcement that limits both government action and property owner claims. Theater ratio is low (0.10) as the rule is genuinely functional.
 *
 * PERSPECTIVAL GAP:
 *   Property owners tend to view the Penn Central factors as overly deferential to government, making it difficult to prove a taking, thus experiencing the constraint as more extractive. Government regulators, conversely, may view the per se rules as an undue restriction on their ability to act for the public good, experiencing extraction in those specific instances. The courts attempt to maintain a neutral, balancing perspective, but their application of the rule is often contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners are beneficiaries when a per se taking is found, receiving compensation, but payers when a Penn Central claim fails. Government regulators are beneficiaries when their regulations are upheld under Penn Central, but payers when a per se taking requires compensation. This inherent tension and shifting burden define the 'tangled' nature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to balance property rights and public regulation remains live. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring its coordination function) or a pure Rope (ignoring its asymmetric extraction). The ongoing contestation and active enforcement confirm its functional, albeit complex, role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    total_value_elimination_ambiguity,
    'What constitutes ''total value elimination'' in practice, and how is it measured, especially for partial interests or future development potential?',
    'Further judicial clarification or legislative definition of ''total value'' and the methods for its assessment in various contexts.',
    'If ''total value elimination'' is interpreted narrowly, fewer regulations will trigger per se takings, shifting more cases to Penn Central and increasing extraction from property owners. If interpreted broadly, more regulations will be deemed per se takings, increasing costs for government.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_value_elimination_ambiguity, empirical, 'Ambiguity in defining and measuring ''total value elimination'' for per se takings.').

omega_variable(
    penn_central_subjectivity,
    'To what extent do the Penn Central factors (economic impact, interference with investment-backed expectations, character of government action) provide objective guidance versus allowing for subjective judicial discretion?',
    'Empirical analysis of Penn Central outcomes across different courts and jurisdictions, or a shift towards more structured, rule-like application of the factors.',
    'If the factors are highly subjective, the constraint''s predictability for property owners decreases, increasing their perceived extraction. If more objective, predictability increases, and the coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(penn_central_subjectivity, conceptual, 'Subjectivity in the application of the Penn Central balancing test.').

omega_variable(
    balance_predictability_flexibility,
    'What is the optimal balance between providing clear, predictable rules for property owners and maintaining sufficient flexibility for government to regulate for the public good?',
    'Ongoing legislative debate, public discourse, and judicial evolution, reflecting societal preferences and changing needs.',
    'A shift towards more bright-line rules would increase predictability but reduce regulatory flexibility. A shift towards more balancing tests would increase flexibility but reduce predictability. This is a fundamental policy choice that impacts the perceived fairness and efficiency of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_predictability_flexibility, preference, 'The inherent tension between predictability and regulatory flexibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(taki_tr_t1988, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(taki_tr_t1998, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(taki_tr_t2018, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1998, 0.6).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(taki_be_t2018, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(taki_su_t2018, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'takings_clause_boundary' kernel, each representing a distinct interpretation of the Takings Clause. This reading incorporates elements of both physical appropriation and regulatory takings, adding specific per se rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
