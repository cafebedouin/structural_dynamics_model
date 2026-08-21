% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution: Parliamentary Constraint on Executive
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary constraint' reading
 *   of the Fifth Republic Constitution, where the President functions as a
 *   coordinated executive requiring legislative authorization for policy
 *   implementation. This reading emphasizes the role of the National Assembly
 *   in checking executive power, ensuring democratic accountability. The
 *   constraint is claimed as a Rope, reflecting its function in coordinating
 *   governance and providing a beneficial check on power, even if it
 *   'extracts' autonomy from the executive. The metrics reflect a functional,
 *   actively enforced constraint with moderate costs of coordination.
 *
 * KEY AGENTS:
 *   - President of France: Payer (institutional/constrained)
 *   - Prime Minister of France: Payer (institutional/constrained)
 *   - National Assembly Majority: Agenda Setter (institutional/arbitrage)
 *   - Citizens of France: Beneficiary (organized/constrained)
 *   - Constitutional Council: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.38).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.55).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution: Parliamentary Constraint on Executive").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '49a1f17d-fa0f-4ef5-8648-4dd36d4c6623').
narrative_ontology:cs_kernel_codification('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', fixed_text).
narrative_ontology:cs_authority_grounding('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', lineage).
narrative_ontology:cs_interpretation_layer_present('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623').
narrative_ontology:cs_reading_relation('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', foundational, parliamentary_supremacy_in_policy).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_in_policy, holdable).
narrative_ontology:cs_axiom_grounding('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', parliamentary_supremacy_in_policy, conventional).
narrative_ontology:cs_axiom('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', foundational, executive_accountability_to_assembly).
narrative_ontology:cs_axiom_status(executive_accountability_to_assembly, holdable).
narrative_ontology:cs_axiom_grounding('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', executive_accountability_to_assembly, deontological).
narrative_ontology:cs_reference_frame('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', parliamentary_republic_model).
narrative_ontology:cs_drift_state('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', contemporary_french_politics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('49a1f17d-fa0f-4ef5-8648-4dd36d4c6623', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, citizens_of_france).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_of_france).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_of_france).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As head of state, the President must seek legislative authorization for major policy implementation, especially when the National Assembly is controlled by an opposing party. This constrains their ability to act unilaterally, requiring negotiation and compromise.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president_of_france, payer,
    institutional, biographical, constrained, national).

% As head of government, the Prime Minister and their cabinet are accountable to the National Assembly. They must secure legislative support for their program and can be removed by a vote of no confidence, making them a direct target of parliamentary constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_of_france, payer,
    institutional, biographical, constrained, national).

% Holds the power to authorize legislation, approve government programs, and censure the Prime Minister. This majority benefits from the constraint by asserting its policy agenda and ensuring executive accountability to the elected legislature.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter,
    institutional, biographical, arbitrage, national).

% While not directly setting the agenda, the minority scrutinizes executive actions, participates in debates, and can influence public opinion, contributing to the overall democratic accountability fostered by the constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_minority, observer,
    organized, biographical, mobile, national).

% Benefit from a system where executive power is checked by legislative oversight, ensuring greater democratic accountability and representation in policy-making. This reduces the risk of unchecked presidential authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens_of_france, beneficiary,
    organized, generational, constrained, national).

% Acts as the guardian of the Constitution, reviewing the constitutionality of laws and executive actions. It ensures that both the executive and legislative branches operate within their constitutional bounds, reinforcing the constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that major policy implementation requires the coordinated action and authorization of both the executive and legislative branches, preventing unilateral executive action and fostering democratic legitimacy.
% TRANSFER_FUNCTION: Transfers policy-making authority and legitimacy from the executive to the legislative majority, requiring the executive to 'pay' in terms of autonomy and political capital to secure parliamentary consent.
% ABSENT_VOICES: Proponents of a purely presidential system, who believe the President's direct mandate from the people should override legislative checks, are structurally marginalized by this reading. They would argue for greater executive autonomy.
% DISAPPEARANCE_RATIONALE: If the requirement for legislative authorization vanished, the President would gain unchecked power to implement policy, fundamentally altering the balance of power in the Fifth Republic and potentially leading to a more authoritarian system. The entire political system would reorganize around a hyper-presidential model.
% FOUNDING_PROBLEM: The Fifth Republic Constitution was designed to overcome the governmental instability of previous republics by strengthening the executive, but also to ensure that executive power remained democratically accountable and subject to legislative checks.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical analyses consistently corroborate that balancing executive strength with democratic accountability remains a central, live challenge in French politics, especially during periods of 'cohabitation' or strong presidential mandates.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.38, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate (0.38) because while the executive's autonomy is curtailed, this is viewed as a legitimate cost of democratic coordination, not undue extraction. `Suppression` is also moderate (0.55) as the legislative majority actively enforces its prerogatives through votes of confidence, budget control, and legislative review. `Theater_ratio` is low (0.18) because the constraint is genuinely functional; legislative authorization is a real and necessary step. `Accessibility_collapse` and `resistance` are moderate, reflecting that while the executive is constrained, it can still exert influence and sometimes push the boundaries of its powers, but not easily bypass the legislature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President and Prime Minister, this constraint can feel like a burden or an obstacle to efficient governance, especially during cohabitation. However, from the perspective of the National Assembly majority and the citizens, it is a vital mechanism for democratic accountability and policy legitimacy. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and Prime Minister are declared as 'payers' because they bear the direct cost of the constraint in terms of reduced unilateral action. The National Assembly majority is the primary 'beneficiary' as it gains policy influence and control. The citizens are also 'beneficiaries' of the overall democratic accountability. The Constitutional Council acts as an 'observer' ensuring the rules are followed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as pure extraction by emphasizing its genuine coordination function in a democratic system. The constraint's mandate to ensure democratic accountability remains live, preventing it from degrading into a Piton. The executive's 'cost' is a feature of the system, not a bug, from this reading's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_vs_parliamentary_primacy,
    'Is the Fifth Republic Constitution fundamentally designed for presidential primacy, with parliamentary checks, or for a more balanced, parliamentary-constrained executive?',
    'Comparative constitutional analysis across different presidential tenures and cohabitation periods, focusing on actual power dynamics and judicial interpretations over time.',
    'If resolved towards presidential primacy, this reading''s assessment of executive extractiveness and legislative suppression would be lower, potentially shifting its classification towards a more ''hyper-presidential'' model. If resolved towards parliamentary constraint, the current metrics are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_vs_parliamentary_primacy, conceptual, 'Ambiguity in the Fifth Republic''s core power balance.').

omega_variable(
    cohabitation_impact_on_constraint,
    'Does the phenomenon of ''cohabitation'' (President and Prime Minister from opposing parties) fundamentally alter the nature of this constraint, or merely highlight its underlying structure?',
    'Empirical study of policy outcomes and executive-legislative relations during cohabitation periods versus periods of unified government.',
    'If cohabitation fundamentally alters the constraint, it suggests the constraint''s strength is highly contingent on political alignment, potentially increasing its perceived extractiveness on the executive during such periods. If it merely highlights the structure, the constraint''s intrinsic properties remain stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_impact_on_constraint, empirical, 'Effect of political alignment on executive-legislative power balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1970, 0.17).
narrative_ontology:measurement(fift_tr_t1985, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1985, 0.19).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2012, 0.16).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1970, 0.37).
narrative_ontology:measurement(fift_be_t1985, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1985, 0.39).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2012, 0.36).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.5).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1970, 0.53).
narrative_ontology:measurement(fift_su_t1985, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1985, 0.56).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fifth_republic_constitution' kernel, alongside 'hyper_presidential_reading' and 'cohabitation_equilibrium_reading'. Each reading offers a distinct structural interpretation of the constitutional balance of power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
