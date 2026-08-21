% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity: Veto Trap Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story analyzes the EU Council's unanimity rule through
 *   the 'veto trap' reading, where it functions as a structural vulnerability
 *   enabling minoritarian extraction. While formally intended to protect
 *   national sovereignty and ensure consensus, this reading highlights how
 *   the credible threat of a veto by a single or small group of member states
 *   systematically forces concessions from the majority, leading to diluted
 *   policies, opt-outs, or other benefits for the blocking party. This is one
 *   reading of the broader 'eu_council_unanimity' kernel, focusing on its
 *   extractive dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.85).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity: Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '30b5fa17-ce68-422b-9746-298bb69fe965').
narrative_ontology:cs_kernel_codification('30b5fa17-ce68-422b-9746-298bb69fe965', formalized).
narrative_ontology:cs_authority_grounding('30b5fa17-ce68-422b-9746-298bb69fe965', extraction).
narrative_ontology:cs_interpretation_layer_present('30b5fa17-ce68-422b-9746-298bb69fe965').
narrative_ontology:cs_reading_relation('30b5fa17-ce68-422b-9746-298bb69fe965', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('30b5fa17-ce68-422b-9746-298bb69fe965', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('30b5fa17-ce68-422b-9746-298bb69fe965', foundational, unanimity_grants_minority_veto_power).
narrative_ontology:cs_axiom_status(unanimity_grants_minority_veto_power, holdable).
narrative_ontology:cs_axiom_grounding('30b5fa17-ce68-422b-9746-298bb69fe965', unanimity_grants_minority_veto_power, conventional).
narrative_ontology:cs_axiom('30b5fa17-ce68-422b-9746-298bb69fe965', foundational, veto_threat_enables_value_transfer).
narrative_ontology:cs_axiom_status(veto_threat_enables_value_transfer, holdable).
narrative_ontology:cs_axiom_grounding('30b5fa17-ce68-422b-9746-298bb69fe965', veto_threat_enables_value_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('30b5fa17-ce68-422b-9746-298bb69fe965', minority_leverage_framework).
narrative_ontology:cs_drift_state('30b5fa17-ce68-422b-9746-298bb69fe965', contemporary_eu_policy_making, gap(stable, minor, true)).
narrative_ontology:cs_created_at('30b5fa17-ce68-422b-9746-298bb69fe965', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, majority_coalition_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_commission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single member state or a small minority of states that can credibly threaten to veto a legislative proposal, thereby forcing concessions, opt-outs, or other benefits from the majority coalition to advance its national interests. This leverage is the core of the veto trap.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter,
    institutional, biographical, arbitrage, national).

% The larger group of member states that support a policy proposal but are forced to make concessions to a blocking minority to avoid policy paralysis. They bear the cost of diluted policy outcomes or diverted resources, which are extracted by the blocking state.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, majority_coalition_member_states, payer,
    institutional, biographical, constrained, continental).

% The executive body of the EU, responsible for proposing legislation. It experiences policy paralysis and dilution when unanimity rules enable blocking, hindering its ability to advance the EU's collective agenda and effectively paying the cost of inaction.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, observer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, eu_commission, payer).

% Citizens across the EU who bear the diffuse costs of delayed, suboptimal, or entirely blocked policies that would otherwise address collective challenges. They have no direct mechanism to overcome the veto trap and are effectively victims of the extraction.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens, payer,
    powerless, generational, trapped, continental).

% Administers the Council's legislative process, facilitating negotiations. It is tasked with finding compromises that satisfy all member states, often under pressure from blocking threats, which can lead to complex, less effective policy outcomes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_council_secretariat, agenda_setter,
    institutional, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To formally ensure that all member states, particularly on matters touching national sovereignty or core interests, consent to collective action, thereby theoretically enhancing the legitimacy and buy-in for EU-level decisions.
% TRANSFER_FUNCTION: Systematically transfers policy concessions, financial benefits, or opt-outs from the majority coalition of member states to a blocking minority, in exchange for their consent to a broader legislative package.
% ABSENT_VOICES: EU citizens directly affected by policy paralysis or diluted policy outcomes, as well as future generations of Europeans who will inherit the consequences of delayed or suboptimal collective action. Their interests are often subordinated to short-term national leverage games.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, the EU's legislative process would fundamentally transform. Policy-making would become more efficient, but the balance of power would shift dramatically, potentially leading to a more federalized structure and less protection for minority national interests. The entire institutional design would need to be re-evaluated.
% FOUNDING_PROBLEM: The unanimity rule was established to protect the national sovereignty of member states, ensuring that no state could be forced into collective action against its fundamental interests, particularly in sensitive policy areas.
% FOUNDING_PROBLEM_CORROBORATION: Blocking member states and some legal scholars continue to assert that the rule is a vital guarantor of national sovereignty. However, a majority of member states, political economists, and EU institutions argue that its primary function has drifted towards enabling minoritarian extraction, citing numerous instances of policy paralysis and forced concessions documented in political science research and EU policy reports.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant policy and resource transfers compelled by veto threats. Suppression (0.78) is high because the unanimity rule effectively suppresses the majority's preferred policy alternatives, forcing them into a constrained negotiation space. The theater ratio is low (0.20) because the veto threat is a highly functional, not merely performative, tool for achieving national interests. Accessibility collapse is high (0.70) as the veto power severely limits the policy options available to the majority. Resistance (0.60) is moderate, manifested through diplomatic pressure, public criticism, and attempts to reform voting rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a blocking state, the unanimity rule is a vital tool for protecting national interests and ensuring fair representation. From the perspective of the majority coalition or the EU Commission, it is a mechanism for minoritarian extraction and policy paralysis. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking member state is a clear beneficiary (d near 0.0) as it directly gains concessions and leverage. The majority coalition member states and EU citizens are victims (d near 1.0) as they bear the costs of policy dilution and paralysis. The EU Commission, while an observer, also experiences the negative impact of policy stagnation, placing it closer to the payer end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_sovereignty_protection,
    'To what extent does the unanimity rule primarily function as a mechanism for minoritarian extraction, versus genuinely protecting national sovereignty from majoritarian coercion?',
    'Empirical analysis of vetoed or threatened policies: track the nature of concessions granted (e.g., direct financial transfers, policy opt-outs, dilution of common standards) versus the direct impact on core sovereign interests (e.g., defense, constitutional law).',
    'If extraction is consistently dominant, the ''veto_trap_reading'' is strongly corroborated, reinforcing a Snare classification. If sovereignty protection is consistently dominant, the ''sovereignty_guarantor_reading'' is corroborated, suggesting a Mountain or Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_sovereignty_protection, empirical, 'Distinguishing the primary function of the unanimity rule.').

omega_variable(
    veto_threat_vs_genuine_consensus,
    'Does the threat of veto primarily lead to genuine consensus-building through iterative negotiation, or does it merely compel concessions under duress?',
    'Qualitative case studies of negotiation processes: analyze the extent of substantive policy improvement and shared understanding versus one-sided concessions to avoid blocking. Compare outcomes with and without credible veto threats.',
    'If genuine consensus is rare and concessions under duress are common, the ''veto_trap_reading'' is strengthened. If robust, mutually beneficial consensus is frequently achieved, the ''diplomatic_capital_reading'' gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_threat_vs_genuine_consensus, empirical, 'Assessing whether veto threats foster consensus or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__veto_trap_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(eu_c_tr_t1998, eu_council_unanimity__veto_trap_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(eu_c_tr_t2003, eu_council_unanimity__veto_trap_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(eu_c_tr_t2008, eu_council_unanimity__veto_trap_reading, theater_ratio, 2008, 0.17).
narrative_ontology:measurement(eu_c_tr_t2013, eu_council_unanimity__veto_trap_reading, theater_ratio, 2013, 0.19).
narrative_ontology:measurement(eu_c_tr_t2018, eu_council_unanimity__veto_trap_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(eu_c_tr_t2023, eu_council_unanimity__veto_trap_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(eu_c_be_t1998, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(eu_c_be_t2003, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2003, 0.75).
narrative_ontology:measurement(eu_c_be_t2008, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2008, 0.8).
narrative_ontology:measurement(eu_c_be_t2013, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2013, 0.83).
narrative_ontology:measurement(eu_c_be_t2018, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2018, 0.84).
narrative_ontology:measurement(eu_c_be_t2023, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(eu_c_su_t1998, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(eu_c_su_t2003, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement(eu_c_su_t2008, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement(eu_c_su_t2013, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2013, 0.76).
narrative_ontology:measurement(eu_c_su_t2018, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2018, 0.77).
narrative_ontology:measurement(eu_c_su_t2023, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eu_council_unanimity' kernel, each focusing on a distinct structural function. This 'veto_trap_reading' emphasizes the extractive potential, while sibling readings focus on sovereignty protection and consensus-building.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
