% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling (Coordination Scaffold Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint models the statutory debt ceiling as a procedural
 *   coordination mechanism. In this reading, its primary function is to
 *   facilitate the Treasury Department's routine operations by setting an
 *   aggregate borrowing limit, avoiding repeated congressional
 *   micromanagement of individual debt issuances. It is intended to be
 *   adjusted periodically and without controversy, acting as a scaffold for
 *   efficient fiscal management rather than a tool for political leverage.
 *   The low extractiveness and suppression reflect this intended function.
 *
 * KEY AGENTS:
 *   - treasury_department: Primary beneficiary (institutional/analytical) — gains operational efficiency
 *   - congressional_majority: Beneficiary (institutional/mobile) — avoids micromanagement, maintains fiscal oversight
 *   - congressional_minority: Payer (institutional/constrained) — bears the procedural burden of routine adjustments, but does not weaponize it in this reading
 *   - financial_markets: Observer (organized/analytical) — react to stability or instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.2).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.15).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling (Coordination Scaffold Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '77ad343e-df4b-46dc-958b-effaccd3b8aa').
narrative_ontology:cs_kernel_codification('77ad343e-df4b-46dc-958b-effaccd3b8aa', formalized).
narrative_ontology:cs_authority_grounding('77ad343e-df4b-46dc-958b-effaccd3b8aa', lineage).
narrative_ontology:cs_interpretation_layer_present('77ad343e-df4b-46dc-958b-effaccd3b8aa').
narrative_ontology:cs_reading_relation('77ad343e-df4b-46dc-958b-effaccd3b8aa', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('77ad343e-df4b-46dc-958b-effaccd3b8aa', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('77ad343e-df4b-46dc-958b-effaccd3b8aa', foundational, procedural_efficiency_is_paramount).
narrative_ontology:cs_axiom_status(procedural_efficiency_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('77ad343e-df4b-46dc-958b-effaccd3b8aa', procedural_efficiency_is_paramount, instrumental).
narrative_ontology:cs_axiom('77ad343e-df4b-46dc-958b-effaccd3b8aa', foundational, aggregate_oversight_is_sufficient).
narrative_ontology:cs_axiom_status(aggregate_oversight_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('77ad343e-df4b-46dc-958b-effaccd3b8aa', aggregate_oversight_is_sufficient, conventional).
narrative_ontology:cs_reference_frame('77ad343e-df4b-46dc-958b-effaccd3b8aa', routine_fiscal_management).
narrative_ontology:cs_drift_state('77ad343e-df4b-46dc-958b-effaccd3b8aa', contemporary_political_polarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('77ad343e-df4b-46dc-958b-effaccd3b8aa', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, congressional_minority).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_responsibility_principle).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the streamlined process of managing federal debt within an aggregate limit, avoiding the need for constant, granular congressional approval for each bond issuance. Its operational efficiency is enhanced by this procedural coordination.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary,
    institutional, immediate, constrained, national).

% Benefits from a mechanism that provides aggregate fiscal oversight without requiring micromanagement of daily Treasury operations. It can routinely adjust the ceiling to reflect spending decisions without political brinkmanship.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_majority, beneficiary,
    institutional, biographical, mobile, national).

% Participates in the routine process of adjusting the debt ceiling. In this reading, it bears the procedural costs of this oversight function but does not weaponize the ceiling for political extraction or default threats.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_minority, payer,
    institutional, biographical, constrained, national).

% Observe the debt ceiling process for signs of fiscal stability or instability. They benefit from the predictability and routine nature of adjustments in this reading, which reduces sovereign risk premiums.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, financial_markets, observer,
    organized, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the Treasury Department's operational need for continuous borrowing with Congress's constitutional power of the purse, by setting an aggregate limit that is routinely adjusted.
% TRANSFER_FUNCTION: Primarily transfers operational efficiency and predictability to the Treasury Department and congressional oversight to Congress, rather than direct financial transfers. It prevents the transfer of political leverage in this reading.
% ABSENT_VOICES: Advocates for a 'clean' repeal of the debt ceiling, arguing it is an unnecessary and dangerous procedural relic, are present in academic and policy debates but are not currently able to shift the legislative process.
% DISAPPEARANCE_RATIONALE: If the debt ceiling vanished, Congress would either need to approve every bond issuance (creating immense micromanagement) or delegate unlimited borrowing authority (ceding fiscal control), fundamentally altering the balance of power and operational procedures for federal debt.
% FOUNDING_PROBLEM: The problem of managing federal debt efficiently while maintaining congressional oversight, avoiding the impracticality of individual legislative approvals for every borrowing action.
% FOUNDING_PROBLEM_CORROBORATION: The Treasury Department and congressional leadership (across parties) consistently attest to the ongoing need for a mechanism to manage federal debt efficiently, even if they disagree on the debt ceiling's current form. Independent fiscal policy experts corroborate the administrative burden that would arise without such a mechanism.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.2) reflects that, in this reading, the debt ceiling imposes minimal direct costs beyond administrative overhead, primarily serving a procedural function. Suppression (0.15) is low because compliance is largely voluntary due to the shared benefit of efficient governance, and alternatives (like micromanaging every bond issuance) are genuinely less efficient. Theater ratio (0.1) is low, indicating that the stated purpose of facilitating Treasury operations is largely aligned with its actual function. The 'has_sunset_clause: true' reflects the expectation of routine, non-controversial adjustments.
 *
 * PERSPECTIVAL GAP:
 *   From the Treasury Department's perspective, the debt ceiling, when functioning as a scaffold, is a net benefit, streamlining operations. From a congressional majority's perspective, it provides a necessary, if sometimes cumbersome, oversight mechanism. A congressional minority, while participating in the process, would not experience it as highly extractive in this reading, as it is not used for hostage-taking.
 *
 * DIRECTIONALITY LOGIC:
 *   The Treasury Department is a clear beneficiary (d=0.0-0.1) as the constraint simplifies its borrowing process. The congressional majority also benefits (d=0.1-0.2) by delegating routine debt management while retaining aggregate control. The congressional minority, in this reading, is a minor payer (d=0.4-0.5) of procedural costs, but not a victim of extraction. Financial markets are observers (d=0.5) whose primary concern is the stability the mechanism provides.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a genuine coordination mechanism as extraction. By framing the debt ceiling as a scaffold, it highlights its intended, temporary support function for efficient governance. If the constraint were to persist without its coordination function, or if the 'sunset clause' of routine adjustment failed, it would drift towards a Piton or Snare, indicating mandatrophy. The low extractiveness and high accessibility collapse (of alternatives to this coordination) are key to this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_ceiling_kernel_reading,
    'Is the statutory debt ceiling primarily a coordination scaffold, an extraction snare, or a constitutional nullity?',
    'Empirical observation of legislative practice over time: frequency of routine adjustments vs. default threats; judicial rulings on 14th Amendment challenges.',
    'If primarily an extraction snare, the constraint''s extractiveness and suppression are significantly higher, reclassifying it to a Snare. If a constitutional nullity, the constraint is effectively a Mountain (of constitutional law) from the Treasury''s perspective, with zero extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_ceiling_kernel_reading, conceptual, 'This constraint is one reading of the ''statutory_debt_ceiling'' kernel, specifically the ''coordination_scaffold_reading''. Sibling readings include ''extraction_snare_reading'' and ''constitutional_nullity_reading''.').

omega_variable(
    sunset_clause_enforcement_ambiguity,
    'Is the ''sunset clause'' (routine adjustment) genuinely enforced, or is it merely a procedural formality that can be weaponized?',
    'Analysis of historical legislative behavior: frequency of clean debt ceiling increases versus attached riders or brinkmanship tactics.',
    'If the routine adjustment mechanism is consistently subverted for political leverage, the ''has_sunset_clause'' property becomes performative, increasing the theater_ratio and potentially reclassifying to a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforcement_ambiguity, empirical, 'Ambiguity regarding the effective enforcement of the debt ceiling''s intended sunset (routine adjustment).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(stat_su_t10, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(stat_su_t20, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, federal_budget_process).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, treasury_bond_market_stability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statutory_debt_ceiling' kernel, each representing a distinct structural claim about its function and impact. This 'coordination_scaffold_reading' focuses on its intended role in fiscal governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
