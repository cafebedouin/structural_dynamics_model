% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling (Extraction Snare Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the statutory debt ceiling as an
 *   'extraction snare,' focusing on its contemporary use as a weaponized
 *   boundary by legislative minority factions to extract policy concessions
 *   under the threat of national default. This reading views the debt ceiling
 *   not as a genuine fiscal coordination mechanism, but as a tool for
 *   asymmetric political leverage. This is one reading of the
 *   'statutory_debt_ceiling' kernel, distinct from
 *   'coordination_scaffold_reading' and 'constitutional_nullity_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.85).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.9).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling (Extraction Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, 'a1e6101c-0d21-4380-a8f2-4eb3960484de').
narrative_ontology:cs_kernel_codification('a1e6101c-0d21-4380-a8f2-4eb3960484de', formalized).
narrative_ontology:cs_authority_grounding('a1e6101c-0d21-4380-a8f2-4eb3960484de', extraction).
narrative_ontology:cs_interpretation_layer_present('a1e6101c-0d21-4380-a8f2-4eb3960484de').
narrative_ontology:cs_reading_relation('a1e6101c-0d21-4380-a8f2-4eb3960484de', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_reading_relation('a1e6101c-0d21-4380-a8f2-4eb3960484de', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('a1e6101c-0d21-4380-a8f2-4eb3960484de', foundational, statutory_debt_ceiling_is_valid_law).
narrative_ontology:cs_axiom_status(statutory_debt_ceiling_is_valid_law, holdable).
narrative_ontology:cs_axiom_grounding('a1e6101c-0d21-4380-a8f2-4eb3960484de', statutory_debt_ceiling_is_valid_law, conventional).
narrative_ontology:cs_axiom('a1e6101c-0d21-4380-a8f2-4eb3960484de', foundational, legislative_minority_leverage_is_legitimate_tactic).
narrative_ontology:cs_axiom_status(legislative_minority_leverage_is_legitimate_tactic, holdable).
narrative_ontology:cs_axiom_grounding('a1e6101c-0d21-4380-a8f2-4eb3960484de', legislative_minority_leverage_is_legitimate_tactic, conventional).
narrative_ontology:cs_reference_frame('a1e6101c-0d21-4380-a8f2-4eb3960484de', legislative_leverage_tool).
narrative_ontology:cs_drift_state('a1e6101c-0d21-4380-a8f2-4eb3960484de', contemporary_political_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a1e6101c-0d21-4380-a8f2-4eb3960484de', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_treasury).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_citizens).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_party).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These factions leverage the debt ceiling as a hostage mechanism, threatening default to extract policy concessions or block legislation. They benefit from the political leverage and policy outcomes achieved through this brinkmanship, with low direct costs to themselves.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, agenda_setter,
    powerful, biographical, mobile, national).

% The Treasury is legally obligated to pay the nation's bills but is constrained by the debt ceiling. It faces the immediate operational challenge of managing cash flow to avoid default, often resorting to extraordinary measures. Its options are severely limited by the statutory constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_treasury, payer,
    institutional, immediate, trapped, national).

% These markets react to debt ceiling impasses with volatility, increased borrowing costs for the U.S. government, and potential credit rating downgrades. While they can shift investments, the systemic importance of U.S. debt means they are deeply exposed to the risk of default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets, payer,
    institutional, immediate, constrained, global).

% Bear the indirect costs of debt ceiling crises through increased national borrowing costs, potential cuts to government services, and economic uncertainty. They have no direct mechanism to influence the debt ceiling debate or its resolution.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_citizens, payer,
    powerless, immediate, trapped, national).

% Often forced to negotiate with minority factions under duress to avoid default, leading to policy compromises they would otherwise reject. Their options are limited by the need to maintain government functionality and avoid economic catastrophe.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_party, payer,
    powerful, biographical, constrained, national).

% Analyze the constitutional validity and historical evolution of the debt ceiling, often arguing for its abolition or reinterpretation under the 14th Amendment. They provide critical analysis but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the debt ceiling's claimed coordination function (ensuring fiscal responsibility and congressional oversight of spending) is largely a cover for its use as an extraction mechanism. Any genuine coordination is incidental to the primary function of leverage.
% TRANSFER_FUNCTION: Transfers political leverage and policy concessions from the majority party (and indirectly, the broader public) to legislative minority factions. It also transfers economic risk and increased borrowing costs to the U.S. Treasury and global financial markets.
% ABSENT_VOICES: Advocates for the abolition of the debt ceiling, or for its constitutional nullification under the 14th Amendment, are often marginalized in the immediate political negotiations. Their arguments for a more stable fiscal governance structure are excluded from the brinkmanship dynamic.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling vanished overnight, the U.S. Treasury would be able to pay its bills without political interference, eliminating the threat of default. This would fundamentally alter the balance of power in fiscal policy, removing a key leverage point for legislative minorities and stabilizing global financial markets. The political economy of fiscal governance would reorganize significantly.
% FOUNDING_PROBLEM: The debt ceiling was originally established to streamline government borrowing by allowing the Treasury to issue debt up to a certain limit without seeking specific congressional approval for each issuance, while still maintaining overall congressional control over the national debt.
% FOUNDING_PROBLEM_CORROBORATION: While some legislative factions and conservative think tanks still claim the debt ceiling serves its original purpose of fiscal discipline, a broad consensus among economists, former Treasury officials, and many political observers (including the majority party and constitutional scholars) attests that its original coordination function is dead, having been superseded by its weaponization as a tool for political extraction. Independent economic analyses consistently highlight its disruptive, rather than coordinating, effects.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the mechanism forces significant policy transfers and economic costs onto the majority and the public, disproportionate to any claimed fiscal benefit. Suppression is also very high (0.90) due to the catastrophic consequences of default, which effectively traps the Treasury and the majority party into acceding to demands. The theater ratio is high (0.70) because much of the public debate and legislative activity around the debt ceiling is performative brinkmanship, designed to maximize political pressure rather than to genuinely address fiscal policy. Accessibility collapse is moderate (0.60) as alternatives like the 14th Amendment or unilateral Treasury action are debated but not easily implemented, while resistance is high (0.75) from those who bear the costs and risks.
 *
 * PERSPECTIVAL GAP:
 *   Legislative minority factions (agenda_setter) perceive the debt ceiling as a legitimate and effective tool for fiscal leverage, yielding significant political gains. In contrast, the U.S. Treasury, global financial markets, and the majority party (payers) experience it as a highly extractive and suppressive mechanism that imposes severe costs and risks, with no genuine coordination function in its current application. Constitutional scholars (observers) often view it as an anachronism or a constitutional nullity.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative minority factions are clear beneficiaries, using the constraint to extract policy concessions and political capital (low d). The U.S. Treasury, global financial markets, U.S. citizens, and the majority party are all targets, bearing the direct and indirect costs of the brinkmanship and potential default (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The debt ceiling exhibits clear mandatrophy. Its original mandate (streamlining borrowing and ensuring fiscal oversight) has atrophied, replaced by its function as a political weapon. The persistence of the constraint is due to the concentrated benefits it provides to legislative minority factions, who actively enforce its use, rather than any enduring coordination function. This prevents mislabeling it as a 'rope' or 'scaffold' by highlighting the active extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_function_ambiguity,
    'Is the debt ceiling primarily a coordination mechanism for fiscal responsibility, or an extraction mechanism for political leverage?',
    'Analysis of legislative outcomes during debt ceiling impasses: if impasses consistently lead to policy concessions unrelated to fiscal discipline, it supports the extraction reading. If they lead to genuine, broadly supported fiscal reforms, it supports coordination.',
    'If resolved as primarily coordination, the constraint would reclassify towards a Rope or Scaffold; if resolved as extraction, it solidifies as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_function_ambiguity, empirical, 'Ambiguity regarding the debt ceiling''s primary functional role.').

omega_variable(
    constitutional_validity_ambiguity,
    'Is the statutory debt ceiling constitutionally valid, or is it superseded by the 14th Amendment''s Public Debt Clause?',
    'A definitive Supreme Court ruling on the constitutionality of the debt ceiling, or a legislative act to repeal it based on constitutional grounds.',
    'If ruled unconstitutional, the constraint would effectively cease to exist as a legal mechanism, rendering this ''snare'' reading moot and shifting analysis to the ''constitutional_nullity_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_validity_ambiguity, conceptual, 'Ambiguity regarding the constitutional legitimacy of the debt ceiling.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (threat of default) primarily structural (legal/economic necessity) or internalized (political fear of being blamed for default)?',
    'Analysis of political rhetoric and decision-making during crises: if parties consistently prioritize avoiding blame over constitutional alternatives, it suggests internalized suppression. If actions are purely driven by legal/economic constraints, it''s structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as political actors carry the suppression with them even when legal alternatives might exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in political decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(stat_tr_t1990, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2010, 0.65).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(stat_be_t1990, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(stat_su_t1980, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(stat_su_t1990, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'statutory_debt_ceiling' kernel. This 'extraction_snare_reading' focuses on its use for political leverage, while other readings emphasize its coordination function or constitutional invalidity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
