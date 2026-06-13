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
 *   The statutory debt ceiling, in this 'extraction snare' reading, functions
 *   as a legislative boundary that is weaponized by minority factions to
 *   extract policy concessions from the executive branch under the threat of
 *   sovereign default. This reading emphasizes the systematic use of the debt
 *   ceiling as a hostage mechanism, leading to credit rating downgrades and
 *   significant economic instability, with identifiable beneficiaries being
 *   the legislative minority factions who gain leverage, and victims being
 *   the broader economy and the executive branch.
 *
 * KEY AGENTS:
 *   - legislative_minority_factions: Primary beneficiary (powerful/arbitrage) — extracts policy concessions
 *   - executive_branch: Primary target (institutional/constrained) — bears the threat of default, forced to negotiate
 *   - us_bondholders: Victim (organized/trapped) — exposed to default risk, suffer market volatility
 *   - global_financial_markets: Victim (global/constrained) — experience instability and uncertainty
 *   - constitutional_scholars: Observer (analytical/analytical) — analyze the legal and constitutional implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.85).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.9).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling (Extraction Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '48e8f161-a59c-41f3-8dbb-17138eb70eac').
narrative_ontology:cs_kernel_codification('48e8f161-a59c-41f3-8dbb-17138eb70eac', formalized).
narrative_ontology:cs_authority_grounding('48e8f161-a59c-41f3-8dbb-17138eb70eac', lineage).
narrative_ontology:cs_interpretation_layer_present('48e8f161-a59c-41f3-8dbb-17138eb70eac').
narrative_ontology:cs_reading_relation('48e8f161-a59c-41f3-8dbb-17138eb70eac', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('48e8f161-a59c-41f3-8dbb-17138eb70eac', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('48e8f161-a59c-41f3-8dbb-17138eb70eac', foundational, debt_ceiling_as_legitimate_leverage).
narrative_ontology:cs_axiom_status(debt_ceiling_as_legitimate_leverage, holdable).
narrative_ontology:cs_axiom_grounding('48e8f161-a59c-41f3-8dbb-17138eb70eac', debt_ceiling_as_legitimate_leverage, conventional).
narrative_ontology:cs_axiom('48e8f161-a59c-41f3-8dbb-17138eb70eac', secondary, default_threat_as_policy_tool).
narrative_ontology:cs_axiom_status(default_threat_as_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('48e8f161-a59c-41f3-8dbb-17138eb70eac', default_threat_as_policy_tool, instrumental).
narrative_ontology:cs_reference_frame('48e8f161-a59c-41f3-8dbb-17138eb70eac', legislative_supremacy_over_debt).
narrative_ontology:cs_drift_state('48e8f161-a59c-41f3-8dbb-17138eb70eac', contemporary_political_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('48e8f161-a59c-41f3-8dbb-17138eb70eac', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, executive_branch).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_agencies).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These factions use the debt ceiling as a leverage point to extract policy concessions from the executive branch, often unrelated to fiscal policy. They benefit from the crisis atmosphere it creates, which amplifies their political power.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions, beneficiary,
    powerful, biographical, arbitrage, national).

% The President and Treasury Department are forced to manage the threat of default, often having to make policy concessions or take extraordinary measures to avoid economic catastrophe. Their options are severely limited by the statutory constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, executive_branch, payer,
    institutional, immediate, constrained, national).

% Holders of US Treasury bonds face uncertainty and potential losses during debt ceiling impasses, as the risk of default, however remote, impacts bond prices and investor confidence. Their investment is tied to the full faith and credit of the US government.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_bondholders, payer,
    organized, generational, trapped, global).

% These markets react negatively to debt ceiling crises, leading to increased volatility, higher borrowing costs for the US government, and potential global economic contagion. They are victims of the uncertainty created by the weaponization of the ceiling.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, global_financial_markets, payer,
    institutional, immediate, constrained, global).

% Academics and legal experts who analyze the constitutional validity and implications of the debt ceiling, particularly in light of the 14th Amendment. They observe and critique its operation without direct participation in its enforcement or extraction.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% These agencies face potential shutdowns or disruptions to their operations if the debt ceiling is not raised, impacting their ability to deliver services and fulfill their mandates. They are indirect victims of the political brinkmanship.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_agencies, payer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_factions).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The debt ceiling nominally coordinates the government's borrowing authority with its spending commitments, ensuring fiscal discipline by requiring legislative approval for new debt.
% TRANSFER_FUNCTION: It transfers political leverage and policy outcomes from the executive branch to legislative minority factions, at the cost of economic stability and the government's creditworthiness.
% ABSENT_VOICES: Future generations, who will inherit the economic consequences of these political maneuvers, are absent. They would argue for long-term fiscal stability over short-term political gains. Also, the global community, which relies on US financial stability, has no direct voice in the domestic legislative process.
% DISAPPEARANCE_RATIONALE: If the debt ceiling vanished, the immediate threat of default would be removed, fundamentally altering the balance of power between the legislative and executive branches regarding fiscal policy. The legislative minority would lose a key tool for extraction, and the executive would gain more flexibility in managing the national debt, leading to a significant rearrangement of political dynamics and potentially more stable financial markets.
% FOUNDING_PROBLEM: The debt ceiling was established to provide Congress with a check on executive spending and to ensure fiscal responsibility by requiring explicit authorization for the total amount of national debt.
% FOUNDING_PROBLEM_CORROBORATION: Legislative minority factions and some fiscal conservatives attest that the founding problem of unchecked spending is still live. However, the executive branch, constitutional scholars, and economists (outside the benefiting parties) widely attest that the problem is largely 'dead' in its original intent, having been superseded by annual appropriations processes and transformed into a tool for political brinkmanship, with the actual fiscal discipline function being minimal or counterproductive.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).

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
 *   The extractiveness (0.85) is high because the debt ceiling is consistently used to force policy changes unrelated to the debt itself, effectively transferring political capital and policy outcomes to the minority. Suppression (0.90) is also high, as the threat of default leaves the executive branch with severely constrained options, effectively suppressing its ability to govern. The theater ratio (0.20) is relatively low, as the threat of default is a real and potent force, not merely performative, though the 'fiscal responsibility' rhetoric often serves as cover. Accessibility collapse (0.70) is significant because the executive's alternatives to negotiation are severely limited, and resistance (0.75) is high, reflecting the intense political battles and public outcry during debt ceiling impasses.
 *
 * PERSPECTIVAL GAP:
 *   The legislative minority factions perceive the debt ceiling as a legitimate tool for fiscal discipline and leverage, enabling them to achieve policy goals. The executive branch and global financial markets, however, experience it as a coercive mechanism that creates artificial crises and extracts concessions under duress. This divergence is central to its classification as a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative minority factions are full beneficiaries (d=0.0) as they gain disproportionate leverage and policy wins. The executive branch is a full target (d=1.0) as it is forced to bear the costs of negotiation and the threat of default. US bondholders and global financial markets are victims (d=0.9) as they are exposed to systemic risk and volatility without direct agency in the negotiations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the debt ceiling as a 'coordination mechanism' (Rope or Tangled Rope) when its primary function, in this reading, has atrophied from fiscal management to political extraction. The persistent use of default threats, rather than genuine fiscal debate, indicates a shift from a potentially legitimate (if flawed) coordination function to a pure extraction mechanism. The 'founding problem' of fiscal discipline is now largely a cover for political maneuvering, indicating mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_ceiling_nature_ambiguity,
    'Is the statutory debt ceiling a legitimate fiscal constraint or a political weapon?',
    'Legal precedent from a Supreme Court ruling on the 14th Amendment''s applicability, or a legislative reform that structurally separates spending authorization from debt issuance.',
    'If ruled a legitimate constraint, its classification might shift towards a Tangled Rope (coordination with extraction). If ruled a political weapon, its Snare classification is reinforced, and its legitimacy is undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_ceiling_nature_ambiguity, conceptual, 'Ambiguity between fiscal constraint and political weapon.').

omega_variable(
    extraction_vs_coordination_function,
    'To what extent does the debt ceiling genuinely coordinate fiscal policy versus enabling minority extraction?',
    'Empirical analysis of legislative outcomes: if policy concessions extracted consistently outweigh any genuine fiscal discipline, the extraction function dominates.',
    'If coordination is negligible, the Snare classification is robust. If a genuine coordination function is identified, it might be reclassified as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_function, empirical, 'Distinguishing coordination from extraction in debt ceiling operations.').

omega_variable(
    kernel_reading_extraction_snare,
    'This constraint is the ''extraction_snare_reading'' of the ''statutory_debt_ceiling'' kernel. What would change if the ''coordination_scaffold_reading'' or ''constitutional_nullity_reading'' were adopted?',
    'A shift in judicial interpretation or legislative practice towards one of the sibling readings.',
    'If the ''coordination_scaffold_reading'' were adopted, the constraint would be seen as a temporary support mechanism, with lower extraction and a sunset clause. If the ''constitutional_nullity_reading'' were adopted, the constraint would effectively cease to exist as a binding force, rendering its extraction and suppression moot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_extraction_snare, conceptual, 'Impact of alternative readings of the statutory debt ceiling kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t5, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(stat_su_t10, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(stat_su_t15, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_budget_process).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, us_credit_rating).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, global_financial_stability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statutory_debt_ceiling' kernel. This 'extraction_snare_reading' emphasizes its use as a political weapon, distinct from the 'coordination_scaffold_reading' (which views it as a procedural mechanism) and the 'constitutional_nullity_reading' (which views it as unconstitutional).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
