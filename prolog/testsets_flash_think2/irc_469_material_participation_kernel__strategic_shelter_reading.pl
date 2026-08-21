% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: Strategic Passive Loss Shelter via Permissive Material Participation Rules
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint describes the permissive interpretation of Internal
 *   Revenue Code (IRC) Section 469's material participation rules, which
 *   allows high-net-worth investors to strategically reclassify passive
 *   losses as active. This reading, often facilitated by aggressive
 *   hour-counting and grouping elections, enables significant tax sheltering,
 *   effectively shifting tax burdens from wealthy investors to the general
 *   public. It is one reading of the broader IRC 469 kernel, which originally
 *   aimed to limit such deductions.
 *
 * KEY AGENTS:
 *   - high_net_worth_investors: Primary beneficiary (powerful/arbitrage) — benefits from reduced tax liability.
 *   - tax_advisors: Secondary beneficiary (organized/mobile) — profits from facilitating sheltering strategies.
 *   - us_treasury: Primary payer (institutional/trapped) — bears the cost of lost revenue.
 *   - general_taxpayers: Secondary payer (powerless/constrained) — indirectly bears the burden of revenue loss.
 *   - internal_revenue_service_irs: Agenda setter (institutional/constrained) — administers the rules but faces limitations in challenging aggressive interpretations.
 *   - tax_court: Observer (institutional/analytical) — adjudicates disputes, shaping interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.65).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.4).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "Strategic Passive Loss Shelter via Permissive Material Participation Rules").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '1b4ba27a-3a5f-4e18-b461-1743152363f5').
narrative_ontology:cs_kernel_codification('1b4ba27a-3a5f-4e18-b461-1743152363f5', formalized).
narrative_ontology:cs_authority_grounding('1b4ba27a-3a5f-4e18-b461-1743152363f5', lineage).
narrative_ontology:cs_interpretation_layer_present('1b4ba27a-3a5f-4e18-b461-1743152363f5').
narrative_ontology:cs_reading_relation('1b4ba27a-3a5f-4e18-b461-1743152363f5', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('1b4ba27a-3a5f-4e18-b461-1743152363f5', foundational, tax_code_allows_aggressive_planning).
narrative_ontology:cs_axiom_status(tax_code_allows_aggressive_planning, holdable).
narrative_ontology:cs_axiom_grounding('1b4ba27a-3a5f-4e18-b461-1743152363f5', tax_code_allows_aggressive_planning, conventional).
narrative_ontology:cs_axiom('1b4ba27a-3a5f-4e18-b461-1743152363f5', foundational, economic_activity_should_be_incentivized_via_tax_breaks).
narrative_ontology:cs_axiom_status(economic_activity_should_be_incentivized_via_tax_breaks, holdable).
narrative_ontology:cs_axiom_grounding('1b4ba27a-3a5f-4e18-b461-1743152363f5', economic_activity_should_be_incentivized_via_tax_breaks, instrumental).
narrative_ontology:cs_reference_frame('1b4ba27a-3a5f-4e18-b461-1743152363f5', taxpayer_friendly_interpretation_framework).
narrative_ontology:cs_drift_state('1b4ba27a-3a5f-4e18-b461-1743152363f5', contemporary_tax_planning_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b4ba27a-3a5f-4e18-b461-1743152363f5', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, us_treasury).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize aggressive hour-counting and grouping elections to meet material participation thresholds, allowing them to deduct passive losses against active income, thereby preserving wealth and reducing tax liability.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Profit from advising high-net-worth clients on how to navigate and exploit the permissive interpretation of material participation rules, structuring investments and activities to maximize tax benefits.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, beneficiary,
    organized, biographical, mobile, national).

% Bears the direct cost of reduced tax revenue due to the widespread use of strategic passive loss deductions, impacting federal budgets and public services.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, us_treasury, payer,
    institutional, generational, trapped, national).

% Indirectly bear the burden of reduced federal tax revenue through potentially higher taxes, increased national debt, or reduced public services, without access to similar tax sheltering strategies.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, constrained, national).

% Administers and enforces IRC Section 469, but its ability to challenge aggressive interpretations is limited by the permissive nature of the rules, judicial precedent, and resource constraints, leading to a de facto acceptance of many sheltering strategies.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, internal_revenue_service_irs, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates disputes over material participation, shaping the interpretation of the rules through case law. Its decisions can either reinforce or challenge the permissive reading, but often reflect the existing interpretive landscape.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines criteria for distinguishing active business income/loss from passive investment income/loss, aiming to prevent unlimited deduction of passive losses against active income.
% TRANSFER_FUNCTION: Transfers tax liability from high-net-worth investors to the general tax base by allowing strategic reclassification of passive losses as active, effectively reducing the tax burden on certain forms of wealth.
% ABSENT_VOICES: Advocates for tax fairness, progressive taxation, and stricter enforcement of anti-abuse provisions are largely excluded from the interpretive process. They would argue for a higher, more verifiable bar for material participation to prevent wealth preservation strategies.
% DISAPPEARANCE_RATIONALE: If the permissive interpretation of material participation vanished overnight, high-net-worth investors would lose a significant tax shelter, leading to a substantial increase in their taxable income and a corresponding increase in federal revenue. Investment and tax planning strategies would reorganize dramatically to adapt to a stricter regime.
% FOUNDING_PROBLEM: The original problem IRC Section 469 was built to solve was that wealthy individuals could use passive investment losses (e.g., from real estate partnerships) to offset active income (e.g., salaries or business profits), effectively reducing or eliminating their tax liability.
% FOUNDING_PROBLEM_CORROBORATION: Tax fairness advocates and some economists argue the founding problem is still live, as the permissive interpretation undermines the original intent of limiting passive loss deductions. High-net-worth investors and tax advisors argue the rules provide necessary incentives for investment and reflect legitimate business activities. Independent academic studies often corroborate the significant revenue loss associated with these strategies.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it has a genuine coordination function (defining material participation to distinguish income types) but its permissive interpretation enables significant asymmetric extraction from the public fisc. Extractiveness is high (0.65) because the strategic use of these rules leads to substantial revenue loss. Suppression is moderate (0.40) as the IRS's ability to challenge these interpretations is constrained, and the rules themselves are permissive. Theater ratio is moderate (0.30) as the IRS maintains the appearance of strict enforcement while many aggressive strategies proceed unchallenged. Accessibility collapse is low (0.40) for beneficiaries, as the permissive interpretation opens avenues for tax sheltering rather than closing them. Resistance is low (0.20) from beneficiaries, who benefit from the permissive rules, and from the IRS, which is constrained in its ability to resist the prevailing interpretation.
 *
 * PERSPECTIVAL GAP:
 *   High-net-worth investors and tax advisors perceive this constraint as a legitimate framework for tax planning and investment incentives, a 'Rope' that facilitates economic activity. The US Treasury and general taxpayers, however, experience it as a 'Snare' or 'Tangled Rope' that extracts public resources through legal loopholes. The IRS, caught between legislative intent and judicial interpretation, struggles to enforce a 'strict gatekeeper' reading against a 'strategic shelter' reality.
 *
 * DIRECTIONALITY LOGIC:
 *   High-net-worth investors and tax advisors are clear beneficiaries, as the constraint directly enables them to reduce tax liabilities or profit from advising on such reductions. The US Treasury and general taxpayers are victims, bearing the cost of lost revenue. The IRS, while the agenda setter, is constrained in its ability to fully enforce a stricter interpretation, placing it in a complex position where it administers a system that allows for significant extraction from its own mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The permissive interpretation of material participation rules allows the original anti-abuse mandate of IRC Section 469 to atrophy. What was intended as a gatekeeper against passive loss deductions has, through aggressive interpretation and limited enforcement, become a mechanism for wealth preservation. The constraint's mandate has outlived its original function in many cases, but the structure persists due to the benefits it provides to powerful stakeholders and the difficulty of legislative or judicial reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_definition_ambiguity,
    'Is the definition of ''material participation'' inherently ambiguous, or has its interpretation been deliberately broadened through judicial and administrative practice?',
    'A comprehensive legislative redefinition of ''material participation'' with clear, objective criteria, or a landmark Supreme Court ruling that definitively narrows its scope.',
    'If inherently ambiguous, the current reading is a natural outcome of legal interpretation. If deliberately broadened, it points to regulatory capture or a failure of enforcement, suggesting a higher effective suppression of alternative interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_participation_definition_ambiguity, conceptual, 'Ambiguity in the core definition of material participation.').

omega_variable(
    irs_enforcement_capacity_impact,
    'To what extent does the IRS''s enforcement budget and staffing levels contribute to the permissive nature of this reading?',
    'Empirical studies correlating IRS audit rates and success rates in challenging material participation claims with changes in enforcement resources.',
    'If resource constraints are a primary driver, increased funding could shift the effective interpretation towards a stricter reading, reducing extractiveness. If not, the permissiveness is more deeply embedded in legal precedent and interpretive norms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irs_enforcement_capacity_impact, empirical, 'Impact of IRS enforcement capacity on the effective permissiveness of the rules.').

omega_variable(
    revenue_loss_policy_intent,
    'Is the revenue loss from these strategies an unintended consequence of the rules, or an implicitly accepted feature of tax policy designed to incentivize certain investments?',
    'Analysis of legislative history, lobbying efforts, and policy statements from key lawmakers and Treasury officials regarding the intent behind the current interpretive flexibility.',
    'If implicitly accepted, the ''extraction'' from the public fisc is a deliberate policy choice, reclassifying part of the extractiveness as a ''cost of coordination'' for economic incentives. If unintended, it reinforces the ''Snare'' or ''Tangled Rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revenue_loss_policy_intent, preference, 'Whether revenue loss is an intended or unintended outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.18).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1996, 0.45).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2006, 0.42).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2016, 0.41).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, tax_avoidance_strategies).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_investment_incentives).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, tax_fairness_discourse).

% DUAL FORMULATION NOTE:
% This constraint is the 'strategic_shelter_reading' of the 'irc_469_material_participation_kernel', which also has a 'strict_gatekeeper_reading'. Both readings are distinct constraints that interpret the same underlying tax law, leading to different structural outcomes and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
