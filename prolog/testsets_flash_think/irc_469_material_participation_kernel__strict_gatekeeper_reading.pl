% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC Section 469 Material Participation (Strict Gatekeeper Reading)
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'strict gatekeeper' reading of Internal
 *   Revenue Code Section 469, which governs the deductibility of passive
 *   activity losses. Under this reading, taxpayers must demonstrate
 *   verifiable, substantial personal labor to materially participate in a
 *   business, with a high documentation bar. This interpretation aims to
 *   narrowly qualify who can deduct passive losses against active income,
 *   thereby preventing tax sheltering. This story is one reading of the
 *   `irc_469_material_participation_kernel`.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.85).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.9).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, snare).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC Section 469 Material Participation (Strict Gatekeeper Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '816ffca1-fceb-4aef-8454-ea33782bd892').
narrative_ontology:cs_kernel_codification('816ffca1-fceb-4aef-8454-ea33782bd892', fixed_text).
narrative_ontology:cs_authority_grounding('816ffca1-fceb-4aef-8454-ea33782bd892', lineage).
narrative_ontology:cs_interpretation_layer_present('816ffca1-fceb-4aef-8454-ea33782bd892').
narrative_ontology:cs_reading_relation('816ffca1-fceb-4aef-8454-ea33782bd892', irc_469_material_participation_kernel__strategic_shelter_reading, forecloses).
narrative_ontology:cs_axiom('816ffca1-fceb-4aef-8454-ea33782bd892', foundational, passive_income_separate_from_active).
narrative_ontology:cs_axiom_status(passive_income_separate_from_active, holdable).
narrative_ontology:cs_axiom_grounding('816ffca1-fceb-4aef-8454-ea33782bd892', passive_income_separate_from_active, conventional).
narrative_ontology:cs_axiom('816ffca1-fceb-4aef-8454-ea33782bd892', secondary, tax_avoidance_is_undesirable).
narrative_ontology:cs_axiom_status(tax_avoidance_is_undesirable, holdable).
narrative_ontology:cs_axiom_grounding('816ffca1-fceb-4aef-8454-ea33782bd892', tax_avoidance_is_undesirable, instrumental).
narrative_ontology:cs_reference_frame('816ffca1-fceb-4aef-8454-ea33782bd892', original_legislative_intent_1986).
narrative_ontology:cs_drift_state('816ffca1-fceb-4aef-8454-ea33782bd892', contemporary_tax_planning_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('816ffca1-fceb-4aef-8454-ea33782bd892', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, taxpayers_with_passive_losses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces IRC Section 469, setting high bars for documentation and verification of material participation. Benefits from increased tax revenue and reduced tax avoidance. Actively audits and litigates cases to uphold strict interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs, agenda_setter,
    institutional, generational, analytical, national).

% Receives the tax revenue that would otherwise be offset by passive losses if material participation rules were less stringent. Benefits from a stable tax base and reduced revenue leakage.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, beneficiary,
    institutional, generational, analytical, national).

% Own substantial real estate portfolios and seek to deduct passive losses against active income. Face significant burdens in documenting 'material participation' and often incur substantial tax liabilities or professional fees to navigate the rules. Exit options are limited to divesting or accepting non-deductibility.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    powerful, biographical, constrained, national).

% Individuals with passive business activities generating losses. Struggle to meet the high documentation and labor requirements for material participation, often resulting in suspended losses that cannot be used to offset other income. Exit options are limited by the nature of their investments and financial situation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, taxpayers_with_passive_losses, payer,
    moderate, biographical, constrained, national).

% Provide complex tax planning and compliance services to investors navigating IRC 469. Benefit from the high complexity and strict documentation requirements, which create demand for their expertise.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors, beneficiary,
    organized, biographical, mobile, national).

% Adjudicate disputes between the IRS and taxpayers regarding material participation. Their rulings shape the interpretation and enforcement of the constraint, often reinforcing the strict gatekeeper reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the abuse of passive loss rules, ensuring that tax deductions are genuinely tied to active engagement in a business, thereby maintaining the integrity and fairness of the tax system.
% TRANSFER_FUNCTION: Transfers potential tax deductions (and thus tax revenue) from real estate investors and other taxpayers with passive losses to the U.S. Treasury, by disallowing the offset of passive losses against active income unless strict participation criteria are met.
% ABSENT_VOICES: Small-scale investors or those with less liquid assets who cannot afford extensive legal and accounting services to document their participation, or who are simply unaware of the stringent requirements. Their voices are often unheard in the legislative and judicial processes that shape the rule's interpretation.
% DISAPPEARANCE_RATIONALE: If the material participation rules and their strict enforcement vanished overnight, there would be a rapid and widespread proliferation of tax shelters, leading to a significant erosion of the tax base, massive shifts in investment behavior, and a fundamental reorganization of tax planning strategies across the economy.
% FOUNDING_PROBLEM: In the 1980s, aggressive tax shelters allowed high-income individuals to offset substantial active income with passive losses from investments (e.g., real estate, oil and gas), leading to a perception of unfairness and significant revenue loss for the government.
% FOUNDING_PROBLEM_CORROBORATION: Congressional committee reports from the 1986 Tax Reform Act, IRS enforcement statistics, and ongoing academic research in tax policy consistently corroborate the original problem and the continued potential for tax sheltering if the rules were relaxed. Independent tax policy analysts also attest to its ongoing relevance.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the strict interpretation significantly limits the ability of investors to deduct losses, effectively transferring potential tax savings to the government. Suppression (0.90) is very high due to the stringent documentation requirements, the complexity of the rules, and the active enforcement by the IRS, which collectively make it very difficult for many taxpayers to qualify. The theater ratio (0.10) is low, indicating that the IRS's enforcement efforts are genuinely aimed at upholding the rule's intent, with minimal performative activity. Accessibility collapse (0.80) is high as legitimate alternatives for deducting passive losses are severely restricted. Resistance (0.70) is substantial, evidenced by ongoing taxpayer litigation and continuous efforts by tax advisors to find compliant strategies.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS and U.S. Treasury's perspective, this constraint is a necessary and effective mechanism to ensure tax fairness and revenue integrity. From the perspective of real estate investors and taxpayers with passive losses, it is a burdensome and often unfair barrier that prevents them from offsetting legitimate business losses, leading to significant financial strain and compliance costs. The tax courts often mediate these divergent views, but their rulings tend to reinforce the strict interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and IRS are clear beneficiaries, receiving increased tax revenue and maintaining the integrity of the tax system. Real estate investors and taxpayers with passive losses are the primary targets/payers, bearing the direct financial cost of disallowed deductions and the compliance burden. Tax preparers and advisors benefit from the complexity, as their services become essential. The strict interpretation amplifies the extraction from payers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_burden_vs_substance,
    'Is the high documentation bar for material participation genuinely necessary to verify substantial personal labor, or does it primarily function as an arbitrary barrier to deduction?',
    'Empirical study comparing audit outcomes and compliance costs for different documentation thresholds, or legislative review of alternative verification methods.',
    'If the documentation burden is found to be disproportionate to its verification utility, it would suggest a higher effective extraction and suppression than strictly necessary for the coordination function, potentially reclassifying the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_vs_substance, empirical, 'Assesses the functional necessity of the high documentation requirements.').

omega_variable(
    passive_loss_definition_ambiguity,
    'How much personal labor truly constitutes ''material participation'' across the diverse range of business activities and investment structures, and is the current interpretation consistently applied?',
    'Analysis of tax court rulings and IRS guidance across various industries, coupled with expert surveys on typical labor inputs for different business types.',
    'Greater clarity or a more flexible interpretation could reduce compliance costs and perceived unfairness for some taxpayers, potentially lowering effective extraction for certain seats. Persistent ambiguity amplifies extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_loss_definition_ambiguity, conceptual, 'Examines the clarity and consistency of the ''material participation'' definition.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the IRC 469 material participation rule fundamentally a strict gatekeeper to prevent tax avoidance, or is it a permissive threshold achievable through aggressive hour-counting and grouping elections?',
    'Judicial precedent from higher courts, legislative clarification, or a shift in IRS enforcement policy that explicitly endorses one interpretation over the other.',
    'If the ''strategic shelter'' reading were to prevail, the constraint''s extractiveness and suppression would significantly decrease, and its classification would shift from Snare towards a more permissive Tangled Rope or even Rope, as more taxpayers could legitimately deduct losses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'The core ambiguity between the strict gatekeeper and strategic shelter readings of IRC 469.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(irc__tr_t1994, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(irc__tr_t2002, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(irc__tr_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(irc__tr_t2018, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1986, 0.75).
narrative_ontology:measurement(irc__be_t1994, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1994, 0.8).
narrative_ontology:measurement(irc__be_t2002, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2002, 0.83).
narrative_ontology:measurement(irc__be_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(irc__be_t2018, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1986, 0.8).
narrative_ontology:measurement(irc__su_t1994, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1994, 0.85).
narrative_ontology:measurement(irc__su_t2002, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2002, 0.88).
narrative_ontology:measurement(irc__su_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(irc__su_t2018, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2018, 0.91).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
