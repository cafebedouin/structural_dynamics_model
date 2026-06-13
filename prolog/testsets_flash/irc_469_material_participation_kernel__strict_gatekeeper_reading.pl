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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC 469 Material Participation: Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'strict gatekeeper' interpretation of IRC
 *   Section 469, which governs material participation in passive activities
 *   for tax purposes. Under this reading, investors must demonstrate
 *   verifiable, substantial personal labor in their real estate activities,
 *   backed by rigorous documentation, to deduct passive losses against
 *   ordinary income. This interpretation aims to prevent tax shelters and
 *   ensure that only genuinely active participants receive favorable tax
 *   treatment. It is one reading of the broader
 *   'irc_469_material_participation_kernel', which is contested by a
 *   'strategic_shelter_reading' that seeks more permissive thresholds.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.65).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC 469 Material Participation: Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '3a33b6bd-f4ab-4daf-81cd-172079508baf').
narrative_ontology:cs_kernel_codification('3a33b6bd-f4ab-4daf-81cd-172079508baf', formalized).
narrative_ontology:cs_authority_grounding('3a33b6bd-f4ab-4daf-81cd-172079508baf', lineage).
narrative_ontology:cs_interpretation_layer_present('3a33b6bd-f4ab-4daf-81cd-172079508baf').
narrative_ontology:cs_reading_relation('3a33b6bd-f4ab-4daf-81cd-172079508baf', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('3a33b6bd-f4ab-4daf-81cd-172079508baf', foundational, tax_equity_requires_active_participation_verification).
narrative_ontology:cs_axiom_status(tax_equity_requires_active_participation_verification, holdable).
narrative_ontology:cs_axiom_grounding('3a33b6bd-f4ab-4daf-81cd-172079508baf', tax_equity_requires_active_participation_verification, deontological).
narrative_ontology:cs_axiom('3a33b6bd-f4ab-4daf-81cd-172079508baf', secondary, documentation_is_primary_evidence_of_participation).
narrative_ontology:cs_axiom_status(documentation_is_primary_evidence_of_participation, holdable).
narrative_ontology:cs_axiom_grounding('3a33b6bd-f4ab-4daf-81cd-172079508baf', documentation_is_primary_evidence_of_participation, conventional).
narrative_ontology:cs_reference_frame('3a33b6bd-f4ab-4daf-81cd-172079508baf', anti_shelter_tax_integrity).
narrative_ontology:cs_drift_state('3a33b6bd-f4ab-4daf-81cd-172079508baf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3a33b6bd-f4ab-4daf-81cd-172079508baf', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, ordinary_income_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_net_worth_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces IRC Section 469, interpreting 'material participation' strictly to limit passive loss deductions. Benefits from increased tax revenue and maintaining tax system integrity.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs, agenda_setter,
    institutional, generational, analytical, national).

% Engage in real estate activities and seek to deduct losses. Bear the burden of high documentation requirements and disallowed passive losses, leading to higher tax liabilities. Their options are to comply, litigate, or exit the market.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    powerful, biographical, constrained, national).

% Often invest in complex real estate ventures and are primary targets for passive loss limitations. Face significant compliance costs and potential disallowance of substantial deductions, increasing their effective tax rate.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_net_worth_individuals, payer,
    powerful, biographical, constrained, national).

% Benefit indirectly from the constraint by preventing wealthy individuals from sheltering ordinary income, which helps maintain a fairer distribution of the tax burden and potentially reduces the need for higher taxes on other income sources.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, ordinary_income_taxpayers, beneficiary,
    organized, generational, mobile, national).

% Provide services to investors navigating the complex material participation rules. Benefit from the high demand for their expertise due to the strict documentation and interpretation requirements.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To distinguish between active business income and passive investment income for tax purposes, ensuring that tax benefits (like loss deductions) are appropriately allocated to genuinely active participants and preventing the use of passive activities as tax shelters.
% TRANSFER_FUNCTION: Transfers potential tax deductions (passive losses) from real estate investors and high-net-worth individuals to the IRS (as increased tax revenue) and indirectly to ordinary income taxpayers (by maintaining a broader tax base).
% ABSENT_VOICES: Advocates for small-scale, genuinely active real estate investors who may struggle with the high documentation burden despite substantial personal labor. They would argue for more flexible or proportional documentation requirements.
% DISAPPEARANCE_RATIONALE: If the material participation rules vanished, there would be a massive shift in tax planning, with widespread creation of passive loss tax shelters. This would significantly reduce government tax revenue, shift the tax burden, and fundamentally alter investment strategies in real estate and other passive activities.
% FOUNDING_PROBLEM: The proliferation of tax shelters in the 1970s and early 1980s, where wealthy individuals used passive investments to generate artificial losses to offset ordinary income, eroding the tax base and creating perceived unfairness.
% FOUNDING_PROBLEM_CORROBORATION: The IRS and tax policy experts consistently attest that the problem of tax sheltering remains live, citing ongoing efforts by taxpayers to minimize liabilities through passive activities. Independent economic analyses and legislative debates corroborate the continued relevance of preventing tax avoidance, even if the specific mechanisms evolve.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (preventing tax avoidance and ensuring fairness in tax burden) but also involves significant asymmetric extraction. The high documentation bar and strict interpretation lead to substantial extractiveness (0.65) by disallowing deductions, and high suppression (0.75) by creating significant compliance friction and effectively 'trapping' investors who cannot meet the bar. Theater ratio is low (0.20) as the IRS actively enforces these rules, and the documentation requirements are not merely performative but directly impact deductibility. The increasing extractiveness and suppression over time reflect a hardening of enforcement and interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS's perspective, this is a necessary gatekeeper to maintain tax fairness and prevent abuse. From the perspective of real estate investors, particularly high-net-worth individuals, it is an overly burdensome and extractive rule that unfairly limits legitimate business deductions. Ordinary income taxpayers, as beneficiaries of reduced tax sheltering, would see it as a fair and necessary constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS is a primary beneficiary (d=0.0-0.1) as it collects more tax revenue and maintains the integrity of the tax system. Ordinary income taxpayers are also beneficiaries (d=0.1-0.2) as their tax burden is not disproportionately shifted to cover sheltered income. Real estate investors and high-net-worth individuals are targets (d=0.8-1.0) as they bear the direct costs of disallowed deductions and compliance burdens. Their exit options are constrained by the desire to remain in the real estate market and the high cost of non-compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing tax shelters) is still live, but its application through a strict gatekeeper reading ensures that it continues to extract from a broad base of investors. If the founding problem (tax sheltering) were to disappear, the constraint would likely become a Piton, persisting due to inertia but with no active function. However, as long as tax sheltering remains a concern, the strict interpretation ensures the constraint remains a Tangled Rope, actively enforced and extractive. The classification prevents mislabeling it as a pure Snare by acknowledging its genuine coordination function in maintaining tax equity, while still highlighting its extractive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a strict gatekeeper, or is it a permissive threshold for strategic tax shelters?',
    'Judicial precedent consistently upholding strict IRS interpretations, or legislative clarification explicitly codifying high documentation standards.',
    'If the strategic_shelter_reading were adopted, the constraint would become a Rope or even a Piton, with lower extractiveness and suppression, as passive losses would be more easily deductible. This strict_gatekeeper_reading ensures higher extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the IRC 469 material participation kernel, specifically the strict_gatekeeper_reading, which emphasizes verifiable, substantial personal labor and high documentation to limit passive loss deductions. The alternative is the strategic_shelter_reading, which views material participation as a permissive threshold achievable through aggressive hour-counting and grouping elections.').

omega_variable(
    documentation_burden_vs_substance,
    'Does the high documentation bar genuinely reflect substantial personal labor, or does it primarily serve as a compliance burden that disproportionately excludes legitimate participants?',
    'Empirical study correlating documented hours with actual time commitment and business outcomes, or a regulatory review simplifying documentation requirements without compromising the ''substantial'' labor requirement.',
    'If the documentation burden is found to be disproportionate, the suppression metric would be re-evaluated as higher, and the constraint''s legitimacy as a ''gatekeeper'' would be undermined, potentially shifting it towards a Snare for those genuinely active but poorly documented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_vs_substance, empirical, 'Ambiguity regarding whether the high documentation requirement for material participation is a necessary component of verifying ''substantial'' labor or an excessive barrier to entry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'strict gatekeeper' reading of the IRC 469 material participation kernel. It is structurally distinct from the 'strategic shelter' reading, which would have different extractiveness and suppression metrics due to a more permissive interpretation of participation requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
