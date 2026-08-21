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
 *   human_readable: IRC 469 Material Participation: Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'strict gatekeeper' reading of IRC Section
 *   469, which defines 'material participation' for tax purposes. Under this
 *   reading, taxpayers must demonstrate verifiable, substantial personal
 *   labor in an activity to avoid having losses classified as 'passive,'
 *   which are generally not deductible against ordinary income. This
 *   interpretation narrows the qualifying population and imposes a high
 *   documentation burden, leading to significant compliance friction and
 *   increased tax revenue for the government. It is one reading of the
 *   broader 'irc_469_material_participation_kernel' which is contested by a
 *   'strategic_shelter_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC 469 Material Participation: Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e3c68aa4-b773-4a51-b27d-a4417e319005').
narrative_ontology:cs_kernel_codification('e3c68aa4-b773-4a51-b27d-a4417e319005', fixed_text).
narrative_ontology:cs_authority_grounding('e3c68aa4-b773-4a51-b27d-a4417e319005', lineage).
narrative_ontology:cs_interpretation_layer_present('e3c68aa4-b773-4a51-b27d-a4417e319005').
narrative_ontology:cs_reading_relation('e3c68aa4-b773-4a51-b27d-a4417e319005', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('e3c68aa4-b773-4a51-b27d-a4417e319005', foundational, tax_avoidance_is_a_primary_evil).
narrative_ontology:cs_axiom_status(tax_avoidance_is_a_primary_evil, holdable).
narrative_ontology:cs_axiom_grounding('e3c68aa4-b773-4a51-b27d-a4417e319005', tax_avoidance_is_a_primary_evil, deontological).
narrative_ontology:cs_axiom('e3c68aa4-b773-4a51-b27d-a4417e319005', foundational, verifiable_labor_is_the_only_true_participation).
narrative_ontology:cs_axiom_status(verifiable_labor_is_the_only_true_participation, holdable).
narrative_ontology:cs_axiom_grounding('e3c68aa4-b773-4a51-b27d-a4417e319005', verifiable_labor_is_the_only_true_participation, conventional).
narrative_ontology:cs_reference_frame('e3c68aa4-b773-4a51-b27d-a4417e319005', anti_abuse_legislative_intent).
narrative_ontology:cs_drift_state('e3c68aa4-b773-4a51-b27d-a4417e319005', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3c68aa4-b773-4a51-b27d-a4417e319005', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from increased tax revenue due to fewer passive losses offsetting ordinary income. The strict interpretation aligns with its mandate to prevent tax avoidance and maintain fiscal stability.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, beneficiary,
    institutional, generational, analytical, national).

% Benefit from the complexity and high documentation requirements, leading to increased demand for their services in advising clients on compliance and audit defense. They are incentivized to maintain the high bar.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of meticulous record-keeping and the potential inability to deduct legitimate losses against other income, increasing their effective tax burden. Their exit options are limited by the need to comply with tax law or exit real estate investment entirely.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    moderate, biographical, constrained, local).

% Often struggle to meet the high documentation bar for their active involvement in businesses that might generate passive losses, leading to unexpected tax liabilities. They lack the resources for extensive legal and accounting advice.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_business_owners, payer,
    powerless, immediate, constrained, local).

% Enforce the material participation rules, often interpreting them strictly during audits. Their performance metrics may incentivize disallowing passive losses, reinforcing the high documentation bar.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_auditors, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate taxpayer behavior by clearly distinguishing between active business income and passive investment income, preventing the use of 'paper losses' from passive activities to shelter active income.
% TRANSFER_FUNCTION: Transfers potential tax savings (from deducting passive losses) from real estate investors and small business owners to the U.S. Treasury, while also generating revenue for tax preparers and advisors due to compliance complexity.
% ABSENT_VOICES: Advocacy groups for small businesses and real estate investors, who would argue for a more practical and less burdensome definition of material participation, are often outmatched by the institutional power of the Treasury and the IRS.
% DISAPPEARANCE_RATIONALE: If the strict material participation rules vanished, there would be a significant shift in tax planning strategies, potentially leading to a surge in passive loss deductions, a decrease in federal tax revenue, and a re-evaluation of investment structures, particularly in real estate.
% FOUNDING_PROBLEM: The original problem was widespread tax shelters where wealthy individuals used passive losses from investments (e.g., limited partnerships) to offset active income, eroding the tax base and perceived fairness of the tax system.
% FOUNDING_PROBLEM_CORROBORATION: The U.S. Treasury and IRS consistently argue that the problem of tax shelters remains live, requiring robust enforcement. Independent tax policy analysts generally corroborate that without such rules, tax avoidance would increase, though they may dispute the stringency of the current interpretation.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.68) reflects the substantial tax liabilities incurred by investors and small business owners who cannot meet the stringent participation and documentation requirements. Suppression (0.75) is high due to the IRS's enforcement power and the lack of viable alternatives to compliance. The theater ratio is low (0.20) because the IRS genuinely enforces these rules, though some of the complexity might be performative. The increasing extractiveness and suppression over time reflect a trend towards stricter enforcement and interpretation since the rule's inception.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the U.S. Treasury, this is a necessary anti-abuse measure. From the perspective of investors and small business owners, it is an overly burdensome and extractive regulation that penalizes legitimate business activity. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and tax advisors are beneficiaries, gaining revenue and business from the strict interpretation. Real estate investors and small business owners are payers, bearing the direct costs of compliance and disallowed losses. IRS auditors act as agenda-setters, actively enforcing the strict interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing tax shelters) is still live, but its strict interpretation has arguably expanded beyond its original scope, creating a 'tangled rope' where genuine coordination (preventing abuse) is intertwined with significant extraction from legitimate actors. The high documentation bar, while serving the anti-abuse mandate, also creates a revenue stream for tax advisors and the Treasury, suggesting a hybrid function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_burden_vs_abuse_prevention,
    'Is the current high documentation bar for material participation genuinely necessary to prevent tax abuse, or does it primarily serve to increase compliance costs and tax revenue?',
    'Empirical study comparing tax avoidance rates in regimes with varying documentation requirements for active participation, or a cost-benefit analysis of the current rules.',
    'If the burden is disproportionate to abuse prevention, it would suggest the constraint is more extractive than coordinative, pushing its classification closer to a Snare. If it''s found necessary, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_vs_abuse_prevention, empirical, 'Assesses the functional necessity of the high documentation bar.').

omega_variable(
    strict_vs_permissive_interpretation,
    'Is this constraint a genuine interpretation of legislative intent, or an administrative hardening that benefits the enforcing agency?',
    'Judicial review of IRS interpretations against legislative history, or new legislation clarifying the definition of material participation.',
    'If found to be an administrative hardening, it would highlight the extractive nature of the IRS''s role and potentially shift the constraint towards a Snare. If upheld as consistent with intent, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_vs_permissive_interpretation, conceptual, 'Ambiguity between legislative intent and administrative interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1996, 0.58).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1986, 0.6).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1996, 0.65).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'irc_469_material_participation_kernel'. Its strict interpretation influences tax planning and investment strategies, contrasting with the 'strategic_shelter_reading' which seeks to minimize its impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
