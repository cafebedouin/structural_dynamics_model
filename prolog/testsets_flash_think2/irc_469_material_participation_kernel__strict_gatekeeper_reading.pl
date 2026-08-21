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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC Section 469 Material Participation: Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'strict gatekeeper' reading of IRC Section
 *   469's material participation rules, which requires verifiable,
 *   substantial personal labor and a high documentation bar for taxpayers to
 *   deduct passive losses. This reading emphasizes the anti-abuse intent of
 *   the statute, narrowing the qualifying population and increasing
 *   compliance friction. It is one interpretation of the
 *   'irc_469_material_participation_kernel', contrasting with the
 *   'strategic_shelter_reading' which seeks more permissive thresholds.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.8).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC Section 469 Material Participation: Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '6e7c2778-ac58-4215-83d9-945966f0322f').
narrative_ontology:cs_kernel_codification('6e7c2778-ac58-4215-83d9-945966f0322f', fixed_text).
narrative_ontology:cs_authority_grounding('6e7c2778-ac58-4215-83d9-945966f0322f', lineage).
narrative_ontology:cs_interpretation_layer_present('6e7c2778-ac58-4215-83d9-945966f0322f').
narrative_ontology:cs_reading_relation('6e7c2778-ac58-4215-83d9-945966f0322f', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('6e7c2778-ac58-4215-83d9-945966f0322f', foundational, passive_loss_abuse_prevention).
narrative_ontology:cs_axiom_status(passive_loss_abuse_prevention, holdable).
narrative_ontology:cs_axiom_grounding('6e7c2778-ac58-4215-83d9-945966f0322f', passive_loss_abuse_prevention, instrumental).
narrative_ontology:cs_axiom('6e7c2778-ac58-4215-83d9-945966f0322f', foundational, verifiable_personal_engagement).
narrative_ontology:cs_axiom_status(verifiable_personal_engagement, holdable).
narrative_ontology:cs_axiom_grounding('6e7c2778-ac58-4215-83d9-945966f0322f', verifiable_personal_engagement, conventional).
narrative_ontology:cs_reference_frame('6e7c2778-ac58-4215-83d9-945966f0322f', anti_abuse_framework).
narrative_ontology:cs_drift_state('6e7c2778-ac58-4215-83d9-945966f0322f', contemporary_enforcement_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6e7c2778-ac58-4215-83d9-945966f0322f', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, internal_revenue_service).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, taxpayers_with_passive_losses).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces IRC Section 469, requiring taxpayers to demonstrate 'material participation' in passive activities to deduct losses against ordinary income. Benefits from increased tax revenue and reduced tax sheltering.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, internal_revenue_service, agenda_setter,
    institutional, generational, analytical, national).

% Bear the burden of proving material participation through detailed documentation of personal labor. Face denied deductions and potential audit risk if documentation is insufficient or participation is deemed not 'substantial'.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, taxpayers_with_passive_losses, payer,
    moderate, biographical, constrained, national).

% A specific subset of taxpayers often impacted by passive loss rules. Must navigate complex rules to qualify as 'real estate professionals' or demonstrate material participation in individual properties to deduct losses. Face significant compliance costs.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    powerful, biographical, constrained, national).

% Advise taxpayers on compliance with material participation rules, helping them structure activities and maintain documentation. Benefit from the complexity of the rules, which drives demand for their services.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisors, beneficiary,
    organized, biographical, mobile, national).

% Enacted IRC Section 469 to prevent tax shelters. Continues to oversee tax policy and could amend the statute, but generally supports the anti-abuse intent of the material participation rules.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents widespread abuse of passive activity losses to shelter active income, thereby protecting the tax base and promoting tax fairness by ensuring that only genuinely active participants can deduct losses.
% TRANSFER_FUNCTION: Transfers potential tax deductions (passive losses) from taxpayers who cannot meet the strict material participation criteria to the government (IRS) as increased tax revenue. Also transfers significant compliance costs from taxpayers to tax advisors.
% ABSENT_VOICES: Taxpayers who engage in passive activities with genuine economic intent but without the capacity or inclination for 'substantial personal labor' or meticulous documentation. They would argue for a more flexible or less burdensome standard for participation.
% DISAPPEARANCE_RATIONALE: If the material participation rules and their strict enforcement vanished overnight, there would be a rapid re-emergence of tax shelters using passive losses, leading to a significant erosion of the tax base and a shift in investment strategies towards tax avoidance.
% FOUNDING_PROBLEM: Widespread abuse of passive activity losses, particularly in real estate, where investors claimed large paper losses from activities in which they had little or no genuine involvement, using these losses to offset active income and reduce their tax liability.
% FOUNDING_PROBLEM_CORROBORATION: Congressional committee reports from the 1986 Tax Reform Act, Treasury Department analyses, and ongoing IRS enforcement actions and guidance all corroborate the persistent potential for passive loss abuse, even if the scale has changed.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the significant amount of otherwise deductible losses that are disallowed under this strict interpretation, coupled with the substantial compliance costs. Suppression (0.80) is high due to the IRS's active enforcement, audit risk, and the detailed, often burdensome, documentation requirements that effectively 'trap' taxpayers into compliance or forfeiture. The theater ratio (0.40) indicates that while there's genuine anti-abuse function, a considerable portion of the compliance effort is performative, focused on meeting arbitrary documentation thresholds rather than demonstrating true economic engagement. The increasing trend in extractiveness and suppression over the interval reflects a hardening of this 'strict gatekeeper' interpretation through IRS guidance and court precedents.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS's perspective, this strict reading is a necessary and effective tool for tax fairness and revenue protection. From the perspective of taxpayers and investors, it is an overly burdensome and extractive regime that penalizes legitimate investment activities and creates disproportionate compliance costs. The engine's per-seat classification will reflect this divergence, with the IRS seat computing as a beneficiary of a coordination mechanism, while taxpayer seats compute as targets of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS is the primary beneficiary, gaining revenue from disallowed losses and enforcing the rules (low directionality). Taxpayers with passive losses, especially real estate investors, are the primary targets, bearing the costs of denied deductions and compliance (high directionality). Tax advisors benefit from the complexity, positioning them as beneficiaries. Congress, as the legislative body, acts as an observer and potential agenda-setter, but its intent is largely aligned with the anti-abuse stance of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_interpretation_ambiguity,
    'Is the ''strict gatekeeper'' reading of material participation the only defensible interpretation of IRC Section 469''s legislative intent, or does the ''strategic shelter'' reading offer a plausible alternative?',
    'Further legislative clarification from Congress, or a Supreme Court ruling that definitively establishes the scope and intent of ''material participation'' beyond current regulatory and circuit court interpretations.',
    'If the ''strategic shelter'' reading were to gain legal ascendancy, the constraint''s extractiveness and suppression would likely decrease, potentially reclassifying it from a Tangled Rope to a more permissive Rope or even a Piton if enforcement became purely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_interpretation_ambiguity, conceptual, 'Ambiguity between strict and permissive interpretations of material participation.').

omega_variable(
    documentation_effectiveness_vs_burden,
    'To what extent does the high documentation bar genuinely prevent tax sheltering, versus merely imposing an arbitrary compliance burden on legitimate activities?',
    'Empirical study comparing tax avoidance rates and compliance costs under different documentation regimes (e.g., if a simplified documentation standard were piloted).',
    'If the documentation burden is found to be largely arbitrary, the ''theater_ratio'' would increase, and the ''suppression'' metric might be re-evaluated as less functional and more coercive, pushing the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_effectiveness_vs_burden, empirical, 'Effectiveness of documentation requirements in preventing abuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1986, 2016).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1986, 0.3).
narrative_ontology:measurement(irc__tr_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1992, 0.33).
narrative_ontology:measurement(irc__tr_t1998, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(irc__tr_t2004, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2004, 0.37).
narrative_ontology:measurement(irc__tr_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2016, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement(irc__be_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1992, 0.65).
narrative_ontology:measurement(irc__be_t1998, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(irc__be_t2004, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2004, 0.71).
narrative_ontology:measurement(irc__be_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2016, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1986, 0.65).
narrative_ontology:measurement(irc__su_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1992, 0.7).
narrative_ontology:measurement(irc__su_t1998, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1998, 0.74).
narrative_ontology:measurement(irc__su_t2004, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2004, 0.77).
narrative_ontology:measurement(irc__su_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2016, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'strict gatekeeper' reading of the IRC Section 469 material participation kernel, which also has a 'strategic_shelter_reading' sibling. Both are distinct constraints arising from different interpretations of the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
