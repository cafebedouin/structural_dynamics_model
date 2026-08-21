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
 *   human_readable: Permissive Material Participation Rules for Passive Loss Sheltering
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strategic_shelter_reading' of the
 *   IRC Section 469 material participation kernel. This reading interprets
 *   the rules permissively, allowing taxpayers to achieve material
 *   participation through aggressive hour-counting and grouping elections,
 *   thereby enabling systematic passive loss deductions and wealth
 *   preservation. It stands in contrast to a 'strict_gatekeeper_reading' that
 *   would require verifiable, substantial personal labor. The metrics reflect
 *   the impact of this permissive interpretation on the tax system and
 *   general public.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.75).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.7).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "Permissive Material Participation Rules for Passive Loss Sheltering").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '22cdcc5e-fdfc-47da-9a04-adc5245f43a9').
narrative_ontology:cs_kernel_codification('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', fixed_text).
narrative_ontology:cs_authority_grounding('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', lineage).
narrative_ontology:cs_interpretation_layer_present('22cdcc5e-fdfc-47da-9a04-adc5245f43a9').
narrative_ontology:cs_reading_relation('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', foundational, passive_loss_deduction_is_a_right).
narrative_ontology:cs_axiom_status(passive_loss_deduction_is_a_right, holdable).
narrative_ontology:cs_axiom_grounding('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', passive_loss_deduction_is_a_right, conventional).
narrative_ontology:cs_axiom('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', secondary, aggressive_hour_counting_reflects_materiality).
narrative_ontology:cs_axiom_status(aggressive_hour_counting_reflects_materiality, holdable).
narrative_ontology:cs_axiom_grounding('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', aggressive_hour_counting_reflects_materiality, empirically_contingent).
narrative_ontology:cs_reference_frame('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', taxpayer_flexibility_framework).
narrative_ontology:cs_drift_state('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', contemporary_tax_planning_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('22cdcc5e-fdfc-47da-9a04-adc5245f43a9', '').
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

% Bears the direct cost of reduced tax revenue due to passive loss deductions enabled by this permissive interpretation. Its ability to collect revenue is constrained by the interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, us_treasury, payer,
    institutional, generational, constrained, national).

% Benefit significantly by using passive losses from real estate and other activities to offset active income, reducing their overall tax liability. They actively seek and implement strategies enabled by this interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors, beneficiary,
    powerful, biographical, mobile, national).

% Actively develop and promote strategies (e.g., aggressive hour-counting, grouping elections) that leverage this permissive interpretation to benefit their high-net-worth clients. They shape the practical application of the rules.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, agenda_setter,
    organized, biographical, mobile, national).

% Are tasked with enforcing IRC Section 469, but face challenges in disproving aggressive hour-counting and grouping elections due to the permissive nature of this interpretation. They represent the primary enforcement mechanism against potential abuse.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_auditors, agenda_setter,
    institutional, biographical, constrained, national).

% Indirectly bear the cost of reduced federal revenue through either higher taxes elsewhere or reduced public services. They have no direct means to influence this interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, constrained, national).

% Adjudicates disputes between the IRS and taxpayers regarding material participation. Its rulings contribute to the evolving interpretation of the kernel, often reflecting the tension between strict enforcement and taxpayer flexibility.
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
% COORDINATION_FUNCTION: Provides a framework for high-net-worth investors and their tax advisors to structure real estate and other passive activities in a way that allows them to meet material participation thresholds, thereby enabling passive loss deductions.
% TRANSFER_FUNCTION: Transfers potential tax revenue from the U.S. Treasury (and indirectly, general taxpayers) to high-net-worth investors by allowing them to offset active income with losses from activities that are, in practice, largely passive.
% ABSENT_VOICES: Public interest groups advocating for tax fairness and economists concerned about revenue leakage and wealth inequality are structurally underrepresented in the direct interpretive process, though they may influence legislative efforts. They would argue for a stricter interpretation to ensure equitable tax burdens.
% DISAPPEARANCE_RATIONALE: If this permissive interpretation vanished overnight, high-net-worth investors would face significantly higher tax liabilities, leading to a rapid restructuring of investment strategies, a potential decrease in certain types of real estate investment, and a substantial increase in federal tax revenue. The entire tax planning industry around passive losses would be forced to adapt.
% FOUNDING_PROBLEM: The original IRC Section 469 (enacted in 1986) aimed to prevent taxpayers from using losses from 'tax shelters' (passive activities) to offset active income, thereby ensuring tax fairness and protecting federal revenue integrity.
% FOUNDING_PROBLEM_CORROBORATION: The IRS and tax fairness advocates argue that the original problem of passive loss sheltering persists and is exacerbated by permissive interpretations, leading to ongoing revenue leakage. High-net-worth investors and their advisors argue that the rules, as permissively interpreted, provide necessary incentives for real estate investment and that their activities are genuinely active. Independent tax policy analysts often corroborate the view that the permissive interpretation undermines the original legislative intent.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the substantial amount of tax revenue diverted from the U.S. Treasury to high-net-worth investors due to this permissive interpretation. Suppression (0.70) is high because this reading effectively suppresses the alternative of collecting more tax revenue and limits the IRS's ability to challenge claims. Theater ratio (0.40) is moderate, as while there is genuine compliance activity, aggressive hour-counting and grouping elections often involve a degree of performative justification rather than purely functional activity. Resistance (0.75) is high, reflecting ongoing efforts by the IRS to challenge aggressive interpretations through audits and litigation, even as taxpayers and their advisors actively embrace and expand these strategies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-net-worth investors and tax advisors, this interpretation functions as a beneficial coordination mechanism, providing clear (if flexible) pathways to tax efficiency. From the perspective of the U.S. Treasury and general taxpayers, it operates as an extractive mechanism, diverting public funds and undermining tax fairness. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   High-net-worth investors and tax advisors are clear beneficiaries, as the constraint subsidizes their wealth preservation efforts (low directionality). The U.S. Treasury and general taxpayers are the targets, bearing the costs of reduced revenue and increased tax burden elsewhere (high directionality). IRS auditors, while technically enforcers, are constrained by the permissive nature of the interpretation, placing them closer to the target end when trying to uphold the original intent of the law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    materiality_definition_ambiguity,
    'What constitutes ''material'' participation in a business activity, and how objectively verifiable are the criteria for aggressive hour-counting and grouping elections?',
    'Legislative clarification of ''material participation'' with objective, quantifiable metrics that are less susceptible to subjective interpretation, or judicial precedent establishing stricter evidentiary standards for hour-counting.',
    'If ''materiality'' is strictly defined and verifiable, the extractiveness of this reading would decrease significantly, and its classification would shift towards a more genuine ''rope'' or even ''mountain'' (if truly objective). If ambiguity persists, the current extractive classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(materiality_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''material participation'' and verifiability of compliance methods.').

omega_variable(
    economic_incentive_vs_shelter,
    'To what extent do the tax benefits enabled by this permissive interpretation genuinely incentivize productive real estate investment and economic activity, versus merely facilitating tax sheltering for existing wealth?',
    'Empirical economic studies analyzing investment patterns and economic growth in response to changes in passive loss rules, distinguishing between new productive investment and re-characterization of existing activities.',
    'If the primary effect is sheltering, the ''tangled_rope'' classification is strongly supported, highlighting the extractive nature. If significant productive investment is genuinely incentivized, the coordination function is stronger, potentially shifting towards a ''rope'' for the economy as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_incentive_vs_shelter, empirical, 'Whether tax benefits primarily incentivize investment or sheltering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.28).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.34).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2016, 0.72).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1996, 0.6).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2006, 0.65).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
