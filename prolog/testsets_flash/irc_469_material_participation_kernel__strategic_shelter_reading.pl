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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation: Strategic Shelter Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents a specific, permissive reading of the Internal
 *   Revenue Code (IRC) Section 469's 'material participation' rules, which
 *   govern the deductibility of passive activity losses. Under this reading,
 *   taxpayers, particularly high-net-worth investors, can achieve material
 *   participation through aggressive hour-counting and strategic grouping
 *   elections, effectively converting passive losses into deductible active
 *   losses. This interpretation enables systematic tax sheltering, benefiting
 *   investors and tax advisors while shifting the tax burden and straining
 *   IRS enforcement capacity. This is one reading of the
 *   'irc_469_material_participation_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.65).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.3).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation: Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '450b4403-88b4-4c66-9197-09b837f143f8').
narrative_ontology:cs_kernel_codification('450b4403-88b4-4c66-9197-09b837f143f8', fixed_text).
narrative_ontology:cs_authority_grounding('450b4403-88b4-4c66-9197-09b837f143f8', lineage).
narrative_ontology:cs_interpretation_layer_present('450b4403-88b4-4c66-9197-09b837f143f8').
narrative_ontology:cs_reading_relation('450b4403-88b4-4c66-9197-09b837f143f8', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('450b4403-88b4-4c66-9197-09b837f143f8', foundational, taxpayer_friendly_interpretation_maximizes_investment).
narrative_ontology:cs_axiom_status(taxpayer_friendly_interpretation_maximizes_investment, holdable).
narrative_ontology:cs_axiom_grounding('450b4403-88b4-4c66-9197-09b837f143f8', taxpayer_friendly_interpretation_maximizes_investment, instrumental).
narrative_ontology:cs_axiom('450b4403-88b4-4c66-9197-09b837f143f8', secondary, subjective_hour_counting_is_sufficient_proof).
narrative_ontology:cs_axiom_status(subjective_hour_counting_is_sufficient_proof, holdable).
narrative_ontology:cs_axiom_grounding('450b4403-88b4-4c66-9197-09b837f143f8', subjective_hour_counting_is_sufficient_proof, conventional).
narrative_ontology:cs_reference_frame('450b4403-88b4-4c66-9197-09b837f143f8', taxpayer_autonomy_and_investment_incentive).
narrative_ontology:cs_drift_state('450b4403-88b4-4c66-9197-09b837f143f8', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('450b4403-88b4-4c66-9197-09b837f143f8', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize aggressive hour-counting and grouping elections to meet material participation thresholds, enabling them to deduct passive losses against active income, thereby reducing their overall tax burden and preserving wealth.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors, beneficiary,
    powerful, generational, mobile, national).

% Advise clients on strategies to meet material participation rules, including structuring activities and documenting hours. They benefit from the complexity and permissiveness of the rules, which creates demand for their expertise.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, agenda_setter,
    organized, biographical, mobile, national).

% Struggles to audit and challenge aggressive material participation claims due to the subjective nature of hour-counting and the volume of returns. This reading of the rule strains their resources and reduces effective tax collection.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement_capacity, payer,
    institutional, generational, constrained, national).

% Bear the indirect cost of reduced tax revenue through higher taxes elsewhere or reduced public services. They do not have the means or expertise to utilize these tax shelters.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, trapped, national).

% Oversee the tax code and receive reports on its effectiveness and fairness. They could legislate changes to IRC 469 to tighten material participation rules but face lobbying pressure from beneficiaries.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congressional_tax_committees, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing active business income from passive investment income, aiming to prevent taxpayers from indefinitely deferring tax on passive income by generating artificial losses.
% TRANSFER_FUNCTION: Transfers tax liability from high-net-worth investors (who can strategically meet material participation thresholds) to the general tax base, by allowing passive losses to offset active income.
% ABSENT_VOICES: Advocates for tax fairness and simplified tax codes are often excluded from the technical interpretation and enforcement debates, where the complexity of the rules allows for strategic exploitation. They would argue for clearer, less manipulable definitions of participation.
% DISAPPEARANCE_RATIONALE: If this permissive reading of material participation vanished, high-net-worth investors would lose a significant tax shelter, leading to a substantial increase in their tax liabilities. This would likely trigger a reallocation of investment strategies and a push for new legislative loopholes, fundamentally altering wealth management practices.
% FOUNDING_PROBLEM: The original IRC 469 sought to prevent taxpayers from using passive losses from tax shelters to offset active income, thereby eroding the tax base and creating unfair advantages.
% FOUNDING_PROBLEM_CORROBORATION: The IRS and tax fairness advocates attest that the founding problem of tax base erosion persists, exacerbated by this permissive reading. High-net-worth investors and their advisors argue that the rules, as interpreted, correctly incentivize real estate investment and economic activity, and that the problem is not with the interpretation but with the overall tax burden. Independent economic analyses often highlight the revenue loss, corroborating the 'problem persists' view from outside the benefiting parties.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because this reading allows significant tax savings for a specific group, effectively transferring wealth. Suppression (0.30) is moderate; while the IRS has enforcement power, the subjective nature of 'hours worked' and the complexity of grouping elections make it difficult to challenge. The theater ratio (0.45) is rising as the 'spirit' of the law (preventing shelters) is increasingly overshadowed by the 'letter' (permissive interpretation) used for strategic tax planning. Accessibility collapse is low (0.20) because the rules are complex but navigable for those with resources; resistance is low (0.10) because the beneficiaries are powerful and the victims are diffuse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-net-worth investors and their advisors, this reading of material participation is a legitimate application of tax law, enabling prudent financial planning and incentivizing real estate investment. From the perspective of general taxpayers and the IRS, it represents a loophole that undermines the intent of the law and shifts the tax burden unfairly. The engine's classification will reflect this divergence, likely showing a 'tangled_rope' or 'snare' for victims and a 'rope' or 'scaffold' for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   High-net-worth investors are clear beneficiaries, directly reducing their tax burden. Tax advisors also benefit by providing the expertise to navigate these complex rules. General taxpayers and IRS enforcement capacity are victims, bearing the cost of reduced tax revenue and increased administrative burden, respectively. Congressional tax committees act as observers, with the power to alter the rules but facing political constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_of_hour_counting,
    'To what extent are the ''hours worked'' claimed for material participation genuinely verifiable and reflective of substantial personal labor, rather than administrative tasks or aggregated time?',
    'IRS audits with enhanced scrutiny and clearer, objective guidelines for what constitutes ''material participation'' hours, potentially requiring third-party verification or specific activity logs.',
    'If hours are found to be largely unverifiable or inflated, the effective extractiveness of this reading would be reclassified upward (as a more pure ''snare''), and the ''theater_ratio'' would increase, indicating the performative nature of compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verifiability_of_hour_counting, empirical, 'Ambiguity in what counts as ''material participation'' hours.').

omega_variable(
    legislative_intent_vs_judicial_interpretation,
    'Does this permissive reading of material participation align with the original legislative intent of IRC 469, or has judicial and administrative interpretation drifted to create unintended loopholes?',
    'Congressional review and legislative clarification of IRC 469, explicitly defining ''material participation'' to close perceived loopholes or reaffirming current interpretations.',
    'If found to be a drift from original intent, it would strengthen arguments for legislative reform, potentially reclassifying the constraint towards a ''snare'' for beneficiaries if the ''coordination'' (original intent) is deemed lost. If reaffirmed, it would solidify its ''tangled_rope'' status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_vs_judicial_interpretation, conceptual, 'Gap between original legislative intent and current interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.2).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.3).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1996, 0.5).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1986, 0.2).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1996, 0.25).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2006, 0.28).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2016, 0.29).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'strategic_shelter_reading' of the 'irc_469_material_participation_kernel'. It is linked to the 'strict_gatekeeper_reading' as a sibling interpretation of the same core tax law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
