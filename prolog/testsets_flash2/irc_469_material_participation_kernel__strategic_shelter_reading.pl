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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation (Strategic Shelter Reading)
 *   domain: tax_law/real_estate_investment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of IRC Section 469, where
 *   'material participation' is interpreted permissively, allowing
 *   high-net-worth investors to qualify through aggressive hour-counting and
 *   grouping elections. This enables them to deduct passive losses against
 *   active income, effectively creating a tax shelter. The constraint is
 *   claimed as a Tangled Rope, reflecting its dual function of legitimate
 *   coordination (preventing passive loss abuse) and asymmetric extraction
 *   (enabling strategic tax avoidance). This story is one reading of the
 *   'irc_469_material_participation_kernel' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.25).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '6d6081c9-1eb0-4e83-a524-2455efaba434').
narrative_ontology:cs_kernel_codification('6d6081c9-1eb0-4e83-a524-2455efaba434', fixed_text).
narrative_ontology:cs_authority_grounding('6d6081c9-1eb0-4e83-a524-2455efaba434', lineage).
narrative_ontology:cs_interpretation_layer_present('6d6081c9-1eb0-4e83-a524-2455efaba434').
narrative_ontology:cs_reading_relation('6d6081c9-1eb0-4e83-a524-2455efaba434', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('6d6081c9-1eb0-4e83-a524-2455efaba434', foundational, tax_minimization_is_legitimate_goal).
narrative_ontology:cs_axiom_status(tax_minimization_is_legitimate_goal, holdable).
narrative_ontology:cs_axiom_grounding('6d6081c9-1eb0-4e83-a524-2455efaba434', tax_minimization_is_legitimate_goal, conventional).
narrative_ontology:cs_axiom('6d6081c9-1eb0-4e83-a524-2455efaba434', secondary, substance_over_form_in_participation).
narrative_ontology:cs_axiom_status(substance_over_form_in_participation, holdable).
narrative_ontology:cs_axiom_grounding('6d6081c9-1eb0-4e83-a524-2455efaba434', substance_over_form_in_participation, conventional).
narrative_ontology:cs_reference_frame('6d6081c9-1eb0-4e83-a524-2455efaba434', flexible_business_engagement_recognition).
narrative_ontology:cs_drift_state('6d6081c9-1eb0-4e83-a524-2455efaba434', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d6081c9-1eb0-4e83-a524-2455efaba434', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These investors actively seek to qualify for material participation through aggressive hour-counting and grouping elections, enabling them to deduct passive losses against active income, thereby reducing their overall tax liability and preserving wealth.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors, beneficiary,
    powerful, generational, arbitrage, national).

% Professionals who interpret and apply IRC Section 469, advising clients on strategies to meet the material participation threshold. They benefit from the complexity and ambiguity of the rules, which creates demand for their specialized services in tax planning and compliance.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, agenda_setter,
    organized, biographical, mobile, national).

% Bear the indirect cost of reduced tax revenue due to passive loss deductions by high-net-worth individuals. They do not engage in complex tax planning and are subject to higher effective tax rates as a result of the system's permissiveness for others.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, trapped, national).

% Tasked with enforcing tax law, but faces significant challenges in auditing and disallowing material participation claims due to the subjective nature of hour-counting and the burden of proof. This leads to high administrative costs and limited success in challenging aggressive interpretations.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement, payer,
    institutional, generational, constrained, national).

% Responsible for drafting and amending tax law. They observe the outcomes of IRC 469's application, balancing revenue needs with incentives for investment, but often face lobbying pressure from beneficiaries of the current interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congressional_legislators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing active business income from passive investment income, aiming to prevent taxpayers from sheltering active income with passive losses, while also allowing legitimate business owners to deduct losses.
% TRANSFER_FUNCTION: Facilitates the transfer of tax burden from high-net-worth investors (who can strategically qualify for material participation) to general taxpayers, by enabling the deduction of passive losses against active income.
% ABSENT_VOICES: Advocacy groups for tax fairness and simplified tax codes, who would argue for clearer, more objective material participation rules to prevent strategic sheltering and reduce the burden on the IRS. Their voices are often drowned out by well-funded lobbying efforts.
% DISAPPEARANCE_RATIONALE: If this permissive reading of material participation vanished, high-net-worth investors would lose a significant tax shelter, leading to a substantial increase in their tax liabilities. This would likely trigger a re-evaluation of investment strategies, potentially shifting capital away from real estate or towards other tax-advantaged assets, and significantly increasing tax revenue for the government.
% FOUNDING_PROBLEM: The original intent of IRC 469 was to prevent wealthy individuals from using passive investment losses (e.g., from real estate) to offset active income (e.g., salaries), thereby reducing their tax burden unfairly.
% FOUNDING_PROBLEM_CORROBORATION: High-net-worth investors and their tax advisors argue that the current interpretation allows for legitimate business activity to be recognized. However, general taxpayers and IRS enforcement officials, supported by independent tax policy analysts, contend that the permissive interpretation has largely undermined the original intent, turning it into a mechanism for strategic tax avoidance rather than a gatekeeper against passive loss abuse.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high because the permissive interpretation allows significant tax benefits for a specific group, shifting the tax burden. Suppression (0.25) is relatively low for beneficiaries, as the rules are flexible enough to be navigated, but higher for IRS enforcement due to the difficulty of challenging claims. The theater ratio (0.45) is moderate, as the 'participation' often involves minimal, strategically documented effort rather than substantial operational engagement, creating a performance of compliance. Accessibility collapse is low (0.30) for those with resources to exploit the rules, but high for general taxpayers who lack such options. Resistance is low (0.10) from beneficiaries, who benefit from the status quo, and from victims, who face high costs of challenging the system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-net-worth investors and tax advisors, this reading of material participation is a legitimate mechanism for recognizing active business engagement and optimizing tax outcomes. From the perspective of general taxpayers and IRS enforcement, it functions as a loophole that enables significant tax avoidance, undermining the fairness and revenue-generating capacity of the tax system.
 *
 * DIRECTIONALITY LOGIC:
 *   High-net-worth investors are clear beneficiaries (low d) as they directly reduce their tax burden. Tax advisors are agenda-setters and beneficiaries, profiting from the complexity. General taxpayers are victims (high d) as they indirectly subsidize the tax benefits of others. IRS enforcement is also a victim (high d) due to the high cost and low success rate of challenging these claims, effectively bearing the cost of the system's permissiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent passive loss abuse. This reading, however, has drifted to enable strategic sheltering, suggesting a form of mandatrophy where the original coordination function is overshadowed by an extractive one. The classification as Tangled Rope captures this hybrid nature, preventing mislabeling it as pure coordination (Rope) or pure extraction (Snare), as it still retains a nominal coordination function while facilitating significant asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_of_hour_counting,
    'To what extent are the reported hours for material participation verifiable and reflective of actual, substantial engagement?',
    'Detailed, independent audits of a statistically significant sample of material participation claims, focusing on objective evidence of activity rather than self-reported hours.',
    'If hours are found to be largely unverifiable or inflated, it would strengthen the argument that the ''participation'' is theatrical, increasing the constraint''s effective extractiveness and theater ratio, pushing it closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verifiability_of_hour_counting, empirical, 'Ambiguity regarding the objective verifiability of reported material participation hours.').

omega_variable(
    legislative_intent_drift,
    'Has the judicial and administrative interpretation of ''material participation'' drifted significantly from the original legislative intent of IRC 469?',
    'Historical legal analysis comparing legislative history and early interpretations with contemporary case law and IRS guidance, alongside expert testimony from tax law scholars.',
    'If significant drift is confirmed, it would highlight the constraint''s evolution from a coordination mechanism to an extractive one, supporting a reclassification towards Snare or a more extractive Tangled Rope. If intent is found to be consistent, it would support the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_drift, conceptual, 'Whether the current interpretation aligns with the original legislative purpose of preventing passive loss abuse.').

omega_variable(
    economic_impact_on_tax_revenue,
    'What is the quantifiable impact of this permissive reading on federal tax revenue and the overall tax burden distribution?',
    'Comprehensive economic modeling and data analysis by non-partisan government agencies (e.g., Congressional Budget Office, Joint Committee on Taxation) or independent academic researchers.',
    'A high quantifiable revenue loss would underscore the extractive nature of the constraint, providing empirical basis for policy changes aimed at tightening material participation rules or reallocating tax burdens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_impact_on_tax_revenue, empirical, 'The fiscal impact of strategic material participation on public finances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.3).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2016, 0.42).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1996, 0.55).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(irc__be_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1986, 0.15).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1996, 0.2).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2006, 0.23).
narrative_ontology:measurement(irc__su_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2016, 0.24).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
