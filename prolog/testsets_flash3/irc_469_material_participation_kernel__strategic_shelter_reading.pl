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
 *   This constraint represents a permissive interpretation of IRC Section
 *   469, which defines 'material participation' for passive activity loss
 *   rules. Under this reading, material participation is a threshold easily
 *   achievable through aggressive hour-counting and strategic grouping
 *   elections, primarily benefiting high-net-worth investors by allowing them
 *   to deduct passive losses against active income. This reading enables
 *   systematic tax sheltering, shifting the tax burden to general taxpayers.
 *   The claimed type is 'tangled_rope' because it still performs a nominal
 *   coordination function (defining participation) but is heavily skewed
 *   towards extraction for specific beneficiaries.
 *
 * KEY AGENTS:
 *   - high_net_worth_investors: Primary beneficiary (powerful/arbitrage) — leverages permissive rules for tax benefits.
 *   - tax_advisors: Agenda-setter (organized/mobile) — interprets and applies rules to clients' benefit.
 *   - general_taxpayers: Primary payer (powerless/trapped) — bears indirect costs of reduced tax revenue.
 *   - irs_enforcement: Payer (institutional/constrained) — struggles with verification and enforcement.
 *   - congressional_legislators: Observer (institutional/analytical) — could change the rules but face lobbying pressure.
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
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '62b5ce0a-4b95-4cd0-8c95-0be7487c1da4').
narrative_ontology:cs_kernel_codification('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', formalized).
narrative_ontology:cs_authority_grounding('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', lineage).
narrative_ontology:cs_interpretation_layer_present('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4').
narrative_ontology:cs_reading_relation('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', foundational, flexible_participation_definition).
narrative_ontology:cs_axiom_status(flexible_participation_definition, holdable).
narrative_ontology:cs_axiom_grounding('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', flexible_participation_definition, conventional).
narrative_ontology:cs_axiom('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', secondary, investor_tax_minimization_is_legitimate).
narrative_ontology:cs_axiom_status(investor_tax_minimization_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', investor_tax_minimization_is_legitimate, instrumental).
narrative_ontology:cs_reference_frame('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', taxpayer_friendly_interpretation).
narrative_ontology:cs_drift_state('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62b5ce0a-4b95-4cd0-8c95-0be7487c1da4', '').
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

% Specialized tax attorneys and accountants who interpret IRC Section 469 permissively, advising clients on strategies to meet the material participation threshold. They benefit from the complexity and ambiguity of the rules, charging fees for their expertise in structuring these arrangements.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, agenda_setter,
    organized, biographical, mobile, national).

% Bear the indirect cost of reduced tax revenue due to passive loss deductions by high-net-worth individuals. They have no direct mechanism to influence the interpretation or enforcement of these rules and cannot access similar tax benefits.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, trapped, national).

% Responsible for auditing and enforcing tax compliance, but face significant challenges in verifying the subjective 'hours of participation' claims. The permissive interpretation increases their administrative burden and reduces the effectiveness of their enforcement efforts, leading to a diffuse loss of revenue.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement, payer,
    institutional, generational, constrained, national).

% Oversee the tax code and could amend IRC Section 469 to clarify material participation rules or close loopholes. They are subject to lobbying from various interest groups, including those benefiting from the current permissive interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congressional_legislators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing active business income from passive investment income, aiming to prevent tax shelters by limiting passive loss deductions. This reading coordinates tax planning for investors and advisors.
% TRANSFER_FUNCTION: Facilitates the transfer of tax burden from high-net-worth investors (who can strategically qualify for material participation) to general taxpayers (who cannot access these deductions), by allowing passive losses to offset active income.
% ABSENT_VOICES: Advocates for tax equity and simplified tax codes are often marginalized in the technical debates around tax law interpretation. They would argue for a stricter, more objective definition of material participation to prevent wealth preservation strategies that shift tax burdens.
% DISAPPEARANCE_RATIONALE: If this permissive reading of material participation vanished, high-net-worth investors would lose a significant tax planning tool, leading to higher tax liabilities for them and potentially increased tax revenue for the government. Investment strategies in real estate and other passive activities would need to be fundamentally re-evaluated, causing a substantial rearrangement in tax and investment markets.
% FOUNDING_PROBLEM: The original intent of IRC Section 469 was to prevent taxpayers from sheltering active income with losses from passive activities, particularly those with little economic substance, by creating a clear distinction between active and passive participation.
% FOUNDING_PROBLEM_CORROBORATION: High-net-worth investors and their tax advisors argue the problem is live, requiring flexible rules to incentivize legitimate business activities. General taxpayers and some IRS officials contend the problem of tax sheltering persists, but the current permissive interpretation undermines the original intent, effectively making the founding problem 'dead' in practice for those who can exploit the rules. Independent tax policy analysts corroborate that the permissive interpretation has created new avenues for sheltering, shifting the burden.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) is high because the permissive interpretation allows significant tax savings for a specific group, effectively transferring wealth. Suppression (0.25) is relatively low because the constraint's persistence relies more on the complexity and ambiguity of the rules, and the difficulty of IRS verification, rather than overt coercion. Theater ratio (0.45) is moderate, reflecting that while the rules nominally aim to prevent tax shelters, a substantial portion of their application involves structuring activities to meet the permissive interpretation rather than genuinely active participation. Accessibility collapse (0.30) is low because alternatives (stricter interpretations, legislative changes) are conceptually available but politically difficult to achieve. Resistance (0.10) is low from beneficiaries, but diffuse from general taxpayers and IRS, who lack concentrated power to challenge it.
 *
 * PERSPECTIVAL GAP:
 *   High-net-worth investors and tax advisors perceive this as a legitimate application of tax law, enabling wealth management and investment. General taxpayers and IRS enforcement view it as a loophole that undermines tax fairness and the integrity of the tax system. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a 'rope' or 'scaffold' (coordination/support) and payers experiencing it as a 'snare' (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   High-net-worth investors are clear beneficiaries (d=0.0-0.1) as the constraint directly reduces their tax burden. Tax advisors are also beneficiaries (d=0.1-0.2) as they profit from structuring these arrangements. General taxpayers are targets (d=0.9-1.0) as they indirectly subsidize the tax benefits. IRS enforcement is also a target (d=0.8-0.9) as the permissive interpretation complicates their mission and reduces their effectiveness. Congressional legislators are observers (d=0.5) as they can change the rules but are not directly impacted by this specific interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by acknowledging the original intent of IRC 469 to coordinate tax treatment. However, it highlights how a permissive interpretation has allowed the coordination function to be co-opted for extraction. The constraint's mandate (preventing tax shelters) has been significantly eroded by this reading, leading to a form of mandatrophy where the structure persists but serves a different, more extractive function than intended. The 'contested' status of the founding problem corroborates this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_of_participation_hours,
    'To what extent are the reported ''hours of participation'' verifiable by the IRS, and how much of the claimed participation is genuinely substantial vs. strategically aggregated?',
    'Detailed IRS audits with stricter evidentiary requirements for time tracking, or legislative clarification of what constitutes ''substantial'' participation.',
    'If hours are largely unverifiable or strategically aggregated, it would confirm the ''theater'' aspect of the constraint and strengthen the argument for its extractive nature, potentially leading to reclassification towards a ''snare'' for the IRS and general taxpayers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verifiability_of_participation_hours, empirical, 'Ambiguity in verifying claimed material participation hours.').

omega_variable(
    legislative_intent_vs_judicial_interpretation,
    'Does the current permissive interpretation align with the original legislative intent of IRC Section 469, or has judicial and administrative interpretation drifted significantly?',
    'Historical analysis of legislative records, committee reports, and early case law compared against current rulings and IRS guidance.',
    'A significant divergence would highlight a ''codification_collapse'' or ''practice_drift'' in the cs_structure, indicating that the constraint''s operation has moved away from its foundational grounding, supporting a reclassification towards ''snare'' or ''piton'' if the original problem is no longer addressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_vs_judicial_interpretation, conceptual, 'Gap between original legislative intent and current interpretation.').

omega_variable(
    economic_impact_of_passive_loss_deductions,
    'What is the aggregate economic impact of passive loss deductions enabled by this permissive reading on federal tax revenue and income inequality?',
    'Comprehensive economic modeling and data analysis by non-partisan government agencies (e.g., Congressional Budget Office) or academic researchers.',
    'Quantifying a substantial revenue loss and disproportionate benefit to high-income earners would provide strong empirical evidence for the ''extraction'' component, increasing pressure for legislative reform and strengthening the ''snare'' classification from the perspective of general taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_passive_loss_deductions, empirical, 'Quantifying the fiscal and distributional effects of the permissive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.25).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(irc__tr_t2016, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2016, 0.4).
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
