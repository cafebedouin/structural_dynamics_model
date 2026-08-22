% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: IRC §469 Material Participation Threshold — Strategic Shelter Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This story instantiates the strategic_shelter_reading of the IRC §469
 *   material participation kernel: the temporary regulations' seven
 *   alternative tests, the 750-hour/50%-of-services real estate professional
 *   exception, and the grouping elections under 1.469-4 are read as a
 *   permissive threshold that a well-advised taxpayer can reliably clear
 *   through hour-counting and election structuring, largely independent of
 *   the taxpayer's actual economic involvement in the activity. Under this
 *   reading, the regulation still performs a real coordination function
 *   (separating some genuine operators from passive investors, and giving
 *   courts and preparers a common vocabulary), but is also actively used as a
 *   wealth-preservation mechanism by parties with the resources to engineer
 *   qualification. The sibling reading — strict_gatekeeper_reading — treats
 *   the same text as requiring verifiable, substantial personal labor with a
 *   high documentation bar; it is a separate constraint story with its own
 *   epsilon, not a different measurement of this one. The two readings
 *   diverge on how much interpretive slack the temporary regulations'
 *   language actually contains, and on how the IRS's persistent
 *   under-enforcement should be read: as evidence the threshold is genuinely
 *   low-friction (this reading) or as evidence of resource-starved
 *   enforcement failing to hold a properly strict standard (the sibling
 *   reading).
 *
 * KEY AGENTS:
 *   - high_income_real_estate_professionals: primary beneficiary (powerful/arbitrage) — converts paper hours into active-loss tax treatment
 *   - tax_advisory_industry: secondary beneficiary (organized/arbitrage) — sells qualification engineering as a repeatable service
 *   - wage_earning_taxpayers: diffuse payer (powerless/trapped) — bears aggregate foregone-revenue cost with no comparable shelter
 *   - irs_regulatory_drafters: agenda_setter (institutional/constrained) — could tighten the temporary regulations but has left them largely unchanged since 1988
 *   - tax_court: analytical observer (institutional/analytical) — adjudicates individual disputes without resolving the systemic permissiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.61).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.28).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC §469 Material Participation Threshold — Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'e2c24122-26b8-458b-86f3-3d8a80f66b37').
narrative_ontology:cs_kernel_codification('e2c24122-26b8-458b-86f3-3d8a80f66b37', formalized).
narrative_ontology:cs_authority_grounding('e2c24122-26b8-458b-86f3-3d8a80f66b37', extraction).
narrative_ontology:cs_interpretation_layer_present('e2c24122-26b8-458b-86f3-3d8a80f66b37').
narrative_ontology:cs_reading_relation('e2c24122-26b8-458b-86f3-3d8a80f66b37', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('e2c24122-26b8-458b-86f3-3d8a80f66b37', foundational, documented_hours_constitute_participation).
narrative_ontology:cs_axiom_status(documented_hours_constitute_participation, holdable).
narrative_ontology:cs_axiom_grounding('e2c24122-26b8-458b-86f3-3d8a80f66b37', documented_hours_constitute_participation, conventional).
narrative_ontology:cs_axiom('e2c24122-26b8-458b-86f3-3d8a80f66b37', secondary, grouping_election_flexibility_is_taxpayer_right).
narrative_ontology:cs_axiom_status(grouping_election_flexibility_is_taxpayer_right, holdable).
narrative_ontology:cs_axiom_grounding('e2c24122-26b8-458b-86f3-3d8a80f66b37', grouping_election_flexibility_is_taxpayer_right, conventional).
narrative_ontology:cs_reference_frame('e2c24122-26b8-458b-86f3-3d8a80f66b37', id_1986_tax_reform_shelter_closure_intent).
narrative_ontology:cs_drift_state('e2c24122-26b8-458b-86f3-3d8a80f66b37', post_temporary_regulation_settling, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2c24122-26b8-458b-86f3-3d8a80f66b37', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, wealthy_passive_loss_shelter_users).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement_budget).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, non_real_estate_small_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structures a spouse's or their own hours to clear the 750-hour and >50%-of-personal-services thresholds under Treas. Reg. 1.469-5T, often via aggregation elections across multiple rental properties. Uses generous hour-counting categories (planning, phone calls, review of statements, travel time) that are difficult for the IRS to verify after the fact. Converts what would otherwise be passive losses into fully deductible ordinary losses against W-2 or business income, materially reducing tax liability year over year.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals, agenda_setter).

% Builds contemporaneous log templates, grouping-election memoranda, and audit-defense packages specifically calibrated to the permissive reading of the seven alternative tests. Markets material participation qualification as a repeatable, engineerable outcome rather than a factual finding. Collects recurring advisory fees whose value depends on the threshold remaining loosely verifiable.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Holds real estate as a wealth-preservation vehicle where the true economic activity is close to passive but the paper record is constructed to meet the letter of the hour tests. Benefits from real estate professional status stacking with cost segregation and bonus depreciation to generate large paper losses that offset unrelated ordinary income.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wealthy_passive_loss_shelter_users, beneficiary,
    powerful, generational, arbitrage, national).

% Has W-2 income subject to full withholding and no comparable mechanism to convert passive activity into active-loss offsets; bears a comparatively higher effective tax rate and, in aggregate, an implicit share of the federal revenue foregone through widespread aggressive material-participation claims. Has no realistic exit from the general income tax base to access the same shelter.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers, payer,
    powerless, biographical, trapped, national).

% Bears the diffuse cost of the permissive threshold: audits of material-participation claims require reconstructing hour logs years after the fact, a fact-intensive and resource-heavy inquiry the agency is chronically underfunded to pursue at scale, so most aggressive claims are never examined.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement_budget, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(irc_469_material_participation_kernel__strategic_shelter_reading, irs_enforcement_budget).

% Operates a materially participating trade or business without the real-estate-specific safe harbors and aggregation elections available under the rental real estate rules, and so faces a comparatively less permissive path to the same active-loss tax treatment despite similar or greater actual labor input.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, non_real_estate_small_business_owners, payer,
    moderate, biographical, constrained, national).

% Wrote and periodically could amend Treas. Reg. 1.469-5T's seven alternative tests and the grouping-election rules under 1.469-4. Retains authority to tighten documentation requirements or narrow the hour-counting categories but has left the temporary regulations largely unchanged since 1988, allowing interpretive practice to settle into the permissive reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_regulatory_drafters, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates individual material-participation disputes on the specific facts presented, producing a body of case law that alternately validates and rejects taxpayer hour logs, but cannot by itself resolve the systemic permissiveness of the regulatory text.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, administrable line between active business involvement and passive investment so that passive-loss limitation rules (enacted in 1986 to curb abusive tax shelters) have a concrete test to apply, instead of requiring case-by-case judicial inquiry into every taxpayer's subjective involvement.
% TRANSFER_FUNCTION: Moves federal tax revenue from the general fund to taxpayers who can document threshold-clearing hours and elect favorable groupings, converting what would be passive-loss-limited deductions into fully deductible active losses against unrelated ordinary income.
% ABSENT_VOICES: Wage-earning taxpayers who fund the general revenue base have no seat in the regulatory or advisory process shaping how permissively the hour tests are interpreted; their aggregate subsidization of foregone revenue is never priced into the regulation's cost-benefit framing, which is dominated by real estate industry commentary during rulemaking.
% DISAPPEARANCE_RATIONALE: If the permissive hour-counting and grouping-election apparatus vanished and a strict, verifiable-labor standard replaced it overnight, a substantial share of current real-estate-professional claimants would lose active-loss treatment, passive losses would suspend rather than offset ordinary income, tax advisory practices built around qualification engineering would lose a core service line, and reported taxable income among high-net-worth real estate investors would rise materially in the following filing year.
% FOUNDING_PROBLEM: Congress enacted passive activity loss limitations in the Tax Reform Act of 1986 to stop wealthy taxpayers from using paper losses from activities they were not genuinely involved in (tax shelters) to offset salary and portfolio income; material participation was the dividing line meant to separate genuine operators from passive investors.
% FOUNDING_PROBLEM_CORROBORATION: IRS Large Business & International division reports and GAO audits of passive activity loss compliance (external to the beneficiary set) have found high rates of unsubstantiated or after-the-fact-reconstructed hour logs among real estate professional claimants, corroborating that the threshold is being cleared on paper rather than in substance for a meaningful share of claimants; tax practitioners and industry associations, who benefit from the permissive reading, are not treated as corroborating this status independently.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a substantial but not extreme 0.61: the threshold does gate out taxpayers who plainly do nothing (zero-hour absentee owners still fail even permissive tests), so it is not pure extraction, but the seven-test structure and grouping elections give sophisticated taxpayers wide latitude to construct qualifying records for activity that is economically close to passive. Suppression is comparatively low and rises only slowly (0.22 to 0.28) because there is little active coercion suppressing alternatives — the mechanism works through permissive interpretation and weak after-the-fact verification, not through blocking exits. Theater ratio rises substantially over the interval (0.25 to 0.55) because a growing share of the compliance apparatus — contemporaneous log templates, calendar reconstructions, advisory memoranda — exists to survive an audit that statistically will rarely occur, rather than to reflect genuine operational involvement; this is the Goodhart-drift signature of a threshold whose paper trail has decoupled from the activity it purports to measure.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (IRS regulatory drafters), the regulation looks like a stable, decades-old coordination mechanism performing its intended gatekeeping function adequately. From the payer seats (wage earners, the IRS enforcement budget itself), the same text computes as a widening extraction channel because verification costs have not kept pace with the sophistication of qualification engineering. The engine should show this divergence directly from the structural data — power, exit options, and beneficiary/victim declarations — without either seat's story being privileged in the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income real estate professionals and the tax advisory industry sit near the full-beneficiary end: they collect the tax benefit or the fee stream, and their exit options (arbitrage across entity structures, grouping elections, professional status elections) are the mechanism of benefit itself. Wage-earning taxpayers sit near the full-target end: they cannot access an equivalent shelter, are trapped in the general income tax base, and bear a diffuse share of the foregone revenue through the overall fiscal gap. Non-real-estate small business owners are directionally closer to targets than beneficiaries despite comparable or greater actual labor input, because the real-estate-specific safe harbors are not available to them — this is a same-power-atom divergence worth noting: two moderate-power actors (a real estate professional and a non-real-estate small business owner) experience structurally different exit options under the identical statutory provision.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1986 founding problem (curbing tax shelters built on illusory losses from activities investors were not genuinely involved in) remains partially live — genuine passive shelters still exist and the rule still screens some of them out — which is why founding_problem_status is authored as contested rather than dead. But the mechanism designed to solve that problem has itself become a template for a adjacent, narrower form of the same abuse: paper-documented 'participation' substituting for genuine operational involvement. Classifying this as tangled_rope rather than snare preserves the real coordination function (a workable administrable line does exist and does bind some claimants) while still naming the asymmetric extraction that rides on the same structure — collapsing it to snare would erase the genuine gatekeeping the regulation still performs for the clearest passive-investor cases; collapsing it to rope would erase the documented, corroborated pattern of after-the-fact reconstructed logs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hour_log_verifiability_ambiguity,
    'Is the 750-hour/material-participation threshold genuinely low-friction to satisfy honestly, or does its apparent permissiveness derive from the practical unverifiability of after-the-fact hour reconstruction rather than from the regulatory text itself being lax?',
    'Compare audit outcomes where contemporaneous, third-party-corroborated time records exist against outcomes where records were reconstructed after the fact; a large gap in sustained-deduction rates would indicate the permissiveness is a verification artifact rather than a textual one.',
    'If permissiveness is primarily a verification-capacity problem, the fix is enforcement funding and documentation mandates, not textual amendment — favoring convergence toward the strict_gatekeeper_reading through practice rather than rulemaking. If the text itself is genuinely loose, only regulatory amendment resolves the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_log_verifiability_ambiguity, empirical, 'Whether permissiveness is a text problem or a verification-capacity problem.').

omega_variable(
    kernel_reading_divergence_locus,
    'Where exactly does the strategic_shelter_reading and the strict_gatekeeper_reading diverge — in what counts as a qualifying ''hour,'' in how grouping elections may combine activities, or in the evidentiary weight given to taxpayer-prepared logs absent contemporaneous corroboration?',
    'Systematic review of Tax Court opinions on material participation disputes, coded for which specific sub-issue (hour category, grouping validity, log credibility) drove the outcome in each case, to locate the load-bearing interpretive fork between readings.',
    'If the divergence is concentrated in log-credibility standards rather than the hour categories or grouping rules themselves, the two readings could converge under a documentation-standard reform without any change to the substantive hour thresholds — collapsing much of the ε gap between this story and its sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence_locus, conceptual, 'Locating the precise structural element on which the sibling readings diverge.').

omega_variable(
    real_estate_carveout_naturalness,
    'Is the real estate professional exception''s more favorable treatment (relative to other passive-activity businesses) a considered policy judgment about the sector''s economics, or a product of concentrated real estate industry influence during the 1986 and 1993 rulemaking processes?',
    'Review of legislative and rulemaking history (committee testimony, comment letters) for the 1993 real estate professional exception to identify which interest groups shaped the final threshold language.',
    'If industry-shaped, the carve-out is better understood as captured rulemaking rather than principled sectoral distinction, strengthening the case that non_real_estate_small_business_owners are victims of an arbitrary asymmetry rather than a justified one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_estate_carveout_naturalness, conceptual, 'Whether the real estate sector''s favorable carve-out reflects principled policy or industry capture of rulemaking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1988, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1988, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1988, 0.25).
narrative_ontology:measurement(irc__tr_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1996, 0.33).
narrative_ontology:measurement(irc__tr_t2004, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(irc__tr_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2012, 0.46).
narrative_ontology:measurement(irc__tr_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2018, 0.51).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(irc__be_t1988, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1988, 0.32).
narrative_ontology:measurement(irc__be_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1996, 0.4).
narrative_ontology:measurement(irc__be_t2004, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(irc__be_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2012, 0.54).
narrative_ontology:measurement(irc__be_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1988, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1988, 0.22).
narrative_ontology:measurement(irc__su_t1996, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1996, 0.24).
narrative_ontology:measurement(irc__su_t2004, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2004, 0.25).
narrative_ontology:measurement(irc__su_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2012, 0.26).
narrative_ontology:measurement(irc__su_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2018, 0.27).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, bonus_depreciation_cost_segregation_shelter).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, qualified_business_income_deduction_real_estate_carveout).

% DUAL FORMULATION NOTE:
% This story and irc_469_material_participation_kernel__strict_gatekeeper_reading decompose the single natural-language concept 'material participation standard' into two structurally distinct constraints per the ε-invariance principle. This story (strategic_shelter_reading) authors ε=0.61 against a broad, permissively-satisfied qualifying population; the sibling authors a lower ε against a narrow population meeting a high documentation bar. They share the same statutory and regulatory text (IRC §469, Treas. Reg. 1.469-5T, 1.469-4) but diverge on how much genuine interpretive slack that text contains — this is a reading-indexed ε difference, not a measurement-basis difference, and both stories retain a single stable ε under their own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
