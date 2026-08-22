% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Material Participation as Permissive Threshold (Strategic Shelter Reading)
 *   domain: economic/legal/regulatory
 *
 * SUMMARY:
 *   Under the strategic shelter reading, the material-participation tests of
 *   IRC Section 469(c) operate as permissive thresholds: a taxpayer qualifies
 *   by assembling hour counts that reach the stated numbers and by filing
 *   grouping elections that consolidate activities, with the burden
 *   effectively resting on the government to disprove what the file asserts.
 *   The standing arrangement under contest - and the referent for epsilon -
 *   is this operative permissive-administration regime as actually practiced:
 *   professionally engineered qualification, tolerated administration, and
 *   systematic conversion of suspended passive losses into current
 *   deductions. The reading preserves a real coordination core (an
 *   administrable line between active and passive) while transferring revenue
 *   asymmetrically to a concentrated, well-counseled population. KEY AGENTS
 *   (by structural relationship): - high_income_real_estate_investors:
 *   Primary beneficiary (powerful/mobile) - captures the deduction net of
 *   fees - tax_advisory_industry: Secondary beneficiary and co-agenda-setter
 *   (organized/mobile) - sells the qualification machinery -
 *   small_operating_landlords: Incidental beneficiaries
 *   (moderate/constrained) - genuine relief traveling inside the engineered
 *   population - us_federal_treasury: Payer-administrator
 *   (institutional/constrained) - bears the revenue cost, sets the audit
 *   posture - wage_earning_taxpayers: Diffuse payers (powerless/trapped) -
 *   future_taxpayers: Excluded voice (powerless/trapped) - inherit the
 *   borrowed share - tax_policy_analysts: Analytical observer - sees the full
 *   distributional structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.71).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.57).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "Material Participation as Permissive Threshold (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "economic/legal/regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'b362f6ad-0a3c-4d6d-8e16-eaba91dd2392').
narrative_ontology:cs_kernel_codification('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', fixed_text).
narrative_ontology:cs_authority_grounding('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', practice).
narrative_ontology:cs_interpretation_layer_present('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392').
narrative_ontology:cs_reading_relation('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', foundational, statutory_hour_tests_are_self_executing_floors).
narrative_ontology:cs_axiom_status(statutory_hour_tests_are_self_executing_floors, holdable).
narrative_ontology:cs_axiom_grounding('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', statutory_hour_tests_are_self_executing_floors, conventional).
narrative_ontology:cs_axiom('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', foundational, taxpayer_grouping_elections_bind_government_recharacterization).
narrative_ontology:cs_axiom_status(taxpayer_grouping_elections_bind_government_recharacterization, holdable).
narrative_ontology:cs_axiom_grounding('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', taxpayer_grouping_elections_bind_government_recharacterization, conventional).
narrative_ontology:cs_reference_frame('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', permissive_threshold_framework).
narrative_ontology:cs_drift_state('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', contemporary_post_2018_grouping_reg_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('b362f6ad-0a3c-4d6d-8e16-eaba91dd2392', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, small_operating_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, us_federal_treasury).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, small_operating_landlords).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, formalist_statutory_construction).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, administrability_deference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own rental and business real estate generating large paper losses through depreciation, cost segregation, and accelerated write-offs. Qualify as material participants by assembling hour counts and grouping elections with counsel, converting otherwise suspended losses into current deductions against salary and investment income. Exit is easy in form: restructure entities, shift income character, time elections, or move activity to more favorable jurisdictions.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors, beneficiary,
    powerful, generational, mobile, national).

% CPAs, tax attorneys, and specialty firms design participation documentation, grouping-election strategy, and audit-defense files for fees. They publish planning guides, dominate comment letters during rulemaking, and effectively co-author the operative interpretation through accumulated practice. Their revenue depends on continued demand for participation engineering under whichever reading prevails.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, agenda_setter).

% Owner-operators of one or a few rentals who perform real management work but keep imperfect records. The permissive threshold lets them qualify without contemporaneous time logs. They also pay preparer and advisor fees, and bear audit exposure when their reconstructed counts are challenged.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, small_operating_landlords, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, small_operating_landlords, payer).

% Writes the regulations, selects audit targets, and litigates challenges. Formally committed to policing loss-character gaming, it administers a regime in which most engineered participation goes unexamined for budget reasons. Bears the revenue cost of deducted passive losses and spends scarce enforcement resources defending the line case by case.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, us_federal_treasury, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, us_federal_treasury, agenda_setter).

% Salaried and portfolio-income households without passive losses. They cannot deduct comparable losses and finance the difference through somewhat higher rates, narrower public services, or deficit carry. There is no practical exit from the tax base.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers, payer,
    powerless, biographical, trapped, national).

% Not yet born or not yet filing; inherit the debt-financed share of today's deductions. Present in no rulemaking docket and represented by no counsel in the interpretive settlement.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Academic and governmental scorekeepers - joint tax committees, budget offices, tax faculties - who measure who qualifies, what the deductions cost, and how the case law moves. Hold no stake in outcomes; publish the distributional record the other seats argue over.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies an administrable, predictable line between active and passive participation so that millions of property owners, return preparers, and examiners can classify losses without open-ended factual inquiry; grouping elections cut compliance cost for owners of multiple related activities.
% TRANSFER_FUNCTION: Moves current deduction capacity - roughly the present value of deferred tax revenue - from the federal fisc to households holding passive real estate losses, with an advisory-fee toll taken along the way; part of the cost is borrowed and carried by future taxpayers.
% ABSENT_VOICES: Strict-substantiation advocates inside the Treasury's interpretive offices, unrepresented future taxpayers carrying the deficit share, and middle-income filers without counsel to engineer participation are absent from the operative settlement; the advisory profession speaks loudest during comment periods.
% DISAPPEARANCE_RATIONALE: If the permissive reading vanished overnight - contemporaneous logs demanded, elections disregarded - a large share of currently deducted losses would suspend, participation-engineering products would lose their market, deal structures would reprice around verifiable operator labor, and federal receipts would rise immediately.
% FOUNDING_PROBLEM: Before 1986, promoters manufactured paper losses in passive ventures and sold them to high-bracket professionals to wipe out wage income; Congress built the passive-activity rules and the material-participation carve-out so genuine working owners could deduct real losses while paper-loss syndications could not.
% FOUNDING_PROBLEM_CORROBORATION: The 1986 Act committee reports and the Treasury preamble adopting the temporary Section 469 regulations attest the anti-shelter purpose; Government Accountability Office reviews of passive-loss usage and academic tax-policy scholarship - none of them beneficiary parties - document that the carve-out now functions chiefly as a planning vehicle. The advisory industry attests the old problem is solved and the current threshold is sound. Corroboration for the contested status therefore comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness reaches 0.71 by interval end: the transfer is systematic and professionally optimized, though bounded by the fact that many deducted losses correspond to real depreciation of real assets. Suppression 0.57 is structural rather than personal - nothing coerces individuals, but the strict alternative is held down through interpretive complexity, lobbying weight in rulemaking, and litigation risk that chills examiner challenges. Theater 0.60: a majority of participation documentation is constructed for the file rather than kept for operations; contemporaneous logs are the exception and reconstructed summaries the norm. Accessibility_collapse 0.40: exits persist (recognizing the income, paying the tax, REIT intermediation, jurisdiction shopping), so alternatives never fully close. Resistance 0.60: sustained IRS real-property campaigns, proposed grouping-consistency regulations, and recurring congressional interest keep the reading continuously contested. All three series run on one shared grid (t = 0, 6, 12, 18, 24, 30, 36) so no metric borrows another's endpoints; values are historically grounded estimates drawn from published enforcement and legislative records. The claimed type is authored from structure - a real coordination core joined to an asymmetric, actively defended transfer - independently of these metric values; where the engine's per-seat computations diverge from the claim, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   From the investor and advisor seats the arrangement is lawful planning: the text states numeric tests, the tests are met, elections are expressly authorized. From the treasury and wage-earner seats the same arrangement is systematic leakage dressed as administration. Small landlords occupy a third position: genuine relief that happens to travel inside the engineered population. The engine computes these divergent per-seat types from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Investors sit near the beneficiary pole: declared beneficiaries with mobile exit, capturing the deduction net of advisory fees and residual audit risk. Advisors sit nearest zero: fee-collecting beneficiaries largely indifferent to which reading wins so long as demand persists. Small landlords derive real benefit but pay fees and bear audit exposure - the derivation from their beneficiary declaration plus constrained exit would place them deep at the beneficiary end, so an override corrects toward 0.35 to reflect the two-way flow. Treasury derives high target-direction from its victim declaration (it bears the revenue cost), moderated slightly by its agenda-setting control over enforcement intensity. Wage earners and future taxpayers derive maximal target-direction: declared victims with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding warrant - separating working owners from paper-loss syndicators - has atrophied inside this reading: the carve-out now routes engineered losses rather than filtering them. Yet the arrangement is not inert: the administrable-threshold function still coordinates millions of ordinary filings, and enforcement still occasionally bites. Claiming tangled_rope keeps both halves visible. A pure-extraction label would erase the genuine coordination small operators rely on; a pure-coordination label would launder the systematic transfer. The mandatrophy picture is partial: the anti-shelter mandate survives at the statute level but is dead at the level of this reading's day-to-day practice - precisely the mixed structure the per-seat computation should surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint instantiates the strategic_shelter_reading of the irc_469_material_participation_kernel; the strict_gatekeeper_reading sibling relocates the arrangement''s center of gravity from taxpayer self-certification to government-verifiable labor - which structural elements carry the disagreement?',
    'Compile both sibling stories and compare: divergence in victim sets, epsilon, and per-seat types locates the contested elements (burden of proof on reported hours, bindingness of grouping elections against recharacterization).',
    'If the strict reading prevails institutionally, the qualifying population contracts, compliance friction rises, and the systematic passive-loss transfer narrows toward genuinely active operators - flipping this story''s computed classification toward the sibling''s profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of the material-participation kernel; the disagreement is located in evidentiary burden and election bindingness.').

omega_variable(
    engineered_vs_genuine_participation_mix,
    'What fraction of the qualifying population''s counted hours reflect economically substantive labor versus threshold-engineered documentation?',
    'Examination data from IRS real-property-professional audits, time-log quality studies, and comparison of deduction patterns against observable property-management activity.',
    'A high engineered share pushes the computed type toward pure extraction with the coordination story as cover; a high genuine share strengthens the coordination half and lowers effective extraction for the incidental-beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineered_vs_genuine_participation_mix, empirical, 'Mix of real labor versus constructed hours behind qualification.').

omega_variable(
    fiscal_incidence_of_passive_losses,
    'Who ultimately bears the revenue moved through systematically deducted passive losses - current taxpayers via higher rates or narrower services, or future taxpayers via debt?',
    'Distributional scoring of the passive-loss provisions under a strict-enforcement counterfactual by the joint tax committees and budget offices.',
    'Determines whether the victim seat is current wage earners, future generations, or both - reshaping victim directionality and the coalition analysis for the powerless seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_incidence_of_passive_losses, empirical, 'Ultimate bearer of the shifted fiscal burden.').

omega_variable(
    enforcement_tolerance_vs_endorsement,
    'Does the permissive reading persist because courts and the Treasury endorse it as the correct construction, or because full enforcement against engineered participation exceeds available capacity?',
    'Natural experiment from periods of heightened IRS real-property campaigns: if challenged positions fail at high rates yet planning practice continues unchanged, tolerance rather than endorsement is confirmed.',
    'Endorsement supports a stable hybrid structure; mere tolerance implies fragility - a strict-enforcement shock could drive rapid decay of the reading''s operative force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_tolerance_vs_endorsement, empirical, 'Whether persistence reflects endorsement or an enforcement gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_strategic_shelter_tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t6, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t12, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t18, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t24, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.56).
narrative_ontology:measurement(irc469_strategic_shelter_tr_t36, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 36, 0.6).

% Extraction over time
narrative_ontology:measurement(irc469_strategic_shelter_be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(irc469_strategic_shelter_be_t6, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(irc469_strategic_shelter_be_t12, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(irc469_strategic_shelter_be_t18, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(irc469_strategic_shelter_be_t24, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(irc469_strategic_shelter_be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(irc469_strategic_shelter_be_t36, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 36, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(irc469_strategic_shelter_su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(irc469_strategic_shelter_su_t6, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(irc469_strategic_shelter_su_t12, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(irc469_strategic_shelter_su_t18, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(irc469_strategic_shelter_su_t24, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(irc469_strategic_shelter_su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(irc469_strategic_shelter_su_t36, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 36, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the irc_469_material_participation_kernel per the epsilon-invariance principle: the colloquial label 'material participation' covers two structurally distinct claims. This story authors the strategic_shelter_reading (permissive threshold, engineered qualification, broad qualifying population, systematic passive-loss transfer). The sibling story authors the strict_gatekeeper_reading (verifiable substantial labor, high documentation bar, narrow qualifying population). The upstream statutory text anchors both; each reading yields its own epsilon, beneficiary/victim structure, and classification. Each file links the other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
