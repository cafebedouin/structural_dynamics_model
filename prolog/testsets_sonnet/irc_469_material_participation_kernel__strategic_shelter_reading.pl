% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: IRC §469 Material Participation Threshold — Strategic Shelter Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469 was enacted to stop passive real estate losses from sheltering
 *   active income, but the 'material participation' threshold that separates
 *   active operators from passive investors is not a bright line — it is a
 *   facts-and-circumstances test with seven alternative qualifying methods
 *   (26 CFR 1.469-5T), one of which is simply logging 500+ or 750+ hours (for
 *   real estate professionals) of personal involvement. Courts have
 *   repeatedly accepted approximate, reconstructed, and self-serving hour
 *   logs as sufficient evidence. Combined with the grouping election under
 *   Reg. 1.469-4, which lets taxpayers aggregate or disaggregate activities
 *   to hit hour thresholds most favorably, the standard functions in practice
 *   as a permissive gate: a taxpayer with competent advice and modest
 *   documentation discipline can nearly always structure their way across it.
 *   This story instantiates the STRATEGIC SHELTER READING of the kernel — the
 *   sibling STRICT GATEKEEPER READING (a separate constraint story) holds
 *   that the same statutory text demands verifiable, substantial,
 *   high-documentation personal labor, producing a narrow qualifying
 *   population and a materially different extraction profile. Both readings
 *   share the same statutory kernel (IRC §469 + Treas. Reg. 1.469-5T) but
 *   diverge entirely on how permissively 'material participation' is
 *   interpreted and enforced.
 *
 * KEY AGENTS:
 *   - high_income_real_estate_professionals: primary beneficiary — cross the threshold via structured hour-logging
 *   - tax_advisory_industry: secondary beneficiary — sells the interpretive labor that makes crossing reliable
 *   - leveraged_property_investors: beneficiary — converts passive losses to active deductions
 *   - general_fund_taxpayers: diffuse payer — bears revenue erosion
 *   - wage_earning_taxpayers_without_shelters: payer — subsidizes via relative tax burden with no comparable access
 *   - irs_examination_function: agenda_setter — enforces but under-resourced against permissive case law
 *   - tax_court: observer/entrencher — precedent has consistently favored permissive evidentiary standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.62).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.28).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC §469 Material Participation Threshold — Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'f62e7b57-6338-4b48-b20d-91fd1d7ca9a1').
narrative_ontology:cs_kernel_codification('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', formalized).
narrative_ontology:cs_authority_grounding('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', practice).
narrative_ontology:cs_interpretation_layer_present('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1').
narrative_ontology:cs_reading_relation('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', foundational, facts_and_circumstances_test_favors_taxpayer_documentation).
narrative_ontology:cs_axiom_status(facts_and_circumstances_test_favors_taxpayer_documentation, holdable).
narrative_ontology:cs_axiom_grounding('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', facts_and_circumstances_test_favors_taxpayer_documentation, conventional).
narrative_ontology:cs_axiom('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', secondary, grouping_election_flexibility_reflects_genuine_business_organization).
narrative_ontology:cs_axiom_status(grouping_election_flexibility_reflects_genuine_business_organization, holdable).
narrative_ontology:cs_axiom_grounding('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', grouping_election_flexibility_reflects_genuine_business_organization, instrumental).
narrative_ontology:cs_reference_frame('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', tax_reform_act_1986_shelter_closure_standard).
narrative_ontology:cs_drift_state('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', post_bailey_moss_escalante_case_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f62e7b57-6338-4b48-b20d-91fd1d7ca9a1', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, leveraged_property_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_fund_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers_without_shelters).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, passive_activity_loss_limitation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure their time logs, entity groupings, and elections (the 750-hour test, the real property trade or business election, activity grouping under Reg. 1.469-4) to cross the material participation threshold each year, converting otherwise-passive rental losses into fully deductible losses against wages and other active income. They control the documentation that establishes their own qualification and have latitude in how activities are grouped or ungrouped year to year to optimize the result.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals, agenda_setter).

% CPAs and tax attorneys design and defend hour-logging and grouping-election strategies, charging for the interpretive labor of making borderline participation cases audit-resistant. Their business model depends on the threshold remaining permissive and contestable rather than bright-line; a stricter standard would shrink the market for this advisory work.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, beneficiary,
    organized, generational, mobile, national).

% Own multiple rental properties financed with depreciation-heavy debt; qualifying as materially participating unlocks the ability to offset large paper losses against other income, materially lowering effective tax rates over long holding periods. Exit from the constraint (simply not electing or not logging hours) is available but costly — foregoing the election means losses stay suspended.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, leveraged_property_investors, beneficiary,
    moderate, generational, mobile, national).

% Bear the diffuse cost of forgone federal revenue when passive losses that would otherwise stay suspended are instead deducted against active income by taxpayers who cross a permissive, self-documented threshold. No individual taxpayer can trace or contest a specific instance of this; the cost arrives as a marginally higher aggregate tax burden or reduced public spending.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_fund_taxpayers, payer,
    powerless, civilizational, trapped, national).

% Earn income taxed at full marginal rates with no equivalent access to the material participation election — they lack the capital to hold real estate positions or the time-logging latitude the test rewards. They subsidize, through relative tax burden, a shelter mechanism structurally unavailable to labor income.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers_without_shelters, payer,
    powerless, biographical, trapped, national).

% Audits material participation claims but faces a self-reported, backward-looking hour log with weak contemporaneous documentation standards (courts have accepted post-hoc reconstructed logs and calendars). Resource-constrained relative to the volume of claims, and the permissive case law (Bailey, Moss, Escalante) narrows what examiners can successfully challenge.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_examination_function, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates contested material participation claims and has, across a body of precedent, generally tolerated approximate and reconstructed time records as sufficient evidence, entrenching the permissive reading through accumulated case law rather than statutory tightening.
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
% COORDINATION_FUNCTION: Distinguishes active business/investment involvement from passive investment so that losses from genuinely passive activities cannot offset active income — a real anti-shelter coordination problem Congress targeted in the Tax Reform Act of 1986.
% TRANSFER_FUNCTION: Moves tax liability: qualifying taxpayers convert suspended passive losses into current deductions against active income, reducing their tax bill; the corresponding revenue shortfall is absorbed by the general fund and, relatively, by taxpayers without comparable shelter access.
% ABSENT_VOICES: Wage-earning taxpayers who fund the shortfall have no seat in the rulemaking, litigation, or advisory conversations that shape how permissively 'material participation' is interpreted; they are represented only abstractly, if at all, in congressional revenue-estimate debates.
% DISAPPEARANCE_RATIONALE: If the material participation threshold and its permissive case law vanished, real estate investors would lose the primary mechanism for converting passive rental losses into active-income offsets; the tax advisory industry built around hour-logging and grouping-election strategy would contract sharply; property investment structuring would shift toward other shelters (cost segregation, opportunity zones) or investors would bear the passive loss limitation as originally intended.
% FOUNDING_PROBLEM: Pre-1986, wealthy taxpayers used real estate and other tax-shelter partnerships to generate large paper losses (via aggressive depreciation) that offset unrelated wage and investment income, eroding the tax base with minimal real economic activity. The material participation test was built to separate genuine operators from passive shelter investors.
% FOUNDING_PROBLEM_CORROBORATION: The IRS and congressional Joint Committee on Taxation revenue estimates (outside the beneficiary set) continue to attest that passive loss abuse is a live problem and cite material participation gaming as an active erosion vector in tax-gap studies. Tax advisors and real estate investor associations, who benefit from the permissive reading, counter that the threshold correctly identifies genuine operators; no outside-party corroboration was found affirmatively endorsing the permissive interpretation as functioning as originally intended.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.62 and rising because each additional year of permissive Tax Court precedent (Bailey v. Commissioner, Moss v. Commissioner, Escalante v. Commissioner) lowers the practical documentation bar further, widening the population that can successfully claim material participation without corresponding growth in genuine operational involvement — extraction accumulates as case law hardens the permissive reading into a de facto safe harbor. Suppression is comparatively low (0.28) because nothing coercive prevents alternate interpretations; the mechanism works through permissiveness and low friction, not through blocking exits. Theater ratio is moderate and rising (0.48) because a growing share of taxpayer 'participation' consists of after-the-fact calendar reconstruction and box-checking (attending property-manager calls, periodic site visits) rather than substantive operational labor — the hours are increasingly logged to satisfy the test rather than reflecting activity that would occur regardless of the test.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income real estate professionals and leveraged investors sit near the full-beneficiary end: they control their own qualifying documentation, have mobile exit (can simply not elect, or restructure activities across years), and directly capture the tax benefit. The tax advisory industry benefits without bearing the constraint at all — an analytical/organized beneficiary profiting from the threshold's ambiguity itself. General fund taxpayers and wage earners sit at the target end: trapped exit (no comparable shelter available to labor income), no voice in the interpretive latitude, and diffuse but real cost. The IRS examination function is structurally an agenda_setter but functions with constrained exit — it cannot unilaterally tighten the standard against accumulated favorable case law without new regulation or litigation risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping shelter abuse) is contested rather than resolved: JCT and IRS materials treat passive loss gaming as a live erosion vector, while the permissive case law and advisory industry treat the current threshold as correctly calibrated. This mismatch — dead-or-live disputed, arrangement clearly persisting and expanding in practical effect — is exactly the R5 signal the mandatrophy check exists to surface: a coordination mechanism (separating genuine operators from passive shelter-seekers) has been substantially captured by exactly the population it was designed to constrain, without anyone needing to declare the original function obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissive_reading_is_one_of_two_live_kernel_readings,
    'Is the permissive, hour-logging-friendly interpretation of material participation the operative reading of IRC §469, or is the strict, high-documentation reading (also live in case law and IRS guidance) the one that should govern — and which reading a given taxpayer experiences depends heavily on audit selection and forum (Tax Court circuit).',
    'Track the divergence rate between IRS initial determinations (which tend toward strict) and Tax Court outcomes (which tend toward permissive) across circuits; a persistent gap indicates two live readings rather than one settled standard.',
    'If the strict reading were to become dominant (e.g. through regulatory tightening of documentation requirements), the qualifying population would shrink substantially and the extraction profile modeled here would not hold — this is precisely why the two readings are authored as separate constraint stories rather than one story with a measurement parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_reading_is_one_of_two_live_kernel_readings, conceptual, 'Committer-frame ambiguity: which reading of the material participation kernel is operative is itself contested, not settled.').

omega_variable(
    documentation_reconstruction_evidentiary_value,
    'Does post-hoc reconstructed time documentation (calendars, estimates, narrative reconstruction accepted in Bailey/Moss/Escalante) constitute genuine evidence of material participation, or is it primarily a compliance artifact with no reliable relationship to actual hours worked?',
    'Comparative study of contemporaneous vs. reconstructed logs where both exist (e.g., audit cases with subpoenaed calendar/email metadata) to establish whether reconstructed logs systematically overstate actual involvement.',
    'If reconstruction is shown to systematically overstate hours, the theater_ratio measurement should be revised upward and the effective qualifying population under genuine operational-labor standards is meaningfully smaller than the permissive reading implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documentation_reconstruction_evidentiary_value, empirical, 'Whether self-reported/reconstructed hour logs reliably measure genuine participation or function as compliance theater.').

omega_variable(
    revenue_erosion_scale_uncertainty,
    'What is the actual annual federal revenue effect of permissive material participation qualification relative to a counterfactual strict-gatekeeper regime?',
    'IRS/JCT tax-gap studies isolating passive activity loss claims specifically attributable to marginal material-participation qualifications (as opposed to legitimately active real estate professionals).',
    'A large revenue effect would strengthen the tangled_rope/extraction reading; a small effect would suggest the permissive standard, while doctrinally loose, has limited practical fiscal consequence and the constraint is closer to a low-stakes rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revenue_erosion_scale_uncertainty, empirical, 'Scale of revenue erosion attributable to the permissive reading is not precisely established.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(irc__tr_t36, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 36, 0.48).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(irc__be_t36, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 36, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(irc_469_material_participation_kernel__strategic_shelter_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_professional_status_election).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, passive_activity_loss_carryforward_regime).

% DUAL FORMULATION NOTE:
% This story and irc_469_material_participation_kernel__strict_gatekeeper_reading are two readings of a single contested kernel (the statutory text and regulations of IRC §469 and Treas. Reg. 1.469-5T). They are NOT the same constraint measured two ways — per the ε-invariance principle, since the two readings produce materially different extraction profiles (this reading: substantial, rising extraction via broad qualification; the sibling: narrow qualification, low extraction, closer to a genuine anti-shelter mountain/rope), they are authored as separate constraint stories with independent ε, stakeholders, and classification, linked here via network edges rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
