% ============================================================================
% CONSTRAINT STORY: textualist_severability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_textualist_severability_reading, []).

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
 *   constraint_id: textualist_severability_reading
 *   human_readable: Textualist Severability Reading of the Adverse Effect Wage Guarantee
 *   domain: administrative_law/labor_economics/immigration_policy
 *
 * SUMMARY:
 *   This story instantiates the textualist severability reading of the
 *   adverse effect wage guarantee kernel: the claim that the statutory
 *   guarantee against downward wage pressure exists independently of any
 *   particular wage survey instrument, such that a flawed instrument is an
 *   administrative-law defect correctable through rulemaking and
 *   instrument-specific litigation, not evidence the underlying right was
 *   denied. Under this reading, the worker retains a theoretical path to
 *   prove the guarantee's substance by 'other evidentiary means,' but no
 *   concrete alternative evidentiary channel is specified in practice, and
 *   instrument-adequacy litigation becomes the only real avenue. The rising
 *   theater_ratio reflects a growing gap between the doctrine's formal
 *   promise of severability (the right survives instrument failure) and the
 *   practical reality that instrument litigation is the sole functioning
 *   channel, so procedural sophistication increases while substantive wage
 *   outcomes track the instrument almost one-to-one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(textualist_severability_reading, 0.28).
domain_priors:suppression_score(textualist_severability_reading, 0.22).
domain_priors:theater_ratio(textualist_severability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(textualist_severability_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(textualist_severability_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(textualist_severability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(textualist_severability_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(textualist_severability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(textualist_severability_reading, scaffold).
narrative_ontology:human_readable(textualist_severability_reading, "Textualist Severability Reading of the Adverse Effect Wage Guarantee").
narrative_ontology:topic_domain(textualist_severability_reading, "administrative_law/labor_economics/immigration_policy").

narrative_ontology:has_sunset_clause(textualist_severability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(textualist_severability_reading, '37d94013-ec1c-4d92-ae7d-e7a1f653398c').
narrative_ontology:cs_kernel_codification('37d94013-ec1c-4d92-ae7d-e7a1f653398c', fixed_text).
narrative_ontology:cs_authority_grounding('37d94013-ec1c-4d92-ae7d-e7a1f653398c', lineage).
narrative_ontology:cs_interpretation_layer_present('37d94013-ec1c-4d92-ae7d-e7a1f653398c').
narrative_ontology:cs_reading_relation('37d94013-ec1c-4d92-ae7d-e7a1f653398c', adverse_effect_guarantee_kernel__instrument_dependent_reading, forecloses).
narrative_ontology:cs_reading_relation('37d94013-ec1c-4d92-ae7d-e7a1f653398c', adverse_effect_guarantee_kernel__coverage_neutral_reading, coexists_with).
narrative_ontology:cs_reading_relation('37d94013-ec1c-4d92-ae7d-e7a1f653398c', adverse_effect_guarantee_kernel__capture_reading, influences).
narrative_ontology:cs_reading_relation('37d94013-ec1c-4d92-ae7d-e7a1f653398c', adverse_effect_guarantee_kernel__channel_conversion_reading, influences).
narrative_ontology:cs_axiom('37d94013-ec1c-4d92-ae7d-e7a1f653398c', foundational, guarantee_severable_from_implementing_instrument).
narrative_ontology:cs_axiom_status(guarantee_severable_from_implementing_instrument, holdable).
narrative_ontology:cs_axiom_grounding('37d94013-ec1c-4d92-ae7d-e7a1f653398c', guarantee_severable_from_implementing_instrument, conventional).
narrative_ontology:cs_axiom('37d94013-ec1c-4d92-ae7d-e7a1f653398c', secondary, instrument_inadequacy_is_administrative_not_constitutional_defect).
narrative_ontology:cs_axiom_status(instrument_inadequacy_is_administrative_not_constitutional_defect, holdable).
narrative_ontology:cs_axiom_grounding('37d94013-ec1c-4d92-ae7d-e7a1f653398c', instrument_inadequacy_is_administrative_not_constitutional_defect, instrumental).
narrative_ontology:cs_reference_frame('37d94013-ec1c-4d92-ae7d-e7a1f653398c', statutory_text_as_severable_from_implementing_regulation).
narrative_ontology:cs_drift_state('37d94013-ec1c-4d92-ae7d-e7a1f653398c', post_gao_methodology_reports_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37d94013-ec1c-4d92-ae7d-e7a1f653398c', '').
narrative_ontology:cs_kernel_id(textualist_severability_reading, adverse_effect_guarantee_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(textualist_severability_reading, agency_rulemakers).
narrative_ontology:constraint_beneficiary(textualist_severability_reading, employer_associations_relying_on_program).
narrative_ontology:constraint_beneficiary(textualist_severability_reading, reviewing_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(textualist_severability_reading, migrant_agricultural_workers).
narrative_ontology:constraint_vindicates(textualist_severability_reading, statutory_right_instrument_independence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and revises the wage survey methodology used to compute the adverse effect wage rate, treating the methodology as a discretionary implementing choice rather than as the right itself. Can swap, patch, or replace the instrument without amending the statute, and frames every instrument failure as a fixable administrative defect rather than a legal deficiency in the guarantee.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, agency_rulemakers, agenda_setter,
    institutional, generational, analytical, national).

% Rely on whatever wage instrument is currently in force to set labor costs for the visa-dependent workforce. Benefit whenever the instrument understates prevailing wages, and benefit again from the severability doctrine itself, which insulates the arrangement from a single, cleaner legal challenge to the wage floor and instead forces piecemeal, instrument-specific litigation.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, employer_associations_relying_on_program, beneficiary,
    organized, biographical, mobile, national).

% Are paid according to whatever wage figure the current instrument produces. Under this reading, if that figure is too low, the worker's remedy is to litigate the instrument's adequacy in a separate administrative-law proceeding, using alternative evidentiary means, while continuing to work at the instrument's rate in the meantime. They cannot easily leave the visa program without abandoning employment authorization, and the severability framing means no wage shortfall, however large, is itself proof that the underlying guarantee failed.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, migrant_agricultural_workers, payer,
    powerless, biographical, trapped, national).

% Adjudicate challenges to the wage instrument under arbitrary-and-capricious review rather than under the statute's substantive wage guarantee, which lets courts resolve disputes on procedural administrative-law grounds without ever reaching the question of whether workers were actually paid what the statute promises. This narrows and simplifies judicial review, which functions as institutional relief even as it defers the substantive question indefinitely.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, reviewing_courts, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(textualist_severability_reading, reviewing_courts, observer).

% Compete for jobs against the visa-dependent workforce and are affected by whatever wage floor the instrument sets, but have no standing in the administrative proceedings that adjudicate instrument adequacy and are not party to the litigation this reading channels the dispute into.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, domestic_workers_in_competing_labor_markets, excluded,
    powerless, biographical, constrained, regional).

% Attempt to litigate on behalf of affected workers but are repeatedly redirected by this reading into contesting instrument methodology (survey design, sample size, geographic units) rather than being able to argue the wage guarantee itself has been violated by a low wage figure, since the reading treats those as different legal questions with different, harder-to-win procedural postures.
narrative_ontology:constraint_stakeholder(textualist_severability_reading, worker_advocacy_organizations, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates a durable statutory commitment (the adverse effect guarantee) from the administrative machinery used to measure it, so that flaws in the measuring instrument can be fixed through ordinary rulemaking and administrative litigation without requiring Congress to reopen or courts to invalidate the underlying statutory right.
% TRANSFER_FUNCTION: Moves the practical burden of proving inadequate pay from a single substantive claim (the wage guarantee was violated) onto a narrower, harder-to-win procedural claim (the instrument used to implement the guarantee is defective), shifting litigation cost and evidentiary burden onto the worker seeking the higher wage while leaving employers' cost exposure capped at whatever the current instrument produces until a separate proceeding changes it.
% ABSENT_VOICES: Migrant workers experiencing an actual wage shortfall have no direct voice in the instrument-adequacy proceedings that determine their pay; domestic workers whose wages are affected by the same wage floor have no standing at all; worker advocacy organizations are structurally redirected into contesting methodology rather than substance.
% DISAPPEARANCE_RATIONALE: If this severability reading disappeared, wage-shortfall claims could proceed as direct violations of the statutory guarantee rather than being channeled into instrument-adequacy litigation — employers and agencies dispute whether this would meaningfully change outcomes (since instrument challenges are frequently how wage floors are actually raised) while worker advocates argue it would let underpayment claims reach the merits directly instead of being deflected into procedure.
% FOUNDING_PROBLEM: Congress could not anticipate every future wage-measurement technology or labor-market condition, so the guarantee was written at the level of principle (workers must not depress domestic wages) while leaving the specific measurement method to administrative discretion, allowing the instrument to be updated as data and methods improved.
% FOUNDING_PROBLEM_CORROBORATION: Agency rulemakers and employer associations attest the problem remains live and the severability doctrine is functioning as designed, updating instruments as needed. Independent labor economists and Government Accountability Office reports examining wage survey methodology have documented persistent multi-year gaps between measured and market wages that outlast successive instrument revisions, suggesting from outside the benefiting parties that the 'fixable instrument' framing has not, in practice, produced timely correction.
narrative_ontology:disappearance_verdict(textualist_severability_reading, contested).
narrative_ontology:founding_problem_status(textualist_severability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(textualist_severability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(textualist_severability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(textualist_severability_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(textualist_severability_reading_tests).
:- end_tests(textualist_severability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28) because this reading itself does not directly transfer wealth — it is a legal-interpretive scaffold governing HOW disputes about wages are litigated, not a wage-setting mechanism. The real extraction (if any) occurs at the instrument level, which is a separate constraint. Suppression is moderate-low (0.22): nothing here forces workers to accept low wages by force; the suppression is procedural — the guarantee's practical unenforceability without a successful instrument challenge. Theater ratio is authored higher (0.42, rising over the interval) because the doctrine's central promise — that the right is provable 'by other evidentiary means' — is rarely, if ever, exercised in practice; the severability language functions increasingly as rhetorical insulation for the instrument rather than as an active alternative pathway. Accessibility collapse is moderate (0.35): a worker can theoretically litigate the substantive guarantee directly, but as a practical matter courts route challenges through arbitrary-and-capricious instrument review, collapsing the theoretical alternative. Resistance is moderate-high (0.55) because worker advocates persistently contest the severability framing itself, arguing it is a doctrinal move that insulates outcomes from substantive review.
 *
 * DIRECTIONALITY LOGIC:
 *   Agency rulemakers and reviewing courts benefit from severability because it lets them resolve disputes on narrower procedural grounds without adjudicating the ultimate wage-adequacy question — this is a genuine institutional convenience, not merely capture. Employer associations benefit doubly: they benefit from whatever the current instrument produces, and they benefit from severability's insulation of the wage floor from a single unified substantive challenge. Migrant workers are the payer class: their remedy, if the instrument understates the true wage, is confined to administrative-law channels that are slow, resource-intensive, and rarely successful, while they continue working at the instrument's rate during the dispute. Domestic competing workers and worker advocacy organizations are excluded from the proceedings that actually determine outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is explicitly designed to prevent mislabeling a possibly-legitimate administrative-updating function (instruments genuinely do need periodic revision as data and methods improve) as pure extraction. The scaffold claim asserts the severability doctrine is meant to be a transitional, correctable arrangement — instruments get fixed, the underlying right endures. The founding-problem status is authored contested precisely because the doctrine's practical performance (GAO-documented multi-year wage gaps surviving successive instrument revisions) undercuts its own premise that the instrument is readily fixable. If the sunset/correction mechanism worked as claimed, this would be a clean scaffold; the corpus should flag the divergence between the claimed transitional function and the observed persistence of the gap as exactly the measurement this framework exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severability_versus_capture,
    'Is the textualist severability doctrine a genuine legal-interpretive necessity (the guarantee must be statutorily severable from a changeable instrument because Congress cannot fix measurement technology in statute), or is it a doctrinal technology that was adopted, and is maintained, because it insulates the wage floor from substantive challenge and benefits the parties who rely on low measured wages?',
    'Trace the doctrine''s origin and adoption history: was severability argued for by parties benefiting from low wage instruments, and has litigation success under the severability frame consistently favored those parties over worker claimants? Compare outcomes under this reading against outcomes in comparable statutory schemes that do NOT sever the right from the instrument.',
    'If the doctrine''s adoption and litigation pattern shows systematic favor toward instrument-reliant employers, this reading''s scaffold classification should be revisited toward tangled_rope or snare; if the doctrine functions evenhandedly and instrument corrections are timely, scaffold is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severability_versus_capture, conceptual, 'Whether severability is neutral statutory construction or a captured doctrinal instrument.').

omega_variable(
    alternative_evidentiary_means_existence,
    'Does a real, usable evidentiary channel exist for a worker to prove the substantive wage guarantee was violated independent of challenging the instrument, or is the ''other evidentiary means'' language a theoretical possibility with no functioning practical instantiation?',
    'Survey litigation records for any successful claim that proved wage guarantee violation using evidence other than instrument-adequacy challenge; absence across a substantial case sample would corroborate that the alternative channel is nominal only.',
    'If no such successful claim exists in the historical record, the theater_ratio authored here is likely understated and the doctrine functions closer to instrument_dependent_reading in practice despite its textualist framing — this would sharpen the case for reclassification pressure via T17-style accumulation analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_evidentiary_means_existence, empirical, 'Whether the alternative evidentiary path is real or nominal.').

omega_variable(
    sunset_mechanism_functioning,
    'Is the scaffold''s implicit sunset (instrument correction restoring the guarantee to full function) actually triggering when instruments are shown inadequate, or has the correction mechanism itself atrophied into permanent deferral?',
    'Compare the interval between documented instrument inadequacy findings (e.g., GAO reports) and actual corrective rulemaking across multiple survey-methodology cycles.',
    'A consistently long or non-existent correction interval would support reclassifying this reading''s practical operation as piton-adjacent despite its scaffold self-description, strengthening the mandatrophy signal already flagged in commentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_functioning, empirical, 'Whether the scaffold''s sunset/correction function operates in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(textualist_severability_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(text_tr_t0, textualist_severability_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(text_tr_t4, textualist_severability_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(text_tr_t8, textualist_severability_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(text_tr_t12, textualist_severability_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(text_tr_t16, textualist_severability_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(text_tr_t20, textualist_severability_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(text_tr_t24, textualist_severability_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(text_be_t0, textualist_severability_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(text_be_t4, textualist_severability_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(text_be_t8, textualist_severability_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(text_be_t12, textualist_severability_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(text_be_t16, textualist_severability_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(text_be_t20, textualist_severability_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(text_be_t24, textualist_severability_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(textualist_severability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(textualist_severability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(textualist_severability_reading, instrument_dependent_reading).
narrative_ontology:affects_constraint(textualist_severability_reading, coverage_neutral_reading).
narrative_ontology:affects_constraint(textualist_severability_reading, capture_reading).
narrative_ontology:affects_constraint(textualist_severability_reading, channel_conversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the adverse_effect_guarantee_kernel, each instantiating a structurally distinct constraint with its own epsilon and victim structure. textualist_severability_reading generates NO statutory-level victim set — under this reading, observed wage shortfalls are an instrument-quality problem, not a guarantee violation, so the story is authored as scaffold with no victims declared. instrument_dependent_reading (sibling) would generate a full victim set (migrant workers) because it treats instrument inadequacy AS guarantee failure. capture_reading would treat the severability doctrine itself as the extractive mechanism. All five are linked here per the epsilon-invariance decomposition rule; do not average or reconcile their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
