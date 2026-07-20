% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformativeness-Dominant Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint instantiates the transformative_use_reading of the
 *   fair_use_four_factor_test kernel. Under this reading, the first fair-use
 *   factor (purpose and character of use) dominates when the secondary work
 *   is transformativeâadding new meaning, expression, or messageâand the
 *   fourth factor (market harm) is correspondingly subordinated. The doctrine
 *   is actively enforced by federal courts and enables remix culture,
 *   user-generated content platforms, and appropriation artists to operate
 *   without licensing. Original creators and commercial licensors bear the
 *   cost through lost licensing revenue and diminished exclusivity. The
 *   beneficiary and victim sets shift with the judicially determined
 *   transformation threshold, making the constraint context-dependent and
 *   moderately extractive. This reading coexists with the
 *   creator_centric_reading and user_centric_reading as live positions in
 *   ongoing doctrinal contest.
 *
 * KEY AGENTS:
 *   - Federal judiciary (agenda_setter): Sets the transformation threshold through precedent and district court opinions.
 *   - Tech platforms enabling UGC (beneficiary): Avoid licensing liability for transformative user uploads at scale.
 *   - Remix culture producers (beneficiary): Depend on the defense to create derivative works without clearing rights.
 *   - Original creators (payer): Lose exclusivity and licensing revenue when appropriation is deemed transformative.
 *   - Commercial licensors (payer): Lose licensing opportunities in categories like remix and appropriation art.
 *   - Copyright Office (observer): Advises on policy but does not adjudicate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.55).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.55).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformativeness-Dominant Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'c8e55e94-16c3-4192-be28-7d9780a6c04d').
narrative_ontology:cs_kernel_codification('c8e55e94-16c3-4192-be28-7d9780a6c04d', formalized).
narrative_ontology:cs_authority_grounding('c8e55e94-16c3-4192-be28-7d9780a6c04d', lineage).
narrative_ontology:cs_interpretation_layer_present('c8e55e94-16c3-4192-be28-7d9780a6c04d').
narrative_ontology:cs_reading_relation('c8e55e94-16c3-4192-be28-7d9780a6c04d', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8e55e94-16c3-4192-be28-7d9780a6c04d', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('c8e55e94-16c3-4192-be28-7d9780a6c04d', foundational, transformativeness_trumps_market_harm).
narrative_ontology:cs_axiom_status(transformativeness_trumps_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('c8e55e94-16c3-4192-be28-7d9780a6c04d', transformativeness_trumps_market_harm, conventional).
narrative_ontology:cs_axiom('c8e55e94-16c3-4192-be28-7d9780a6c04d', foundational, new_meaning_defines_fair_use_boundary).
narrative_ontology:cs_axiom_status(new_meaning_defines_fair_use_boundary, holdable).
narrative_ontology:cs_axiom_grounding('c8e55e94-16c3-4192-be28-7d9780a6c04d', new_meaning_defines_fair_use_boundary, conventional).
narrative_ontology:cs_reference_frame('c8e55e94-16c3-4192-be28-7d9780a6c04d', copyright_progress_clause_balance).
narrative_ontology:cs_drift_state('c8e55e94-16c3-4192-be28-7d9780a6c04d', post_digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8e55e94-16c3-4192-be28-7d9780a6c04d', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, downstream_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, commercial_licensors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create derivative works, remixes, and commentary that incorporate existing copyrighted material. Rely on the transformativeness doctrine as a legal defense against infringement claims. Cannot easily exit the copyright system because their creative practice depends on appropriation and reuse.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_producers, beneficiary,
    moderate, biographical, constrained, national).

% Host user-generated content at massive scale. Benefit from the transformativeness reading because it reduces licensing liability for transformative uploads made by users. Can shift operations, lobby for statutory safe harbors, or alter content policies, but are structurally exposed to copyright claims if the doctrine narrows.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc, beneficiary,
    institutional, generational, arbitrage, global).

% Produce appropriation art, documentary films, and critical commentary that quote or incorporate source works. Depend on fair use findings to distribute their work without clearing every excerpt. Their exit options are limited because licensing all sources would be economically prohibitive.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, downstream_creators, beneficiary,
    moderate, biographical, constrained, national).

% Create the primary works that others appropriate. Bear the cost of lost licensing revenue and diminished exclusivity when courts find transformative use. Can litigate individual cases or join collectives, but cannot opt out of the fair use framework that governs all copyrighted works in the jurisdiction.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_creators, payer,
    organized, biographical, constrained, national).

% Manage exclusive licensing catalogs for music, images, and video. Lose licensing opportunities when transformative use findings permit unpaid appropriation. Their business model depends on enforceable exclusivity, which the doctrine overrides in contested categories like remix and appropriation art.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, commercial_licensors, payer,
    institutional, biographical, constrained, global).

% Interprets and applies the four-factor test in infringement litigation. Sets the effective transformation threshold through binding precedent and district court opinions. Does not collect rents from either side but exercises authority by determining which uses are fair and which require licensing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Advises Congress on copyright policy and registers claims. Issues reports on the fair use doctrine and its economic effects. Does not adjudicate cases but influences statutory interpretation through amicus briefs and legislative recommendations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_office, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cumulative cultural production by allowing secondary authors to build upon existing copyrighted works without individual licensing when the new work adds new meaning, message, or expression, reducing transaction costs for commentary, parody, and remix.
% TRANSFER_FUNCTION: Moves the right to control derivative uses and associated licensing revenue from original copyright holders to secondary users and hosting platforms when courts classify the secondary use as transformative.
% ABSENT_VOICES: Small and independent visual artists, photographers, and musicians whose works are frequently appropriated in allegedly transformative compilations or remixes but who lack litigation resources to challenge well-funded defendants; their perspective is structurally underrepresented in the case law that defines the doctrine.
% DISAPPEARANCE_RATIONALE: If the transformativeness-dominant reading vanished, UGC platforms would face immediate liability for hosting unlicensed remixes and commentary, requiring either blanket licensing regimes or mass content removal; remix culture and appropriation art practices would contract sharply as legal risk shifted to secondary users.
% FOUNDING_PROBLEM: Copyright's exclusive rights can block the cumulative creation and commentary that the constitutional progress clause aims to promote when every reuse requires permission from the original rights-holder.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional progress clause and the statutory fair use provision in 17 U.S.C. Â§ 107 attest the founding problem from outside the benefiting tech platforms; empirical communications studies corroborate that licensing markets fail for small-scale remix and commentary, while economic analyses from creator collectives contest that voluntary licensing could resolve the coordination problem without judicial override.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint transfers substantial licensing value from creators to secondary users, but only conditionally upon judicial findings of transformation. Suppression (0.55) reflects the active judicial override of statutory exclusivity that must be maintained through litigation and precedent. Theater ratio (0.30) captures growing ritualization of four-factor analysis in opinions that routinely find transformativeness, while much of the doctrine remains functional in channeling UGC. Accessibility collapse (0.60) is moderate: licensing alternatives exist and are understood, but the fair use shortcut partially collapses them for transformative categories. Resistance (0.50) is significant: creator collectives and media companies actively litigate and lobby against expansive transformativeness readings.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal judiciary) experiences this constraint as interpretive coordinationâbalancing constitutional valuesâwhile payer seats (original creators, commercial licensors) experience it as judicial expropriation of licensing rights. Beneficiary seats (remixers, platforms) experience it as freedom-enhancing coordination. The engine should compute divergent per-seat types: low directionality for beneficiaries, high directionality for victims, near-analytical for the judiciary.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (remix_culture_producers, tech_platforms_ugc, downstream_creators) have low directionality because the constraint subsidizes their expressive and commercial activity by removing licensing friction. Victims (original_creators, commercial_licensors) have high directionality because the constraint extracts licensing revenue and exclusivity from them. The federal_judiciary sits near symmetric because it wields interpretive authority without collecting rents from either side.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the constraint as pure rope (which would ignore identifiable victims losing licensing value) or pure snare (which would deny the genuine coordination function of enabling cumulative culture without prohibitive transaction costs). The active enforcement requirement and the shifting victim set confirm hybrid status: someone is coordinated and someone pays through the same doctrinal structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_indeterminacy,
    'Is the threshold for transformativeness sufficiently determinate to avoid arbitrary extraction from original creators, or does its vagueness enable judicial discretion that systematically benefits well-resourced secondary users?',
    'Quantitative content-analysis of fair use opinions measuring inter-judge consistency in transformativeness findings, paired with economic analysis of creator licensing revenue in jurisdictions with stricter transformation thresholds.',
    'If threshold application is inconsistent and tracks defendant resources rather than expressive change, the constraint extracts arbitrarily and leans toward snare; if consistent and predictable, the extraction is the necessary price of a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_indeterminacy, empirical, 'Indeterminacy of the transformativeness threshold and its effect on predictable rights allocation.').

omega_variable(
    kernel_reading_contest,
    'Does the transformative_use_reading represent a genuine interpretation of the statutory four-factor kernel, or has practice drift created a functionally distinct constraint that coexists uneasily with the statutory text?',
    'Comparative doctrinal analysis measuring the marginal effect of transformativeness on overall fair use outcomes across the pre-Campbell and post-digital eras.',
    'If the reading has functionally drifted from the kernel, it should be treated as a distinct tangled_rope constraint rather than an interpretation; if it remains within interpretive bounds, the kernel is a distributed commitment system with multiple live readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel status of the fair use test and whether transformative use is interpretation or functional drift.').

omega_variable(
    market_harm_subordination_legitimacy,
    'Does subordinating market harm to transformativeness legitimate extraction from creators by ignoring viable licensing markets that the secondary use displaced?',
    'Empirical assessment of licensing market viability for the categories of use most often found transformative, compared with outcomes in jurisdictions where market harm retains primacy.',
    'If licensing markets are viable and displaced, the constraint extracts asymmetrically from creators; if markets fail for these categories, the subordination is a necessary coordination cost rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_legitimacy, empirical, 'Whether subordinating market harm enables extraction via licensing market displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 40, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__transformative_use_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_four_factor_test kernel. The transformative_use_reading emphasizes the first and second factors over the fourth, while the creator_centric_reading weights all factors equally to preserve incentives, and the user_centric_reading elevates the first factor as an affirmative public right. They form a constraint family linked by shared statutory text but divergent structural effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
