% ============================================================================
% CONSTRAINT STORY: colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colorblind_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: colorblind_reading
 *   human_readable: Colorblind Equal Protection Reading: Race-Neutral State Classification Rule
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   The colorblind reading of equal protection is a constitutional doctrine
 *   asserting that the Fourteenth Amendment forbids the state from
 *   classifying individuals by race regardless of remedial intent. This
 *   reading instantiates one interpretation of the equal protection kernel.
 *   The constraint creates a structural tension between the formal principle
 *   (the state cannot see race) and the structural reality (accumulated
 *   racial subordination cannot be addressed through race-conscious
 *   remediation). The colorblind reading prohibits one set of coordination
 *   mechanisms (remedial classification, targeted allocation) while enabling
 *   others (facially neutral criteria that correlate with race). This creates
 *   asymmetric distribution of constraint experience: race-neutral
 *   administrators experience coordination (bright-line rule), privileged
 *   groups experience mixed coordination and extraction (neutral criteria
 *   that benefit them), subordinated groups experience pure extraction
 *   (remedial mechanisms forbidden while disadvantage persists), and
 *   organized legal movements experience the constraint as either temporary
 *   stabilization (scaffold) or degraded ritual (piton). The constraint
 *   exhibits all six DR types from different structural positions, making it
 *   a diagnostic exemplar for how constitutional readings instantiate
 *   constraints.
 *
 * KEY AGENTS:
 *   - Subordinated Racial Groups: Primary victims (powerless/trapped) — structurally unable to exit or use state action for collective remediation; constraint locks in accumulated disadvantage
 *   - Race-Neutral Administrators: Primary beneficiaries (institutional/arbitrage) — benefit from bright-line prohibition on racial classification; reduces litigation risk and discretion; can achieve de facto allocation through facially neutral criteria
 *   - Privileged Racial Groups: Secondary beneficiaries (powerful/mobile) — benefit from neutral allocation mechanisms that correlate with existing advantages; experience constraint as coordination
 *   - Originalist Legal Coalition: Organized advocates (organized/constrained) — push for stricter enforcement of colorblind reading; see constitutional recovery as sunset of precedent permitting remedial classification
 *   - Civil Rights Advocates: Organized opposition (organized/constrained) — push for remedial reading; constrained by colorblind doctrine but have agency through legislative and cultural pressure
 *   - Formal Neutrality Doctrine: Institutional maintenance mechanism (institutional/arbitrage) — self-perpetuating through circular reasoning; high theater as it naturalizes subordination as inevitable
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit: naturalizing the colorblind reading as the only defensible interpretation of equal protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colorblind_reading, 0.32).
domain_priors:suppression_score(colorblind_reading, 0.48).
domain_priors:theater_ratio(colorblind_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colorblind_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(colorblind_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(colorblind_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colorblind_reading, tangled_rope).
narrative_ontology:human_readable(colorblind_reading, "Colorblind Equal Protection Reading: Race-Neutral State Classification Rule").
narrative_ontology:topic_domain(colorblind_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(colorblind_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colorblind_reading, race_neutral_administrators).
narrative_ontology:constraint_beneficiary(colorblind_reading, individual_rights_claimants).
narrative_ontology:constraint_victim(colorblind_reading, racial_equity_coordination).
narrative_ontology:constraint_victim(colorblind_reading, subordinated_racial_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED RACIAL GROUPS (SNARE) — Structurally trapped by the constraint's prohibition on remedial classification. Cannot exit or organize collective redress through state action. The colorblind rule prevents the very mechanisms (affirmative action, targeted allocation, remedial programs) that would address historical subordination. Maximum experienced extraction: the constraint locks in accumulated disadvantage while forbidding correction.
constraint_indexing:constraint_classification(colorblind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACE-NEUTRAL ADMINISTRATORS (ROPE) — Experience the constraint as clarifying coordination: the colorblind rule provides a simple, administrable protocol for state action. Administrators benefit from the bright-line prohibition (low discretion, reduced litigation risk). The constraint solves their coordination problem by forbidding the complexity of race-conscious classification. Exit available through legislative override, but administrative institutions have arbitrage options (neutral criteria that correlate with race).
constraint_indexing:constraint_classification(colorblind_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVILEGED RACIAL GROUPS (TANGLED ROPE) — Experience mixed coordination and extraction. The colorblind rule coordinates rational-basis review (low scrutiny for facially neutral laws), which enables programs and allocations that benefit these groups while maintaining the fiction of neutrality. Real agency through mobility (can adopt colorblind framing or assert individual rights) but also benefits from the constraint's asymmetric effect. Both coordinated with and extracted from depending on the specific allocation.
constraint_indexing:constraint_classification(colorblind_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST LEGAL COALITION (SCAFFOLD) — Organized legal movement (Federalist Society, originalist jurisprudence) views the colorblind reading as a temporary stabilization of jurisprudence pending full original-meaning recovery. Low effective extraction because the coalition has agency and sees an exit path through doctrinal evolution. Theater ratio is moderate: the originalist framing claims fidelity to constitutional text but operates as a progressive doctrinal project rewriting precedent.
constraint_indexing:constraint_classification(colorblind_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL NEUTRALITY DOCTRINE (PITON) — The colorblind rule persists as an institutional ritual despite mounting evidence that formal neutrality masks substantive subordination. The doctrine maintains itself through self-referential circularity: race-consciousness is forbidden because race-consciousness is forbidden. Theater ratio is high: the doctrine performs a commitment to equality while the real function (coordinating which groups benefit from neutral allocation) operates below the formal surface.
constraint_indexing:constraint_classification(colorblind_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN READING / FALSE SUMMIT RISK) — From a civilizational perspective, formal equality before law appears as an immutable principle: the state cannot legitimately classify by race because such classification violates the fundamental commitment to equal rights. This perspective naturalizes the colorblind reading as a logical consequence of equal protection itself. However, the structural data reveals this as a false summit: the reading is one instantiation of the equal protection kernel, not the only defensible reading. Alternative readings (remedial, antisubordination) also claim equal protection authority. The mountain classification reveals that the analytical observer is itself positioned within a particular constitutional reading, not above all readings.
constraint_indexing:constraint_classification(colorblind_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorblind_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colorblind_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorblind_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(colorblind_reading, TR),
    TR >= 0.70.

:- end_tests(colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The colorblind rule coordinates a simple administrative protocol (race-neutral classification) while prohibiting the remedial mechanisms that would address historical subordination. Extractiveness is not high (0.70+) because the prohibition on classification is genuine and produces real constraints on state action, not a mere cover story. But extractiveness is not low (0.05) because the real function includes enabling neutral-seeming allocation that benefits privileged groups while preventing correction of subordination. The value reflects the hybrid: genuine coordination function (brightline rule) plus asymmetric extraction (subordinated groups locked out of remedial mechanisms). Suppression (0.48): Moderate-high. Barriers to exit include the constitutional prohibition itself (legal entrenchment), the difficulty of establishing alternative remedial pathways outside state action (private markets are also stratified), and the public narrative that colorblindness is the correct interpretation (cognitive capture). Suppression is not maximal (0.85+) because legislative routes to remedial classification still exist and because public opinion remains contested. Theater ratio (0.55): Moderate. The colorblind doctrine performs a commitment to equality and non-discrimination while operating in a context of deep racial stratification. The theater has increased over the measurement interval as the gap between formal colorblindness and substantive subordination has widened. Rising theater from 0.38 (1970s, when colorblind reading was newly ascendant) to 0.55 (2010s, after decades of colorblind enforcement with persistent racial gaps) indicates increasing gap between the doctrine's stated function (equality) and its effect (locked-in subordination).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Subordinated racial groups see pure extraction (Snare): the rule forbids the only mechanisms available to them for correction. Race-neutral administrators see coordination (Rope): the rule provides a clear protocol. Privileged groups see mixed coordination and extraction (Tangled Rope): they benefit from neutral allocation while being subject to the formal rule. Originalists see a temporary stabilization (Scaffold) pending stricter enforcement. The formal doctrine sees its own degradation (Piton): it maintains itself through circularity despite mounting evidence that neutral allocation perpetuates stratification. The analytical observer risks seeing an immutable principle (Mountain) — equal protection requires colorblindness — when the structural data reveals this as one reading of a contested kernel. The perspectival gap is not incidental to the constraint; it is constitutive. The colorblind reading exists BECAUSE observers disagree on what equal protection requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Subordinated groups (victims, trapped) experience maximum d ≈ 0.92, producing high f(d) ≈ 1.39. Race-neutral administrators (beneficiaries, arbitrage) experience minimum d ≈ 0.08, producing negative f(d) ≈ -0.17. Privileged groups (mixed beneficiary/victim, mobile) experience moderate d ≈ 0.55, producing moderate f(d) ≈ 0.65. The directionality spread reveals the constraint's asymmetry: it functions as coordination for those with exit options (administrators, legal elites) and extraction for those without (trapped subordinated groups). No override needed: the structural data itself generates the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit positionality. The colorblind reading is a Tangled Rope (genuine coordination function for administrators + asymmetric extraction for trapped groups) from the analytical position, NOT a Mountain. The false-summit perspective (seeing colorblindness as an immutable principle of equal protection) reveals the analytical observer's own positioning within one constitutional reading. The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that all six types are simultaneously true from different structural positions, and that the choice among readings is a constitutional and political decision, not a logical deduction. The constraint is a reading of the equal protection kernel, not equal protection itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contested_equal_protection,
    'Is the colorblind reading the definitive interpretation of equal protection, or one reading among structurally coherent alternatives?',
    'This constraint instantiates ONE reading (colorblind) of the contested kernel equal_protection_commitment. Sibling readings include remedial_reading (prioritizing correction of historical subordination) and antisubordination_reading (prioritizing dismantling status hierarchies). Each reading produces a different constraint with different ε, beneficiaries, and victims. The omega documents that this file represents only the colorblind instantiation, not the kernel itself.',
    'If colorblind reading is only one reading: the constraint''s false-summit mountain classification is explained by its positioning within the kernel. If colorblind is the unique defensible reading: the mountain classification becomes accurate and the sibling readings are errors. The engine cannot resolve this — it is the committer frame''s contribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contested_equal_protection, conceptual, 'Contested kernel: is colorblind the definitive equal protection reading or one reading among alternatives?').

omega_variable(
    remedial_efficacy_empirical,
    'What is the empirical relationship between remedial race-conscious classification (affirmative action, targeted allocation, equity programs) and reduction of cumulative racial disadvantage?',
    'Longitudinal studies of affirmative action beneficiaries vs non-beneficiaries; intergenerational wealth/income/health/educational attainment tracking; comparison to colorblind jurisdictions; analysis of whether facially neutral allocation has reduced racial stratification',
    'If remedial efficacy is high: the colorblind constraint''s suppression of remedial mechanisms increases the cost of the reading (ε moves toward 0.45+). If remedial efficacy is low or null: the colorblind reading''s extraction mechanism weakens and ε moves toward 0.20. This is an empirical gate on whether the remedial classification prohibition is protective or destructive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_efficacy_empirical, empirical, 'Empirical efficacy of remedial race-conscious classification in reducing cumulative disadvantage').

omega_variable(
    colorblindness_achievability,
    'Can state administrators and legal doctrine actually implement race-neutral decision-making, or does the colorblind prohibition simply relocate race-conscious effects from visibility to hidden correlation?',
    'Analysis of correlations between ostensibly race-neutral criteria (zip code, parental education, test scores, criminal history) and racial outcomes; documentation of implicit bias in facially neutral application; comparison of visible racial composition changes under colorblind vs remedial regimes',
    'If true colorblindness is achievable: ε remains moderate (constraint solves a real coordination problem). If colorblindness is illusory: ε increases (constraint functions as a mechanism to hide rather than eliminate racial subordination), and the snare classification becomes stronger across all victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_achievability, empirical, 'Whether race-neutral implementation is achievable or relocates race-conscious effects to hidden correlation').

omega_variable(
    individual_vs_structural_harm,
    'Does the colorblind reading''s prioritization of individual rights over group remediation adequately account for harm that is structurally cumulative across generations?',
    'Philosophical analysis of individual vs collective harm; documentation of whether individual-level equality coexists with group-level subordination; examination of whether the colorblind rule''s protection of ''innocent'' individuals prevents necessary structural correction',
    'If individual rights framing captures the essential harm: colorblind reading is justified; snare classification from subordinated group perspective is a false reading of the distribution. If structural/cumulative harm is primary: colorblind reading''s focus on individual classification misses the constraint it creates (trapping groups in accumulated disadvantage), and ε increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_vs_structural_harm, conceptual, 'Adequacy of individual rights framing vs structural group harm accounting').

omega_variable(
    reading_drift_in_doctrine,
    'Is the colorblind reading drifting toward stronger or weaker enforcement over time?',
    'Tracking Supreme Court doctrine on race classifications: Are scrutiny standards becoming stricter (strict scrutiny on all race-conscious action, including supposedly beneficent classifications)? Are exceptions narrowing? Have remedial classifications become harder to defend?',
    'If drift is toward stricter enforcement: suppression and theater increase, extractiveness rises (ε moves toward 0.45). If drift reverses: remedial space opens, snare aspects weaken, ε moves downward. Measurement trajectory is key diagnostic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_drift_in_doctrine, empirical, 'Temporal drift in colorblind doctrine enforcement strictness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colorblind_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colorblind_theater_1970s, colorblind_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(colorblind_theater_1990s, colorblind_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(colorblind_theater_2010s, colorblind_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(colorblind_extractiveness_1970s, colorblind_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(colorblind_extractiveness_1990s, colorblind_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(colorblind_extractiveness_2010s, colorblind_reading, base_extractiveness, 30, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(colorblind_reading, remedial_reading).
narrative_ontology:affects_constraint(colorblind_reading, antisubordination_reading).
narrative_ontology:affects_constraint(colorblind_reading, formal_neutrality_doctrine).
narrative_ontology:affects_constraint(colorblind_reading, affirmative_action_constitutional_status).

% DUAL FORMULATION NOTE:
% The colorblind reading is one instantiation of the equal_protection_commitment kernel. Sibling readings (remedial_reading, antisubordination_reading) represent alternative authoritative interpretations of the same constitutional text. Each reading produces a different constraint with different ε, beneficiaries, victims, and perspectival gaps. These are not observables of the same constraint but different constraints instantiated by different readings of the same kernel. The network shows that the colorblind reading's enforcement mechanism affects the possibility space for remedial and antisubordination interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
