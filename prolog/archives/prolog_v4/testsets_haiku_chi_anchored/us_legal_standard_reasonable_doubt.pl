% ============================================================================
% CONSTRAINT STORY: us_legal_standard_reasonable_doubt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_legal_standard_reasonable_doubt, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_legal_standard_reasonable_doubt
 *   human_readable: The US Legal Standard of Guilt Beyond a Reasonable Doubt in Political Prosecutions
 *   domain: political/legal
 *
 * SUMMARY:
 *   The standard of 'guilt beyond a reasonable doubt' is a constitutional
 *   protection designed to prevent state overreach in criminal prosecution.
 *   However, when applied to high-profile political defendants, the
 *   constraint exhibits a structural tension between its intended
 *   coordination function (protecting all citizens equally) and its actual
 *   extraction function (weaponized prosecutorial authority during periods of
 *   partisan dominance). This constraint models how a foundational legal
 *   principle can be simultaneously experienced as a guardrail, a theater of
 *   legitimacy, a coordination mechanism, an extractive tool, and an
 *   immutable constitutional mandate — depending on the observer's structural
 *   position relative to the prosecutorial and judicial machinery. The key
 *   insight is that the reasonable doubt standard is NOT a mountain — it is
 *   institutionally contingent and vulnerable to capture. Its perceived
 *   inevitability naturalizes what is actually a vulnerable institutional
 *   arrangement whose enforcement depends on the integrity of jurors,
 *   prosecutors, judges, and appellate courts. The theater ratio (0.68)
 *   reflects the gap between the standard's formal definition and its
 *   substantive application: jury instructions recite the reasonable doubt
 *   language, but in polarized political cases, jurors' political identity
 *   often predetermines verdict regardless of evidence.
 *
 * KEY AGENTS:
 *   - Political Defendant (Powerless/Trapped): High-profile defendant from disfavored political faction — bears costs of asymmetric prosecutorial resources, media narrative control, and jurors' political polarization. Cannot exit the judicial system.
 *   - Prosecutorial Authority (Institutional/Arbitrage): DOJ, state prosecutors with discretion in case selection and charging — benefits from reasonable doubt standard's appearance of legitimacy while exploiting its politicized application. Can exercise prosecutorial discretion (arbitrage exit).
 *   - Jury and General Public (Moderate/Constrained): Citizens who theoretically benefit from rule of law protections but are constrained by polarization making impartial judgment impossible. Both protected and harmed by the standard.
 *   - Judicial System (Institutional/Constrained): Courts maintain formal impartiality theater while substantive enforcement is degraded by partisanship. Constrained by appellate deference and political pressure.
 *   - Rule of Law Coalition (Organized/Constrained): Legal scholars, bar associations, good-government NGOs — see reasonable doubt standard as a temporary defense against legal weaponization. Can activate through amicus briefs, ethics enforcement, and appellate advocacy.
 *   - Opposition Political Coalition (Organized/Constrained): Minority party and opposition groups — benefit from rule-of-law norms as protection against their future prosecution, but currently harmed by selective enforcement. Constrained by institutional position.
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective risking naturalization of contingent institutional arrangements as constitutional necessities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, 0.58).
domain_priors:suppression_score(us_legal_standard_reasonable_doubt, 0.62).
domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_legal_standard_reasonable_doubt, tangled_rope).
narrative_ontology:human_readable(us_legal_standard_reasonable_doubt, "The US Legal Standard of Guilt Beyond a Reasonable Doubt in Political Prosecutions").
narrative_ontology:topic_domain(us_legal_standard_reasonable_doubt, "political/legal").

domain_priors:requires_active_enforcement(us_legal_standard_reasonable_doubt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, prosecutorial_authority).
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, political_faction_in_power).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, defendant_political_opponent).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, legal_system_impartiality).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, institutional_legitimacy_of_courts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL DEFENDANT (SNARE) — Faces prosecutorial machinery with asymmetric legal resources, media narrative control, and politicized jury selection. Cannot exit; bears full cost of politicized standard application. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CITIZEN JURY & PUBLIC (TANGLED ROPE) — Benefits from rule of law and protection by reasonable doubt standard; simultaneously constrained by political polarization making jury impartiality impossible. Standard provides coordination benefit (shared legal framework) and extraction cost (politicized application). d≈0.68, f(d)≈0.98, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTORIAL AUTHORITY (ROPE) — Experiences reasonable doubt standard as a coordination mechanism enabling legitimate prosecution. Has prosecutorial discretion (arbitrage exit); benefits from enforcement authority and political alignment incentives during periods of partisan dominance. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL SYSTEM RITUAL (PITON) — The reasonable doubt standard persists as an institutional guarantee, but its functional enforcement is degraded in high-stakes political cases. Theater ratio 0.68 reflects that jury deliberation in polarized cases is partly performative — jurors' political identity often predetermines verdict independent of evidence presentation. Institution maintains formal impartiality theater while substantive application is captured by partisanship. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RULE OF LAW COALITION (SCAFFOLD) — Organized legal scholars, NGOs, and institutional observers see the reasonable doubt standard as a temporary defense mechanism against political weaponization of law. The sunset logic: as courts face legitimacy crises from perceived partisanship, institutional actors (state bars, appellate courts, Supreme Court) may enforce stricter standards or require supermajority convictions in high-profile political cases. Coalition has agency through amicus briefs, professional ethics complaints, and appellate review. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPPOSITION POLITICAL COALITION (TANGLED ROPE) — Organized opposition sees reasonable doubt standard as both protection (guards against their own future prosecution when power shifts) and extraction (currently weaponized against them). Constrained by institutional position; benefits from rule-of-law norms while harmed by selective prosecution. d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL LAW (MOUNTAIN) — From a civilizational/universal vantage, the reasonable doubt standard is a constitutional-structural necessity: due process requires that the burden of proof remains on the prosecution. No democracy can function if defendants can be convicted on lower evidentiary standards in high-stakes cases. However, this perspectival reading is vulnerable to false summit detection: the structural data (ε=0.58, suppression=0.62, theater=0.68) indicates a contingent institutional constraint subject to politicized capture, not an immutable law.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_legal_standard_reasonable_doubt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, TR),
    TR >= 0.70.

:- end_tests(us_legal_standard_reasonable_doubt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The reasonable doubt standard, when applied in politicized prosecutions, enables extraction of political advantage through asymmetric prosecutorial discretion. Extraction is not total because appellate review, bar ethics, and judicial checks theoretically prevent unlimited abuse. The trajectory (0.32 → 0.58) reflects increasing politicization of the criminal justice system over the measurement interval. Suppression (0.62): Moderate-high. Defendant faces significant barriers to exit: cannot opt out of prosecution, faces asymmetric legal resources against state machinery, constrained by jury selection and media narrative control, and limited appellate remedies. However, suppression is not total — appellate review exists, media scrutiny operates, and some jurors resist partisan pressure. Theater ratio (0.68): High. The reasonable doubt standard is performed with formal rigor in jury instructions and trial proceedings, but its substantive application is degraded by jurors' political identities. The gap between formal procedure (careful jury instructions, evidence presentation) and actual verdict determination (often predetermined by juror political affiliation) defines the theater. Jury deliberation becomes a performance of impartiality rather than genuine application of an evidentiary standard. The trajectory (0.35 → 0.68) reflects degradation of institutional theater as polarization increases — the ritual persists but increasingly hollow.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The political defendant sees a snare (pure extraction, no escape). The jury sees tangled rope (theoretically protected by rule of law, actually constrained by polarization). The prosecutor sees rope (coordination mechanism enabling legitimate state authority, benefiting from prosecutorial discretion). The judicial system sees piton (formal procedure maintained through institutional theater, substantive function degraded). The rule-of-law coalition sees scaffold (temporary defensive mechanism, sunset through appellate enforcement). The opposition coalition sees tangled rope (protected by standard against future persecution, currently harmed by selective application). The analytical observer risks seeing mountain (constitutional necessity) but engine flags this as false summit given the structural data. No single classification captures the constraint's true nature across all observables.
 *
 * DIRECTIONALITY LOGIC:
 *   Political Defendant: Victim + trapped → d≈0.92, f(d)≈1.40. Nearly maximum extraction. Prosecutorial Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary (negative χ indicates benefit). Jury: Theoretically beneficiary, actually victim + constrained → d≈0.68, f(d)≈0.98. Significant extraction due to constrained exit (polarization prevents impartiality). Judicial System: Institutional + constrained → d≈0.45, f(d)≈0.50. Moderate extraction (institutional theater sustained but degraded). Rule-of-Law Coalition: Organized + constrained → d≈0.35, f(d)≈0.32. Low extraction; coalition has agency through institutional mechanisms. Opposition Coalition: Organized + constrained → d≈0.62, f(d)≈0.88. Moderate-high extraction (currently harmed, future protection uncertain). Analytical Observer: analytical → d≈0.72, f(d)≈1.15. False summit detection applies.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that extractiveness (0.58) exceeds the pure-coordination threshold (0.35) while remaining below the pure-extraction threshold (0.66), placing it precisely in the tangled-rope zone. The mandate is: the reasonable doubt standard provides genuine coordination benefits (protects all defendants, theoretical rule of law) AND genuine extraction costs (weaponized against disfavored political defendants, suppresses appeal options). The apparent contradiction — 'how can a protective constitutional standard be extractive?' — resolves when we recognize that institutional capture transforms protective mechanisms into extraction tools. The standard itself is innocent; its capture is the extraction. Mandatrophy_resolved = true because we have identified the dual function explicitly: (1) Coordination: shared legal framework protecting all citizens, legitimate burden of proof allocation. (2) Extraction: selective enforcement by partisan prosecutors, juror political polarization preventing impartial application, appellate deference limiting correction. The mechanism preventing mislabeling as pure rope is the presence of victims (defendant, legal impartiality, institutional legitimacy) and suppression (constrained appeal options, media narrative control). The mechanism preventing mislabeling as pure snare is the genuine coordination function (rule of law standard genuinely protects many defendants) and the scaffold sunset (appellate enforcement and rule-of-law coalition activity can constrain future capture). This is a true tangled rope: asymmetric extraction (victims) layered onto a coordination mechanism (shared legal framework).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jury_impartiality_measurement,
    'Can jury impartiality be meaningfully achieved in high-profile political prosecutions, or does political polarization render the reasonable doubt standard''s coordination function impossible?',
    'Psychological studies of juror bias in political cases; jury selection data and voir dire transcripts; post-conviction interviews with jurors; comparison of conviction rates by juror political affiliation',
    'If achievable: tangled rope classification is accurate (mixed coordination and extraction). If impossible: constraint degrades to pure snare (extraction with zero coordination) from jury perspective; scaffold sunset becomes essential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jury_impartiality_measurement, empirical, 'Whether jury impartiality is achievable in politicized prosecutions').

omega_variable(
    evidence_sufficiency_standard_shift,
    'Has the evidentiary threshold for ''reasonable doubt'' shifted over time in political vs non-political prosecutions, or is the standard genuinely uniform?',
    'Content analysis of jury instructions across decades; comparison of conviction rates for similar evidence quality in political vs non-political cases; appellate decision language analyzing evidence sufficiency; jury deliberation data where available',
    'If shifted: reasonable doubt standard is a piton with theater increasing (false uniformity). If uniform: constraint is rope or tangled rope depending on institutional capture. Shift detection signals degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_sufficiency_standard_shift, empirical, 'Whether reasonable doubt evidentiary threshold has shifted in political cases').

omega_variable(
    prosecutorial_discretion_asymmetry,
    'Does prosecutorial discretion in case selection, charging decisions, and plea bargain offers vary systematically by defendant political alignment, or is selection process neutral?',
    'Statistical analysis of charging patterns across defendants; comparison of cases brought vs cases declined; plea bargain offer analysis; DOJ prosecution data disaggregated by defendant political affiliation and prosecutor party affiliation',
    'If asymmetric: extractive component (ε term) is confirmed; victims include all defendants in disfavored political category. If neutral: constraint approaches pure rope. Asymmetry directly measures suppression of alternatives (constrained exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prosecutorial_discretion_asymmetry, empirical, 'Whether prosecutorial discretion varies by defendant political alignment').

omega_variable(
    appellate_correction_effectiveness,
    'Do appellate courts effectively overturn convictions on evidentiary grounds when reasonable doubt standard was violated, or does appellate deference to jury verdicts prevent correction?',
    'Analysis of appellate decision outcomes; comparison of reversal rates across case types; judicial opinion language on appellate standard of review; post-reversal outcomes if retrial occurs',
    'If effective: scaffold perspective is real (appellate review provides sunset mechanism). If ineffective: constraint is piton (formal appeal ritual is theater). Determines whether rule-of-law coalition has real enforcement power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_correction_effectiveness, empirical, 'Whether appellate review effectively corrects reasonable doubt violations').

omega_variable(
    institutional_legitimacy_threshold,
    'What level of perceived prosecutorial partisanship triggers institutional response (bar complaints, judicial ethics enforcement, congressional oversight) that constrains future politicized prosecutions?',
    'Tracking of bar ethics complaints; appellate court opinion frequency and tone; congressional oversight hearings; judicial recusal rates; public confidence surveys in justice system',
    'If threshold is low: rule-of-law coalition activates early, scaffold sunset accelerates. If threshold is high: system tolerates significant capture; snare classification becomes dominant even from institutional perspectives. Determines whether democratic guardrails remain intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_legitimacy_threshold, preference, 'Institutional threshold for responding to perceived prosecutorial partisanship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_legal_standard_reasonable_doubt, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbd_theater_t0, us_legal_standard_reasonable_doubt, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gbd_theater_t2, us_legal_standard_reasonable_doubt, theater_ratio, 2, 0.55).
narrative_ontology:measurement(gbd_theater_t4, us_legal_standard_reasonable_doubt, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(gbd_extract_t0, us_legal_standard_reasonable_doubt, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gbd_extract_t2, us_legal_standard_reasonable_doubt, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(gbd_extract_t4, us_legal_standard_reasonable_doubt, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_legal_standard_reasonable_doubt, enforcement_mechanism).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, prosecutorial_discretion_asymmetry).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, jury_polarization_verdict_determination).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, appellate_review_deference).

% DUAL FORMULATION NOTE:
% The reasonable doubt standard as a constitutional principle (high accessibility, low extractiveness) should be decomposed from the reasonable doubt standard as applied in politicized prosecutions (lower accessibility, higher extractiveness). The constitutional principle approaches mountain/rope; the application in partisan contexts is tangled rope/snare. Upstream constraint: constitutional due process (ε ≈ 0.05, Mountain). Downstream constraint: reasonable doubt in political prosecutions (ε = 0.58, Tangled Rope). The divergence reflects institutional capture, not change in the standard itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_legal_standard_reasonable_doubt, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
