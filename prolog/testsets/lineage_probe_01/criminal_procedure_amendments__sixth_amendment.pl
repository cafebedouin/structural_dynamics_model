% ============================================================================
% CONSTRAINT STORY: criminal_procedure_amendments__sixth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_procedure_amendments__sixth_amendment, []).

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
 *   constraint_id: criminal_procedure_amendments__sixth_amendment
 *   human_readable: Sixth Amendment Fair Trial Machinery
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Sixth Amendment guarantees six distinct procedural rights: the right
 *   to a speedy trial, public trial, jury of the state and district, notice
 *   of charges, confrontation of witnesses, and compulsory process for
 *   obtaining witnesses, all with the assistance of counsel. This constraint
 *   embodies a **reading of the criminal procedure kernel** that prioritizes
 *   fair trial machinery over state efficiency or victim restitution. The
 *   reading suppresses secret prosecution and counsel-less adjudication,
 *   making the targeted group (criminal defendants) beneficiaries of state
 *   restraint. The constraint exhibits a tension between coordination
 *   function (legitimate convictions require public adversarial testing) and
 *   extraction suppression (the machinery creates burden on prosecutorial
 *   capacity). Over the founding to contemporary period, extractiveness has
 *   accumulated as resource constraints (underfunded public defense, clogged
 *   dockets) have created a gap between formal rights and actual
 *   implementation. The theater ratio has similarly increased as plea
 *   bargaining and pretrial detention have made jury trial practically
 *   unreachable for most defendants, converting formal rights into
 *   performative options.
 *
 * KEY AGENTS:
 *   - Criminal Defendants: Primary beneficiary (powerless/trapped in snare view; moderate/constrained in tangled-rope view) — benefits from suppression of secret trials and counsel-less conviction, but constrained by resource barriers to exercising rights
 *   - Prosecutorial Authority: Primary actor (powerful/mobile) — powerful enough to deploy plea bargaining pressure; mobile exit through charge-stacking and offer structures; constrained by fair trial machinery requirements
 *   - Public Defender System: Secondary actor (institutional/constrained) — resource-constrained institution nominally responsible for implementing right to counsel; actual capacity lags formal mandate
 *   - Court System: Institutional actor (institutional/arbitrage) — manages trial machinery; benefits from legitimacy of public adjudication; arbitrage available through efficiency incentives (plea rates)
 *   - Prison Reform Coalition: Organized pressure (organized/constrained) — advocates for expanding fair trial implementation; constrained by limited political leverage
 *   - Summary Justice Interests: Victim set (institutional/mobile) — efficient quick resolution; victim of the Sixth Amendment's suppression of summary procedures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the constitutional mandate as a natural law of governance rather than a contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_procedure_amendments__sixth_amendment, 0.38).
domain_priors:suppression_score(criminal_procedure_amendments__sixth_amendment, 0.45).
domain_priors:theater_ratio(criminal_procedure_amendments__sixth_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_procedure_amendments__sixth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(criminal_procedure_amendments__sixth_amendment, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(criminal_procedure_amendments__sixth_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_procedure_amendments__sixth_amendment, tangled_rope).
narrative_ontology:human_readable(criminal_procedure_amendments__sixth_amendment, "Sixth Amendment Fair Trial Machinery").
narrative_ontology:topic_domain(criminal_procedure_amendments__sixth_amendment, "political/legal").

domain_priors:requires_active_enforcement(criminal_procedure_amendments__sixth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(criminal_procedure_amendments__sixth_amendment, '9b3bc38b-2d42-4662-a751-a7c658a3f285').
narrative_ontology:cs_kernel_codification('9b3bc38b-2d42-4662-a751-a7c658a3f285', formalized).
narrative_ontology:cs_authority_grounding('9b3bc38b-2d42-4662-a751-a7c658a3f285', lineage).
narrative_ontology:cs_interpretation_layer_present('9b3bc38b-2d42-4662-a751-a7c658a3f285').
narrative_ontology:cs_reading_relation('9b3bc38b-2d42-4662-a751-a7c658a3f285', criminal_procedure_amendments__fourth_amendment, influences).
narrative_ontology:cs_reading_relation('9b3bc38b-2d42-4662-a751-a7c658a3f285', criminal_procedure_amendments__fifth_amendment, influences).
narrative_ontology:cs_reading_relation('9b3bc38b-2d42-4662-a751-a7c658a3f285', criminal_procedure_amendments__eighth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('9b3bc38b-2d42-4662-a751-a7c658a3f285', criminal_procedure_amendments__seventh_amendment, coexists_with).
narrative_ontology:cs_axiom('9b3bc38b-2d42-4662-a751-a7c658a3f285', foundational, adversarial_testing_legitimates_conviction).
narrative_ontology:cs_axiom_status(adversarial_testing_legitimates_conviction, holdable).
narrative_ontology:cs_axiom_grounding('9b3bc38b-2d42-4662-a751-a7c658a3f285', adversarial_testing_legitimates_conviction, conventional).
narrative_ontology:cs_axiom('9b3bc38b-2d42-4662-a751-a7c658a3f285', foundational, secret_trial_produces_arbitrary_punishment).
narrative_ontology:cs_axiom_status(secret_trial_produces_arbitrary_punishment, holdable).
narrative_ontology:cs_axiom_grounding('9b3bc38b-2d42-4662-a751-a7c658a3f285', secret_trial_produces_arbitrary_punishment, deontological).
narrative_ontology:cs_reference_frame('9b3bc38b-2d42-4662-a751-a7c658a3f285', adversarial_fair_trial_standard).
narrative_ontology:cs_drift_state('9b3bc38b-2d42-4662-a751-a7c658a3f285', contemporary_resource_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9b3bc38b-2d42-4662-a751-a7c658a3f285', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(criminal_procedure_amendments__sixth_amendment, criminal_procedure_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__sixth_amendment, criminal_defendants).
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__sixth_amendment, due_process_advocates).
narrative_ontology:constraint_victim(criminal_procedure_amendments__sixth_amendment, summary_justice_interests).
narrative_ontology:constraint_victim(criminal_procedure_amendments__sixth_amendment, prosecutorial_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREPRESENTED DEFENDANT (SNARE) — Faces state power without counsel, publicity, or jury review. Maximum extraction: conviction becomes certain given the asymmetry. No exit available; trapped in adversarial process stripped of its protective machinery.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REPRESENTED DEFENDANT WITH CONSTRAINTS (TANGLED ROPE) — Benefits from counsel and jury availability (coordination function) but constrained by resource barriers (public defender caseloads, delays, plea pressure). High extraction but not total; some agency through adversarial process.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DUE PROCESS ADVOCATE INSTITUTION (ROPE) — Courts, legal defenders, judges committed to fair trial protections see the Sixth Amendment as solving the coordination problem: how to ensure legitimate conviction rather than expedient punishment. Net beneficiary through legitimacy and stability of convictions.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROSECUTORIAL AUTHORITY (TANGLED ROPE) — Powerful actor with mobile exit (discretion, charging decisions, plea offers). Benefits from coordination machinery (legitimate convictions have lower appeal rates and reversal risk). Constrained by discovery obligations, confrontation rights, and compulsory process requirements — genuine constraints but not total suppression. Extraction runs toward defendants; coordination function serves prosecutorial interest in stable convictions.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PRISON REFORM COALITION (SCAFFOLD) — Organized actors (innocence projects, bail reform advocates, public defender unions) see the Sixth Amendment as a framework for building alternative paths: reducing pretrial detention, improving counsel quality, expediting trials. View the constraint as temporary, with sunset through systemic reform. Limited active enforcement currently (many provisions underfunded), but structural mandate creates opening for pressure.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EFFICIENCY-MAXIMIZING SYSTEM (SNARE) — Public defender offices, court dockets, and prosecutorial resource constraints create a system where formal rights (speedy trial, counsel, jury) collide with resource scarcity. The system extracts guilty pleas by making the formal machinery prohibitively expensive to exercise. Constrained by constitutional mandate but functionally degraded.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational scope, fair trial procedures appear as a natural law of legitimate governance: any stable polity requires public adjudication by peers to prevent arbitrary punishment. This perspective sees the Sixth Amendment as discovering/codifying an invariant constraint. However, the structural data contradicts mountain classification — beneficiaries exist, suppression is measurable, and extractiveness flows through specific institutional arrangements. Engine will flag this as false summit.
constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_procedure_amendments__sixth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_procedure_amendments__sixth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(criminal_procedure_amendments__sixth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint suppresses the most efficient path to conviction (secret trial without counsel), forcing the state through more costly adversarial machinery. But extractiveness is not high because (1) the machinery still produces convictions at high rates (~95% plea acceptance), (2) the state retains significant power through charging, plea offers, and bail decisions, and (3) legitimate convictions have lower reversal risk, partially offsetting the procedural cost. The contemporary value (0.38) is higher than founding (0.22) because resource constraints have created a gap: formal rights exist but practical capacity to exercise them has not scaled. Suppression (0.45): Moderate-high. The constraint suppresses secret trials, counsel-less prosecution, and judge-only adjudication (where juries are waived or pressured). But suppression is not total because (1) the state retains charging discretion, plea authority, and bail power, (2) defendants face pressure through pretrial detention and case processing delays, and (3) the constraint applies only post-arrest (searches, interrogation, and charging remain subject to Fourth/Fifth Amendment regime). Theater ratio (0.55): Moderate-high. The right to jury trial has become substantially performative: jury trial rates have fallen to ~2-3% of cases, making the jury right an expensive option that pressures plea acceptance rather than a routinely exercised check. Counsel is guaranteed but often inadequate (public defenders handle 300+ cases annually in some jurisdictions). Speedy trial guarantees are frequently waived. The machinery persists through constitutional mandate but much of it is honored in the breach, creating a gap between formal procedure and actual implementation.
 *
 * PERSPECTIVAL GAP:
 *   The Sixth Amendment reading generates a wide perspectival gap. The powerless unrepresented defendant sees snare (total extraction through state power). The moderate represented defendant sees tangled rope (some agency through counsel but constrained by plea pressure). The due process institution sees rope (legitimate convictions stabilize the system). The prosecutorial authority sees tangled rope (constrained by fair trial machinery but retaining substantial power). The prison reform coalition sees scaffold with sunset (machinery can be strengthened through reform). The efficiency-maximizing system sees snare (resource scarcity extracts guilty pleas by making formal procedure prohibitively costly). The analytical observer risks seeing mountain (fair trial machinery as natural law of governance). The engine will flag the mountain perspective as a false summit because beneficiaries exist and extractiveness is measurable through institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from structural position. Defendants classified as beneficiaries (suppression of secret trials benefits them) but with trapped/constrained exit options (they cannot opt out of prosecution) produce high d values (0.80+), resulting in high experienced extraction χ from their perspective. Prosecutorial authority, while powerful, faces constrained exit (constitutional mandate is binding) and derives benefits from legitimate convictions, producing moderate d (~0.50-0.60). Due process institutions benefit from legitimacy and stability, producing low d (~0.20). The perspectival gap reflects real differences in structural power and extraction flow: those with no exit experience maximum extraction; those with power but binding constraints experience moderate extraction; those benefiting from the legitimacy function experience it as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANCE: This constraint is one reading of the contested criminal procedure kernel. The reading prioritizes fair trial machinery (adversarial testing, counsel, jury) as the core suppression mechanism against state overreach. This reading coexists with Fourth Amendment (search-seizure suppression), Fifth Amendment (self-incrimination + due process), Seventh Amendment (civil jury), and Eighth Amendment (proportionality) readings of the same kernel — each emphasizing different aspects of the anti-state-overreach project. The mandatrophy is resolved by recognizing that all five readings are live simultaneous commitments within the U.S. constitutional system. No single reading forecloses another; they are complementary specifications of different procedural checks. The tension between them (e.g., speedy trial vs. adequate counsel time) is not a contradiction to be resolved but a constitutive feature of the system: no single reading can be maximized without degrading the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_trial_legitimacy_mechanism,
    'Does the Sixth Amendment machinery function as a coordination device ensuring legitimate convictions, or as an extraction suppression mechanism protecting defendants from state overreach?',
    'Historical analysis of conviction appeal and reversal rates pre/post implementation; correlation between fair trial compliance and public confidence in legal system; counterfactual: would efficient secret trials produce equivalent legitimacy?',
    'If primarily coordination: most perspectives see Rope; constraint stabilizes system. If primarily suppression: most perspectives see Snare or Tangled Rope; constraint impedes efficient conviction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_trial_legitimacy_mechanism, conceptual, 'Whether Sixth Amendment functions as coordination or extraction suppression').

omega_variable(
    counsel_adequacy_threshold,
    'What quality and resource level of counsel constitutes ''effective assistance'' vs. performative compliance with right-to-counsel doctrine?',
    'Comparative study of public defender caseloads, preparation time, and trial outcomes across jurisdictions; correlation between counsel resources and conviction appeal rates; expert assessment of trial preparation adequacy',
    'If current public defense standards meet adequacy: Sixth Amendment machinery constrains extraction meaningfully. If standards are theater: constraint is largely degraded piton, not functional snare-suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counsel_adequacy_threshold, empirical, 'Threshold for adequate counsel vs. performative compliance').

omega_variable(
    jury_integrity_under_pressure,
    'Does the jury right function as an independent fact-finding body when combined with plea bargaining pressure and pretrial detention?',
    'Empirical study of jury trial rates over time; analysis of how plea offer structures correlate with jury unavailability; comparative jury conviction rates vs. judge conviction rates in same jurisdictions',
    'If jury functions as independent check: Sixth Amendment constraint is real constraint on prosecution. If jury is systematized option that pressures plea acceptance: jury right is theater masking plea-extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jury_integrity_under_pressure, empirical, 'Whether jury right functions as independent fact-finding vs. theater option').

omega_variable(
    reading_kernel_contest,
    'Which of the Bill of Rights amendments — Fourth, Fifth, Sixth, Seventh, Eighth — represents the foundational anti-state-overreach mechanism, and how do the others derive from or compete with that foundation?',
    'Historical analysis of founding intent; comparative constitutional jurisprudence across the five amendments; empirical analysis of which single amendment''s suppression is most effective at reducing wrongful conviction and arbitrary punishment',
    'If Sixth Amendment is foundational: Fifth Amendment''s self-incrimination protection and Fourth Amendment''s search-seizure limits are upstream. Eighth Amendment''s proportionality is downstream penalty check. If Fifth or Fourth is foundational: Sixth Amendment is derivative, creating litigation machinery to test Fourth/Fifth violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which amendment is foundational to criminal procedure checks').

omega_variable(
    speed_vs_adequacy_tradeoff,
    'How much of the Sixth Amendment''s extractiveness derives from the tension between ''speedy trial'' and ''adequate counsel time''? Can both be satisfied simultaneously at scale?',
    'Empirical analysis of trial duration vs. appeal/reversal rates; case study of jurisdictions with different speed/adequacy weightings; measurement of undefended vs. well-defended trials across speed categories',
    'If tradeoff is real and unsolvable: extractiveness will remain ≥0.38 regardless of enforcement intensity — the constraint is structurally incomplete. If tradeoff is artifact of underfunding: better resource allocation could reduce extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speed_vs_adequacy_tradeoff, empirical, 'Whether speed and adequacy requirements are structurally incompatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_procedure_amendments__sixth_amendment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sixth_amend_theater_founding, criminal_procedure_amendments__sixth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sixth_amend_theater_mid20th, criminal_procedure_amendments__sixth_amendment, theater_ratio, 50, 0.45).
narrative_ontology:measurement(sixth_amend_theater_contemporary, criminal_procedure_amendments__sixth_amendment, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(sixth_amend_extract_founding, criminal_procedure_amendments__sixth_amendment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sixth_amend_extract_mid20th, criminal_procedure_amendments__sixth_amendment, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(sixth_amend_extract_contemporary, criminal_procedure_amendments__sixth_amendment, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sixth_amend_suppress_founding, criminal_procedure_amendments__sixth_amendment, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sixth_amend_suppress_mid20th, criminal_procedure_amendments__sixth_amendment, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(sixth_amend_suppress_contemporary, criminal_procedure_amendments__sixth_amendment, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_procedure_amendments__sixth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_procedure_amendments__sixth_amendment, criminal_procedure_amendments__fourth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__sixth_amendment, criminal_procedure_amendments__fifth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__sixth_amendment, criminal_procedure_amendments__eighth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__sixth_amendment, plea_bargaining_pressure).
narrative_ontology:affects_constraint(criminal_procedure_amendments__sixth_amendment, public_defender_capacity).

% DUAL FORMULATION NOTE:
% The Sixth Amendment constraint models fair trial machinery as a unified specification. Decomposition into separate stories (counsel adequacy, jury integrity, speedy trial) is possible but not needed for DR classification — the three components exhibit similar extractiveness and suppression profiles. The constraint is downstream of Fourth/Fifth Amendment limitations on evidence admissibility and upstream of Eighth Amendment proportionality checks on punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(criminal_procedure_amendments__sixth_amendment, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
