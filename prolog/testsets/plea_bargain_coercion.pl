% ============================================================================
% CONSTRAINT STORY: plea_bargain_coercion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plea_bargain_coercion, []).

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
 *   constraint_id: plea_bargain_coercion
 *   human_readable: Plea Bargain Coercion in Criminal Justice Systems
 *   domain: legal/criminal_justice
 *
 * SUMMARY:
 *   Plea bargaining in modern criminal justice systems creates a structural
 *   coercion mechanism where defendants face asymmetric choice: accept a
 *   guaranteed reduced sentence via guilty plea or face trial with risk of
 *   far harsher penalty, often while detained pretrial without resources for
 *   adequate defense. This constraint exhibits characteristics of a snare
 *   (pure extraction with high suppression) from the defendant's perspective
 *   but appears as coordination (efficient case management) from the
 *   prosecution's perspective and as a degraded but necessary system (piton)
 *   from judicial administration. The extractiveness has increased over the
 *   measurement interval as sentence severity has increased, making the trial
 *   penalty more extreme relative to plea offers. Theater ratio has risen as
 *   the performative aspects of 'voluntary and knowing' plea advisements have
 *   become more ritualistic despite known systemic coercion. The constraint
 *   is a diagnostic case for how institutional efficiency requirements can
 *   generate extraction mechanisms that appear coordinated but are
 *   fundamentally asymmetric.
 *
 * KEY AGENTS:
 *   - Defendants: Primary victim (powerless/trapped) — structurally coerced into confession; bear maximum extraction
 *   - Prosecution: Primary beneficiary (institutional/arbitrage) — discretion over charging, discovery, and sentencing recommendations; case resolution efficiency
 *   - Public Defense Counsel: Secondary victim (moderate/constrained) — caseload pressures and underfunding force inadequate representation; constrained by systemic limits
 *   - Criminal Justice Administration: Institutional actor (institutional/arbitrage) — maintains system for case-volume management; benefits from plea rate predictability
 *   - Criminal Justice Reform Movement: Organized agents (organized/mobile) — innocence projects, bail reform coalitions, sentencing reform advocates building alternative mechanisms
 *   - Systemic Justice: Abstract victim (powerless/trapped) — false convictions, inadequate fact-finding, erosion of trial rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plea_bargain_coercion, 0.68).
domain_priors:suppression_score(plea_bargain_coercion, 0.75).
domain_priors:theater_ratio(plea_bargain_coercion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plea_bargain_coercion, extractiveness, 0.68).
narrative_ontology:constraint_metric(plea_bargain_coercion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(plea_bargain_coercion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plea_bargain_coercion, snare).
narrative_ontology:human_readable(plea_bargain_coercion, "Plea Bargain Coercion in Criminal Justice Systems").
narrative_ontology:topic_domain(plea_bargain_coercion, "legal/criminal_justice").

domain_priors:requires_active_enforcement(plea_bargain_coercion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plea_bargain_coercion, prosecution).
narrative_ontology:constraint_beneficiary(plea_bargain_coercion, courts).
narrative_ontology:constraint_beneficiary(plea_bargain_coercion, criminal_justice_administration).
narrative_ontology:constraint_victim(plea_bargain_coercion, defendants).
narrative_ontology:constraint_victim(plea_bargain_coercion, systemic_justice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENDANT (SNARE) — Structurally trapped with no meaningful exit. Faces severe extraction: accepting a plea deal with guaranteed punishment vs. trial with risk of harsher sentence creates asymmetric risk. Suppression is extreme: pretrial detention, inadequate public defense, limited resources to mount defense, and the crushing weight of potential sentences (mandatory minimums, habitual offender enhancements). The system's architecture coerces confession regardless of guilt. Maximum experienced extraction.
constraint_indexing:constraint_classification(plea_bargain_coercion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSECUTION (ROPE) — Benefits from the plea bargain constraint as a coordination mechanism. Resources are scarce; trials consume vastly more time and expense than plea processing. The constraint solves a genuine collective action problem: without plea incentives, the court system would collapse under caseload. From the prosecution's structural position, the plea bargain is experienced as efficient coordination — burden-sharing, workload management, predictable outcomes. The extraction toward prosecution is net positive due to arbitrage (discretion over charging and sentencing recommendations).
constraint_indexing:constraint_classification(plea_bargain_coercion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC DEFENSE COUNSEL (TANGLED ROPE) — Constrained by caseload, time, and funding. The plea bargain system both enables and extracts from this actor: enables by reducing the burden of preparing for trial (genuine coordination function), but extracts by forcing impossible choices between adequate representation and systemic capacity limits. Counsel benefits from plea efficiency but victimized by systemic underfunding. Mixed experience: genuine coordination with asymmetric extraction layered beneath.
constraint_indexing:constraint_classification(plea_bargain_coercion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENT (SCAFFOLD) — Organized actors (innocence projects, bail reform coalitions, sentencing reform advocates) perceive the plea bargain constraint as a temporary problem with structural sunset: conviction based on actual guilt (not coerced confession), speedy trial guarantees, resource adequacy, and transparent discovery would reduce reliance on plea coercion. Low effective extraction because organized agents see an exit path and are building alternative verification (post-conviction exoneration data showing plea coercion harms). Theater low relative to snare perspectives because reform logic is explicit about the dysfunction.
constraint_indexing:constraint_classification(plea_bargain_coercion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL ADMINISTRATION (PITON) — The system that emerged to solve the trial bottleneck has become largely performative. Plea bargains are supposed to be voluntary and knowing, but the theater of the advisement (reading constitutional rights, asking about coercion) is ritual that does not prevent structural coercion. The system sees its own process as degraded — maintaining the ritual despite knowing the voluntary consent fiction is necessary because alternatives haven't fully replaced it. Theater ratio high (0.58 measured at system level) reflects that much of the 'choice' presented is performative.
constraint_indexing:constraint_classification(plea_bargain_coercion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the argument is made that some form of plea accommodation is inherent to any legal system: finite resources mean not all cases can go to trial; efficiency pressures are universal; hence plea incentives are immutable features of justice administration. However, this perspective naturalizes what is actually a contingent institutional choice: plea coercion is not inherent to justice but rather to a specific architecture (adversarial, resource-constrained, case-volume-driven). The engine's false summit detector will identify this as naturalization of policy choice.
constraint_indexing:constraint_classification(plea_bargain_coercion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plea_bargain_coercion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plea_bargain_coercion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plea_bargain_coercion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plea_bargain_coercion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(plea_bargain_coercion, TR),
    TR >= 0.70.

:- end_tests(plea_bargain_coercion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The baseline extraction arises from the sentence differential between plea and trial outcomes (trial penalty averages 15-25 years longer in federal system depending on offense). This differential is not proportional to case complexity or prosecution cost-savings — it is designed to incentivize plea acceptance. Over the measurement interval (30-year period covering expansion of mandatory minimums and habitual offender enhancements), the trial penalty has grown, making the coercion more severe. Theater ratio (0.58): Moderate-high. The 'knowing and voluntary' ritual is performative — judges ask standardized questions, defendants answer scripted responses, but the structural coercion remains invisible. Theater has increased as reform rhetoric has created more elaborate advisements (discussing trial rights, explaining consequences) while the underlying asymmetry has worsened. Suppression (0.75): Very high. Multiple suppression mechanisms operate simultaneously: pretrial detention, inadequate counsel, discovery limitations, bail bondsmen leverage, and mandatory minimum penalties. Suppression is not a single barrier but a system of cascading barriers that make exit (defending at trial) extremely costly.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and structural. The defendant sees pure extraction (Snare) — a system designed to coerce confession. The prosecution sees coordination (Rope) — efficient case management solving a genuine collective action problem (trial backlogs would destroy the system if all cases went to trial). The public defense counsel sees mixed coordination and extraction (Tangled Rope) — the plea system reduces their workload (genuine coordination) while constraining them to inadequate representation (extraction). The reform movement sees a temporary problem with sunset logic (Scaffold) — bail reform, speedy trial guarantees, discovery rules, and resource adequacy could reduce reliance on coercion. The judicial administration sees its own degraded ritual (Piton) — performing the role of neutral arbiter while managing a system that is fundamentally coercive. The civilizational observer risks seeing immutable constraints (Mountain) — 'all legal systems need plea mechanisms' — but structural data reveals this as false naturalization. The snare perspective is not perceptual bias; it is the accurate reading of the constraint's structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural relationship to extraction flow. Defendants are pure targets: trapped with no exit, bearing maximum extraction through coerced confession. Prosecution is pure beneficiary: arbitrage exit (can dismiss charges, negotiate, recommend sentences) and institutional power mean they experience negative effective extraction — the system subsidizes their workload. Public defense counsel is intermediate: constrained exit (cannot refuse overloaded caseload), mixed victim/beneficiary status (harmed by underfunding, benefited by plea efficiency reducing preparation burden). Reform movement is organized with mobile exit — they are not embedded in the coercive mechanism, they are external organizers building alternatives. The directional asymmetry is stark: the constraint extracts from defendants to benefit prosecutors, with judicial administration maintaining the structure and public defense trapped in the middle.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLUTION: The mandatrophy here is the tension between 'this is just how the justice system works' (coordination framing, mountain view) and 'this is systemic coercion of the innocent' (snare view). The data resolves the mandatrophy toward Snare: (1) extractiveness is high (0.68) and increasing; (2) suppression is very high (0.75) with multiple mechanisms; (3) beneficiary/victim asymmetry is stark (prosecution benefits, defendants victimized); (4) innocent plea rate is non-trivial (innocence omega variable documents this); (5) the trial penalty is designed to coerce, not to fairly price trial risk. The coordination framing (prosecution side) is genuine but does not negate the extraction framing (defendant side) — this is exactly the structure of Snare: high extractiveness with coordination framing as cover. The 'immutable to all legal systems' argument is false naturalization: other countries with adequate resources sustain higher trial rates without systemic collapse, showing the coercion is a policy choice, not a law of nature. Mandatrophy resolved: this is a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_consent_fiction,
    'At what point does risk-asymmetry in trial vs. plea outcome become so severe that ''knowing and voluntary'' consent is a legal fiction?',
    'Empirical comparison of actual sentences received via plea vs. trial outcomes; controls for offense severity, prior record, and defendant characteristics; measurement of threshold where trial premium becomes coercive',
    'If threshold < 5 years sentence differential: majority of pleas are coerced by definition. If threshold > 15 years: many legitimate efficiency-based pleas are misclassified as coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_consent_fiction, empirical, 'Threshold at which trial risk premium becomes unconstitutional coercion').

omega_variable(
    adequacy_of_counsel_definition,
    'What level of public defense resources constitutes ''adequate'' counsel sufficient to make plea decision truly informed vs. coerced by inadequacy?',
    'Controlled comparison of outcomes in well-resourced vs. under-resourced defense jurisdictions; measurement of time counsel spends on case investigation, discovery review, and plea negotiation; post-conviction audit of whether counsel adequately explained alternatives',
    'If threshold is high: most current plea decisions are coerced by inadequate counsel. If threshold is low: under-resourcing is normalized as inherent limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_of_counsel_definition, empirical, 'Resource threshold for adequate counsel in plea negotiation context').

omega_variable(
    pretrial_detention_mechanism,
    'How much of the plea coercion is attributable specifically to pretrial detention vs. sentence risk differential vs. other systemic factors?',
    'Quasi-experimental comparison of plea rates before/after bail reform; stratified analysis of plea decision timing (plea at detention hearing vs. later); measurement of sentence reduction per day of pretrial detention avoided',
    'If pretrial detention drives majority of coercion: bail reform alone could substantially reduce snare classification. If distributed across multiple factors: multifaceted reform needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pretrial_detention_mechanism, empirical, 'Causal attribution of plea coercion to pretrial detention mechanism').

omega_variable(
    system_collapse_necessity,
    'Would elimination of plea coercion mechanisms actually require trial system collapse, or could adequate resourcing sustain higher trial rates?',
    'Historical analysis of trial rates in different resource regimes; hypothetical modeling based on other countries'' trial rates; measurement of what conviction-by-trial rate is achievable with different funding levels',
    'If system would not collapse: plea coercion is unnecessary extraction, not coordination. If collapse is real: snare is constrained by genuine systemic limits and reform requires major resource investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_collapse_necessity, empirical, 'Whether adequate trial access would require system resource expansion beyond current levels').

omega_variable(
    innocence_risk_in_pleas,
    'What proportion of plea bargain acceptances involve actually innocent defendants?',
    'DNA exoneration data; innocence project case analysis; studies of false confession patterns; measurement of innocent plea rate vs. guilty plea rate for same offense',
    'If innocence rate > 5%: coercion is extracting false confessions at scale, degrading systemic justice (victim in base_properties). If < 1%: pleas may be mostly accurate despite coercive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innocence_risk_in_pleas, empirical, 'Rate of innocent defendants accepting plea bargains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plea_bargain_coercion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plbg_tr_t0, plea_bargain_coercion, theater_ratio, 0, 0.4).
narrative_ontology:measurement(plbg_tr_t10, plea_bargain_coercion, theater_ratio, 10, 0.5).
narrative_ontology:measurement(plbg_tr_t20, plea_bargain_coercion, theater_ratio, 20, 0.58).
narrative_ontology:measurement(plbg_tr_t30, plea_bargain_coercion, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(plbg_be_t0, plea_bargain_coercion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(plbg_be_t10, plea_bargain_coercion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(plbg_be_t20, plea_bargain_coercion, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(plbg_be_t30, plea_bargain_coercion, base_extractiveness, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plea_bargain_coercion, enforcement_mechanism).
narrative_ontology:affects_constraint(plea_bargain_coercion, mandatory_minimum_sentencing).
narrative_ontology:affects_constraint(plea_bargain_coercion, bail_system_pretrial_detention).
narrative_ontology:affects_constraint(plea_bargain_coercion, public_defense_underfunding).

% DUAL FORMULATION NOTE:
% Plea bargain coercion is downstream of and structurally coupled to three upstream constraints: mandatory minimums (which increase the trial penalty), pretrial detention (which suppresses the defendant's negotiating position), and public defense underfunding (which limits the defendant's capacity to prepare for trial). Each constraint has its own extractiveness; together they create the coercion cascade. The plea bargain story models the integrated effect; upstream stories model the individual mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plea_bargain_coercion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
