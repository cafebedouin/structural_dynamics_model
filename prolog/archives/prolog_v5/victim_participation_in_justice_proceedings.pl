% ============================================================================
% CONSTRAINT STORY: victim_participation_in_justice_proceedings
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_victim_participation_in_justice_proceedings, []).

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
 *   constraint_id: victim_participation_in_justice_proceedings
 *   human_readable: Victim Participation in Justice Proceedings
 *   domain: legal/criminal_justice/victim_rights
 *
 * SUMMARY:
 *   Victim participation in justice proceedings sits at the intersection of
 *   victim rights advocacy, due process protection, prosecution incentives,
 *   and institutional legitimacy. Over the past 40 years (interval 0–20),
 *   victim participation has evolved from marginal procedural accommodation
 *   (victim rights movement begins, 1980s) toward formal statutory rights
 *   (victim impact statements, notification procedures, participation
 *   consultation) while remaining substantively limited in decision-making
 *   authority. The constraint exhibits extraction (prosecution monopoly over
 *   victim narratives, retraumatization costs borne by victims) alongside
 *   genuine coordination (victim testimony provides evidentiary reliability,
 *   victim input can influence sentencing outcomes, advocacy organizations
 *   provide support services). The theater ratio has risen over the interval
 *   (0.32→0.58) reflecting that victim participation structures have become
 *   increasingly performative: victims have more 'voice' in procedural stages
 *   but less agency in strategic prosecution decisions. The extractiveness
 *   has risen (0.28→0.52) as prosecution-side benefits (credible witnesses,
 *   legitimacy performance, sentencing leverage) have been institutionalized
 *   without corresponding victim agency expansion. This pattern is diagnostic
 *   of Tangled Rope degrading toward Snare: genuine coordination (victim
 *   testimony) becoming asymmetric extraction (victim narrative captured by
 *   state apparatus).
 *
 * KEY AGENTS:
 *   - Crime Victims: Primary victims (powerless/trapped) — bear participation costs (retraumatization, schedule burden, narrative displacement) with minimal control over case strategy or outcome
 *   - State Prosecution: Primary beneficiary (institutional/arbitrage) — captures evidentiary material, legitimacy performance, and sentencing leverage from victim participation; low friction
 *   - Defense Counsel: Secondary institutional actor (institutional/constrained) — both benefits from victim testimony as evidentiary material and faces constraint from victim protection procedures; mixed position
 *   - Victim Advocacy Organizations: Secondary beneficiary (moderate/constrained) — coordinate support services and exercise advocacy within the system; funding and legitimacy depend on victim case volume
 *   - Victim Rights Movement: Organized reform movement now degraded (organized/constrained) — maintains procedural rights through inertia; original transformative vision (victim agency in prosecution) largely unrealized
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing adversarial system tensions as immutable when comparative systems demonstrate contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(victim_participation_in_justice_proceedings, 0.52).
domain_priors:suppression_score(victim_participation_in_justice_proceedings, 0.68).
domain_priors:theater_ratio(victim_participation_in_justice_proceedings, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(victim_participation_in_justice_proceedings, extractiveness, 0.52).
narrative_ontology:constraint_metric(victim_participation_in_justice_proceedings, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(victim_participation_in_justice_proceedings, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(victim_participation_in_justice_proceedings, tangled_rope).
narrative_ontology:human_readable(victim_participation_in_justice_proceedings, "Victim Participation in Justice Proceedings").
narrative_ontology:topic_domain(victim_participation_in_justice_proceedings, "legal/criminal_justice/victim_rights").

domain_priors:requires_active_enforcement(victim_participation_in_justice_proceedings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(victim_participation_in_justice_proceedings, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(victim_participation_in_justice_proceedings, victim_advocacy_organizations).
narrative_ontology:constraint_victim(victim_participation_in_justice_proceedings, crime_victims).
narrative_ontology:constraint_victim(victim_participation_in_justice_proceedings, justice_system_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CRIME VICTIM (SNARE) — Trapped by legal obligation to participate, emotional investment in case outcome, and lack of alternative justice mechanisms. Bears full cost of participation (retraumatization during testimony, schedule disruption, witness preparation burden) while retaining minimal control over prosecution strategy or outcome. Cannot exit: testimony is often compulsory, victim impact statements are procedurally constrained, and alternative restorative justice mechanisms remain marginal. Maximum experienced extraction.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE VICTIM ADVOCATE (TANGLED ROPE) — Constrained by resource limitations, professional licensing, and reliance on prosecution infrastructure, but also coordinates genuine support services and exercises meaningful advocacy within the system. Benefits from victim participation structures (advocacy role legitimacy, funding dependent on victim case volume) while also bearing costs (secondary trauma, inadequate resources for full victim support). Significant extraction alongside real coordination function.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE STATE PROSECUTION (ROPE) — Benefits substantially from victim participation: credible witnesses increase conviction rates, victim testimony satisfies public legitimacy demands, victim impact statements influence sentencing (expanding prosecution leverage). Experiences the constraint as coordination: victim participation solves the legitimate problem of ensuring witness testimony and establishing harm narrative. Net beneficiary with low friction — can arbitrage between victim narratives and prosecution objectives.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE DEFENSE COUNSEL (TANGLED ROPE) — Constrained by evidentiary rules and cross-examination requirements that limit interaction with victim-witnesses, but also benefits from victim testimony (evidentiary material for defensive strategy, opportunity for credibility challenges). Coordination function: adversarial testing of victim narrative ensures trial reliability. Extraction component: victim testimony can be retraumatizing during cross-examination without sufficient procedural protection. Mixed experience.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE VICTIM RIGHTS MOVEMENT (PITON) — Once a genuine reform movement (1980s–2000s) creating victim advocacy services and rebalancing courtroom participation, now substantially degraded into theater: 'victim participation' rituals (impact statements, status updates) that provide symbolic inclusion without material change in prosecution strategy or outcome. Victim rights are maintained through institutional inertia and public legitimacy performance rather than functional transformation of the justice system. Theater ratio high because the participatory rights do not translate to decisional power.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / ADVERSARIAL SYSTEM VIEW (MOUNTAIN) — From a civilizational analytical position, victim participation tension with adversarial due process is inherent to the structure of criminal justice: the right to cross-examine witnesses (necessary for defendant protection) necessarily creates retraumatization risk for victim-witnesses; the state's monopoly on prosecution (necessary to prevent private revenge) necessarily displaces victim agency. The constraint appears as an immutable feature of adversarial systems. However, comparative systems (inquisitorial, restorative justice models) demonstrate that the tension is contingent, not universal. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(victim_participation_in_justice_proceedings_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(victim_participation_in_justice_proceedings, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(victim_participation_in_justice_proceedings, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(victim_participation_in_justice_proceedings, TR),
    TR >= 0.70.

:- end_tests(victim_participation_in_justice_proceedings_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The constraint extracts victim agency (prosecution strategy determined without victim input), victim emotional labor (retraumatization during cross-examination), and victim narrative control (state frames victim's story for legitimacy purposes). The extraction is not maximal because victim testimony provides genuine evidentiary value and some victim input (impact statements) does influence outcomes. The rise from 0.28 to 0.52 reflects accumulation of procedural rights that lack material decision-making power — theater grows while coordination mechanisms stagnate. Suppression (0.68): High. Victims are legally obligated to participate (compulsory testimony), face significant retraumatization costs, have no viable exit alternative (alternative justice mechanisms remain marginal), and operate within prosecution-controlled procedural structure. Suppression includes both structural barriers (legal obligation, alternative inadequacy) and internalized factors (victim identity fusion with case outcome, belief that participation is necessary for justice). Theater Ratio (0.58): Moderate-high. Victim rights procedures (impact statements, notification, participation consultation) are substantially performative: they provide symbolic voice without substantive decision-making power. The original reform vision (victim agency in prosecution) has degraded into ritual participation confirming legitimacy. Claimed Type: Tangled Rope. The constraint coordinates genuine functions (victim testimony ensures evidentiary reliability, victim advocacy provides support services) alongside asymmetric extraction (prosecution captures narrative and agency, victims bear retraumatization cost). Requires enforcement (compulsory testimony) and has both beneficiaries (prosecution, advocacy orgs) and victims (crime victims, justice legitimacy).
 *
 * PERSPECTIVAL GAP:
 *   Crime victims perceive snare (trapped, high extraction, no exit) while state prosecution perceives rope (coordination problem solved, low friction, beneficiary position). This gap is the constraint's diagnostic signature. Victim advocates perceive tangled_rope (genuine services alongside resource constraints and secondary trauma). The victim rights movement perceives piton (original reform function degraded to theater, maintained by institutional inertia and public legitimacy performance). Defense counsel perceive tangled_rope with the extraction vector reversed — they benefit from victim testimony (evidentiary material) while also constrained by victim protection procedures. The analytical observer risks perceiving mountain (adversarial tension inherent to justice systems) but comparative analysis reveals contingency: inquisitorial and restorative systems show different victim participation structures with different extraction profiles. The false summit reveals that 'inherent to adversarial systems' naturalizes institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Crime victims as powerless/trapped agents experience maximum directionality (d≈0.95), producing high effective extraction χ. Their structural position yields low f(d)→high χ because they cannot exit and bear full participation costs. State prosecution as institutional/arbitrage agents experience low directionality (d≈0.10), producing low or negative χ — they are net beneficiaries with high exit flexibility. Victim advocates as moderate/constrained agents occupy an intermediate position (d≈0.55) reflecting mixed beneficiary/victim status — they benefit from victim case volume but also bear secondary trauma and resource constraints. The perspectival gap between prosecution (rope experience, d≈0.10, χ negative) and victim (snare experience, d≈0.95, χ high) is maximal, revealing the asymmetric extraction. The piton perspective (organized/constrained) reflects institutional inertia where the victim rights movement has lost transformative function but persists through legitimacy performance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that victim participation cannot be reduced to a single type. The state prosecution's rope perspective (coordination function: witness testimony reliability) is structurally real but incomplete — it describes only the prosecution's structural position. The victim's snare perspective (high extraction, no exit) is also structurally real, describing what the same constraint looks like from below. The tangled_rope classification is the analytical resolution: the constraint does coordinate (victim testimony improves evidentiary reliability) while also extracting (prosecution controls victim narrative, victims bear retraumatization cost). The theater ratio rising over time (0.32→0.58) while extractiveness also rises (0.28→0.52) indicates Goodhart drift: procedural rights (victim notifications, impact statements) have become proxy measures of victim participation without reflecting actual agency expansion. This pattern distinguishes a genuine tangled_rope (where coordination and extraction remain in structural tension) from a degraded institutional ritual (piton) where procedural form persists while functional participation has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_agency_vs_witness_function,
    'Can victim participation serve both as witness testimony (extraction mechanism) and as genuine agency in prosecution decisions, or are these structurally incompatible?',
    'Comparative analysis of jurisdictions with victim veto power, victim prosecution authority, and victim impact weighting in plea negotiations. Measurement of case outcomes where victim preferences diverged from prosecution strategy.',
    'If incompatible: victim participation is inherently tangled_rope/snare with extraction unavoidable. If compatible: the constraint could shift toward pure coordination (rope). Current evidence suggests incompatible due to state monopoly on prosecution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_agency_vs_witness_function, empirical, 'Whether victim agency and witness function can coexist or are structurally incompatible').

omega_variable(
    retraumatization_necessity,
    'Is retraumatization from cross-examination a necessary cost of adversarial due process protection, or could procedural reforms (victim advocates in examination, protective screens, remote testimony) reduce retraumatization without compromising defendant rights?',
    'Longitudinal trauma assessment of victims in jurisdictions with vs without protective procedures. International comparison of conviction rates and defendant appeal success in systems with varying victim protection measures.',
    'If necessary: suppression (0.68) is justified as coordination overhead. If avoidable: suppression reflects institutional choice rather than structural necessity, shifting classification toward snare (higher χ). Current evidence suggests substantial reduction possible with procedural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retraumatization_necessity, empirical, 'Whether retraumatization is necessary to adversarial due process or procedurally avoidable').

omega_variable(
    victim_rights_functionality_degradation,
    'Has victim rights legislation created substantive change in prosecution decision-making and case outcomes, or has it degraded into symbolic participation theater while prosecution strategy remains victim-indifferent?',
    'Quantitative analysis: correlate victim impact statements with sentencing variation, measure frequency of victim preferences affecting plea negotiations, track prosecution policy changes attributable to victim rights statutes over 20+ years.',
    'If substantive: theater_ratio should be lower (0.35–0.45), classification shifts toward tangled_rope with real coordination. If degraded: theater_ratio confirmed (0.58+), piton perspective valid, victim rights movement is institutional inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_rights_functionality_degradation, empirical, 'Whether victim rights legislation created substantive change or degraded to theater').

omega_variable(
    restorative_justice_scalability,
    'Can restorative justice mechanisms (victim-offender dialogues, community healing circles) scale beyond marginal practice to become primary alternative pathways, or are they structurally limited by victim/offender willingness and community capacity?',
    'Analysis of jurisdictions with restorative-first policies (New Zealand, parts of Canada). Measurement of victim satisfaction, recidivism, and system capacity. Assessment of which offense types and victim populations restorative processes serve.',
    'If scalable: the scaffold perspective becomes viable (restorative sunset for traditional adversarial participation theater). If structurally limited: victim participation in adversarial proceedings remains the binding constraint, extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restorative_justice_scalability, empirical, 'Whether restorative justice can scale to primary alternative or remains marginal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(victim_participation_in_justice_proceedings, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vpjp_tr_t0, victim_participation_in_justice_proceedings, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vpjp_tr_t10, victim_participation_in_justice_proceedings, theater_ratio, 10, 0.45).
narrative_ontology:measurement(vpjp_tr_t20, victim_participation_in_justice_proceedings, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(vpjp_be_t0, victim_participation_in_justice_proceedings, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vpjp_be_t10, victim_participation_in_justice_proceedings, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(vpjp_be_t20, victim_participation_in_justice_proceedings, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(victim_participation_in_justice_proceedings, enforcement_mechanism).
narrative_ontology:affects_constraint(victim_participation_in_justice_proceedings, prosecutorial_discretion).
narrative_ontology:affects_constraint(victim_participation_in_justice_proceedings, victim_compensation_systems).
narrative_ontology:affects_constraint(victim_participation_in_justice_proceedings, restorative_justice_scaling).

% DUAL FORMULATION NOTE:
% Victim participation in adversarial proceedings is upstream of specific prosecution outcomes but represents a distinct structural constraint. Related constraints: prosecutorial discretion (whether state uses victim input in charging decisions), victim compensation systems (parallel extraction mechanism addressing victim financial harm), restorative justice alternatives (downstream constraint representing potential sunset mechanism if scaled). The victim participation constraint operates at the interface between the state's prosecution monopoly and victims' agency demands.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(victim_participation_in_justice_proceedings, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
