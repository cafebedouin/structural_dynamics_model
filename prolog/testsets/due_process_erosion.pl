% ============================================================================
% CONSTRAINT STORY: due_process_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_due_process_erosion, []).

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
 *   constraint_id: due_process_erosion
 *   human_readable: Due Process Erosion in Administrative and Criminal Systems
 *   domain: legal/political/governance
 *
 * SUMMARY:
 *   Due process erosion describes the structural degradation of procedural
 *   protections for accused persons across administrative, civil, and
 *   criminal legal systems. The constraint exhibits characteristics of a
 *   Tangled Rope: it coordinates efficient state enforcement while
 *   simultaneously extracting compliance, freedom, and resources from
 *   defendants through mechanisms including plea bargain pressure, inadequate
 *   legal representation, discovery limitations, bail systems, and reduced
 *   appellate protections. The erosion has accelerated over three decades
 *   (1995-2025) as caseload pressures, prosecutorial discretion expansion,
 *   and resource limitations have compounded. From the accused defendant's
 *   perspective, due process erosion appears as a Snare — structural
 *   entrapment with no exit option and maximum suppression. From the state
 *   enforcement perspective, it appears as Rope — pure coordination that
 *   enables efficient prosecution. From organized rights advocacy, it appears
 *   as Scaffold — a temporary institutional failure being corrected through
 *   litigation, exonerations, and legislative reform. The constraint's
 *   theater ratio (0.68) reflects that formal procedural protections persist
 *   through ritual (motions, appeals, discovery rules) while substance
 *   erodes, creating a Piton-like degradation where the institutional form
 *   (federal constitutional protection for due process) persists through
 *   performative commitment while functional reality has shifted toward
 *   efficiency.
 *
 * KEY AGENTS:
 *   - Accused Defendants (Powerless/Trapped): Primary victims bearing maximum extraction through forced participation, resource asymmetry, plea coercion, and suppression of alternatives.
 *   - State Enforcement Agencies (Institutional/Arbitrage): Primary beneficiaries extracting efficiency gains, prosecutorial discretion, faster case resolution, and resource concentration.
 *   - Public Defenders and Defense Counsel (Moderate/Constrained): Secondary victim-beneficiary dyad constrained by caseloads and funding while benefiting from steady court appointments; genuine coordination function mixed with extraction pressure.
 *   - Wealthy Defendants with Private Counsel (Powerful/Mobile): Secondary beneficiaries able to exploit eroded procedures through superior resources and information access.
 *   - Civil Liberties Organizations and Innocence Projects (Organized/Mobile): Counterweight agents building alternative pathways (appellate review, exoneration, legislative reform) with sunset logic.
 *   - Constitutional Legal Doctrine and Appellate Courts (Institutional/Arbitrage): Maintain formal protection while substance erodes; piton perspective reflects institutional inertia.
 *   - Analytical Observer (Analytical/Analytical): Risk position of naturalizing political-institutional choices as inevitable structural properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(due_process_erosion, 0.58).
domain_priors:suppression_score(due_process_erosion, 0.72).
domain_priors:theater_ratio(due_process_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(due_process_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(due_process_erosion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(due_process_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(due_process_erosion, tangled_rope).
narrative_ontology:human_readable(due_process_erosion, "Due Process Erosion in Administrative and Criminal Systems").
narrative_ontology:topic_domain(due_process_erosion, "legal/political/governance").

domain_priors:requires_active_enforcement(due_process_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(due_process_erosion, state_enforcement_agencies).
narrative_ontology:constraint_beneficiary(due_process_erosion, prosecutorial_efficiency_interests).
narrative_ontology:constraint_victim(due_process_erosion, accused_defendants).
narrative_ontology:constraint_victim(due_process_erosion, legal_precedent_integrity).
narrative_ontology:constraint_victim(due_process_erosion, judicial_system_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ACCUSED DEFENDANT (SNARE) — Structurally trapped by dependence on state-provided counsel, bail systems that extract wealth, plea bargain pressure, and asymmetric information access. Exit is impossible: refusing to participate in the legal process results in contempt charges or default judgment. Maximum suppression through mandatory participation and resource asymmetry. The constraint extracts guilty pleas, confession information, and assets through bail/fines regardless of eventual guilt or innocence.
constraint_indexing:constraint_classification(due_process_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE ENFORCEMENT APPARATUS (ROPE) — Benefits from streamlined prosecution efficiency, reduced trial burden, faster case resolution through plea bargaining, and expanded prosecutorial discretion. Experiences the constraint as pure coordination: eroded due process enables quick conflict resolution that serves state interests. Net beneficiary through arbitrage — can shift resources between enforcement priorities without procedural constraints.
constraint_indexing:constraint_classification(due_process_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY DEFENDANTS (TANGLED ROPE) — Mobile agents with access to private counsel, bail money, and legal expertise can navigate eroded due process and partially extract benefit through it. They coordinate with the system (participate in trials, plea agreements) while extracting advantage through information asymmetry and resource advantage. Experience mixed: some extraction upward (toward state interests) but also ability to leverage eroded procedures for their own benefit through superior resources.
constraint_indexing:constraint_classification(due_process_erosion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE ATTORNEYS / PUBLIC DEFENDERS (TANGLED ROPE) — Constrained by caseload pressures, funding limitations, and court-pressure toward quick resolution. Also benefit from the constraint through steady work and court appointments. Genuine coordination function (representing defendants) exists alongside extraction (inadequate resources, perverse incentives toward plea bargains, erosion of appellate rights). Experience moderate extraction: structurally mobile but constrained by institutional pressure.
constraint_indexing:constraint_classification(due_process_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED RIGHTS ADVOCACY (SCAFFOLD) — Civil liberties organizations, innocence projects, and appellate advocates see due process erosion as a temporary institutional failure with a sunset clause. Legal reform movements, wrongful conviction exonerations, and constitutional litigation are rebuilding due process protections at appellate and statutory levels. Low effective extraction because organized agents have agency, exit pathways (appellate review, legislative reform), and demonstrated ability to reverse erosion in specific domains. Theater ratio is declining as these movements surface systemic failures.
constraint_indexing:constraint_classification(due_process_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL DUE PROCESS FRAMEWORK (PITON) — Formal legal doctrine and court precedent assert that due process is protected, right to counsel is guaranteed, and defendants have appellate rights. These formal commitments persist through ritual (motions, appeals, oral arguments) while substance erodes. Theater ratio is high (0.68) — much procedural ritual performs protection while defendants navigate systems where counsel is overwhelmed, discovery is limited, and plea pressure is structural. Piton classification reflects maintenance through institutional inertia despite functional degradation.
constraint_indexing:constraint_classification(due_process_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some friction between individual rights and state enforcement is inherent to all legal systems. Every society must balance due process with public safety and efficiency. This perspective risks naturalizing the erosion as inevitable — 'due process always erodes under enforcement pressure' — treating what is a contingent institutional choice as an immutable structural law. The engine's false summit detector will identify this as naturalization of a political choice.
constraint_indexing:constraint_classification(due_process_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(due_process_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(due_process_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(due_process_erosion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(due_process_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(due_process_erosion, TR),
    TR >= 0.70.

:- end_tests(due_process_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts compliance, freedom, and resources from defendants through plea coercion, inadequate counsel, discovery limitations, and bail systems. The trajectory from 0.32 (1995) to 0.58 (2025) reflects measurable erosion: caseload pressures have increased, prosecutorial discretion has expanded, and public defense funding has stagnated relative to case volume. The 0.58 value reflects that extraction is substantial but not maximal — appellate review, exoneration movements, and occasional high-profile cases prevent total suppression of alternatives. Suppression (0.72): High. Defendants face multiple binding mechanisms: mandatory participation in legal process, dependence on state-provided counsel with massive caseloads, bail systems that extract assets before conviction, plea pressure created by trial uncertainty and inadequate resources, information asymmetry favoring prosecution, and limited appellate rights. Suppression is not total (some defendants can afford private counsel, some cases result in acquittal) but structural and severe for powerless agents. Theater ratio (0.68): Moderately high and rising from 0.48 to 0.68. Formal due process protections persist through ritual (discovery motions, appellate briefs, trial procedures) while substance has eroded. Public defenders go through procedural motions with inadequate time and resources. Appellate review exists but catches only visible errors. The theater has increased as visible institutional forms persist while actual protection declines — the constraint becomes increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. The accused defendant sees pure extraction (Snare) — trapped with no exit, bearing maximum cost. The state enforcement apparatus sees pure coordination (Rope) — solving the legitimate problem of efficient case resolution. Wealthy defendants see mixed benefit (Tangled Rope) — able to extract advantage through superior resources despite the general constraint. Defense attorneys see genuine tension (Tangled Rope) — coordination function (representing defendants) mixed with extraction pressure (caseload, efficiency incentives). Organized advocates see temporary institutional failure (Scaffold) — appellate wins, exonerations, and legislative reform demonstrating that erosion is reversible. The piton perspective observes that formal constitutional protections persist through ritual while function degrades. The mountain perspective risks treating the trade-off between efficiency and protection as inherent to all legal systems, naturalizing a political choice. The engine's false summit detector should flag this as naturalization: the degree of due process protection is a policy choice, not an immutable constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position: accused defendants are trapped victims (d ≈ 0.92), experiencing high f(d) → high chi. State enforcement agencies are beneficiaries with arbitrage options (d ≈ 0.08), experiencing low/negative f(d) → negative chi. Wealthy defendants are mobile agents with power (d ≈ 0.35), experiencing moderate f(d). Defense attorneys are constrained victims with some benefit (d ≈ 0.58), experiencing moderate f(d). Rights advocates are organized with mobile exit paths (d ≈ 0.42), experiencing moderate f(d). The constraint operates at national scope (σ=1.0), meaning extractiveness is scaled by f(d) without scope modification. The directionality derivation automatically produces the perspectival gap: trapped powerless agents experience maximum chi, while beneficiary institutional actors experience negative chi, creating the classification divergence observed across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that due process erosion is NOT an immutable constraint but a tangled coordination-extraction hybrid that persists through political choice and institutional inertia. The false summit (mountain) perspective naturalizes this choice, claiming efficiency-protection trade-offs are inevitable. The analytical observer should reject this — exoneration data shows innocent people are convicted, wrongful conviction rates correlate with under-resourcing, and jurisdictions with adequate public defense funding achieve both due process protection AND efficient prosecution. The constraint is therefore a Tangled Rope from the analytical perspective when properly measured: it genuinely coordinates efficient case resolution (benefiting the state enforcement interest in manageable caseloads) while simultaneously extracting compliance and freedom from defendants through suppression of alternatives. The scaffold perspective is empirically grounded: rights advocacy movements have successfully reversed erosion in specific domains (DNA exoneration, appellate review of new evidence, statutory sentencing reforms), demonstrating that the sunset clause is not aspirational but structural. The piton perspective is concerning: formal protections persist despite substance erosion, suggesting institutional capture of due process doctrine itself. The primary analytical task is to differentiate between the necessary coordination function (managing caseload, resolving conflicts) and the extractive overhead (plea coercion, resource starvation, suppression of appellate review), and to recognize that the ratio between these is a policy choice, not an inevitable structural property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plea_coercion_mechanism,
    'Is plea bargaining pressure coercive extraction or legitimate efficiency coordination?',
    'Empirical analysis: compare plea rates under different resource-adequacy conditions; measure conviction rates and sentence disparities between pleaded and tried cases; assess whether innocent defendants plead guilty at systematic rates',
    'If pressure is coercive: extractiveness increases to 0.68+, classification shifts to pure Snare for powerless agents. If legitimate: classification becomes Rope-leaning Tangled Rope. Plea data shows innocent people plead guilty at measurable rates under resource pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plea_coercion_mechanism, empirical, 'Whether plea bargaining represents coercive extraction or legitimate efficiency').

omega_variable(
    appellate_reversal_sufficiency,
    'Does appellate review and wrongful conviction exoneration actually constitute a functional correction mechanism or merely theatrical post-hoc review that catches only visible errors?',
    'Longitudinal exoneration data; comparison of DNA exoneration rates to estimated innocent conviction rates; analysis of which appellate review mechanisms actually identify and reverse errors vs which operate as formality',
    'If appellate review is sufficient: scaffold perspective gains strength, sunset clause becomes credible. If review is largely theatrical: piton classification is strengthened, suggesting erosion is maintained despite formal appellate guarantees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_reversal_sufficiency, empirical, 'Whether appellate review provides meaningful correction for due process violations').

omega_variable(
    state_efficiency_necessity,
    'Is erosion of due process a necessary functional requirement for state enforcement capacity or a contingent choice reflecting resource allocation decisions?',
    'Comparative analysis: jurisdictions with adequate public defense funding and investigative resources; historical periods with higher due process protection and simultaneous lower crime rates; modeling of prosecution efficiency under different funding scenarios',
    'If necessary: falsifies mountain perspective and moves classification toward legitimate coordination. If contingent: extraction framing is confirmed, supporting Snare and Tangled Rope classifications as primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_efficiency_necessity, empirical, 'Whether due process erosion is functionally necessary or contingently chosen').

omega_variable(
    legitimacy_feedback,
    'Does visible due process erosion (wrongful convictions, inadequate representation, plea coercion) reduce public legitimacy of the legal system in ways that increase future enforcement costs, creating feedback that destabilizes the constraint?',
    'Public confidence data in legal institutions; analysis of jurisdictional variations in cooperation with law enforcement (witness participation, jury verdicts, restitution compliance); long-term enforcement effectiveness trends',
    'If legitimacy loss is significant: erosion is self-undermining over generational timescales. Scaffold and rights advocacy perspectives gain analytic strength. If legitimacy is decoupled from due process visibility: extraction is more stable and sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_feedback, empirical, 'Whether due process erosion degrades legal system legitimacy and enforcement effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(due_process_erosion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dproc_tr_t0, due_process_erosion, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dproc_tr_t10, due_process_erosion, theater_ratio, 10, 0.62).
narrative_ontology:measurement(dproc_tr_t20, due_process_erosion, theater_ratio, 20, 0.68).
narrative_ontology:measurement(dproc_tr_t30, due_process_erosion, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(dproc_be_t0, due_process_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dproc_be_t10, due_process_erosion, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(dproc_be_t20, due_process_erosion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(dproc_be_t30, due_process_erosion, base_extractiveness, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(due_process_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(due_process_erosion, plea_bargaining_efficiency_incentive).
narrative_ontology:affects_constraint(due_process_erosion, public_defense_resource_constraint).
narrative_ontology:affects_constraint(due_process_erosion, incarceration_system_feedback).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(due_process_erosion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
