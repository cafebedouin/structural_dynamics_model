% ============================================================================
% CONSTRAINT STORY: signal_without_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_signal_without_control, []).

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
 *   constraint_id: signal_without_control
 *   human_readable: The Passive Observational Trap
 *   domain: technological/social
 *
 * SUMMARY:
 *   The passive observational trap arises when structural architecture grants
 *   agents high-fidelity access to system state information while
 *   systematically barring them from adjusting system parameters. This
 *   creates a cognitive and political trap: observers see problems clearly
 *   but cannot solve them, generating cognitive dissonance, responsibility
 *   without authority, and often blame directed at observers for 'not acting'
 *   despite their actionlessness being structural rather than volitional. The
 *   constraint operates across multiple domains—financial regulation
 *   (regulators observe market behavior without direct control levers),
 *   public health surveillance (epidemiologists track disease spread without
 *   authority over lockdowns or vaccine deployment), climate science
 *   (researchers measure atmospheric parameters without control over emission
 *   policy), and workplace monitoring (employees see company metrics but
 *   cannot change strategy). The trap is extractive because it concentrates
 *   decision-making authority while using observer credibility and expertise
 *   to legitimize decisions made without observer input, and it is
 *   suppressive because observers cannot exit (the expertise they provide
 *   makes them essential) or coordinate (each observer is isolated with their
 *   signals). The theater ratio (0.58) reflects performative accountability
 *   mechanisms—public briefings, advisory committees, impact assessments—that
 *   create an appearance of responsiveness without redistributing control.
 *
 * KEY AGENTS:
 *   - Passive Observer: Primary victim (powerless/trapped) — possesses signal, cannot control; blamed for inaction despite powerlessness
 *   - Dependent Populations: Primary victim (powerless/trapped) — doubly powerless: neither see signals nor control system; bear consequences
 *   - System Controller: Primary beneficiary (institutional/arbitrage) — exercises control unilaterally, leverages observer expertise without obligation to respond
 *   - Regulatory Oversight Bodies: Secondary actor (organized/constrained) — mandate signal access and accountability reporting but lack direct control authority; face extraction through accountability without power
 *   - Historical Institutional Precedent: Maintains constraint through inertia (piton perspective) — observer-without-control model persists because power redistribution would require controller to voluntarily dilute authority
 *   - Emerging Participatory Governance Models: Organized agents building alternatives (organized/mobile) — distributed oversight and real-time response protocols creating sunset pathway to dissolved trap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(signal_without_control, 0.52).
domain_priors:suppression_score(signal_without_control, 0.68).
domain_priors:theater_ratio(signal_without_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(signal_without_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(signal_without_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(signal_without_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(signal_without_control, snare).
narrative_ontology:human_readable(signal_without_control, "The Passive Observational Trap").
narrative_ontology:topic_domain(signal_without_control, "technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(signal_without_control, system_controller).
narrative_ontology:constraint_victim(signal_without_control, passive_observer).
narrative_ontology:constraint_victim(signal_without_control, dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE OBSERVER (SNARE) — Possesses real-time access to system state but cannot modify parameters. Bears the cost of information asymmetry: sees the problem, cannot fix it, and is often blamed for inaction. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(signal_without_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT POPULATIONS (SNARE) — Have no access to signals and cannot influence control. Trapped by dual powerlessness: neither seeing the system nor directing it. Bear consequences of controller decisions with no recourse. d≈0.98, f(d)≈1.44, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(signal_without_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEM CONTROLLER (ROPE) — Exercises full control authority. Benefits from observer's signal production (intelligence) without obligation to respond. Experiences constraint as pure coordination benefit: leveraging observer expertise while maintaining decision autonomy. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(signal_without_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY OVERSIGHT BODIES (TANGLED ROPE) — Can mandate signal access and circulation but lack direct control authority over system parameters. Face extraction through accountability requirements (report without power) while providing coordination function (public transparency). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(signal_without_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL INSTITUTIONAL PRECEDENT (PITON) — The observer-without-control model persists through institutional inertia despite demonstrable failures. Theater_ratio=0.58 reflects performative accountability mechanisms (advisory boards, public reporting) that create appearance of responsiveness without granting control. The constraint maintains itself through ritualized transparency theater.
constraint_indexing:constraint_classification(signal_without_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING PARTICIPATORY GOVERNANCE MODELS (SCAFFOLD) — Decentralized oversight mechanisms and real-time response protocols are creating pathways to distributed control that respond to observed signals. This represents a temporary coordination structure with an explicit sunset: as participatory architecture matures, the passive observation trap dissolves. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.17.
constraint_indexing:constraint_classification(signal_without_control, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION LAG VIEW (MOUNTAIN) — From a civilizational/universal frame, signal-lag is inherent to complex systems: observation creates measurement overhead, control introduces feedback delays, and some information asymmetry is inescapable. However, the structural data (ε=0.52, suppression=0.68) contradicts the mountain gate—the engine will detect this as a false summit, revealing that 'inherent system complexity' naturalizes what is actually a contingent power allocation.
constraint_indexing:constraint_classification(signal_without_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(signal_without_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(signal_without_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(signal_without_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(signal_without_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(signal_without_control, TR),
    TR >= 0.70.

:- end_tests(signal_without_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The controller captures decision authority and legitimacy by leveraging observer expertise without obligation to respond. The extraction is not total because some observer input does influence decisions (indirect coordination function), but the asymmetry is severe—observers bear responsibility for outcomes they cannot control. The trajectory shows increasing extractiveness (0.35→0.52) as controller learns to rely on observer signal production while tightening control, creating dependency. Suppression (0.68): High. Multiple barriers prevent observers from escaping or coordinating: (1) specialized expertise makes them essential (cannot quit), (2) isolation structure prevents coordination among observers, (3) professional norms discourage public challenge to controller authority, (4) control over information dissemination gives controller veto power over observer communication. Theater ratio (0.58): Moderate-high and increasing. Performative mechanisms include public briefings (create transparency theater), advisory boards (create participation theater), impact assessments (create accountability theater), and formal acknowledgment of observer expertise (creates legitimacy theater). These create surface responsiveness without distributing control. The trend (0.42→0.58) reflects increasing sophistication in performance—controllers learn to manage observer visibility to enhance legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The controller sees pure coordination (Rope)—observers providing valuable signals, the controller making informed decisions. The observer sees pure extraction (Snare)—trapped, blamed for outcomes they cannot control, their expertise captured without reciprocal authority. Dependent populations see amplified snare (unable to even see signals, doubly powerless). Regulatory bodies see tangled rope (coordination function of transparency + extraction of accountability without power). Historical precedent sees piton (performative mechanisms maintaining inertia). Participatory models see scaffold (buildable exit path with sunset). The analytical observer risks seeing mountain (information asymmetry is inherent to complex systems), but the structural data reveals this as false naturalization—the trap is contingent on control monopoly, not physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Passive observer: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction minus epsilon. Cannot exit (expertise makes them essential); cannot organize (isolation prevents coordination); experiences cognitive dissonance between knowledge and agency. Dependent populations: Victim + trapped (doubly) → d≈0.98, f(d)≈1.44. Higher than observer because they lack even signal access. System controller: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit at any time; leverages observer expertise without reciprocal constraint. Regulatory bodies: Victim + constrained (partially) → d≈0.55, f(d)≈0.75. Mandated to monitor and report (extraction of accountability) but cannot modify system directly (constrained). Participatory models: Organized + mobile → d≈0.35, f(d)≈0.32. Mobilized agents with exit pathway (decentralization); low effective extraction because coalition has agency. Mountain perspective: Analytical → d≈0.72, f(d)≈1.15. False summit detection: naturalizes contingent power allocation as immutable feature of information asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.52 < 0.70): The mandatrophy is resolved by showing that the constraint is genuinely extractive (not coordination), but the extraction is policy-contingent rather than natural. The key test: can distributed control coexist with high-quality observation? Participatory governance models (Scaffold perspective) answer yes—the constraint's extraction mechanism depends on control monopoly being actively defended, not on inherent incompatibility between observation and distributed control. The false mountain (analytical perspective naturalizing information lag as inevitable) is detected by the structural data: if the trap were immutable physics, we would expect ε to be ≤0.25 (mountain range), not 0.52. The intermediate ε value reveals a hybrid constraint where coordination (signal provision) is real but subordinated to extraction (control monopoly). The mandatrophy turns on whether dispersed decision-making can actually improve outcomes: if yes, the constraint is pure policy choice (Snare). If participatory models fail to improve outcomes, the constraint graduates to Tangled Rope (coordination function is real because distributed control creates new failure modes that signal-only cannot address). Current evidence: early participatory systems show promise but have not yet reached scale. Classification: Snare with Tangled Rope possibility, mediated by implementation success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_versus_control_sufficiency,
    'Does signal fidelity compensate for control barring, or does the architecture create irreducible psychological/structural harm that observation cannot address?',
    'Longitudinal studies comparing observer stress/efficacy under signal-only vs signal-plus-limited-control conditions; measure responsiveness metrics (time to meaningful system change) under each regime',
    'If fidelity suffices: constraint is coordination problem (Rope). If it causes residual harm: constraint is pure extraction (Snare confirmed). If harm is context-dependent: depends on observer expectations (preference type).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observation_versus_control_sufficiency, empirical, 'Whether signal fidelity alone addresses observer needs or leaves irreducible structural harm').

omega_variable(
    control_redistribution_feasibility,
    'Is distributed control to observers technically feasible, or does system complexity create genuine (not fabricated) incompatibility between observation and control authority?',
    'Technical assessment of control-system architectures; identification of fundamental constraints (real-time overhead, cascading failure risk) vs policy-imposed barriers (delegation refusal, accountability diffusion)',
    'If feasible: constraint is purely extractive policy choice (Snare confirmed). If infeasible: constraint approaches Mountain. If mixed: Tangled Rope (coordination + extraction both real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(control_redistribution_feasibility, empirical, 'Whether control redistribution is technically feasible or fundamentally constrained').

omega_variable(
    observer_capture_risk,
    'Does granting observers signal-only access create incentive for strategic information distortion or selective reporting, especially when observers know they cannot act on bad news?',
    'Empirical analysis of signal accuracy in signal-only vs signal-plus-control regimes; measure reporting divergence between public and internal assessments under each condition',
    'If capture occurs: signal-only regime creates additional victim class (the public, misled by distorted signals). If minimal: information asymmetry is primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_capture_risk, empirical, 'Whether signal-only access incentivizes strategic information distortion').

omega_variable(
    power_asymmetry_necessity,
    'Is the control monopoly necessary for system stability, or does it serve primarily to concentrate decision-making authority and insulate it from accountability?',
    'Historical comparison of stability outcomes in centralized control vs distributed control systems; test whether system actually requires monopoly control or whether decentralization introduces risk primarily to controller authority (not system robustness)',
    'If necessary: constraint is real coordination requirement (Mountain or Rope). If arbitrary: constraint is pure extraction (Snare). If mixed: Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_necessity, empirical, 'Whether control monopoly serves system stability or concentrates authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(signal_without_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(signal_tr_t0, signal_without_control, theater_ratio, 0, 0.42).
narrative_ontology:measurement(signal_tr_t5, signal_without_control, theater_ratio, 5, 0.5).
narrative_ontology:measurement(signal_tr_t10, signal_without_control, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(signal_be_t0, signal_without_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(signal_be_t5, signal_without_control, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(signal_be_t10, signal_without_control, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(signal_without_control, information_standard).
narrative_ontology:affects_constraint(signal_without_control, information_asymmetry_in_markets).
narrative_ontology:affects_constraint(signal_without_control, delegation_without_accountability).
narrative_ontology:affects_constraint(signal_without_control, knowledge_capture_by_expertise).

% DUAL FORMULATION NOTE:
% The signal-without-control constraint is downstream of fundamental information asymmetry but represents a distinct structural choice about how to allocate observation vs control authority. The upstream constraint (information_asymmetry_in_markets, ε≈0.35) is more fundamental; signal_without_control (ε=0.52) shows how policy decisions can amplify inherent asymmetry by institutionalizing unequal access to both information and decision power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(signal_without_control, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
