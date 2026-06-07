% ============================================================================
% CONSTRAINT STORY: predictive_surveillance_capability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_predictive_surveillance_capability, []).

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
 *   constraint_id: predictive_surveillance_capability
 *   human_readable: Predictive Surveillance Capability Development Window
 *   domain: technology_governance/surveillance_studies/export_control_policy
 *
 * SUMMARY:
 *   Predictive surveillance capabilities — AI models trained to forecast
 *   future dissent from behavioral patterns — create a structural window
 *   between technological emergence and widespread deployment. Export
 *   controls on advanced AI chips, training infrastructure, and algorithmic
 *   techniques attempt to delay authoritarian state acquisition long enough
 *   for counter-surveillance technologies, legislative constraints, and
 *   international norms to mature. This constraint is scaffold from multiple
 *   perspectives because its justification is explicitly transitional: the
 *   goal is not permanent control but buying time for durable barriers to
 *   emerge. The constraint exhibits rising extractiveness (0.25 → 0.35) and
 *   suppression (0.35 → 0.45) over the 6-year interval as compliance costs
 *   increase and enforcement intensifies, while theater ratio rises modestly
 *   (0.20 → 0.30) as indigenous capability development and academic transfer
 *   channels begin to bypass formal export controls. The sunset clause is
 *   structural: either the window closes with counter-surveillance mature
 *   (transition to rope) or the capability proliferates and controls fail
 *   (transition to snare for dissidents, piton for the control regime).
 *
 * KEY AGENTS:
 *   - Targeted Dissident: Primary victim (powerless/trapped) — faces predictive action before public opposition emerges; no exit option within authoritarian state
 *   - Technology Development Ecosystem: Secondary victim (moderate/constrained) — AI labs, chip manufacturers, cloud providers bear compliance costs and market restrictions; benefits from norm-setting but constrained by enforcement
 *   - Export Control Regime: Primary beneficiary (institutional/mobile) — Wassenaar states, US Commerce, allied intelligence coordinate to delay capability proliferation; can exit to alternative strategies if controls fail
 *   - Civil Society Organizations: Secondary beneficiary (organized/mobile) — human rights groups, transparency advocates benefit from window for norm development and counter-surveillance research; low extraction due to voice and exit options
 *   - Authoritarian State Security Apparatus: Constrained actor (institutional/constrained) — Public Security Bureaus experience controls as temporary obstacle; pursuing indigenous development and transfer channels to bypass
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural temporariness: window either closes with counter-surveillance mature or capability proliferates and controls become theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(predictive_surveillance_capability, 0.35).
domain_priors:suppression_score(predictive_surveillance_capability, 0.45).
domain_priors:theater_ratio(predictive_surveillance_capability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(predictive_surveillance_capability, extractiveness, 0.35).
narrative_ontology:constraint_metric(predictive_surveillance_capability, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(predictive_surveillance_capability, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(predictive_surveillance_capability, scaffold).
narrative_ontology:human_readable(predictive_surveillance_capability, "Predictive Surveillance Capability Development Window").
narrative_ontology:topic_domain(predictive_surveillance_capability, "technology_governance/surveillance_studies/export_control_policy").

domain_priors:requires_active_enforcement(predictive_surveillance_capability).
narrative_ontology:has_sunset_clause(predictive_surveillance_capability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(predictive_surveillance_capability, export_control_regime).
narrative_ontology:constraint_beneficiary(predictive_surveillance_capability, civil_society_organizations).
narrative_ontology:constraint_beneficiary(predictive_surveillance_capability, democratic_states).
narrative_ontology:constraint_victim(predictive_surveillance_capability, authoritarian_state_dissidents).
narrative_ontology:constraint_victim(predictive_surveillance_capability, technology_development_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED DISSIDENT (SNARE) — Faces predictive surveillance with no exit option. The system acts before public opposition emerges, eliminating the biographical window for organizing or escape. Maximum extraction: the constraint exists to suppress this agent's future action.
constraint_indexing:constraint_classification(predictive_surveillance_capability, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TECHNOLOGY DEVELOPMENT ECOSYSTEM (TANGLED ROPE) — AI research labs, chip manufacturers, cloud providers face genuine coordination problem (preventing capability proliferation) but bear asymmetric costs: export restrictions, compliance overhead, foregone markets. Benefits from norm-setting that prevents race-to-bottom but constrained by enforcement apparatus.
constraint_indexing:constraint_classification(predictive_surveillance_capability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EXPORT CONTROL REGIME (SCAFFOLD) — Wassenaar Arrangement states, US Commerce Dept, allied intelligence agencies see this as temporary coordination: the window between capability emergence and authoritarian deployment is finite. Sunset logic: either export controls succeed in delaying deployment long enough for counter-surveillance tech to mature, or they fail and the capability proliferates. Not a permanent equilibrium.
constraint_indexing:constraint_classification(predictive_surveillance_capability, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL SOCIETY ORGANIZATIONS (ROPE) — Human rights groups, transparency advocates, democratic legislatures benefit from the coordination function: export controls create a window for norm development, legislative oversight, and counter-surveillance research. Low extraction because these actors have voice and can exit to alternative strategies if controls become extractive.
constraint_indexing:constraint_classification(predictive_surveillance_capability, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTHORITARIAN STATE SECURITY APPARATUS (SCAFFOLD from their position) — Public Security Bureaus see export controls as temporary obstacle: indigenous capability development, technology transfer through academic channels, and commercial espionage will eventually bypass controls. The constraint delays but does not prevent. They experience it as coordination failure (from their perspective) with a sunset.
constraint_indexing:constraint_classification(predictive_surveillance_capability, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — The predictive surveillance capability window is structurally temporary: either (a) export controls delay deployment long enough for counter-surveillance technology, legislative constraints, and international norms to mature, creating durable barriers; or (b) the capability proliferates and the window closes. The constraint's justification is the transition, not the steady state. Moderate extraction reflects real costs to technology ecosystem and real suppression of future dissent, but the coordination function (buying time for democratic response) is genuine.
constraint_indexing:constraint_classification(predictive_surveillance_capability, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(predictive_surveillance_capability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(predictive_surveillance_capability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(predictive_surveillance_capability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(predictive_surveillance_capability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The technology ecosystem bears real costs (compliance overhead, foregone markets, research restrictions) and dissidents face real suppression risk, but the extraction is not as severe as pure surveillance deployment would be. The value reflects that export controls impose costs on both developers and targets while providing genuine coordination value (time for response). Suppression (0.45): Moderate. Significant barriers to capability proliferation include chip export restrictions, cloud infrastructure controls, algorithmic technique classification, and end-use monitoring. But suppression is not total — academic channels, indigenous development programs, and commercial espionage create bypass routes. The rising trajectory (0.35 → 0.45) reflects enforcement intensification as states close loopholes. Theater ratio (0.30): Low-moderate. Export controls have genuine functional content — they demonstrably delay capability acquisition — but theater is rising (0.20 → 0.30) as indigenous development and academic transfer begin to bypass formal controls. The modest theater reflects that the coordination mechanism still works but is degrading.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates scaffold classification from multiple structural positions, which is unusual — most constraints show type divergence across perspectives. Here, the export control regime, civil society organizations, the authoritarian security apparatus (from their position), and the analytical observer all see scaffold, though for different reasons. The regime sees temporary coordination buying time for norms. Civil society sees a window for counter-surveillance development. The security apparatus sees a temporary obstacle they will bypass. The analytical observer sees structural temporariness: the window closes one way or another. The dissident sees snare (no exit, maximum extraction). The technology ecosystem sees tangled rope (genuine coordination function but asymmetric costs). The convergence on scaffold across beneficiaries, constrained actors, and analytical position is diagnostic: when a constraint's primary justification is explicitly transitional and multiple parties with different interests agree the steady state is elsewhere, the scaffold classification is robust.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. The targeted dissident is a declared victim with trapped exit options — maximum directionality toward full target (d → 1.0), producing high effective extraction. The technology ecosystem is a declared victim but with constrained exit options and some coordination benefit — moderate directionality (d ≈ 0.5-0.6), producing moderate effective extraction. The export control regime and civil society organizations are declared beneficiaries with mobile exit options — low directionality toward beneficiary end (d → 0.0-0.2), producing low or negative effective extraction (they benefit from the constraint). The authoritarian security apparatus is neither declared beneficiary nor victim in the base_properties (they are adversary, not participant in the coordination) — their directionality is derived from institutional power + constrained exit, producing moderate values. No overrides are needed because the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that scaffold classification can be robust across multiple perspectives when the constraint's justification is explicitly and structurally transitional. The mandate (delay capability proliferation to buy time for counter-surveillance and norms) has not outlived its function — the window is still open. If the window closes without counter-surveillance maturing (capability proliferates), mandatrophy triggers: the constraint becomes theater (piton) for the control regime and pure extraction (snare) for dissidents globally. If the window closes with counter-surveillance mature (durable barriers emerge), the constraint transitions to rope (coordination without extraction). The scaffold classification is diagnostic of the transition state, not the terminal state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counter_surveillance_maturation_timeline,
    'Will counter-surveillance technologies (differential privacy, federated learning, encrypted computation) mature fast enough to create durable barriers before predictive capability proliferates?',
    'Comparative timeline analysis: rate of counter-surveillance research vs rate of predictive model capability improvement and deployment; measurement of adoption rates in vulnerable populations',
    'If counter-surveillance matures first: scaffold sunset is real, constraint transitions to rope (coordination without extraction). If predictive capability proliferates first: scaffold fails, constraint becomes snare for dissidents globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_surveillance_maturation_timeline, empirical, 'Whether counter-surveillance tech matures before capability proliferates').

omega_variable(
    export_control_effectiveness_threshold,
    'What delay threshold makes export controls worth their cost to the technology ecosystem? Is 5 years enough? 10 years?',
    'Cost-benefit analysis: compliance costs and foregone innovation vs value of time bought for norm development and counter-surveillance research; historical analysis of technology diffusion timelines',
    'If threshold < 5 years: current controls are net extractive (costs exceed coordination value). If threshold > 10 years: controls are justified coordination even with high costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(export_control_effectiveness_threshold, preference, 'Delay threshold that justifies export control costs').

omega_variable(
    indigenous_capability_development_rate,
    'How quickly can authoritarian states develop equivalent predictive surveillance capabilities indigenously, bypassing export controls entirely?',
    'Intelligence assessment of domestic AI research programs, chip fabrication capacity, data infrastructure; tracking of academic publications and patent filings in target states',
    'If indigenous development < 5 years: export controls are theater (high theater_ratio, low coordination value). If indigenous development > 10 years: controls provide genuine window for response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_capability_development_rate, empirical, 'Timeline for indigenous capability development bypassing controls').

omega_variable(
    democratic_deployment_risk,
    'Do export controls prevent democratic states from deploying predictive surveillance, or only delay authoritarian deployment while democratic states proceed unconstrained?',
    'Comparative analysis of surveillance capability deployment in democratic vs authoritarian states; legislative and judicial constraints in democracies; evidence of capability use by democratic security services',
    'If democratic states deploy without constraint: the coordination story is cover for selective enforcement (snare from technology ecosystem perspective). If democratic states face genuine constraints: coordination function is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_deployment_risk, empirical, 'Whether democratic states face equivalent deployment constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(predictive_surveillance_capability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pred_surv_theater_initial, predictive_surveillance_capability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pred_surv_tr_t2, predictive_surveillance_capability, theater_ratio, 2, 0.25).
narrative_ontology:measurement(pred_surv_tr_t4, predictive_surveillance_capability, theater_ratio, 4, 0.28).
narrative_ontology:measurement(pred_surv_tr_t6, predictive_surveillance_capability, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(pred_surv_be_t0, predictive_surveillance_capability, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pred_surv_be_t2, predictive_surveillance_capability, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(pred_surv_be_t4, predictive_surveillance_capability, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(pred_surv_be_t6, predictive_surveillance_capability, base_extractiveness, 6, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pred_surv_su_t0, predictive_surveillance_capability, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pred_surv_su_t2, predictive_surveillance_capability, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(pred_surv_su_t4, predictive_surveillance_capability, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(pred_surv_su_t6, predictive_surveillance_capability, suppression_requirement, 6, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(predictive_surveillance_capability, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of compute_constraint_as_brake (the physical limits on AI capability development) and export_control_reversibility (the institutional mechanisms for controlling technology transfer). The predictive surveillance capability window exists because compute constraints create a delay between capability emergence and proliferation, and export controls attempt to extend that delay. If compute constraints were not binding (mountain fails) or export controls were fully reversible (tangled_rope degrades to piton), this scaffold would collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
