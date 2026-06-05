% ============================================================================
% CONSTRAINT STORY: state_surveillance_capacity_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_surveillance_capacity_extraction, []).

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
 *   constraint_id: state_surveillance_capacity_extraction
 *   human_readable: State Surveillance Capacity Extraction
 *   domain: political/technological
 *
 * SUMMARY:
 *   State surveillance capacity extraction models the structural asymmetry
 *   between state security apparatus and the population under surveillance.
 *   The constraint emerges from technological capability (signals
 *   intelligence, data aggregation, pattern analysis) combined with
 *   asymmetric knowledge (state knows what it collects; population does not)
 *   and asymmetric exit options (population cannot opt out; state apparatus
 *   can reallocate resources if surveillance is curtailed). The
 *   extractiveness has grown substantially over the 20-year measurement
 *   interval (0.42 → 0.68) as technical capacity for data collection and
 *   analysis has increased exponentially. Theater ratio has also risen (0.35
 *   → 0.55) as formal oversight frameworks have become increasingly
 *   performative — creating the appearance of constraint while technical
 *   capability has outpaced oversight's functional reach. Suppression
 *   requirement (the active coercive force needed to maintain the constraint)
 *   has intensified (0.65 → 0.78) as awareness of surveillance has grown and
 *   resistance to it has organized. This constraint exemplifies
 *   extraction-dominant dynamics: the apparatus benefits, the population
 *   bears costs, and the constraint persists through a combination of
 *   technical capability, legal authorization, and suppression of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Surveilled Citizen: Primary victim (powerless/trapped) — bears full extraction cost; cannot exit without emigration; experiences asymmetric knowledge and behavioral chilling effects
 *   - Political Opposition: Secondary victim (moderate/constrained) — faces targeted surveillance, differential enforcement, prosecution based on surveillance data; constrained exit options
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures security coordination benefits; asymmetric knowledge advantage; can reallocate resources if surveillance curtailed
 *   - Civil Rights Coalition: Organized actors (organized/constrained) — recognize both coordination (legitimate security needs) and extraction (oppression); attempt to constrain via legal challenge, documentation, technical countermeasures
 *   - Formal Oversight Framework: Institutional actor (institutional/arbitrage) — designed to constrain but largely performative; maintains appearance of constraint while capacity grows unchecked
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political surveillance choices as inevitable technology consequences (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_surveillance_capacity_extraction, 0.68).
domain_priors:suppression_score(state_surveillance_capacity_extraction, 0.78).
domain_priors:theater_ratio(state_surveillance_capacity_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_surveillance_capacity_extraction, snare).
narrative_ontology:human_readable(state_surveillance_capacity_extraction, "State Surveillance Capacity Extraction").
narrative_ontology:topic_domain(state_surveillance_capacity_extraction, "political/technological").

domain_priors:requires_active_enforcement(state_surveillance_capacity_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_surveillance_capacity_extraction, state_security_apparatus).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, surveilled_population).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, political_opposition).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Trapped within national territory; cannot exit surveillance without emigration (high cost, legal barriers, family separation). Asymmetric knowledge: state observes behavior, communications, financial transactions; citizen cannot know what is collected or how it is used. No coordination benefit — the surveillance apparatus does not solve citizen problems, only monitors for state benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL OPPOSITION (SNARE) — Constrained by surveillance targeting and differential enforcement. Opposition figures face heightened monitoring, account freezes, travel restrictions, and prosecution based on surveillance data. Exit options exist (emigration, legitimization through formal politics) but at high cost. Surveillance is explicitly coercive — designed to suppress opposition organizing. No coordination function; pure extraction with suppression as the mechanism.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Sees surveillance as solving a genuine coordination problem: detecting threats, preventing terrorism, managing national security. From this perspective, the asymmetry is necessary and legitimate — state needs to know, population needs protection. Experiences surveillance as coordination mechanism with benefits (threat prevention, order maintenance). Net beneficiary; extraction runs toward this agent. High arbitrage: can reallocate resources if surveillance curtailed, redirect to other monitoring priorities.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized agents (NGOs, lawyers, technologists, international bodies) see surveillance both as a governance coordination mechanism (legitimate security needs) AND as an extraction mechanism (suppression of dissent, targeting of minorities, chilling effects on speech). These agents have partial exit: they can organize internationally, document abuses, build technical countermeasures, pressure governments. Genuine coordination function exists (threat detection); asymmetric extraction also exists (oppression). The constraint exhibits both features simultaneously — the classification depends on which function is foregrounded and at what time scale.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL OVERSIGHT FRAMEWORK (PITON) — Legal frameworks (warrants, judicial review, legislative oversight committees) are designed to constrain state surveillance. In practice, these frameworks are largely performative: oversight committees lack enforcement power, classified proceedings prevent public scrutiny, national security exemptions erode warrant requirements, and technical complexity outpaces judicial comprehension. Oversight persists as institutional theater — the apparatus maintains the appearance of constraint while technical capacity has grown beyond oversight's functional reach. Theater ratio high; actual constraint on extraction low.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECHNOLOGY NATURALIZATION (MOUNTAIN) — From a civilizational/universal perspective, surveillance capacity follows from the laws of information technology: once data can be collected and analyzed, state capacity to do so becomes a natural law-like fact of modernity. This perspective sees surveillance asymmetry as inevitable, emerging naturally from technological capability rather than political choice. However, this perspective instantiates a false summit: the natural law framing naturalizes what is actually a contingent institutional and political choice — the beneficiary group (state security apparatus) has strong incentives to present surveillance capability as inevitable rather than chosen.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_surveillance_capacity_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_surveillance_capacity_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_surveillance_capacity_extraction, TR),
    TR >= 0.70.

:- end_tests(state_surveillance_capacity_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state security apparatus captures substantial benefits — security coordination, threat detection, operational intelligence — while the population bears costs without proportional benefit. The asymmetric knowledge (state observes; population does not) is the key extraction mechanism. The population cannot price the cost of surveillance because they do not know what is collected or how it is used. Rising extractiveness over the interval reflects accumulating technical capacity: early surveillance (0.42) was expensive and targeted; modern surveillance (0.68) is automated and comprehensive. Suppression (0.78): High. The constraint requires active suppression mechanisms: legal authorization (national security exemptions), technical barriers (encrypted communications detection), political suppression (opposition targeting), informational suppression (classified programs prevent public knowledge). Exit options are suppressed — emigration requires resources and legal permission; domestic opposition faces surveillance targeting; organizing outside the apparatus is monitored. Theater ratio (0.55): Moderate-high. Formal oversight frameworks (warrants, judicial review, legislative committees) create the appearance of constraint without providing effective functional constraint. Oversight committees lack resources to understand technical systems; classification prevents public scrutiny; national security exception clauses erode warrant requirements. The theater has increased as technical complexity has outpaced oversight comprehension.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental perspectival gap between the security apparatus and the surveilled population. The apparatus sees coordination (rope) — surveillance solves the genuine problem of threat detection and enables operational security. The population sees extraction (snare) — surveillance is purely coercive, provides no benefit, and cannot be refused. The civil rights coalition sees both (tangled rope) — acknowledging legitimate security needs while documenting extraction mechanisms. The formal oversight framework sees its own degradation (piton) — designed to constrain but reduced to theater. The analytical observer risks seeing inevitability (mountain) — but the structure data reveals contingency: surveillance capacity is increasing not because of technology necessity but because of political choice by beneficiary actors. The gap is not a matter of interpretation — it is a structural fact that the apparatus benefits while the population bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: beneficiary vs victim, power level, and exit options. The state security apparatus (institutional/arbitrage) has low d — they benefit, have high exit capacity (can redirect resources), and receive positive net value from the constraint. The surveilled citizen (powerless/trapped) has high d — they are the extraction target, have zero exit options, and experience maximum chi. The political opposition (moderate/constrained) has moderately-high d — they are explicitly targeted for suppression, have some exit options (emigration, legitimization) but at high cost. The civil rights coalition (organized/constrained) has moderate d — they have agency and international exit options, but are constrained by resource limits and legal barriers. The formal oversight framework (institutional/arbitrage) has low d despite attempting constraint — it is a beneficiary of the surveillance apparatus (provides legitimacy) and has arbitrage options (can shift to different oversight forms). The piton classification derives from theater ratio, not from measured d — the oversight framework is performative rather than extractive. The mountain classification at the analytical context produces a false summit signal: the beneficiary-driven naturalization of surveillance as inevitable technology rather than political choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the snare classification is perspectival but structurally justified. From the surveilled citizen's view, this IS a snare (trapped, no coordination benefit, pure extraction, maximum suppression). From the security apparatus's view, it IS a rope (coordination function, benefit delivery, legitimate asymmetry). From the analytical civilizational view, it appears to be a mountain (surveillance is inevitable given technology) but this is a false summit — the beneficiary group (state security apparatus) has strong incentive to naturalize surveillance as technological inevitability rather than political choice. The mandatrophy resolves: the constraint is a snare-dominant structure that benefits from false-summit naturalization. Different perspectives see different types, but the asymmetry of benefit (apparatus gains, population loses) and the asymmetry of exit options (apparatus can reallocate, population cannot opt out) are structural facts that make snare the dominant classification from the population's structural reality, regardless of how the apparatus frames it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oppression_threshold,
    'At what level of surveillance scope and targeting does the constraint shift from legitimate coordination (threat detection) to pure extraction (oppression)?',
    'Comparative analysis across regimes: mapping surveillance capacity against opposition mortality/imprisonment rates, minority targeting, and prosecution of political speech; identifying correlates of shift from coordination-dominant to extraction-dominant regimes',
    'If threshold is capacity-based (surveillance naturally becomes oppressive above certain tech level): supports mountain framing. If threshold is governance-choice based (same capacity used differently in different regimes): supports snare/tangled_rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_oppression_threshold, empirical, 'Threshold separating surveillance-for-security from surveillance-for-oppression').

omega_variable(
    exit_option_feasibility,
    'Is emigration a genuine exit option or a theoretical one? What proportion of surveilled population has realistic capacity to emigrate?',
    'Analysis of emigration barriers by socioeconomic class, documentation requirements, visa availability, asset seizure risk, family separation costs; calculating effective exit cost as proportion of lifetime income',
    'If emigration is genuinely available (cost < 5% lifetime income, legal pathways exist): agents classified with ''mobile'' exit options, reducing experienced chi. If emigration is illusory (cost > 30% lifetime income, paths blocked): confirms ''trapped'' classification, increases experienced chi.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Whether emigration is a feasible exit or theoretical escape').

omega_variable(
    oversight_effectiveness_delta,
    'Do formal oversight frameworks (warrants, judicial review, legislative committees) actually constrain surveillance scope and target selection, or are they purely performative?',
    'Audit of rejected surveillance requests vs total requests; analysis of court cases overturning surveillance; timeline analysis of oversight process vs technical deployment cycle; comparison of surveillance scope in high-oversight vs low-oversight democracies',
    'If oversight is effective (>30% rejection rate, <6 month lag behind technical deployment): piton classification is premature, snare may be overstated. If oversight is performative (<5% rejection rate, lags by years): piton confirmed, snare severity confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_effectiveness_delta, empirical, 'Functional effectiveness of formal surveillance oversight').

omega_variable(
    false_summit_naturalization,
    'Is the inevitability of surveillance capacity a genuine natural law of information technology, or a false summit that naturalizes political choices benefiting the state security apparatus?',
    'Historical analysis: surveying regimes with identical technical capability but different surveillance scope/targeting. If variation exists: political choice, not technology. If uniform: technology is the limiting factor. Examine cases where surveillance capacity was deliberately curtailed or refused (e.g., EU privacy regulations) despite technical feasibility.',
    'If technology is determining: surveillance capacity extraction is mountain-like (ineliminable). If politics is determining: the natural law framing is a false summit (beneficiary-driven). The engine''s false_summit_mountain signature will flag this if beneficiary presence is declared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether surveillance inevitability is technology or politics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_surveillance_capacity_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, state_surveillance_capacity_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(surv_tr_t10, state_surveillance_capacity_extraction, theater_ratio, 10, 0.45).
narrative_ontology:measurement(surv_tr_t20, state_surveillance_capacity_extraction, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, state_surveillance_capacity_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(surv_be_t10, state_surveillance_capacity_extraction, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(surv_be_t20, state_surveillance_capacity_extraction, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(surv_su_t0, state_surveillance_capacity_extraction, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(surv_su_t10, state_surveillance_capacity_extraction, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(surv_su_t20, state_surveillance_capacity_extraction, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_surveillance_capacity_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, political_opposition_suppression).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, financial_transaction_monitoring).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, communication_interception).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, minority_targeting_systems).

% DUAL FORMULATION NOTE:
% State surveillance capacity extraction is the primary structural constraint. Downstream constraints (opposition suppression, financial monitoring, communication interception, minority targeting) are specific instantiations of the surveillance apparatus's extraction capacity in particular domains. Each downstream constraint has its own ε and perspectives but shares the upstream extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
