% ============================================================================
% CONSTRAINT STORY: nuclear_deterrence_credibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_deterrence_credibility, []).

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
 *   constraint_id: nuclear_deterrence_credibility
 *   human_readable: Nuclear Deterrence Credibility
 *   domain: geopolitical/military/strategic
 *
 * SUMMARY:
 *   Nuclear deterrence credibility represents one of the highest-stakes
 *   constraint systems in human civilization. The constraint operates at the
 *   intersection of military strategy, institutional inertia, and existential
 *   risk. It is the mechanism by which nuclear-armed powers maintain their
 *   dominance over non-nuclear states and manage conflicts with each other by
 *   making mutual destruction unthinkable. The constraint exhibits
 *   fundamental classification instability: from the perspective of
 *   nuclear-armed powers and their beneficiaries, it is primarily a
 *   coordination mechanism (rope) that prevents major wars. From the
 *   perspective of global civilian populations with no agency in strategic
 *   decision-making, it is pure extraction (snare) — they bear maximal
 *   existential risk with zero exit options. The key structural tension is
 *   between credibility and safety: maintaining deterrence credibility
 *   appears to require continuous performance (weapons tests, military
 *   exercises, doctrinal statements, threat displays), yet this performance
 *   creates accident risk and maintains the apparatus of institutional
 *   dominance. The theater_ratio has risen from 0.48 (1975) to 0.64 (2026) as
 *   the strategic environment has changed — the Soviet Union dissolved, the
 *   justification for massive nuclear arsenals weakened, yet the
 *   infrastructure persists. Modern deterrence is increasingly performative:
 *   it maintains the appearance and threat of use to prevent use, creating a
 *   paradoxical feedback loop where preventing war requires maintaining the
 *   credible threat of apocalypse.
 *
 * KEY AGENTS:
 *   - Nuclear-Armed Powers: Institutional beneficiaries (institutional/arbitrage) — extract strategic dominance, prevent peer wars, maintain technological leadership, constrain non-nuclear competitors
 *   - Global Civilian Populations: Primary victims (powerless/trapped) — bear existential risk with zero agency, no legal recourse, no exit option, no alternative governance structure
 *   - Non-Nuclear States: Secondary victims (organized/constrained) — constrained by NPT framework, alliance dependencies, and threat from nuclear-armed neighbors; have limited exit options (acquire weapons, join alliance, attempt disarmament)
 *   - Military Establishments (Non-Nuclear): Mixed position (institutional/constrained) — experience genuine coordination benefits (deterrence, stability) alongside extraction (technology restrictions, strategic autonomy constraints)
 *   - Regional Hegemons: Complex intermediate actors (powerful/constrained) — benefit from implied nuclear umbrella but face strategic coercion and constrained autonomy
 *   - Cold War Institutional Apparatus: Institutional inertia actor (institutional/constrained) — persists through budget allocation, doctrinal momentum, and coordinated unilateral risk; maintains theater with declining functional justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_deterrence_credibility, 0.68).
domain_priors:suppression_score(nuclear_deterrence_credibility, 0.72).
domain_priors:theater_ratio(nuclear_deterrence_credibility, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_deterrence_credibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_deterrence_credibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_deterrence_credibility, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_deterrence_credibility, snare).
narrative_ontology:human_readable(nuclear_deterrence_credibility, "Nuclear Deterrence Credibility").
narrative_ontology:topic_domain(nuclear_deterrence_credibility, "geopolitical/military/strategic").

domain_priors:requires_active_enforcement(nuclear_deterrence_credibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_deterrence_credibility, nuclear_armed_powers).
narrative_ontology:constraint_victim(nuclear_deterrence_credibility, global_civilian_populations).
narrative_ontology:constraint_victim(nuclear_deterrence_credibility, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_deterrence_credibility, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL CIVILIAN POPULATION (SNARE) — Trapped in a system that offers no exit. Hundreds of millions exist under the shadow of potential nuclear use with zero agency over the strategic calculations that maintain deterrence. Suppression is near-total: no alternative governance structure available, no legal recourse, no ability to opt out of geopolitical sovereignty structures. Extraction is maximum: the population bears the full risk of accidental escalation, misfire, or rational deliberate use, while the benefits of 'security' accrue to nation-state elites. The existential threat itself is the suppression mechanism.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NUCLEAR STATES (SNARE) — Officially organized as states but functionally constrained by nuclear-armed powers. Exit options are severely limited: joining a nuclear umbrella (alliance dependence), acquiring nuclear weapons (triggering sanctions and isolation), or attempting disarmament treaties (historically ineffective). The NPT framework itself is extractive — non-nuclear states surrendered the option to develop deterrents in exchange for 'security assurances' that nuclear powers are not obligated to honor. High suppression through legal/diplomatic coercion; high extraction through military vulnerability and coercive diplomacy.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY ESTABLISHMENT (NON-NUCLEAR) (TANGLED ROPE) — Professional military officers in non-nuclear states experience genuine coordination benefits: deterrence against regional rivals, prevention of conquest, stable military doctrines. Simultaneously, nuclear-armed powers extract: constraining military options, imposing foreign policy alignment, limiting technological development. High suppression through weapons treaties and alliance dependencies; genuine but asymmetric extraction. This perspective shows why tangled rope is stable — coordination benefits exist alongside extraction.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NUCLEAR-ARMED POWER (ROPE) — Experiences deterrence primarily as coordination mechanism: communicating capability to prevent war, enabling stable deterrent relationships with other nuclear powers. Theater_ratio is moderate here — signaling must be credible, so some performative dimension exists (weapons tests, military exercises), but the coordination function is genuine. Net beneficiary with maximum exit optionality (can escalate unilaterally, can negotiate arms reductions, can threaten use). Effective extraction runs toward this actor; they experience constraint as solution to coordination problem.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR DETERRENCE DOCTRINE (PITON) — The apparatus of mutual assured destruction, strategic command structures, and nuclear command authority persists largely through institutional inertia. The primary function — preventing USSR-US direct conflict — ended in 1991, yet the infrastructure, budgets, and doctrinal frameworks persist. Theater_ratio is high: nuclear war games, strategic force readiness exercises, and declaratory policy perform deterrence constantly without serving its original coordination function. Modern deterrence is maintained not because it solves current strategic problems but because dismantling it requires coordinating unilateral risk. The piton classification emerges from high theater despite institutional power.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL HEGEMON (TANGLED ROPE) — A powerful non-nuclear or transitional state experiences both genuine coordination benefits (regional stability through implicit nuclear umbrella) and extraction (strategic autonomy constrained by nuclear-armed patron, military technology restricted, coercive diplomacy during crises). Suppression is asymmetric: exit options exist in principle but carry catastrophic cost. This perspective demonstrates why tangled rope is stable at intermediate power levels — the coordination benefits are real enough to maintain the relationship despite significant asymmetric extraction.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MAD LOGIC (MOUNTAIN) — From a civilizational perspective, nuclear deterrence appears as a natural law of 21st-century geopolitics: a technological fact that cannot be 'solved' by policy because the knowledge of weaponization is irreversible. Once nuclear weapons are possible, deterrence becomes structurally necessary — the only way to prevent their use is to make use unthinkable through the threat of annihilation. This perspective sees the constraint as an immutable feature of the modern world. However, the structural data reveals this as a false summit: the extractive asymmetries, the suppression mechanisms, and the beneficiary/victim structure are contingent institutional arrangements, not laws of physics. The 'inevitability' framing naturalizes a set of political choices.
constraint_indexing:constraint_classification(nuclear_deterrence_credibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_deterrence_credibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_deterrence_credibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_deterrence_credibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_deterrence_credibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_deterrence_credibility, TR),
    TR >= 0.70.

:- end_tests(nuclear_deterrence_credibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts strategic dominance and enforces asymmetric vulnerability, but the extraction is not absolute because it also genuinely prevents certain wars and provides some security benefits to non-nuclear allies. The value reflects that beneficiaries capture most of the coordination gains (asymmetric), victims bear existential risk (high cost), and exit options are severely constrained. The value has risen from 0.42 (1975, during mutual deterrence confidence) to 0.68 (2026, during proliferation anxiety and alliance uncertainty). Suppression (0.72): High. The suppression mechanisms include legal structures (NPT enforced through sanctions), military dominance (nuclear powers can coerce non-nuclear states), diplomatic isolation of proliferators, and the irreversibility of nuclear knowledge itself (cannot 'undo' the possibility of nuclear weapons). Some alternatives exist (arms control, disarmament treaties) but are historically ineffective and do not reduce suppression materially. Theater_ratio (0.64): Moderate-high. Modern deterrence requires continuous performance: strategic force readiness exercises, doctrinal statements, weapons modernization, alliance management, threat displays. Much of this is performative in the sense that it maintains belief in willingness to use rather than actual capability. However, the theater is necessary for credibility — if a nuclear power fails to signal its willingness to use weapons, deterrence collapses. The rise from 0.48 to 0.64 reflects that post-Cold War deterrence is less grounded in the material threat of mutual destruction and more dependent on theatrical signaling.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (rope) and victim (snare) perspectives is maximal because they have fundamentally different exit options. Nuclear powers can exit credibly (disarm, negotiate, threaten escalation); global populations cannot (they cannot opt out of geopolitical sovereignty). This creates an unbridgeable gap in classification. The non-nuclear state perspective (tangled rope) is intermediate — they have some agency (can acquire weapons, can join alliances) but face high costs and legal coercion (suppression via NPT). The institutional inertia perspective (piton) reveals that the apparatus persists despite losing its original Cold War justification, suggesting the extraction mechanism has become self-perpetuating through institutional momentum rather than functional necessity. The false mountain at the analytical level is the greatest gap: the 'inevitability' framing of nuclear deterrence naturalizes what is actually a contingent set of strategic choices and institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Nuclear-armed powers are beneficiaries with arbitrage exit (can escalate unilaterally, can negotiate down, can withdraw from treaties): d ≈ 0.15, f(d) ≈ -0.01, experienced extraction minimal or negative (they benefit). Global populations are victims with trapped exit (no way out of geopolitical sovereignty structures): d ≈ 0.95, f(d) ≈ 1.42, experienced extraction maximal. Non-nuclear states are victims with constrained exit (can acquire weapons, can join alliances, can attempt disarmament, but all options are costly or ineffective): d ≈ 0.72, f(d) ≈ 1.08, experienced extraction high. Military establishments in non-nuclear states are mixed (beneficiaries of deterrence coordination, victims of technological constraints): d ≈ 0.50, f(d) ≈ 0.65, experienced extraction moderate. The calculation across perspectives shows that the constraint systematically benefits organized actors with exit options while extracting from unorganized actors with no exit. This asymmetry is the definition of a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE AT HIGH EXTRACTIVENESS (0.68) RESOLVES MANDATROPHY: The constraint is classified as snare from the victim perspective (global populations, non-nuclear states) at high extractiveness because it extracts strategic dominance and existential risk with minimal coordination benefit to the victims. The coordination benefit exists but is asymmetric: it benefits nuclear-armed powers and prevents certain wars between them, but this benefit is not shared with the populations bearing the risk. The snare classification is not contradicted by the rope perspective from nuclear powers — both are true. The mandatrophy is resolved by recognizing that the constraint system contains multiple constraints with different ε values: (1) the coordination problem between nuclear-armed peers (high ε for snare, low ε for rope depending on perspective), (2) the dominance extraction of non-nuclear states (high ε for snare), (3) the institutional inertia apparatus (high theater_ratio, moderate ε). The system is integrated but structurally decomposable. The comprehensive classification is snare because the constraint's primary structural effect is extractive dominance of non-nuclear actors by nuclear-armed powers, masked by rhetoric of 'coordination' and 'stability'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_measurement_mechanism,
    'What evidence counts as proof that nuclear deterrence is credible? Is deterrence success defined as non-use (absence of war) or as believable commitment to use (threat capacity)?',
    'Distinction between historical non-use as selection bias (absence of evidence confounded with evidence of absence) vs. explicit commitment signaling (weapons tests, doctrine statements, military exercises). Empirical test: compare deterrence effectiveness under transparent vs. opaque commitment strategies.',
    'If credibility = believable commitment: extractiveness classification holds at 0.68 (high performative component necessary to maintain belief). If credibility = actual prevention of conflict: extractiveness might be lower (the constraint works precisely by making use unthinkable, not by requiring constant signaling theater). If non-use is selection bias: the entire deterrence logic is unfalsifiable and the extraction mechanism is pure — classify as snare at 0.82+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_measurement_mechanism, conceptual, 'Definitional question: what counts as deterrence success?').

omega_variable(
    proliferation_feedback_loop,
    'Does nuclear deterrence by existing powers prevent or encourage proliferation among non-nuclear states? Is the constraint''s primary function stabilization or domination?',
    'Empirical analysis of proliferation timelines: did states acquire weapons despite deterrence threats (Iran, North Korea, Pakistan) or because deterrence credibility was doubted? Comparative analysis of security outcomes for sheltered vs. independent nuclear powers vs. non-nuclear alliance members.',
    'If deterrence prevents proliferation: constraint stabilizes international order, snare classification reflects victim vulnerability but not extraction for extraction''s sake. If deterrence enables proliferation through credibility competition: constraint is structurally extractive, forcing asymmetric power structures that incentivize nuclear acquisition among threatened states. Extractiveness would rise to 0.78+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proliferation_feedback_loop, empirical, 'Whether deterrence prevents or induces proliferation').

omega_variable(
    credible_commitment_paradox,
    'To maintain deterrence credibility, must nuclear powers continuously perform willingness to use (weapons tests, threats, military exercises), and does this performance create accident risk that exceeds the war-prevention benefit?',
    'Historical analysis of close calls (Cuban Missile Crisis, Able Archer 83, false alarms in early warning systems). Statistical modeling of accident probability under high alert vs. low alert postures. Comparison of war probabilities in pre-nuclear vs. nuclear eras controlling for technological and institutional variables.',
    'If theater is necessary for credibility and theater creates accident risk: suppression may be higher than measured (continuous low-level existential risk maintained to prevent higher-level wars). Extractiveness could rise through the mechanism of ''necessary risk theater''. If credible commitment can be maintained without continuous performance: theater_ratio is artificially inflated and could be reduced to 0.45+ without losing deterrent effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credible_commitment_paradox, empirical, 'Whether credible commitment requires continuous performance theater').

omega_variable(
    adversary_rationality_assumption,
    'Does nuclear deterrence depend on the assumption that all adversaries are rational actors who will calculate costs and benefits? What happens if an adversary is irrational, suicidal, or operating under a different rationality framework (religious, revolutionary, honor-based)?',
    'Historical case analysis of state actors who violated rational deterrence predictions (Japan 1941, Iran-Iraq War escalation, North Korea brinkmanship). Empirical research on decision-making under existential threat. Cross-cultural comparison of strategic rationality frameworks.',
    'If deterrence is robust to irrationality: extractiveness estimate is conservative, constraint may be snare at 0.68 as modeled. If deterrence fails against irrational actors: the constraint fails precisely when most needed, and the extraction from global population becomes maximal (0.85+) because protection is illusory. This would suggest the snare classification is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adversary_rationality_assumption, conceptual, 'Rationality assumption in deterrence logic').

omega_variable(
    arms_control_effectiveness,
    'Are arms control treaties (non-proliferation, test bans, reduction treaties) genuine alternatives to the current deterrence system, or do they perpetuate the same extraction mechanisms under different institutional forms?',
    'Comparison of extracted populations'' risk profiles under deterrence vs. under arms control regimes. Analysis of treaty compliance and verification mechanisms. Historical tracking of which treaties have actually reduced the number or readiness of nuclear weapons.',
    'If arms control is structural alternative: snare classification might decompose into multiple constraint stories with different ε values. If arms control perpetuates extraction under new framing: all paths within the constraint system are snares and the exit option is truly ''trapped'' (not just ''constrained''). This would raise the victim agent_power classification toward ''powerless'' across more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arms_control_effectiveness, empirical, 'Whether arms control offers structural exit or perpetuates extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_deterrence_credibility, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucdeter_theater_t0, nuclear_deterrence_credibility, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nucdeter_theater_t25, nuclear_deterrence_credibility, theater_ratio, 25, 0.56).
narrative_ontology:measurement(nucdeter_theater_t50, nuclear_deterrence_credibility, theater_ratio, 50, 0.64).
narrative_ontology:measurement(nucdeter_theater_t10, nuclear_deterrence_credibility, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(nucdeter_extractiveness_t0, nuclear_deterrence_credibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nucdeter_extractiveness_t25, nuclear_deterrence_credibility, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(nucdeter_extractiveness_t50, nuclear_deterrence_credibility, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(nucdeter_extractiveness_t10, nuclear_deterrence_credibility, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_deterrence_credibility, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_deterrence_credibility, nonproliferation_treaty_framework).
narrative_ontology:affects_constraint(nuclear_deterrence_credibility, alliance_dependence).
narrative_ontology:affects_constraint(nuclear_deterrence_credibility, strategic_weapons_development).

% DUAL FORMULATION NOTE:
% Nuclear deterrence credibility is upstream of multiple structural constraints: the NPT framework constrains non-nuclear states, alliance dependence creates strategic asymmetries, and weapons development cycles generate institutional momentum. The network captures how deterrence credibility is maintained through interconnected institutional arrangements, each with its own extractiveness value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
