% ============================================================================
% CONSTRAINT STORY: asymmetric_deterrence_credibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_deterrence_credibility, []).

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
 *   constraint_id: asymmetric_deterrence_credibility
 *   human_readable: Asymmetric Deterrence Credibility
 *   domain: geopolitical/security
 *
 * SUMMARY:
 *   Asymmetric deterrence credibility is the structural constraint that
 *   emerges when one party possesses weapons of mass destruction and the
 *   other does not. The constraint operates through a paradoxical mechanism:
 *   credibility requires demonstrated willingness to use catastrophic force,
 *   yet actual use would be strategically irrational. Deterrence thus depends
 *   on maintaining a credible threat of irrational action — a commitment
 *   problem that generates continuous institutional and communicative
 *   overhead. The constraint exhibits the full range of classification types
 *   depending on the observer's structural position. For the nuclear power,
 *   it functions as coordination (Rope) — a way to communicate red lines and
 *   prevent war. For the non-nuclear adversary, it functions as pure
 *   extraction (Snare) — compliance extracted through coercive threat with no
 *   coordination benefit. For civilian populations, it is mixed (Tangled
 *   Rope) — deterrence prevents major war but externalizes catastrophic risk.
 *   For military institutions, it is hybrid (Tangled Rope) — genuine
 *   deterrent coordination function combined with institutional extraction
 *   through budget capture and mission legitimacy. The measurements show
 *   increasing extractiveness (0.35 → 0.58) and theater ratio (0.52 → 0.68)
 *   over a 40-year interval, indicating that deterrence maintenance has
 *   become progressively more costly and performative as weapons systems have
 *   proliferated and strategic communication has required larger
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Adversary: Primary target (powerless/trapped) — cannot credibly escalate; faces maximum extraction through asymmetric threat
 *   - Civilian Population: Secondary victim (moderate/constrained) — benefits from conventional war prevention; bears catastrophic escalation risk with no exit option
 *   - Nuclear-Capable State Leadership: Primary beneficiary (institutional/arbitrage) — captures strategic advantage and credibility extraction; can modify deterrent posture via arms control or détente
 *   - Military Establishment of Nuclear Power: Secondary beneficiary (institutional/constrained) — benefits from mission legitimacy and budgets; extraction target for credibility maintenance costs; constrained by strategic logic preventing posture reduction
 *   - Arms Control Institutions (NPT, etc.): Institutional actor (organized/constrained) — maintain performative verification role; actual proliferation prevention delegated to strategic calculation rather than institutional enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the commitment problem as a law of strategic interaction rather than a choice to maintain nuclear arsenals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_deterrence_credibility, 0.58).
domain_priors:suppression_score(asymmetric_deterrence_credibility, 0.65).
domain_priors:theater_ratio(asymmetric_deterrence_credibility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_deterrence_credibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(asymmetric_deterrence_credibility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asymmetric_deterrence_credibility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_deterrence_credibility, tangled_rope).
narrative_ontology:human_readable(asymmetric_deterrence_credibility, "Asymmetric Deterrence Credibility").
narrative_ontology:topic_domain(asymmetric_deterrence_credibility, "geopolitical/security").

domain_priors:requires_active_enforcement(asymmetric_deterrence_credibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_deterrence_credibility, nuclear_capable_state).
narrative_ontology:constraint_beneficiary(asymmetric_deterrence_credibility, military_establishment).
narrative_ontology:constraint_victim(asymmetric_deterrence_credibility, non_nuclear_adversary).
narrative_ontology:constraint_victim(asymmetric_deterrence_credibility, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR ADVERSARY (SNARE) — Structurally trapped by asymmetric military capability. Cannot credibly threaten escalation beyond conventional means. Deterrence operates through coercive threat of disproportionate response. No exit option — withdrawal concedes strategic position; engagement risks catastrophic loss. Maximum extraction: compliance extracted through existential fear rather than coordination benefit.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVILIAN POPULATION (TANGLED ROPE) — Experiences both coordination (deterrence prevents major war) and extraction (risk of nuclear escalation is externalized onto civilian safety). High suppression — cannot exit jurisdiction or credibly challenge deterrent regime. Constrained by geographic, legal, and economic barriers. Mixed extraction and coordination.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR-CAPABLE STATE (ROPE) — Primary beneficiary. Experiences deterrence as a coordination mechanism: credibly communicating red lines enables conflict avoidance and strategic stability. Net beneficiary through credibility extraction and reduced conventional military burden. Can exit via arms control agreements or détente if strategic calculus changes. Arbitrage options preserve freedom.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY ESTABLISHMENT (TANGLED ROPE) — Institutional beneficiary with constrained options. Benefits from deterrence framework (mission legitimacy, budgets, strategic relevance). Also extraction target: credibility maintenance requires continuous military readiness, expensive infrastructure, and willingness to accept mutual vulnerability. Constrained by institutional lock-in and strategic logic — cannot credibly reduce nuclear posture without undermining deterrent.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NPT AND ARMS CONTROL INSTITUTIONS (PITON) — Institutional actor maintaining a performative role. The regime's stated function (preventing proliferation) has degraded — multiple threshold states possess or approach capability despite NPT. The institutions persist through legitimacy theater and procedural maintenance rather than functional prevention. Theater ratio high because compliance monitoring and verification are largely performative; the actual constraint on proliferation comes from strategic cost-benefit calculations, not from institutional enforcement.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GAME-THEORETIC NATURAL LAW (MOUNTAIN) — From a civilizational analytical view, deterrence credibility appears as an immutable feature of strategic interaction: any asymmetric capability differential creates an incentive for preemption that can only be managed through credible counter-threat. Mutual assured vulnerability is a logical consequence of nuclear weapons, not a contingent institutional arrangement. However, this perspective risks naturalizing what is partly a choice: credibility is maintained through active military posturing, signaling, and willingness to accept escalation risk — all contingent decisions, not laws of nature.
constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_deterrence_credibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_deterrence_credibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_deterrence_credibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_deterrence_credibility, TR),
    TR >= 0.70.

:- end_tests(asymmetric_deterrence_credibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts significantly from non-nuclear adversaries and civilian populations through coercive threat and risk externalization, but is experienced as coordination by the nuclear power. Base extractiveness of 0.58 reflects that the primary function (war prevention) is genuine, but the mechanism (catastrophic threat) creates asymmetric harm and extraction. The measurement trajectory shows increasing extractiveness (0.35 → 0.58) as deterrence infrastructure has become more elaborate and costly, suggesting that institutional expansion is amplifying the extractiveness beyond the minimal level required for strategic credibility. Suppression (0.65): High but not maximal. Non-nuclear adversaries cannot credibly exit (trapped), but some exit options exist through diplomatic accommodation, conventional build-up, or external alliance. Civilian populations face geographic, legal, and economic barriers to exit (constrained). Military institutions face institutional lock-in but retain some policy influence. Theater ratio (0.68): Substantial performative content. Credibility maintenance requires signaling through weapons deployments, strategic communications, military exercises, and declaratory policy — much of which is performative (communicating resolve to adversaries and domestic audiences) rather than functionally necessary for actual deterrence. The measurement trajectory (0.52 → 0.68) shows increasing theater as weapons systems have become more elaborate and strategic communication infrastructure has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The non-nuclear state sees snare because it experiences maximum extraction with no exit. The nuclear power sees rope because it benefits from coordination (preventing war) at low cost. These are not different measurements of the same thing — they are genuinely incompatible experiences of the same structural fact. The gap is not resolution-able by getting better data about deterrence; it is a fundamental perspectival difference rooted in structural position. This is exactly what the DR framework is designed to reveal: when a single constraint produces incompatible classifications from different positions, the framework captures that irreducible disagreement rather than forcing a false unity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. The non-nuclear adversary is a victim with no exit (trapped) → d ≈ 0.95 → f(d) ≈ 1.42, experiencing maximum extractiveness chi. The nuclear power is a beneficiary with exit options (arbitrage) → d ≈ 0.05 → f(d) ≈ -0.12, experiencing negative or low extractiveness chi (net benefit). Civilian populations are victims with constrained exit → d ≈ 0.85 → f(d) ≈ 1.15, experiencing high chi. Military institutions are beneficiaries with constrained exit → d ≈ 0.35 → f(d) ≈ 0.25, experiencing moderate chi. The analytical observer at civilizational scope with analytical exit → d ≈ 0.72 → f(d) ≈ 1.15, viewing the constraint as nearly natural but with hints of contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival decomposition. The question 'Is deterrence a coordination mechanism or an extraction mechanism?' has no single answer — it is both, depending on the observer. The non-nuclear adversary experiences extraction; the nuclear power experiences coordination. The mandatrophy is resolved by showing that all six types are legitimate readings from their respective positions. The analytical observer's mountain classification (strategic interaction laws) is a false summit revealed by the structural data: the constraint depends on contingent institutional choices (willingness to maintain nuclear posture, decisions about signaling, policy choices about arms control) rather than immutable logic. The measured theater ratio of 0.68 supports this: if deterrence were a natural law, it would require minimal theater. The high theater indicates significant performative content, which suggests that institutional choices are amplifying the extractiveness beyond what minimal deterrence would require. The constraint is not natural; it is maintained through active institutional and communicative work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_maintenance_cost,
    'What proportion of deterrence extraction is inherent to the security problem versus deliberate amplification by military institutions seeking budgetary and strategic advantage?',
    'Comparative analysis of deterrent stability in high-credibility low-cost regimes (e.g., Pakistan-India post-Kargil stability through costless signaling) versus high-credibility high-cost regimes (continuous platform modernization, strategic communication campaigns); identification of cost drivers that serve deterrence versus institutional self-interest',
    'If cost is mostly inherent: deterrence is lower-extraction (Rope from more perspectives). If cost is mostly institutional choice: deterrence is higher-extraction (Snare from more perspectives). Determines whether extractiveness should be 0.45 (coordination-heavy) or 0.65 (extraction-heavy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_maintenance_cost, empirical, 'Whether deterrence cost is inherent to strategic problem or amplified by institutional interests').

omega_variable(
    alternative_credibility_mechanisms,
    'Could deterrence credibility be maintained through lower-cost mechanisms (arms control agreements with verification, graduated response protocols, communication channels) without sacrificing strategic stability?',
    'Historical case analysis of periods of strategic stability achieved at lower deterrence cost (Cold War arms control era, Cuban Missile Crisis aftermath); identification of institutional barriers preventing adoption of lower-cost mechanisms; game-theoretic modeling of graduated-response versus overwhelming-force credibility',
    'If alternatives exist but are institutionally blocked: constraint is tangled_rope with high institutional extraction (current assessment confirmed). If alternatives are structurally impossible: constraint approaches rope (coordination-dominated). If alternatives are deliberately rejected: constraint is snare (pure extraction framed as deterrence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credibility_mechanisms, empirical, 'Whether lower-cost credibility mechanisms could maintain deterrent stability').

omega_variable(
    non_nuclear_escalation_dynamics,
    'Does deterrence credibility actually prevent conventional conflict escalation, or does it create incentive structures that make lower-intensity conflicts more likely as ''safe'' alternatives below the nuclear threshold?',
    'Longitudinal analysis of conventional conflict frequency in nuclear versus non-nuclear dyads; examination of whether nuclear deterrence increases conventional conflict risk by creating ''safe'' escalation zones; comparison of dyad stability metrics across different deterrent posture configurations',
    'If deterrence prevents escalation: coordination benefit is real (Rope perspective more accurate). If deterrence creates conventional-conflict safety zone: coordination benefit is overstated and constraint is more extractive than claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_nuclear_escalation_dynamics, empirical, 'Whether nuclear deterrence prevents or redirects conflict').

omega_variable(
    epistemic_closure_credibility_claim,
    'To what extent does the requirement for ''credible deterrence'' function as epistemic closure that prevents questioning the institutional arrangements maintaining deterrent posture?',
    'Analysis of policy discourse constraints: identify topics treated as non-negotiable versus negotiable in deterrence debates; track institutional suppression of alternative strategic frameworks; examine whether credibility-maintenance framing forecloses cost-benefit analysis of deterrence versus alternative security arrangements',
    'High epistemic closure indicates the constraint includes a suppression function not explicitly modeled in the chi calculation. Suggests true suppression may be higher than measured (0.65 → 0.75+), pushing toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_closure_credibility_claim, conceptual, 'Whether credibility requirement functions as epistemic closure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_deterrence_credibility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_deter_tr_t0, asymmetric_deterrence_credibility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(asym_deter_tr_t20, asymmetric_deterrence_credibility, theater_ratio, 20, 0.61).
narrative_ontology:measurement(asym_deter_tr_t40, asymmetric_deterrence_credibility, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(asym_deter_be_t0, asymmetric_deterrence_credibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(asym_deter_be_t20, asymmetric_deterrence_credibility, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(asym_deter_be_t40, asymmetric_deterrence_credibility, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_deterrence_credibility, enforcement_mechanism).
narrative_ontology:affects_constraint(asymmetric_deterrence_credibility, arms_control_negotiation_dynamics).
narrative_ontology:affects_constraint(asymmetric_deterrence_credibility, preemption_incentive_structure).
narrative_ontology:affects_constraint(asymmetric_deterrence_credibility, nuclear_proliferation_threshold).

% DUAL FORMULATION NOTE:
% Asymmetric deterrence credibility decomposes into multiple constraint families depending on the observable: (1) credibility-maintenance costs (extractiveness driven by institutional overhead), (2) non-nuclear escalation dynamics (conventional conflict risk below nuclear threshold), (3) epistemic closure effects (suppression of alternative security frameworks). Each has different ε and different institutional actors. The current story focuses on the credibility-maintenance trajectory. See linked constraints for alternative decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asymmetric_deterrence_credibility, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
