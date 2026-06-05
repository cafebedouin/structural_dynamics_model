% ============================================================================
% CONSTRAINT STORY: cuban_missile_crisis_excomm_delibration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuban_missile_crisis_excomm_delibration, []).

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
 *   constraint_id: cuban_missile_crisis_excomm_delibration
 *   human_readable: The ExComm Multi-Channel Deliberation Protocol
 *   domain: political/military
 *
 * SUMMARY:
 *   The Executive Committee of the National Security Council (ExComm)
 *   established by President Kennedy following the Bay of Pigs failure
 *   represents an institutional response to decision failure: the Bay of Pigs
 *   relied on hierarchical information flow and limited deliberation; ExComm
 *   introduced multi-channel debate, back-channel communication, adversarial
 *   option testing, and recorded discussions to improve crisis
 *   decision-making. During the Cuban Missile Crisis (October 16-28, 1962),
 *   ExComm operated continuously, cycling through formal meetings (where all
 *   perspectives were aired), working groups (where options were analyzed),
 *   and back-channel communications (where negotiation signals were exchanged
 *   with Soviet leadership). The protocol successfully achieved
 *   de-escalation: Kennedy and Khrushchev negotiated a settlement (Soviet
 *   missile removal + US pledge not to invade Cuba + secret US missile
 *   withdrawal from Turkey) that avoided nuclear war. From an institutional
 *   perspective, ExComm appears as pure coordination (Rope): the protocol
 *   improved decision quality through distributed expertise and reduced
 *   hierarchical filtering. From a perspective of those unable to participate
 *   (Cuban population, Soviet public), the same protocol represents
 *   extraction: critical life-and-death decisions made in closed deliberation
 *   by external actors. The constraint's extractiveness (0.32) reflects that
 *   the protocol does coordinate expertly without significant coercion
 *   overhead, but the underlying decision-making power is concentrated and
 *   non-participatory. The low theater ratio (0.35) indicates that
 *   deliberation was substantive rather than performative — genuine
 *   disagreements (strike vs blockade), genuine debate of tradeoffs, genuine
 *   uncertainty about outcomes. The protocol does not exist primarily to
 *   legitimize predetermined decisions; it exists to improve decisions under
 *   uncertainty.
 *
 * KEY AGENTS:
 *   - President John F. Kennedy: Primary decision-maker (institutional/arbitrage) — benefits from protocol's coordination function; experiences deliberation as information aggregation enabling better choices
 *   - Secretary of Defense Robert McNamara: Strategic leader (institutional/arbitrage) — co-architect of ExComm structure; benefits from distributed expertise input
 *   - Joint Chiefs of Staff: Military authority (powerful/mobile) — contribute military analysis and options; maintain exit option of direct escalation if overruled; experience coordination function
 *   - CIA Director John McCone and intelligence analysts: Specialist experts (powerful/mobile) — provide crucial reconnaissance intelligence (U-2 photographic evidence); can exit through refusal or leak; benefit from deliberation platform
 *   - Diplomatic specialists (Dean Rusk, Averell Harriman, Theodore Sorensen): Negotiation experts (powerful/mobile) — contribute diplomatic options and risk assessment; mobile exit through resignation or leaking
 *   - Mid-level staffers and aides: Institutional subordinates (moderate/constrained) — contribute analysis and options; face career risk of public dissent; constrained by hierarchy
 *   - Soviet leadership (Nikita Khrushchev): Parallel decision-maker (institutional/constrained) — reciprocal constraint: ExComm's decisions constrain Soviet options, and vice versa
 *   - Cuban population and humanity at large: Affected non-participants (powerless/trapped) — bear existential risk of decisions made in ExComm with no voice in deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_delibration, 0.32).
domain_priors:suppression_score(cuban_missile_crisis_excomm_delibration, 0.28).
domain_priors:theater_ratio(cuban_missile_crisis_excomm_delibration, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, extractiveness, 0.32).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuban_missile_crisis_excomm_delibration, rope).
narrative_ontology:human_readable(cuban_missile_crisis_excomm_delibration, "The ExComm Multi-Channel Deliberation Protocol").
narrative_ontology:topic_domain(cuban_missile_crisis_excomm_delibration, "political/military").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, decision_quality).
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, crisis_de_escalation).
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, institutional_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCOMM INSTITUTIONAL CORE (ROPE) — Kennedy and McNamara experience the deliberation protocol as pure coordination: multi-channel debate (formal meetings, back-channel communication, adversarial groups) reduces decision risk and distributes accountability. No extraction; genuine coordination benefit. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: JOINT CHIEFS OF STAFF (ROPE) — Military commanders maintain exit option (direct escalation to President if blocked) but experience ExComm as coordination mechanism that channels their input into decision-making. Theater is low (genuine debate, not performative ritual). d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.23. Moderate effective extraction reflects constraint-relative power asymmetry (civilian override possible) but also genuine coordination function.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SPECIALIST ANALYSTS / INTELLIGENCE COMMUNITY (ROPE) — CIA and military intelligence provide crucial information inputs. Can exit by refusing to brief or by leaking to media (mobile exit). Experience ExComm as coordination that elevates their expertise into decisions. d≈0.45, f(d)≈0.56, σ=1.2 → χ≈0.21.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-LEVEL SUBORDINATES AND AIDES (TANGLED ROPE) — Junior officials and staffers have constrained exit (career risk of dissent in hierarchical structure) but also benefit from coordination function: their analysis and recommendations reach the highest decision-making level. Mixed extraction and coordination. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CUBAN POPULATION / HUMANITY AT LARGE (SNARE) — Affected by decisions made in ExComm but cannot participate, exit, or even know deliberations occurring. Pure extraction: bear risk of nuclear annihilation with no voice. d≈0.98, f(d)≈1.43, σ=1.2 → χ≈0.65. This perspective reveals extraction hidden within the coordination mechanism.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL REFORM VIEW (SCAFFOLD) — From a generational perspective, ExComm is temporary coordination scaffolding addressing Bay of Pigs failure. The protocol's specific deliberation structure (formal meetings + back-channels + devil's advocate + recorded discussions) is adopted as a model for crisis decision-making but is contingent on Kennedy administration. Later presidents adopt modified versions or abandon the structure. The sunset is institutional: when the immediate crisis ends, the protocol reverts to standard NSC procedures. χ≈0.18; theater_ratio=0.35 (genuine deliberation with some performative briefing ritual).
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuban_missile_crisis_excomm_delibration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(cuban_missile_crisis_excomm_delibration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The protocol genuinely improves decision-making through distributed expertise (intelligence analysis, military options, diplomatic tradoffs all incorporated). No actor is coerced into participation by ExComm itself — all participants are institutional officials already in the decision structure. The extractiveness score reflects that while the protocol distributes expertise inputs upward into presidential decision-making (a form of extraction from the experts' labor), it does so without suppressing alternatives or enforcing participation against will. The beneficiaries (decision quality, de-escalation success, institutional legitimacy) are real and substantial. Suppression (0.28): Low-moderate. Significant constraints on deliberation include: formal hierarchy (Kennedy's authority is never questioned), information asymmetry (inability to know Soviet intentions), time pressure (13-day crisis), and implicit pressure for consensus (public dissent would signal weakness to Soviets). But suppression is not severe — dissenting views are aired (devil's advocate role), military strike option is seriously debated, and multiple paths are explored. Theater ratio (0.35): Low. Deliberation is substantive rather than performative. Recorded transcripts show genuine disagreement (Hawk vs Dove positions), genuine uncertainty (will blockade work?), genuine risk assessment (how will Soviets respond?). The theater component (presentation of options to appear deliberative) is present but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. The institutional beneficiaries (Kennedy, McNamara) experience ExComm as pure coordination (Rope: d≈0.08) — improved decisions through aggregated expertise. The powerful specialists (military, intelligence) experience it as coordination with modest constraint (Rope: d≈0.45-0.48) — their expertise is valued but ultimate authority remains with civilian leadership. The moderate mid-level actors experience mixed coordination and constraint (Tangled Rope: d≈0.68) — they benefit from the platform for their analysis but face career risk of dissent. The powerless actors excluded from deliberation (Cuban population) experience pure extraction (Snare: d≈0.98) — decisions made about their survival with no participation. The analytical observer (generational view) sees a temporary scaffold (Scaffold: analytical) — the protocol is adopted as a model for crisis decision-making but is contingent on specific historical moment and Kennedy's management style. All these classifications emerge from the same base extractiveness (0.32), suppression (0.28), theater (0.35) — the divergence is perspectival, not structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Kennedy administration (institutional/arbitrage): Beneficiary of coordination function; d≈0.08, f(d)≈-0.11 → negative effective extraction (net beneficiary through improved decisions). Military and intelligence specialists (powerful/mobile): Contribute expertise, maintain exit options, experience modest constraint from civilian authority; d≈0.45-0.48, f(d)≈0.56-0.60 → moderate effective extraction (0.21-0.23 χ). Mid-level staffers (moderate/constrained): Benefit from platform, constrained by hierarchy and career risk; d≈0.68, f(d)≈1.02 → moderate effective extraction (0.33 χ). Cuban population (powerless/trapped): No voice, no exit, bear existential risk; d≈0.98, f(d)≈1.43 → high effective extraction (0.65 χ). The directionality chain reveals that the same institutional protocol benefits institutional decision-makers, moderately constrains powerful specialists, meaningfully constrains moderate participants, and severely extracts from those outside the deliberation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    back_channel_effectiveness,
    'Did the back-channel communications (Dobrynin-RFK channel) provide genuine coordination benefit or primarily extract information advantage for Soviet negotiating position?',
    'Declassified Soviet records; comparison of information flows in formal ExComm meetings vs back-channel communications; analysis of which side''s proposals ultimately prevailed',
    'If genuine coordination: confirms rope classification. If information extraction by Soviets: reveals snare or tangled_rope hidden within the protocol''s appearance of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(back_channel_effectiveness, empirical, 'Whether back-channel communications achieved genuine coordination or enabled extraction').

omega_variable(
    devil_advocate_authenticity,
    'Was Theodore Sorensen''s devil''s advocate role (arguing for military strike options) a genuine epistemic check or performative dissent masking a predetermined decision path?',
    'Analysis of ExComm transcripts and declassified records; comparison of devil''s advocate arguments to actual option evaluation criteria; determination of whether strike option was ever seriously considered after initial rejection',
    'If authentic: supports rope classification — genuine deliberation structure. If performative: reveals piton elements (ritual dissent without real option reconsideration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devil_advocate_authenticity, empirical, 'Whether devil''s advocate role provided authentic epistemic check').

omega_variable(
    information_asymmetry_regarding_soviet_intent,
    'Did ExComm''s information disadvantage regarding Soviet decision-making (inability to know whether Khrushchev was committed to removing missiles or testing US resolve) constitute a structural constraint on deliberation quality or a manageable uncertainty?',
    'Comparison of ExComm''s estimate of Soviet intentions with actual Soviet decision-making revealed in declassified records; analysis of how uncertainty shaped option evaluation',
    'If structural constraint: ExComm''s deliberation quality was inherently limited by opacity (snare or tangled_rope features). If manageable: coordination mechanism functioned despite information gaps.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_regarding_soviet_intent, empirical, 'Whether information asymmetry about Soviet intent constrained deliberation quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuban_missile_crisis_excomm_delibration, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excomm_tr_t0, cuban_missile_crisis_excomm_delibration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(excomm_tr_t7, cuban_missile_crisis_excomm_delibration, theater_ratio, 7, 0.32).
narrative_ontology:measurement(excomm_tr_t13, cuban_missile_crisis_excomm_delibration, theater_ratio, 13, 0.35).

% Extraction over time
narrative_ontology:measurement(excomm_be_t0, cuban_missile_crisis_excomm_delibration, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(excomm_be_t7, cuban_missile_crisis_excomm_delibration, base_extractiveness, 7, 0.26).
narrative_ontology:measurement(excomm_be_t13, cuban_missile_crisis_excomm_delibration, base_extractiveness, 13, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuban_missile_crisis_excomm_delibration, enforcement_mechanism).
narrative_ontology:affects_constraint(cuban_missile_crisis_excomm_delibration, cuban_missile_crisis_blockade_escalation).
narrative_ontology:affects_constraint(cuban_missile_crisis_excomm_delibration, soviet_missile_deployment_strategy).
narrative_ontology:affects_constraint(cuban_missile_crisis_excomm_delibration, bay_of_pigs_intelligence_failure).

% DUAL FORMULATION NOTE:
% ExComm deliberation protocol is upstream of the actual blockade decision and missile negotiation. The protocol's effectiveness (ε=0.32 as pure coordination mechanism) should be distinguished from the downstream policy outcomes (blockade vs strike, negotiation success). This story captures the institutional deliberation structure; related constraints capture the strategic decisions made within that structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cuban_missile_crisis_excomm_delibration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
