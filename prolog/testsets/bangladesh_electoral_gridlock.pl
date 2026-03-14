% ============================================================================
% CONSTRAINT STORY: bangladesh_electoral_gridlock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bangladesh_electoral_gridlock, []).

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
 *   constraint_id: bangladesh_electoral_gridlock
 *   human_readable: Bangladesh Electoral Gridlock and Institutional Stalemate
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Bangladesh electoral politics exhibits a structural constraint where the
 *   formal democratic system (regular elections, constitutional procedures,
 *   multi-party competition) coexists with systematic manipulation that
 *   undermines electoral credibility. The constraint manifests as a gridlock
 *   between the ruling coalition's need for electoral legitimacy and the
 *   opposition's inability to achieve power through elections. This creates a
 *   hybrid mechanism: elections continue (coordination function), but
 *   outcomes are predetermined through administrative pressure, vote
 *   manipulation, and strategic disqualification (extraction function). The
 *   theater ratio has risen from 0.55 to 0.68 over the interval, indicating
 *   increasing emphasis on electoral performance without corresponding
 *   increase in genuine competition. The constraint is neither pure
 *   coordination (Rope) nor pure extraction (Snare) but a tangled
 *   combination. Military influence, formal institutional procedures,
 *   opposition incapacity, and international pressure all interact to sustain
 *   the gridlock. The analytical observer risks naturalizing this as an
 *   inevitable feature of post-colonial democratic transition, but the
 *   structural data reveals it as a contingent institutional capture with
 *   identifiable beneficiaries (ruling coalition, military) and victims
 *   (opposition, electoral credibility, public trust).
 *
 * KEY AGENTS:
 *   - Ruling Coalition and Government: Primary beneficiary (institutional/arbitrage) — maintains power through elections while controlling outcomes; uses electoral legitimacy to resist international pressure
 *   - Military Establishment: Primary beneficiary (institutional/constrained) — benefits from civilian cover for military influence; requires elections to maintain legitimacy but constrains outcomes
 *   - Opposition Parties: Primary victim (powerless/trapped) — trapped in elections they cannot win while boycotts are punished; participation legitimizes fraudulent outcomes
 *   - Civil Society and Media: Secondary victim (moderate/constrained) — face harassment and censorship; constrained but not immobilized; provide monitoring function
 *   - Electoral Commission: Institutional actor (institutional/arbitrage) — performs electoral procedures; credibility has degraded to piton status
 *   - International Community: Organized pressure (organized/mobile) — classifies gridlock as temporary institutional failure with sunset path through norm diffusion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing capture as post-colonial inevitability; the engine identifies this as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bangladesh_electoral_gridlock, 0.58).
domain_priors:suppression_score(bangladesh_electoral_gridlock, 0.65).
domain_priors:theater_ratio(bangladesh_electoral_gridlock, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bangladesh_electoral_gridlock, extractiveness, 0.58).
narrative_ontology:constraint_metric(bangladesh_electoral_gridlock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bangladesh_electoral_gridlock, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bangladesh_electoral_gridlock, tangled_rope).
narrative_ontology:human_readable(bangladesh_electoral_gridlock, "Bangladesh Electoral Gridlock and Institutional Stalemate").
narrative_ontology:topic_domain(bangladesh_electoral_gridlock, "political/institutional").

domain_priors:requires_active_enforcement(bangladesh_electoral_gridlock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bangladesh_electoral_gridlock, incumbent_ruling_coalition).
narrative_ontology:constraint_beneficiary(bangladesh_electoral_gridlock, military_establishment).
narrative_ontology:constraint_victim(bangladesh_electoral_gridlock, opposition_parties).
narrative_ontology:constraint_victim(bangladesh_electoral_gridlock, electoral_credibility).
narrative_ontology:constraint_victim(bangladesh_electoral_gridlock, democratic_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPPOSITION PARTIES (SNARE) — Trapped in a system where electoral participation is rigged against them; boycotts are punished through detention and violence; participation legitimizes fraudulent outcomes. No viable exit from the constraint. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY AND INDEPENDENT MEDIA (TANGLED ROPE) — Constrained by legal harassment, funding cuts, and threat to safety, but also embedded in the electoral coordination ecosystem. Election monitoring functions provide genuine coordination value (voter information) alongside suppression mechanisms (intimidation, censorship). High suppression but not total immobility.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RULING COALITION (ROPE) — Benefits from the electoral coordination function while using state machinery to ensure favorable outcomes. Experiences the constraint as a coordination mechanism: elections legitimize governance and distribute patronage. Net beneficiary with high exit optionality through administrative control.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY ESTABLISHMENT (TANGLED ROPE) — Constrained by need for civilian legitimacy but benefits from behind-scenes influence over electoral outcomes. Coordinates security/stability functions while extracting decision-making power from civilian institutions. Requires active enforcement (detention, implicit threats) to maintain the balance.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL COMMUNITY (SCAFFOLD) — Organized agents (UN, bilateral missions, civil society networks) classify the gridlock as a temporary institutional failure with a developmental sunset: electoral norms and international pressure create incentives toward genuine competition. Exit path visible through generational institutional reform and voting norm internalization.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ELECTORAL COMMISSION (PITON) — The electoral machinery persists through institutional inertia despite documented dysfunction. Elections are held (theater ratio 0.68), procedures are performed, commissions operate — but the real selection occurs through extra-institutional mechanisms (military signaling, administrative pressure, vote suppression). The formal coordination function has atrophied; the constraint is maintained because alternatives haven't yet fully displaced it.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some friction between democratic aspiration and institutional capacity is inherent to post-colonial state development. The gridlock reflects inescapable tension between civilian institutions, military power, and electoral legitimacy. However, the structural data contradicts this naturalization — the engine will identify this as a false summit, revealing that the 'inevitable transition problem' framing obscures contingent institutional capture.
constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bangladesh_electoral_gridlock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bangladesh_electoral_gridlock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bangladesh_electoral_gridlock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bangladesh_electoral_gridlock, TR),
    TR >= 0.70.

:- end_tests(bangladesh_electoral_gridlock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The ruling coalition captures significant value from the electoral constraint — they gain legitimacy from elections while controlling outcomes, extracting political power without competitive risk. The measurement trajectory (0.42 → 0.58) indicates accumulating extraction: each cycle, the confidence in predetermined outcomes increases, and opposition capacity to compete decreases. Suppression (0.65): Moderate-high. Opposition parties face detention, legal harassment through caretaker government provisions, voter registration irregularities, and implicit threats of violence. But suppression is not total — elections occur, some opposition candidates run, limited international observation is permitted. The suppression is calibrated to ensure outcomes while maintaining a facade of electoral contestation. Theater ratio (0.68): High and rising. Electoral procedures are increasingly performative — results are known before voting; international observers are managed; media coverage is controlled; the ritual of elections obscures the predetermined nature of outcomes. The trajectory from 0.55 to 0.68 reflects growing gap between electoral theater and actual competitive function.
 *
 * PERSPECTIVAL GAP:
 *   Opposition perspective (Snare) perceives maximum extraction and immobility. Ruling coalition perspective (Rope) perceives coordination and legitimacy benefit. Military perspective (Tangled Rope) perceives both benefit (power) and constraint (legitimacy requirement). Civil society perspective (Tangled Rope) perceives both monitoring function and suppression. International perspective (Scaffold) perceives temporary failure with reform path. Electoral commission perspective (Piton) perceives degraded ritual maintained by inertia. The analytical observer perspective (Mountain) risks naturalizing the gridlock as inevitable — but the perspectival gap itself reveals the gap is institutional capture, not natural law. If the constraint were truly immutable, all perspectives would converge on Mountain. Instead, they diverge sharply, indicating structural contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling coalition's directional position (d ≈ 0.20) reflects beneficiary status with arbitrage exit: they control electoral administration and can move outcomes without incurring costs. Opposition parties (d ≈ 0.92) reflect victim status with trapped exit: they have no institutional mechanism to escape the constraint and bear full suppression costs. Military (d ≈ 0.55) reflects split position: constrained by legitimacy requirement but beneficiary from behind-scenes influence, creating a stable mixed extraction. Civil society (d ≈ 0.65) reflects secondary victim status with mobile exit: they face suppression but can partially exit through international networks. International observers (d ≈ 0.72, analytical position) derive moderate d from observer status without direct extraction or benefit. These directionality values, fed through the sigmoid f(d), produce the chi values that differentiate how each agent experiences the constraint intensity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how two-institutional-actor perspectives (ruling coalition and military) both classify as beneficiaries but with different relationship types. The ruling coalition sees pure coordination benefit (Rope). The military sees mixed benefit-and-constraint (Tangled Rope) because they require civilian legitimacy to justify their power. This is not a paradox but a decomposition: the same electoral gridlock is Rope from the ruling coalition's coordinate-and-benefit perspective, but Tangled Rope from the military's coordinate-but-constrained perspective. The opposition sees Snare because they are trapped with no exit. The international community sees Scaffold because they perceive a generational sunset path through norm diffusion. No single type is 'the' answer — the presheaf of perspectives over the institutional space reveals that the constraint is a stable hybrid: genuine coordination of electoral procedure coexists with extraction of political power. The mandatrophy is resolved by recognizing that hybrid types are exactly the correct classification for systems where coordination and extraction are inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_exit_mechanism,
    'What would induce the military establishment to voluntarily exit the electoral constraint, and is such an exit structurally possible without regime collapse?',
    'Comparative analysis with successful democratic transitions in South Korea, Indonesia, and Pakistan; identification of institutional safeguards that reduced military perception of existential threat from civilian elections',
    'If exit mechanism exists: the constraint is contingent institutional capture (Tangled Rope confirmed). If no viable exit: the constraint approaches Mountain status — military intervention becomes a structural necessity of state maintenance in this context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_exit_mechanism, empirical, 'Whether military exit from electoral control is institutionally possible').

omega_variable(
    opposition_coalition_threshold,
    'What level of opposition coalition strength would force a genuine democratic opening, and is such strength achievable through current institutional channels?',
    'Historical analysis of opposition mobilization during 2013-2014 and 2023-2024 cycles; identification of critical mass thresholds for collective defection by ruling-party insiders',
    'If threshold is achievable: opposition Snare is temporary (Scaffold sunset becomes plausible). If threshold is unachievable: opposition is structurally powerless, and Snare becomes entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_threshold, empirical, 'Opposition coalition critical mass for forcing democratic opening').

omega_variable(
    electoral_credibility_recovery_path,
    'Can electoral credibility be recovered through institutional reform (independent commission, observer access, vote counting transparency) without regime change?',
    'Case comparison with Sri Lanka, Malaysia, and Thailand electoral reforms post-2000; identification of specific institutional changes that restored observer confidence without triggering power transfer',
    'If recovery is possible: scaffold perspective gains structural credibility — generational reform path is real. If recovery requires regime turnover: the constraint is not solvable through incremental reform, and opposition Snare perception is strategically justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_credibility_recovery_path, empirical, 'Whether electoral credibility can recover through institutional reform').

omega_variable(
    suppression_internalization,
    'To what degree has electoral suppression been internalized by opposition supporters as inevitable, creating identity-locked behavior that persists independent of structural change?',
    'Qualitative analysis of opposition political narratives; measurement of participation intent vs actual voting behavior; tracking of behavioral change post-structural reform in comparison countries',
    'If internalization is high: constraint persists through cognitive capture even if institutional barriers are removed. If low: removing suppression mechanisms would rapidly restore participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree of internalized suppression among opposition supporters').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bangladesh_electoral_gridlock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bgd_elec_tr_t0, bangladesh_electoral_gridlock, theater_ratio, 0, 0.55).
narrative_ontology:measurement(bgd_elec_tr_t5, bangladesh_electoral_gridlock, theater_ratio, 5, 0.62).
narrative_ontology:measurement(bgd_elec_tr_t10, bangladesh_electoral_gridlock, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(bgd_elec_be_t0, bangladesh_electoral_gridlock, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bgd_elec_be_t5, bangladesh_electoral_gridlock, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(bgd_elec_be_t10, bangladesh_electoral_gridlock, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bangladesh_electoral_gridlock, enforcement_mechanism).
narrative_ontology:affects_constraint(bangladesh_electoral_gridlock, military_civilian_balance).
narrative_ontology:affects_constraint(bangladesh_electoral_gridlock, opposition_coalition_credibility).
narrative_ontology:affects_constraint(bangladesh_electoral_gridlock, international_pressure_efficacy).

% DUAL FORMULATION NOTE:
% The electoral gridlock is downstream of institutional separation-of-powers failure but represents a distinct structural constraint. The military influence constraint and opposition capacity constraint are upstream; the electoral gridlock is their synthesis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bangladesh_electoral_gridlock, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
