% ============================================================================
% CONSTRAINT STORY: the_calm_protocol_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_calm_protocol_suppression, []).

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
 *   constraint_id: the_calm_protocol_suppression
 *   human_readable: The "Calm" of Antarctic Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The 'Calm' protocol in Antarctic research stations represents a
 *   institutional constraint that operates simultaneously as a coordination
 *   mechanism (preventing psychological crisis under extreme isolation) and
 *   an extraction mechanism (suppressing individual emotional autonomy and
 *   enforcing institutional control). The constraint emerges from the genuine
 *   coordination problem of maintaining psychological and social stability
 *   when crews of 10-50 people are isolated for 8-12 months in a harsh
 *   environment with limited exit options and high interdependence. However,
 *   the protocol has accumulated extractive features beyond its original
 *   functional necessity: enforced emotional suppression, prohibition of
 *   conflict expression, normalization of psychological constraint, and
 *   institutional authority over individual emotional life. The narrative of
 *   Soh and Mbatha captures the phenomenology of this extraction — the
 *   'weight' of maintained calm, the suppression of legitimate emotion, the
 *   institutional enforcement of compliance through cultural prestige and
 *   career dependencies. The constraint demonstrates how coordination
 *   mechanisms can degrade into extraction while maintaining their original
 *   framing: 'calm is necessary for safety' becomes 'emotional suppression is
 *   necessary for institutional survival.' The theater_ratio (0.64) reflects
 *   that much of the protocol's current enforcement is performative — rituals
 *   of calm, ceremonial regard for stability, institutional signaling —
 *   rather than active structural necessity, given modern communication and
 *   resupply capabilities that substantially reduce isolation's original
 *   severity.
 *
 * KEY AGENTS:
 *   - Individual Crew Members: Primary victims (powerless/trapped) — cannot exit Antarctic environment; bear full cost of emotional suppression; suffer deferred trauma and psychological autonomy loss
 *   - Research Mission: Primary beneficiary (institutional/arbitrage) — benefits from uninterrupted data collection and operational stability; can reassign or rotate personnel
 *   - Station Administration: Secondary beneficiary (institutional/arbitrage) — maintains institutional authority and control; manages institutional reputation
 *   - Research Community: Secondary victim and beneficiary (moderate/constrained) — depends on station continuity but also vulnerable to crew psychological crisis and collaboration breakdown
 *   - Antarctic Treaty System: Organized institutional actor (organized/constrained) — coordinates international scientific cooperation; enforces protocol as coordination mechanism with sunset logic
 *   - Cold War Institutional Legacy: Structural force (institutional/arbitrage) — maintains suppression mechanism through inertia and cultural prestige; reduced functional necessity in modern context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_calm_protocol_suppression, 0.52).
domain_priors:suppression_score(the_calm_protocol_suppression, 0.68).
domain_priors:theater_ratio(the_calm_protocol_suppression, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_calm_protocol_suppression, extractiveness, 0.52).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_calm_protocol_suppression, tangled_rope).
narrative_ontology:human_readable(the_calm_protocol_suppression, "The \"Calm\" of Antarctic Protocol").
narrative_ontology:topic_domain(the_calm_protocol_suppression, "social/psychological").

domain_priors:requires_active_enforcement(the_calm_protocol_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_calm_protocol_suppression, station_administration).
narrative_ontology:constraint_beneficiary(the_calm_protocol_suppression, institutional_hierarchy).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, individual_psychological_autonomy).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, crew_social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED CREW MEMBER (SNARE) — Cannot exit the Antarctic environment; cannot escape the institutional mandate to maintain 'calm.' Trapped by geography and contract, bearing full cost of psychological suppression without recourse. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.59.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from continuous data collection and uninterrupted station operations (coordination function) but also bears costs of crew psychological strain and elevated conflict risk. Exit constrained by research mission criticality and career dependencies. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATION ADMINISTRATION (ROPE) — Primary beneficiary. Experiences 'calm' protocol as genuine coordination mechanism: preventing conflict escalation, maintaining operational safety, protecting institutional reputation. Arbitrage exit: can rotate out, delegate enforcement, or reframe protocol. d≈0.10, f(d)≈0.02, σ=0.9 → χ≈0.005. Near-zero effective extraction from administration's perspective.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANTARCTIC TREATY SYSTEM (SCAFFOLD) — Organized institutional framework (scientific cooperation, environmental protection, conflict prevention) that sees crew welfare protocols as temporary scaffolding toward mature international scientific governance. Has sunset logic: as international collaboration norms mature and multi-national crews normalize, centralized psychological suppression mechanisms become unnecessary. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR LEGACY (PITON) — The 'calm' protocol originated in mid-20th century military/strategic isolation contexts (single-nation stations, extreme isolation, high political stakes). Modern Antarctic stations are substantially more connected (communication, resupply, multi-national presence), making the suppression mechanism theatrically maintained through institutional inertia. theater_ratio=0.64 reflects that the protocol persists despite reduced functional necessity. Enforced through habit and ceremonial regard for 'Antarctic tradition' rather than active structural need.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL ANALYST (TANGLED ROPE) — From a long-view perspective, the 'calm' protocol is a legitimate coordination mechanism (preventing institutional collapse under extreme isolation) that has accumulated extraction features (psychological control, suppression of dissent, normalization of emotional constraint) beyond its original functional necessity. The protocol persists because it solved a real coordination problem and now extracts value from the fact that solving it established institutional power. d≈0.65, f(d)≈0.88, σ=1.0 → χ≈0.46.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_calm_protocol_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_calm_protocol_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_calm_protocol_suppression, TR),
    TR >= 0.70.

:- end_tests(the_calm_protocol_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The protocol extracts psychological autonomy, emotional expression, and individual agency from crew members in service to institutional stability and control. The extraction is not maximal because some portion of the suppression is genuinely necessary for safety under extreme isolation. However, the measurement trajectory (0.32 → 0.52) shows that extractiveness has increased as external conditions improved (better communication, more regular resupply, shorter intervals between rotations) while the suppression mechanism remained constant or intensified. This indicates that modern extractiveness exceeds functional necessity. Suppression (0.68): High. Institutional mechanisms enforce emotional constraint through multiple channels: command structure authority, cultural prestige (selection of 'calm' as professional value), career dependencies (compliance required for favorable assignments), and environmental factors (limited exit options, high interdependence). Crew members experience suppression as structural — they cannot exit, protest openly, or organize alternative arrangements. Theater ratio (0.64): High. The protocol's current enforcement is substantially performative: rituals of calm, ceremonial regard for stability, institutional signaling through leadership behavior, public expressions of control and order. These are performative in the sense that they serve institutional legitimacy more than functional psychological necessity. The theater has increased over the measurement interval as external connectivity has reduced actual isolation severity while the protocol's ceremonial aspects have been elaborated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. From the isolated crew member's view, the protocol is a snare — pure extraction of psychological autonomy with no exit. From station administration's view, the protocol is genuine coordination — solving the real problem of maintaining safety and institutional function. From the research community's view, it is tangled rope — necessary stability but also damaging to crew wellbeing and long-term collaboration quality. From the Antarctic Treaty System's view, it is temporary scaffolding being superseded by better international collaboration norms. From the Cold War institutional legacy's view, it persists as a piton — maintained through inertia and prestige despite reduced functional necessity. From the civilizational analyst's view, it is tangled rope — legitimate origin as coordination, now accumulated extraction. The perspectival gap reveals that the constraint's classification is not stable across observers; the same institutional phenomenon appears as pure extraction to the powerless, coordination to institutional actors, and a mixture to those with moderate power and analysis capability.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual crew members: Victim + trapped → d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.59. Maximum extraction. Cannot exit the Antarctic environment; cannot exit the institutional mandate; bear full cost of psychological suppression. Research community: Victim + constrained → d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.49. Significant extraction but moderated by benefits from research continuity. Can exit through career changes but face costs in professional standing. Station administration: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02, σ=0.9 → χ≈0.005. Near-zero effective extraction. Can rotate personnel, reassign authority, reframe protocol. Experiences protocol as coordination, not extraction. Antarctic Treaty System: Organized + constrained → d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16. Low effective extraction; organization has agency and sees sunset path through improved international norms. Cold War institutional legacy: Institutional + arbitrage → d≈0.10, f(d)≈0.02, σ=1.0 → χ≈0.02. Piton classification emerges from theater gate (0.64 ≥ 0.70 criterion not met, but high enough to indicate substantial performativity) combined with low current extractiveness relative to historical necessity. Civilizational analyst: Analytical → d≈0.65, f(d)≈0.88, σ=1.0 → χ≈0.46. Long-view perspective sees both coordination origin and extraction accumulation; tangled rope classification captures hybrid nature.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids conflation of coordination and extraction by decomposing the 'calm' protocol into its functional and extractive components. The coordination function is genuine — preventing psychological crisis under extreme isolation requires some form of normative stability and conflict management. The extraction is also genuine — the protocol suppresses emotional autonomy, enforces institutional control over individual psychology, and maintains mechanisms of suppression well beyond their functional necessity. The perspectival gap does not collapse these into a false choice between 'coordination' and 'extraction.' Instead, it reveals that the protocol is a tangled rope: it coordinates to solve a real safety problem AND extracts psychological compliance as the mechanism for solving it. The measurement trajectory (theater increasing from 0.35 to 0.64, extractiveness increasing from 0.32 to 0.52) shows that the extraction component is growing faster than the coordination component — the protocol is becoming more theatrical and more extractive as external isolation severity decreases. This is the signature of rent-seeking: once a coordination mechanism is established, institutional actors often layer extraction onto it because the coordination infrastructure is already in place. The mandatrophy is resolved by recognizing that BOTH the snare classification (from the crew perspective) AND the rope classification (from the administration perspective) are accurate descriptions of different structural realities. The system's meta-level classification as tangled rope captures that the constraint genuinely serves both functions simultaneously, with the extraction component increasingly dominant in the modern context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_necessity_threshold,
    'At what degree of external connectivity and communication access does the ''calm'' suppression mechanism transition from coordination necessity to pure extraction?',
    'Comparative analysis of crew psychological health outcomes across stations with different communication/resupply infrastructure; controlled intervention studies introducing enhanced communication protocols',
    'If threshold is low (high connectivity still justifies suppression): piton classification confirmed — institutional inertia. If threshold is high (only extreme isolation justifies it): protocol requires substantial reform in modern context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_threshold, empirical, 'Connectivity threshold for suppression necessity').

omega_variable(
    conflict_prevention_efficacy,
    'Does enforced ''calm'' actually prevent conflict escalation and psychological crisis, or does suppression increase underlying tension and deferred trauma?',
    'Longitudinal mental health data comparison: crews under strict calm protocols vs. crews with explicit emotional expression support; post-deployment psychological evaluations; incident rates across protocol regimes',
    'If suppression prevents crisis: coordination function confirmed. If suppression masks and deepens crisis: snare classification dominates — extraction of psychological compliance disguised as safety.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_prevention_efficacy, empirical, 'Whether enforced calm prevents or masks psychological crisis').

omega_variable(
    normalization_legitimacy,
    'Does the normalization of emotional suppression in Antarctic contexts (cultural prestige, institutional selection of compliance) constitute consent or coercion masquerading as culture?',
    'Ethnographic analysis of recruitment messaging and pre-deployment framing; exit interviews documenting voluntary vs. coerced compliance; analysis of how ''calm'' is presented to incoming crew',
    'If genuinely consented to: suppression is voluntary coordination. If coerced through selection and institutional prestige: consent is manufactured, extractive mechanism confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_legitimacy, conceptual, 'Whether emotional suppression is consented or manufactured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_calm_protocol_suppression, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(calm_tr_t0, the_calm_protocol_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(calm_tr_t25, the_calm_protocol_suppression, theater_ratio, 25, 0.5).
narrative_ontology:measurement(calm_tr_t50, the_calm_protocol_suppression, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(calm_be_t0, the_calm_protocol_suppression, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(calm_be_t25, the_calm_protocol_suppression, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(calm_be_t50, the_calm_protocol_suppression, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_calm_protocol_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(the_calm_protocol_suppression, isolation_psychological_resilience).
narrative_ontology:affects_constraint(the_calm_protocol_suppression, institutional_authority_legitimacy).

% DUAL FORMULATION NOTE:
% The 'calm' protocol decomposes into two structurally distinct constraints: (1) isolation_coordination_necessity (ε≈0.20, Mountain-type) — genuine psychological safety requirement under extreme conditions; (2) the_calm_protocol_suppression (ε=0.52, Tangled Rope) — the specific institutional mechanism and its accumulated extraction features. The first is relatively immutable; the second is contingent and could be replaced by alternative protocols with lower suppression and theater. This story focuses on the second constraint; the upstream Mountain constraint (isolation necessity) explains why suppression exists, but does not justify its current level or form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_calm_protocol_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
