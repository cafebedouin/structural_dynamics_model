% ============================================================================
% CONSTRAINT STORY: individual_revolution_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_revolution_autonomy, []).

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
 *   constraint_id: individual_revolution_autonomy
 *   human_readable: The One-Man Revolution: Individual Autonomy vs Mass Socio-Political Revolution
 *   domain: political/social
 *
 * SUMMARY:
 *   The one-man revolution constraint describes a structural asymmetry in how
 *   political change is framed: mass socio-political revolution is presented
 *   as inherently futile (free-rider problem, coordination failure,
 *   inevitable repression), while individual autonomy — self-improvement,
 *   personal transcendence, voluntary exit from social obligations — is
 *   celebrated as the only 'fertile' path to freedom. This constraint
 *   operates as a hybrid coordination-extraction mechanism. For institutional
 *   power structures, it solves the coordination problem of preventing mass
 *   mobilization without requiring direct coercion: if people believe
 *   collective action is futile but individual improvement is possible, they
 *   will redirect their energy into self-optimization rather than systemic
 *   change. For mass participants, it appears as extraction disguised as
 *   freedom: the promise of autonomy obscures the denial of collective
 *   agency. For counter-institutional movements (revolutionary parties,
 *   syndicalists, liberation organizations), it is an explicit trap — a
 *   narrative weapon used to pacify potential mobilization. The constraint's
 *   theater ratio has risen from 0.35 to 0.65 over the past century,
 *   reflecting that the performative content of individual autonomy — the
 *   ritual of self-improvement, personal branding, lifestyle optimization —
 *   has grown relative to its actual systemic impact. Simultaneously, the
 *   extractiveness has risen from 0.32 to 0.58 as institutional investment in
 *   the individual-autonomy frame has intensified through educational
 *   systems, corporate messaging, therapy culture, and state ideology.
 *
 * KEY AGENTS:
 *   - Mass Movement Participants: Primary victims (powerless/trapped) — face the false dilemma between futile collective action and achievable individual autonomy
 *   - Activist Cohorts: Secondary victims (moderate/constrained) — coordinate through organizational structures but face legal/social barriers and co-optation of their message into individual self-improvement
 *   - Institutional Power Structures: Primary beneficiaries (institutional/arbitrage) — use the autonomy frame as low-coercion coordination to prevent mass mobilization
 *   - Counter-Institutional Movements: Secondary beneficiaries (organized/mobile) — reject the autonomy frame and coordinate alternative narratives emphasizing collective efficacy
 *   - Self-Help and Personal Development Industries: Institutional maintenance actors (institutional/arbitrage) — monetize the individual-autonomy narrative; maintain it through performative function (coaching, therapy, startups) that gives impression of structural change
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the autonomy constraint as inevitable outcome of collective action logic, missing the contingency of institutional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_revolution_autonomy, 0.58).
domain_priors:suppression_score(individual_revolution_autonomy, 0.68).
domain_priors:theater_ratio(individual_revolution_autonomy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_revolution_autonomy, extractiveness, 0.58).
narrative_ontology:constraint_metric(individual_revolution_autonomy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(individual_revolution_autonomy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_revolution_autonomy, tangled_rope).
narrative_ontology:human_readable(individual_revolution_autonomy, "The One-Man Revolution: Individual Autonomy vs Mass Socio-Political Revolution").
narrative_ontology:topic_domain(individual_revolution_autonomy, "political/social").

domain_priors:requires_active_enforcement(individual_revolution_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, institutional_power_structures).
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, libertarian_autonomy_ideologues).
narrative_ontology:constraint_victim(individual_revolution_autonomy, collective_political_agency).
narrative_ontology:constraint_victim(individual_revolution_autonomy, mass_social_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MASS MOVEMENT PARTICIPANT (SNARE) — Trapped in the dilemma: collective action appears futile (free-rider problem, coordination failure, repression), yet individual exit via autonomy pursuit is presented as the only 'fertile' option. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. High extraction: the constraint suppresses collective agency while offering only individual escape.
constraint_indexing:constraint_classification(individual_revolution_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACTIVIST COHORT (TANGLED ROPE) — Constrained by legal/social barriers and risk, but also coordinates through organizational infrastructure (unions, parties, NGOs). Benefits from collective bargaining and mutual aid; bears extraction through surveillance, co-optation, and deprioritization of systemic change in favor of individual self-improvement narrative. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(individual_revolution_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL POWER STRUCTURES (ROPE) — Experiences the constraint as pure coordination: the ideological work of the individual-autonomy frame solves the coordination problem of preventing mass mobilization. No need for direct repression when people believe collective action is futile but individual transcendence is possible. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary; uses the constraint as low-coercion coordination.
constraint_indexing:constraint_classification(individual_revolution_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTER-INSTITUTIONAL MOVEMENTS (TANGLED ROPE) — Organized groups (revolutionary parties, syndicalists, liberation movements) explicitly reject the individual-autonomy frame as a trap. They coordinate alternative narratives emphasizing collective efficacy, structural change, and mutual aid. See the constraint as enforceable extraction dressed in libertarian rhetoric. Benefits from ideological clarity; bears extraction from state repression + capitalist co-optation of individualism. d≈0.70, f(d)≈1.06, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(individual_revolution_autonomy, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SELF-HELP AND PERSONAL DEVELOPMENT INDUSTRIES (PITON) — Capitalism's institutional apparatus for monetizing the individual-autonomy narrative (coaching, therapy, self-improvement literature, startup culture). Maintains performative function: the ritual of personal transformation gives the impression of structural change while actual power distributions remain intact. Theater_ratio=0.65 (high performative content relative to systemic impact). The industry sees its own function as partially degraded — awareness that personal growth cannot substitute for systemic change has risen, but the apparatus persists through inertia and marketing.
constraint_indexing:constraint_classification(individual_revolution_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — A tempting but misleading perspective frames the individual-autonomy constraint as a natural law: 'Mass revolution is inherently impossible due to free-rider logic; individual autonomy is the only rational choice.' This invokes Olson's collective action problem as though it were a law of physics. However, historical evidence (successful mass movements, revolutionary transitions, mutual aid networks) contradicts the necessity claim. The mountain perspective naturalizes a contingent institutional arrangement (specific forms of surveillance, specific media ecosystems, specific property regimes) as inevitable. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67. The engine's false summit detector reveals this as not actually a mountain.
constraint_indexing:constraint_classification(individual_revolution_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_revolution_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_revolution_autonomy, TR),
    TR >= 0.70.

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts collective political agency and redirects it into individual self-optimization. However, the extraction is not absolute — some individuals do pursue collective paths, and counter-institutional movements explicitly reject the frame. The value reflects that the constraint is real but contested, with measurable institutional investment required to maintain it. Suppression (0.68): High. Significant barriers to collective mobilization include legal restrictions on organizing, surveillance and policing of protest, media representation favoring individual solutions, educational curricula teaching free-rider logic and rational self-interest, and the persistent framing of collective action as utopian vs pragmatic individual autonomy. Suppression has risen over time as surveillance capacity and media control have expanded. Theater ratio (0.65): High. The performative content of individual autonomy — personal branding, self-improvement rituals, lifestyle optimization, therapy culture — has grown. The theater has increased because the constraint's function is increasingly dependent on the *appearance* of structural possibility rather than actual mechanisms of change. Early versions of the constraint (pre-1960) could point to genuine barriers (slow mass communication, geographic dispersal); modern versions increasingly rely on performative reassurance (self-help literature, motivational culture) to maintain the illusion of individual efficacy in the absence of systemic change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The powerless participant (trapped exit) experiences it as a snare: the frame is presented as liberation but functions as confinement. The activist (constrained exit) experiences it as tangled rope: the constraint enables some coordination (through organizational forms) while extracting collective efficacy. The institutional beneficiary (arbitrage exit) experiences it as rope: pure coordination — the ideological work of the autonomy frame solves the problem of preventing mass mobilization without direct repression. The counter-institutional movement (mobile exit) experiences it as tangled rope but with inverted value: they coordinate around rejecting the frame itself. The self-help industry (arbitrage exit) experiences it as piton: the ritual persists through inertia and marketing despite awareness that personal transformation cannot substitute for systemic change. The civilizational observer risks experiencing it as mountain (natural law of collective action logic) but the structural data reveals this as a false summit — the constraint is contingent on specific forms of institutional enforcement, not universal logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional power structures: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; the constraint solves their coordination problem with minimal coercive overhead. Mass participants: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option within the frame. Activist cohorts: Victim + constrained → d≈0.65, f(d)≈0.95. Significant extraction; some organizational capacity for resistance but constrained by legal barriers and surveillance. Counter-institutional movements: Victim-status inverted to agent + organized/mobile → d≈0.70, f(d)≈1.06. These actors explicitly reject the victim classification and organize to counter the constraint; they experience extraction from state repression and capitalist co-optation but have agency and coordination capacity. Self-help industries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; they profit from the performance of individual transformation. Analytical observer: analytical → d≈0.72, f(d)≈1.15. The false mountain perspective naturalizes the constraint as inevitable; the engine's false summit detector identifies this as a misclassification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.58 > 0.46): The constraint's mandatrophy is resolved by recognizing that it is NOT a pure snare of mass extraction (which would require ε ≥ 0.66 and χ ≥ 0.66), but rather a tangled rope with high suppression and moderate-high extractiveness. The coordination function is real: the individual-autonomy frame genuinely solves the coordination problem for institutional power structures by making collective mobilization appear irrational. The extraction is also real: people redirect political energy into self-optimization rather than systemic change. The key insight is that this is hybrid, not pure extraction. Some perspectives (the powerless participant) experience it as snare because their exit options are trapped within the frame. Other perspectives (the institutional beneficiary, the self-help industry) experience it as rope because the frame provides genuine coordination benefit. The counter-institutional movement's perspective reveals the frame as contestable — the constraint only holds if the framing is accepted. Therefore: the constraint is tangled rope (not snare) from the analytical perspective, but classifies differently from different structural positions. The mandatrophy-resolved claim is that treating this as pure extraction misses the genuine coordination function, while treating it as pure coordination ignores the real asymmetry in who benefits from the frame and who bears the cost of suppressed collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_action_threshold,
    'Under what conditions does the free-rider problem become negligible vs dominant in preventing mass mobilization?',
    'Empirical study of successful mass movements and their organizational structures; analysis of how selective incentives, social pressure, and mutual aid overcome free-ridership; comparison of movement success rates under different institutional conditions (pre-digital vs digital, authoritarian vs democratic, high-density vs dispersed communities)',
    'If free-ridership is context-dependent (not universal): the mountain perspective is false, and individual autonomy is not the only rational exit. The constraint becomes contingent on specific institutional conditions (surveillance capacity, media ecosystem, property regime) rather than universal logic. If free-ridership is universal: the individual autonomy frame is justified, and the constraint is coordination (rope) not extraction (snare/tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Whether free-rider logic is universal or context-dependent').

omega_variable(
    autonomy_versus_agency_ontology,
    'Does individual autonomy (self-direction within existing power structure) represent genuine agency or a colonized pseudo-agency that leaves structural power untouched?',
    'Philosophical analysis and empirical measurement: track whether individual autonomy pursuits correlate with or contradict systemic power shifts; examine whether personal transformation generates structural change; measure whether autonomy-focused interventions reduce inequality or merely allow individual escape from it',
    'If autonomy is genuine agency: the constraint is a coordination problem (rope) or hybrid (tangled rope) where both individual and collective paths are viable. If autonomy is pseudo-agency: the constraint is pure extraction (snare) disguised as freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_versus_agency_ontology, conceptual, 'Whether individual autonomy represents genuine agency or colonized pseudo-agency').

omega_variable(
    historical_contingency_of_mass_revolution,
    'Are successful mass revolutions (Haitian Revolution, Soviet Union, Cuban Revolution, Chinese Revolution, Vietnam, Nicaragua) evidence that collective action is not inherently futile, or are they exceptional cases that prove the general rule?',
    'Historical analysis of revolutionary success factors; statistical comparison of movement outcomes (success rates, sustainability, transformation of power structures) across different organizational models (hierarchical vs horizontal, individual-autonomy-focused vs collective-agency-focused); study of why collective-action frames persist in successful movements despite theoretical predictions of free-rider dominance',
    'If revolutions are outcome-representative: the mountain perspective''s ''futility of mass action'' is empirically false, and the constraint is artificial (institutional, extractive). If revolutions are exceptional: the futility prediction is justified, and autonomy-seeking becomes rational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_mass_revolution, empirical, 'Whether mass revolutions represent contingent history or confirm futility of collective action').

omega_variable(
    institutional_reinforcement_mechanism,
    'To what degree is the individual-autonomy frame actively enforced (through education, media, law enforcement) vs passively inherited through cultural inertia?',
    'Analysis of institutional investment in autonomy rhetoric (educational curricula, corporate messaging, NGO framing, state ideology); measurement of media representation of individual vs collective solutions; tracking of regulatory barriers to collective organizing (union laws, protest restrictions) vs incentives for individual self-improvement',
    'If actively enforced: the constraint is a clear tangled rope with high suppression. If passively inherited: the constraint might be weaker than measured; collective counter-framing could shift it toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reinforcement_mechanism, empirical, 'Whether the autonomy frame is actively enforced or passively inherited').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_revolution_autonomy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indrev_tr_t0, individual_revolution_autonomy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indrev_tr_t50, individual_revolution_autonomy, theater_ratio, 50, 0.55).
narrative_ontology:measurement(indrev_tr_t100, individual_revolution_autonomy, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(indrev_be_t0, individual_revolution_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(indrev_be_t50, individual_revolution_autonomy, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(indrev_be_t100, individual_revolution_autonomy, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_revolution_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_revolution_autonomy, collective_action_problem).
narrative_ontology:affects_constraint(individual_revolution_autonomy, free_rider_equilibrium).
narrative_ontology:affects_constraint(individual_revolution_autonomy, narrative_hegemony).

% DUAL FORMULATION NOTE:
% The individual-revolution-autonomy constraint is downstream of the collective action problem (formal coordination failure) and free-rider equilibrium (rational incentive structure). It represents the institutional enforcement of those theoretical results into a practical constraint on mobilization. Upstream: mathematical limits on coordination. Downstream: actual barriers to organizing. The constraint family includes: (1) collective_action_problem (ε≈0.05, mountain — the formal logic), (2) free_rider_equilibrium (ε≈0.15, rope — the strategic logic), (3) narrative_hegemony (ε≈0.68, tangled_rope — the institutional enforcement), (4) individual_revolution_autonomy (ε≈0.58, tangled_rope — the lived experience). Each story has different ε because they measure different aspects: logical necessity vs institutional choice vs experiential reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_revolution_autonomy, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
