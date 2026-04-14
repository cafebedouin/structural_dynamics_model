% ============================================================================
% CONSTRAINT STORY: mass_mobilization_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mass_mobilization_capacity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: mass_mobilization_capacity
 *   human_readable: Mass Mobilization Capacity and State Control
 *   domain: political/social/institutional
 *
 * SUMMARY:
 *   Mass mobilization capacity is the structural ability of a state or
 *   organizing authority to coordinate large populations toward collective
 *   goals. This constraint exhibits fundamental tension: genuine coordination
 *   functions (organizing collective action that individuals cannot achieve
 *   alone) are intertwined with extraction mechanisms (coercive
 *   participation, asymmetric benefit distribution, suppression of
 *   alternatives). The constraint manifests differently across observation
 *   positions. The individual conscript experiences pure extraction and
 *   suppression. The community organizer experiences mixed coordination
 *   benefit and extraction. The state apparatus experiences pure
 *   coordination. Counter-movements experience the apparatus capturing their
 *   own infrastructure. The ideological superstructure justifying
 *   mobilization appears increasingly performative as alternative mechanisms
 *   mature. From civilizational distance, mobilization appears as natural
 *   law, but structural data reveals this as false naturalization of
 *   contingent institutional arrangements. The constraint's extractiveness
 *   has increased from 0.35 to 0.58 over the measured interval, while theater
 *   ratio has remained moderate (0.30 to 0.48), suggesting that the
 *   coordination function is partly genuine but increasingly layered with
 *   performative and extractive elements. The suppression level (0.65)
 *   reflects significant barriers to alternative mechanisms — conscription
 *   laws, legal restrictions on voluntary association, surveillance of
 *   organizing, career penalties for non-participation.
 *
 * KEY AGENTS:
 *   - Individual Citizens: Primary victims (powerless/trapped) — face legal and economic coercion into participation; bear costs without proportional benefit
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — captures organizational capacity, demonstrates state power, extracts productive capacity; experiences constraint as coordination mechanism
 *   - Community Organizers: Secondary agents (moderate/constrained) — genuinely coordinate collective action while being extracted from and surveilled; face career risk and legal jeopardy
 *   - Counter-Movement Coalitions: Organized opposition (organized/constrained) — benefit from mobilization infrastructure while targeted by state repurposing of that infrastructure against them
 *   - Ideological Justification System: Institutional actor (institutional/arbitrage) — maintains grand narratives legitimizing mobilization through inertia; function has degraded as alternatives emerge
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (state monopoly on mobilization) as immutable laws of collective organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mass_mobilization_capacity, 0.58).
domain_priors:suppression_score(mass_mobilization_capacity, 0.65).
domain_priors:theater_ratio(mass_mobilization_capacity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mass_mobilization_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(mass_mobilization_capacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mass_mobilization_capacity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mass_mobilization_capacity, tangled_rope).
narrative_ontology:human_readable(mass_mobilization_capacity, "Mass Mobilization Capacity and State Control").
narrative_ontology:topic_domain(mass_mobilization_capacity, "political/social/institutional").

domain_priors:requires_active_enforcement(mass_mobilization_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mass_mobilization_capacity, state_apparatus).
narrative_ontology:constraint_beneficiary(mass_mobilization_capacity, mobilizing_authority).
narrative_ontology:constraint_victim(mass_mobilization_capacity, individual_autonomy).
narrative_ontology:constraint_victim(mass_mobilization_capacity, distributed_organizers).
narrative_ontology:constraint_victim(mass_mobilization_capacity, competing_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED PARTICIPANT (SNARE) — Individual citizens face mandatory participation in state mobilizations through legal, economic, and social coercion. Exit is materially unavailable: conscription laws, employment dependency on state favor, social ostracism for non-participation. The individual experiences pure extraction with suppression of alternatives. No genuine coordination benefit reaches the powerless agent — only coerced compliance.
constraint_indexing:constraint_classification(mass_mobilization_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY ORGANIZER (TANGLED ROPE) — Local organizers genuinely coordinate collective action (solving coordination problems within communities) while simultaneously being extracted from by state apparatus. Constrained exit: organizing at odds with state interests creates career risk, surveillance exposure, and legal jeopardy. The organizer benefits from the coordination infrastructure while bearing asymmetric costs of state capture of that infrastructure. Mixed experience: real coordination function + extraction.
constraint_indexing:constraint_classification(mass_mobilization_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — State experiences mobilization capacity as pure coordination mechanism: organizing dispersed populations to achieve collective goals (infrastructure projects, national defense, crisis response). The state apparatus has exit options (can choose not to mobilize, can use alternative mechanisms like markets or private contractors). Net beneficiary: extraction flows toward state; state experiences the constraint as coordination benefit. High arbitrage capacity — can substitute mechanisms depending on effectiveness.
constraint_indexing:constraint_classification(mass_mobilization_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COUNTER-MOVEMENT COALITION (TANGLED ROPE) — Organized opposition movements also benefit from mobilization infrastructure (organizing capacity, communication networks) while bearing extraction costs as the state repurposes their own infrastructure against them. Constrained because exit means surrendering the mobilization capacity itself, yet staying means feeding the state apparatus. Generational perspective reveals pattern: each cycle of repression increases suppression, but mobilization capacity itself persists as coordinating force.
constraint_indexing:constraint_classification(mass_mobilization_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IDEOLOGICAL JUSTIFICATION SYSTEM (PITON) — The grand narratives legitimizing mass mobilization (national purpose, collective sacrifice, civilizational destiny) are substantially theatrical. Modern states have alternative mechanisms (market mechanisms, technological solutions, professional bureaucracies) that could replace mobilization in many domains. The ideological superstructure persists through institutional inertia despite reduced functional necessity. Theater ratio reflects that mobilizations are often ceremonial performances rather than functionally required activities.
constraint_indexing:constraint_classification(mass_mobilization_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN FALSE SUMMIT) — From civilizational distance, mobilization appears as immutable: human collectives above certain scale necessarily require coordination mechanisms, and mass mobilization is the only mechanism that scales to universal application. The constraint appears as a natural law of collective organization. However, structural data contradicts this — alternative mechanisms exist (markets, digital platforms, voluntary associations), suppression is contingent institutional choice (not inherent), and extractiveness declines as alternatives mature. This is a false summit: the 'universal necessity' framing naturalizes a contingent institutional arrangement.
constraint_indexing:constraint_classification(mass_mobilization_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mass_mobilization_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mass_mobilization_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mass_mobilization_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mass_mobilization_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mass_mobilization_capacity, TR),
    TR >= 0.70.

:- end_tests(mass_mobilization_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The base extraction reflects asymmetric benefit distribution — individual participation yields disproportionate return to state apparatus. The 65% increase over the interval (0.35→0.58) indicates that while genuine coordination occurs, extraction mechanisms are accumulating: career penalties for non-participation, surveillance systems, legal frameworks have all intensified. Suppression (0.65): High. Legal conscription, restrictions on alternative organizing, social ostracism for non-participation, surveillance of organizers. Theater ratio (0.48): Moderate. Contemporary mass mobilizations often serve performative functions alongside genuine coordination — parades, ceremonies, ideological expression. However, mobilizations retain functional necessity (national defense, infrastructure, crisis response), so theater has not reached Piton levels. The measurement trajectory shows theater increasing from 0.30 to 0.48, reflecting that modern states substitute performance for function in some mobilization contexts (nationalist ceremonies, celebratory parades alongside resource mobilization).
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is extreme. The powerless agent and institutional beneficiary classify the same structural phenomenon as opposite types: Snare (pure extraction, minimal coordination) vs Rope (pure coordination, minimal extraction). This gap reveals the extraction mechanism precisely: the constraint presents as coordination to those who benefit and control it, but as extraction to those who bear costs. The community organizer's Tangled Rope perspective bridges the gap — they see both the genuine coordination function and the extraction layered over it. The counter-movement's perspective shows how the infrastructure can be captured and weaponized. The piton perspective reveals performative degradation. The false mountain shows how distance enables naturalization of contingent arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position relative to extraction flow. Powerless conscripts with trapped exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42 effective extraction. Community organizers with constrained exit and mixed victim/beneficiary status (d ≈ 0.60) experience moderate f(d) ≈ 0.95 extraction. State apparatus with beneficiary status and arbitrage exit (d ≈ 0.10) experience negative or low f(d) ≈ -0.02 effective extraction — the apparatus benefits from the constraint. Scope modifier σ(national) = 1.0 applies across all perspectives except universal analytical (σ = 1.0). Counter-movements as organized opposition (d ≈ 0.58) with constrained exit experience f(d) ≈ 0.75 — high extraction despite organized power due to targeting by apparatus. The piton perspective at institutional level (d ≈ 0.15, arbitrage exit) experiences low effective extraction because the institution (ideological system) is degrading and optional, not actively targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION REQUIRED: Mass mobilization capacity as a natural-language concept should decompose into at least three structurally distinct constraints: (1) coordination_infrastructure (ε ≈ 0.15, Rope) — the genuine problem-solving of collective action coordination; (2) state_extraction_overlay (ε ≈ 0.72, Snare) — the coercive machinery and suppression systems; (3) ideological_legitimation (ε ≈ 0.35, Piton) — the grand narratives justifying the apparatus, increasingly performative. These three stories, linked via network.affects_constraints, would resolve the mandatrophy by showing that the 'constraint' is actually a composite of a genuine coordination mechanism (Rope) overlaid with extraction machinery (Snare) maintained by performative narrative (Piton). The tangled_rope classification at the moderate/organized level captures this hybrid correctly — agents at this level perceive both components. The snare classification at the powerless level correctly isolates the extraction component. The rope classification at the institutional level correctly isolates what the beneficiary experiences. The false mountain correctly diagnoses how distance naturalizes the hybrid into perceived necessity. Rather than resolving to a single true type, the constraint's mandatrophy is resolved by recognizing that perspectives from different structural positions perceive genuinely different constraint types within the composite system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_substitution_sufficiency,
    'Can digital coordination technologies and voluntary association mechanisms substitute for state-directed mass mobilization at scale, or are there irreducible coordination problems requiring coercive capacity?',
    'Comparative analysis of voluntary vs coercive mobilization outcomes: infrastructure completion rates, quality metrics, cost per capita, sustainability. Case studies of large-scale projects completed via voluntary coordination (open-source software, Wikipedia, crowdsourcing) vs state mobilization.',
    'If digital substitution is sufficient: mobilization capacity is Tangled Rope or Scaffold (not Snare) — suppression is contingent policy, not structural necessity. If irreducible coordination deficit exists: mobilization capacity is Mountain (or stronger Snare) — coercion is functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_substitution_sufficiency, empirical, 'Whether digital technologies can substitute for coercive mass mobilization').

omega_variable(
    voluntary_participation_extraction_difference,
    'Is extractiveness primarily inherent to mobilization coordination itself, or does it arise from state-enforced participation? Would voluntary mobilization coordinated by non-state entities exhibit lower extractiveness?',
    'Analysis of self-organized mobilizations (protest movements, community organizing, disaster mutual aid) measuring extractiveness and suppression levels; comparison to state-directed mobilization. Examine whether volunteer participants report extraction or coordination benefit.',
    'If extraction is inherent to coordination: mobilization capacity is Rope or Tangled Rope even in voluntary contexts. If extraction arises from state coercion: it''s a Snare primarily due to suppression policy choice, suggesting Scaffold perspective (sunset through institutional reform).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_extraction_difference, empirical, 'Whether extraction is inherent to mobilization or dependent on state enforcement').

omega_variable(
    suppression_mechanism_structural_vs_performative,
    'Does suppression of alternative mobilization mechanisms serve genuine functional necessity (state cannot coordinate without monopoly) or primarily serves to prevent loss of state control and extractive capacity?',
    'Analysis of state suppression patterns: targeting of alternative organizers, legal barriers to voluntary association, surveillance intensity. Correlation between functional mobilization need and suppression intensity — states suppress competitors even during low-functional-need periods.',
    'If functional necessity: suppression is inherent (Mountain or Snare classification stands). If performative control: suppression is contingent institutional choice (supports Scaffold perspective with sunset as states transition to alternative mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_performative, empirical, 'Whether suppression serves functional necessity or maintains state control').

omega_variable(
    identity_lock_mechanism_strength,
    'For conscripted participants and community organizers, is the binding mechanism primarily material (legal enforcement, economic dependency) or partially internalized (identity fusion with national/community identity, cognitive capture by collective narratives)?',
    'Comparative analysis of participation rates when enforcement mechanisms weaken (post-authoritarian transitions, legal reforms reducing conscription); post-exit interview data from defectors and voluntary organizers; cross-cultural variation in participation absent formal enforcement.',
    'If primarily material: trapped/constrained classification is correct (exit is possible if external barriers removed). If partially internalized: identity_locked classification applies to some agents (exit would require identity reconstruction, persists even after legal barriers removed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Degree of internalized identity lock vs material barrier in mobilization participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mass_mobilization_capacity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mob_tr_t0, mass_mobilization_capacity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mob_tr_t25, mass_mobilization_capacity, theater_ratio, 25, 0.4).
narrative_ontology:measurement(mob_tr_t50, mass_mobilization_capacity, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(mob_be_t0, mass_mobilization_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mob_be_t25, mass_mobilization_capacity, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(mob_be_t50, mass_mobilization_capacity, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mass_mobilization_capacity, resource_allocation).
narrative_ontology:boltzmann_floor_override(mass_mobilization_capacity, 0.12).
narrative_ontology:affects_constraint(mass_mobilization_capacity, state_surveillance_capacity).
narrative_ontology:affects_constraint(mass_mobilization_capacity, individual_autonomy_restriction).
narrative_ontology:affects_constraint(mass_mobilization_capacity, volunteer_coordination_suppression).

% DUAL FORMULATION NOTE:
% Mass mobilization capacity decomposes into three structurally distinct constraints: (1) coordination_infrastructure (genuine collective action problem-solving, ε ≈ 0.15, Rope) — upstream claim; (2) state_extraction_overlay (coercive machinery, legal frameworks, suppression systems, ε ≈ 0.72, Snare) — downstream extractive apparatus; (3) ideological_legitimation (grand narratives, ε ≈ 0.35, Piton) — performative maintenance system. This story represents the composite phenomenon. Decomposed stories should exist for each component, linked via affects_constraints array showing upstream-to-downstream dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mass_mobilization_capacity, institutional, 0.1).
constraint_indexing:directionality_override(mass_mobilization_capacity, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
