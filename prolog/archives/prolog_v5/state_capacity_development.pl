% ============================================================================
% CONSTRAINT STORY: state_capacity_development
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_development, []).

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
 *   constraint_id: state_capacity_development
 *   human_readable: State Capacity Development as Coordination and Extraction
 *   domain: political_economy/institutional_development
 *
 * SUMMARY:
 *   State capacity development represents one of the central institutional
 *   arrangements in post-Cold War international relations. Framed as
 *   coordination — helping recipient states build effective institutions
 *   through technical assistance, training, and knowledge transfer — capacity
 *   development programs operate across 180+ countries, channeling hundreds
 *   of billions in annual financing through bilateral donors, multilateral
 *   development banks, and UN agencies. Yet the constraint exhibits a
 *   fundamental tension: genuine coordination problems in state-building
 *   coexist with structural extraction of institutional autonomy. Recipient
 *   states face real capacity constraints (weak bureaucracies, limited
 *   technical expertise, underfunded institutions), and donors provide
 *   genuine resources and knowledge. Simultaneously, capacity programs impose
 *   donor-preferred governance architectures, external accountability
 *   mechanisms that crowd out locally-rooted legitimacy, and create long-term
 *   dependency on donor financing and advisors. The constraint's
 *   classification varies dramatically by perspective: powerless recipients
 *   experience it as a snare; organized reformers see temporary scaffolding
 *   with a sunset; the donor industry maintains it as degraded theater;
 *   analytical observers risk naturalizing a contingent institutional
 *   arrangement as an inevitable feature of state-building.
 *
 * KEY AGENTS:
 *   - Donor Development Institutions (institutional/arbitrage): World Bank, IMF, bilateral donors — net beneficiaries with arbitrage exit options. Control capital flows, technical expertise, and performance metrics.
 *   - Recipient State Officials (moderate/constrained): Domestic bureaucrats trained and employed through donor programs. Experience mixed coordination and extraction; careers depend on donor-supported positions.
 *   - Recipient State Institutional Integrity (powerless/trapped): Abstract collective good — the autonomy and legitimacy of state institutions as products of domestic political deliberation. Bears extraction costs; cannot exit.
 *   - Donor Development Industry (institutional/arbitrage): Consulting firms, training organizations, monitoring contractors — operate within donor system with arbitrage exit options. Theater ratio (0.64) reflects that industry persists through institutional inertia.
 *   - Local Institutional Reform Coalitions (organized/constrained): Domestic civil society, professional associations, reform-minded officials. Use capacity programs as leverage while building exit paths from donor dependence.
 *   - Citizens and Local Communities (powerless/trapped): Bear epistemic extraction — institutions designed by external actors reduce citizen participation in institutional legitimation; bear suppression costs as capacity programs impose compliance with donor-preferred governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_development, 0.58).
domain_priors:suppression_score(state_capacity_development, 0.52).
domain_priors:theater_ratio(state_capacity_development, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_development, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_capacity_development, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(state_capacity_development, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_development, tangled_rope).
narrative_ontology:human_readable(state_capacity_development, "State Capacity Development as Coordination and Extraction").
narrative_ontology:topic_domain(state_capacity_development, "political_economy/institutional_development").

domain_priors:requires_active_enforcement(state_capacity_development).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_development, donor_institutions).
narrative_ontology:constraint_beneficiary(state_capacity_development, international_development_bureaucracy).
narrative_ontology:constraint_beneficiary(state_capacity_development, domestic_political_elites).
narrative_ontology:constraint_victim(state_capacity_development, recipient_state_autonomy).
narrative_ontology:constraint_victim(state_capacity_development, local_institutional_legitimacy).
narrative_ontology:constraint_victim(state_capacity_development, citizen_epistemic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECIPIENT STATE INSTITUTIONAL INTEGRITY (SNARE) — Trapped within a dependency structure. Capacity development programs impose external governance frameworks, audit protocols, and accountability mechanisms that crowd out locally-rooted institutional legitimacy. Exit is structurally blocked: rejecting donor capacity support means losing access to finance, technical expertise, and international standing. The constraint extracts institutional autonomy while providing the resources necessary to function — a snare that binds through necessity.
constraint_indexing:constraint_classification(state_capacity_development, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC STATE OFFICIALS (TANGLED ROPE) — Constrained by capacity limits and career dependence on donor-supported positions, but also benefiting from training, technical expertise, and access to global networks through capacity development. Experiences mixed coordination (genuine skill transfer, institutional learning) and extraction (loss of discretionary authority to externally-appointed advisors, performance metrics imposed by donors). Career mobility is constrained — moving outside the donor-supported bureaucracy means losing resources and status.
constraint_indexing:constraint_classification(state_capacity_development, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DONOR DEVELOPMENT INSTITUTIONS (ROPE) — Experience capacity development as coordination: solving collective action problems in international development (reducing poverty, building functioning states, preventing state failure). Arbitrage exit options (can redirect capital to other recipient states, can shift priority countries, can operate through alternative modalities). Net beneficiary — access to recipient state institutions, demonstration of impact for funding justification, positioning as problem-solver for global governance.
constraint_indexing:constraint_classification(state_capacity_development, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOCAL INSTITUTIONAL REFORM COALITIONS (SCAFFOLD) — Organized domestic actors (civil society, professional associations, reform-minded bureaucrats) use donor capacity programs as leverage for local institutional change while building alternative pathways to institutional legitimacy independent of donor support. See the constraint as temporary — capacity programs provide resources and international visibility during reform windows, but successful reform results in institutional autonomy from donor requirements. Sunset logic: successful capacity development makes external capacity programs unnecessary.
constraint_indexing:constraint_classification(state_capacity_development, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPACITY DEVELOPMENT INDUSTRY (PITON) — The institutional machinery of development (consulting firms, training programs, monitoring frameworks, performance metrics) persists through inertia despite evidence of limited effectiveness. Theater ratio (0.64) reflects that much capacity activity is performative: training sessions that don't transfer to practice, monitoring systems that measure outputs rather than outcomes, technical assistance that produces paper institutions without functional change. The constraint is maintained because the development industry has become an end in itself — producing reports, conducting workshops, justifying annual budgets — rather than solving the stated problem.
constraint_indexing:constraint_classification(state_capacity_development, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational perspective, some state capacity constraint appears immutable: every political community faces the problem of building effective institutions, and the gap between institutional design and institutional function is fundamental to politics. However, this classification is a false summit — the constraint conflates a genuine coordination problem (all states need institutional capacity) with a contingent structural arrangement (donor-led capacity development as the mechanism). The natural law framing obscures how the constraint extracts institutional autonomy while appearing to solve the capacity problem.
constraint_indexing:constraint_classification(state_capacity_development, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_development_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_development, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_development, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_development, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_capacity_development, TR),
    TR >= 0.70.

:- end_tests(state_capacity_development_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Base extractiveness started at 0.35 (genuine capacity transfer and knowledge benefits) but increased to 0.58 as capacity programs accumulated and became institutionalized. The increasing trajectory reflects that donor presence, initially temporary, becomes permanent — advisors become embedded, performance metrics become internalized, institutional forms become locked in. The 0.58 final value reflects the equilibrium between real coordination (donors do provide genuine technical goods) and real extraction (donors impose institutional forms that reduce recipient autonomy). Suppression (0.52): Moderate-high. Multiple barriers prevent recipient states from building autonomous institutions: financial dependence (donors control capital access), technical dependence (knowledge and expertise concentrated in donor organizations), political dependence (donors shape governance priorities), and epistemological dependence (external frameworks crowd out local institutional knowledge). These are genuine structural barriers, not total — some recipient states have found exit paths, some have built alternative capacity sources — but they are substantial enough to trap most recipients. Theater ratio (0.64): Moderately high, increasing over interval. Capacity development includes genuine institutional capacity building, but a significant portion is performative: workshops that don't transfer to practice (participants attend for credentials, not implementation intent), monitoring systems that measure donor-defined outputs rather than citizen-relevant outcomes, technical assistance that produces paper institutions matching donor templates without corresponding functional change. The increase over interval reflects that as donor presence becomes institutional, the theater increases — maintaining the appearance of capacity building becomes more important than solving capacity problems.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_transfer_mechanism,
    'Does capacity development actually transfer institutional capability from donors to recipients, or does it create performative institutions that replicate donor-preferred forms without corresponding functional change?',
    'Longitudinal institutional effectiveness measurement: compare recipient state institution performance pre/post capacity programs; measure whether reforms persist after donor support withdraws; track whether trained officials remain in capacity-built positions',
    'If transfer occurs: constraint is genuine coordination (Rope/Scaffold from more perspectives). If performative: constraint is pure extraction masked as coordination (Snare/Piton from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_transfer_mechanism, empirical, 'Whether capacity development achieves genuine institutional transfer').

omega_variable(
    local_legitimacy_crowding,
    'Does donor-led institutional design crowd out locally-rooted legitimacy and accountability mechanisms that would emerge from domestic political deliberation?',
    'Comparative institutional analysis: recipient states with strong pre-capacity baseline legitimacy vs those with weak baseline; measurement of citizen trust in institutions before/after external capacity interventions; analysis of institutional forms that persist independent of donor requirements',
    'If crowding occurs: suppression is higher than measured (0.52) — donors reduce local agency and epistemic autonomy. If local mechanisms persist: suppression is lower and victim group (local legitimacy) is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_legitimacy_crowding, empirical, 'Whether donor capacity development crowds out local institutional legitimacy').

omega_variable(
    donor_dependency_exit_possibility,
    'Can recipient states exit capacity development programs without material loss of financing, technical access, and international standing?',
    'Historical analysis of state exits from donor programs; measurement of economic/diplomatic penalties following program withdrawal; tracking of alternative financing sources available to states rejecting donor capacity terms',
    'If exit is costless: exit_options should be ''mobile'' not ''trapped'' — classification shifts to Tangled Rope or Rope. If exit is catastrophically costly: trapped classification is confirmed and Snare perspective is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_dependency_exit_possibility, empirical, 'Whether states can exit capacity development without material penalties').

omega_variable(
    institutional_autonomy_restoration,
    'After capacity development programs conclude, do recipient state institutions retain externally-imposed structures and performance metrics, or do they revert to locally-preferred institutional forms?',
    'Post-program institutional analysis: measurement of institutional continuity vs institutional change following program completion; tracking of external advisor presence and influence after formal capacity programs end; comparison of institutional forms to pre-intervention baselines',
    'If reversion occurs: scaffold sunset logic is confirmed, constraint has genuine sunset clause. If structures persist: institutional extraction is permanent (Snare or Piton features), not temporary coordination support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autonomy_restoration, empirical, 'Whether recipient institutions revert to local forms after capacity programs').

omega_variable(
    donor_coordination_function_necessity,
    'Do recipient states require donor institutions to solve the coordination problems that capacity development claims to address, or do alternative domestic or peer-state mechanisms exist?',
    'Comparative institutional analysis: states with and without donor capacity programs; measurement of institutional coordination problem resolution through domestic deliberation, regional peer learning, or non-donor international mechanisms; identification of which coordination functions donors provide vs which are duplicative',
    'If donor coordination is necessary: Rope/Tangled Rope classification is structural. If coordination occurs through alternatives: donor role is extractive overlay (Snare/Piton), not coordination solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_coordination_function_necessity, empirical, 'Whether donor institutions are necessary for state capacity coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_development, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_capacity_development, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stat_tr_t5, state_capacity_development, theater_ratio, 5, 0.54).
narrative_ontology:measurement(stat_tr_t10, state_capacity_development, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_capacity_development, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t5, state_capacity_development, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(stat_be_t10, state_capacity_development, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_development, enforcement_mechanism).
narrative_ontology:affects_constraint(state_capacity_development, institutional_isomorphism).
narrative_ontology:affects_constraint(state_capacity_development, development_conditionality_extraction).
narrative_ontology:affects_constraint(state_capacity_development, local_knowledge_suppression).

% DUAL FORMULATION NOTE:
% State capacity development decomposes into structurally distinct constraints: (1) the genuine coordination problem of building state institutions (ε~0.15, Rope), (2) donor-led institutional form imposition (ε~0.62, Snare/Tangled Rope), and (3) performance metric theater (ε~0.45, Piton/Tangled Rope). This story addresses the aggregate constraint as experienced by recipient states. Upstream constraints (institutional isomorphism in global development discourse, conditionality extraction in donor financing) feed the capacity development constraint; downstream constraints (institutional legitimacy loss, knowledge suppression) result from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_capacity_development, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
