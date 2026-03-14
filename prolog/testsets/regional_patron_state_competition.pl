% ============================================================================
% CONSTRAINT STORY: regional_patron_state_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_patron_state_competition, []).

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
 *   constraint_id: regional_patron_state_competition
 *   human_readable: Regional Patron State Competition
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Regional patron state competition creates a structural constraint where
 *   geopolitical rivalry between great powers transforms medium-sized and
 *   small states into contested territories. The constraint exhibits the full
 *   spectrum of DR classifications depending on the observer's structural
 *   position. Patron states compete to expand their regional spheres of
 *   influence by offering conditional benefits (security guarantees, economic
 *   aid, diplomatic recognition) to client states. Client states face
 *   pressure to choose exclusive alignment or face extraction through
 *   conditional benefit withdrawal, military pressure, or sanctions. The
 *   constraint has intensified over the measured interval as patron
 *   competition has become more explicit and extractive demands more
 *   intrusive. The theater ratio remains moderate because patron-client
 *   relationships involve genuine security and economic coordination
 *   alongside explicit political demands. Regional organizations attempting
 *   to build autonomy experience the constraint as both enabling (patron
 *   security umbrellas allow regional cooperation) and extractive (patron
 *   competition destabilizes regional unity). The analytical observer risks
 *   naturalizing this contingent institutional arrangement as an immutable
 *   law of international relations.
 *
 * KEY AGENTS:
 *   - Client States: Primary victims (powerless/trapped) — structurally dependent on patron security and economic benefits; face extraction through conditional benefit provision and pressure for exclusive alignment
 *   - Non-Aligned States: Secondary victims (moderate/constrained) — face competitive pressure from patron bidding; constrained by extraction costs of diversifying or switching patrons
 *   - Regional Coalition/Organizations: Moderate agent (organized/constrained) — attempting to build regional autonomy; face internal destabilization from patron exploitation of inter-member disputes
 *   - Great Power Patrons: Primary beneficiaries (institutional/arbitrage) — capture geopolitical influence, market access, and strategic positioning; experience the constraint as coordination of regional alignment
 *   - International Institutions: Organized agent (organized/constrained) — working to reduce patron leverage through regional institutional capacity building; face enforcement suppression from patrons undercutting competing blocs
 *   - Cold War Legacy Structures: Institutional remnant (institutional/arbitrage) — NATO and successor alliances persist through inertia; provide patron states with legitimacy structures and burden-sharing theater
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable structural features of international anarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_patron_state_competition, 0.58).
domain_priors:suppression_score(regional_patron_state_competition, 0.65).
domain_priors:theater_ratio(regional_patron_state_competition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_patron_state_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_patron_state_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regional_patron_state_competition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_patron_state_competition, tangled_rope).
narrative_ontology:human_readable(regional_patron_state_competition, "Regional Patron State Competition").
narrative_ontology:topic_domain(regional_patron_state_competition, "geopolitical/economic").

domain_priors:requires_active_enforcement(regional_patron_state_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_patron_state_competition, great_powers).
narrative_ontology:constraint_beneficiary(regional_patron_state_competition, regional_hegemon).
narrative_ontology:constraint_victim(regional_patron_state_competition, client_states).
narrative_ontology:constraint_victim(regional_patron_state_competition, regional_stability).
narrative_ontology:constraint_victim(regional_patron_state_competition, non_aligned_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIENT STATE (SNARE) — Trapped in structural dependency. Economic integration, military aid, security guarantees, and diplomatic recognition are conditioned on alignment with patron state preferences. Exit costs include loss of security umbrella, economic isolation, and potential sanctions or intervention. Alternatives (neutrality, regional integration on equal terms) are foreclosed by patron demand for exclusive loyalty.
constraint_indexing:constraint_classification(regional_patron_state_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-ALIGNED STATE (TANGLED ROPE) — Structurally mobile but constrained by competitive pressure. Can nominally choose alignment, but patron competition creates extraction: each patron offers conditional benefits (trade, investment, security) that require demonstrable loyalty, creating bidding wars and escalating demands. Genuine coordination function (security provision, economic integration) exists alongside asymmetric extraction (patron shapes internal policy, constrains foreign policy options).
constraint_indexing:constraint_classification(regional_patron_state_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL COALITION (TANGLED ROPE) — Collective action by mid-size states attempting to build regional autonomy. Coordination function is genuine (mutual defense, trade bloc integration). But extraction mechanism persists: patrons exploit inter-member disputes, offering selective benefits to break coalition unity. Requires active enforcement (institutionalization, dispute resolution). Coalition sees the constraint as both enabling (patron security umbrella allows coalition members to invest in regional cooperation) and extractive (patron competition destabilizes the coalition from within).
constraint_indexing:constraint_classification(regional_patron_state_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GREAT POWER PATRON (ROPE) — Experiences the constraint as pure coordination: providing security guarantees, economic benefits, and diplomatic recognition solves the collective action problem of regional alignment. Client states benefit from patron protection; patron benefits from geopolitical influence and market access. Extraction runs toward the patron (clients sacrifice autonomy), but patron frames this as legitimate payment for services rendered. Low experienced extractiveness because patron has exit options (can reallocate patron role to other regions) and framing power (control of legitimacy narratives).
constraint_indexing:constraint_classification(regional_patron_state_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL INSTITUTIONS (SCAFFOLD) — Regional organizations (ASEAN, African Union, MERCOSUR) aim to build regional autonomy and reduce patron dependence through institutionalized cooperation and dispute resolution. These mechanisms have a sunset clause: as regional capacity for self-governance increases, patron leverage decreases. Theater ratio is low because these institutions focus on functional integration rather than performative sovereignty. However, enforcement suppression is high — patrons can undermine institutions by funding competing regional blocs or threatening non-members with isolation.
constraint_indexing:constraint_classification(regional_patron_state_competition, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR LEGACY (PITON) — NATO, Warsaw Pact-successor relationships, and other alliance remnants persist largely through institutional inertia. The original function (containing communist/capitalist bloc expansion) has degraded. Modern patron-client relationships are maintained through alliance rituals, burden-sharing theater, and historical legitimacy rather than current strategic necessity. Theater ratio is high — many allied states maintain military integration and treaty obligations that would be reconsidered in a post-ideological environment. The piton persists because alternatives require institutional reorganization, not because current structures optimize patron-client coordination.
constraint_indexing:constraint_classification(regional_patron_state_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL / REALIST NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some patron-client structural relationship appears inevitable in international anarchy: states without military capacity must seek security guarantees; states without economic scale must seek trade partnerships; small states must align with larger powers. This perspective naturalizes patron competition as an immutable feature of the international system. However, structural data reveals this as a false summit: specific patron-client relationships (Cold War alliances, contemporary client states) are contingent institutional arrangements, not laws of nature. The 'inevitability' framing obscures how patron competition dynamics can be reshaped through institutional design.
constraint_indexing:constraint_classification(regional_patron_state_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_patron_state_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_patron_state_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_patron_state_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_patron_state_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_patron_state_competition, TR),
    TR >= 0.70.

:- end_tests(regional_patron_state_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The base extractiveness reflects patron competitive dynamics creating escalating demands for client state loyalty. Measurement trajectory (0.35 → 0.58 over interval) shows intensifying extraction as patrons compete more explicitly for regional positioning. The increase is driven by: (1) patron willingness to use conditional benefits as leverage; (2) client state vulnerability to patron pressure due to limited alternatives; (3) competitive pressure driving patrons to demand more exclusive alignment. Suppression (0.65): High. Significant barriers to client state exit include: military vulnerability without patron security umbrella; economic integration creating trade dependency; diplomatic isolation risk if patron recognition is withdrawn; sanctions threat. However, suppression is not absolute — some client states maintain diversified patron relationships and switch patrons at significant but surmountable cost. Theater ratio (0.48): Moderate and stable. Patron-client relationships involve genuine security and economic coordination (low theater component) alongside explicit political demands and alliance rituals (higher theater component). Theater is increasing slightly (0.32 → 0.48) as patron competition drives more performative alliance maintenance and burden-sharing negotiations. The moderate theater reflects that these relationships genuinely coordinate regional security while simultaneously extracting political concessions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural arrangement (patron provision of security and economic benefits in exchange for client alignment) is experienced as coordination (by patrons and regional institutions) or extraction (by client states and non-aligned states) depending on structural position. The perspectival gap reveals that the constraint functions as both: genuine coordination of regional security alongside asymmetric extraction of client state autonomy. The gap is not resolvable into a single 'true' classification — the constraint genuinely is both rope and snare simultaneously, depending on observational position. The theater component (0.48) reflects this hybrid: alliance structures perform both real security functions and explicit power-sharing theater. The cold war legacy piton perspective highlights how some aspects of the constraint persist despite degraded original function, maintained through institutional inertia rather than current strategic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are great power patrons and regional hegemon states — they capture geopolitical influence, market access, and alliance leverage. Victims are client states (bear extraction), non-aligned states (face competitive pressure), and regional stability (destabilized by patron competition). Regional institutions are ambiguous: they benefit from patron security umbrellas while victimized by patron-exploited divisions. Derived directionality ranges from d ≈ 0.95 (trapped client state) to d ≈ 0.10 (beneficiary patron state), producing f(d) range from 1.42 to -0.05. The broad range reflects the constraint's hybrid character: extraction concentrates on trapped agents while dissipates toward beneficiary agents with exit options. No directionality overrides needed — the structural derivation captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that patron-client competition genuinely coordinates regional security while simultaneously extracting client state autonomy. The constraint is legitimately classified as tangled rope: it has a real coordination function (providing security guarantees reduces inter-regional conflict and stabilizes regional hierarchy) AND asymmetric extraction (client states sacrifice autonomy in exchange). The bifurcation in perspectives (rope from patrons, snare from clients, tangled rope from analysts) reflects the constraint's genuine hybrid character, not misclassification. The false summit risk is that realist international relations scholarship naturalizes patron-client competition as an inevitable feature of anarchic international order — a natural law. The structural data contradicts this: patron-client relationships are contingent institutional arrangements that could be reshaped through different coordination mechanisms (regional integration institutions, non-aligned coalitions, multipolarity without patron competition). The analytics showing increasing extractiveness (0.35 → 0.58) and theater (0.32 → 0.48) over the measurement interval suggest that the constraint is degrading — shifting from coordination-dominated (early interval) to extraction-dominated (current). This trajectory would eventually shift the classification toward snare if extractiveness continues rising above 0.66.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_alignment_threshold,
    'At what point does conditional benefit provision cross from coordination incentive to coercive extraction?',
    'Comparative analysis of client state policy changes attributable to patron pressure vs organic policy shifts; measurement of client state policy autonomy before/after patron integration; analysis of exit costs for client states attempting reorientation',
    'If threshold is low (easily crossed): most patron-client relationships are snares. If threshold is high (rarely crossed): most relationships are ropes with asymmetric benefits. Current uncertainty: whether conditional benefits represent fair payment for services or extractive coercion depends on counterfactual client state security options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_alignment_threshold, conceptual, 'Threshold distinguishing coordination from coercive extraction in patron-client relationships').

omega_variable(
    regional_alternative_viability,
    'Could regional states achieve equivalent security and economic outcomes through autonomous regional integration instead of patron alignment?',
    'Historical counterfactual analysis (Yugoslavia, Non-Aligned Movement outcomes); current experimentation (ASEAN integration, African Union capacity); modeling of regional military balance and economic integration feasibility without patron involvement',
    'If viable alternatives exist: patron competition suppresses access to them (high suppression). If alternatives are structurally infeasible: client states'' attachment to patrons reflects genuine rational choice, not extraction. Classification consequence: snare vs rope depends on alternative availability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_alternative_viability, empirical, 'Whether viable regional autonomy alternatives exist to patron alignment').

omega_variable(
    patron_substitutability,
    'How easily can client states switch patrons or diversify patron relationships without incurring extraction costs?',
    'Analysis of switching costs: economic decoupling, military reorientation, diplomatic realignment timelines, sanctions risk; case studies of patron switching (Egypt, India, Vietnam transitions); measurement of client state freedom to maintain multiple patron relationships simultaneously',
    'If switching is costly: client states are trapped (snare). If switching is feasible: client states are constrained but mobile (tangled rope). If switching is costless: client states are mobile (rope-level extraction only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_substitutability, empirical, 'Switching costs for patron diversification or reorientation').

omega_variable(
    patron_competitive_dynamics,
    'Does patron competition in a region reduce or intensify extraction of client states?',
    'Comparative analysis: regions with patron competition (Middle East, Southeast Asia) vs regions with patron monopoly (Eastern Europe post-2014); measurement of client state autonomy, benefit provision generosity, and extraction intensity across competition regimes; analysis of how patron switching threats affect patron behavior',
    'If competition reduces extraction: client states benefit from patron bidding wars (rope-level). If competition intensifies extraction: each patron demands exclusive loyalty to retain position (snare-level). Current data ambiguous — depends on whether patrons compete on benefits or on coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_competitive_dynamics, empirical, 'Whether patron competition reduces or intensifies client state extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_patron_state_competition, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpsc_tr_t0, regional_patron_state_competition, theater_ratio, 0, 0.32).
narrative_ontology:measurement(rpsc_tr_t15, regional_patron_state_competition, theater_ratio, 15, 0.4).
narrative_ontology:measurement(rpsc_tr_t30, regional_patron_state_competition, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(rpsc_be_t0, regional_patron_state_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rpsc_be_t15, regional_patron_state_competition, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rpsc_be_t30, regional_patron_state_competition, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_patron_state_competition, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_patron_state_competition, regional_hegemonic_stability).
narrative_ontology:affects_constraint(regional_patron_state_competition, client_state_autonomy_erosion).
narrative_ontology:affects_constraint(regional_patron_state_competition, non_aligned_movement_viability).

% DUAL FORMULATION NOTE:
% Regional patron state competition is a higher-order constraint affecting multiple downstream constraints: hegemonic stability in specific regions, autonomy erosion of specific client states, and viability of non-aligned movement alternatives. Each downstream constraint has its own ε value reflecting local structural conditions. The upstream constraint (patron competition dynamics) creates the environmental conditions that drive downstream constraint intensification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_patron_state_competition, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
