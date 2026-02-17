% ============================================================================
% CONSTRAINT STORY: internet_evolution_lifecycle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-14
% ============================================================================

:- module(constraint_internet_evolution_lifecycle, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: internet_evolution_lifecycle
 *   human_readable: The Lifecycle of the Open Internet
 *   domain: technological/social
 *
 * SUMMARY:
 *   Models the evolution of the internet as a constraint on information and
 *   communication. It begins as a purely coordinative academic network (Rope),
 *   is built out by commercial entities (Scaffold), matures into a platform-
 *   dominated economy (Tangled Rope), and solidifies into a system of
 *   algorithmic enclosure and data extraction (Snare from the user perspective).
 *
 * KEY AGENTS (by structural relationship):
 *   - internet_users: The targets of data extraction and algorithmic control (powerless/trapped).
 *   - platform_corporations: The beneficiaries of network effects and data monetization (institutional/arbitrage).
 *   - A Digital Privacy Advocate: The analytical observer seeing the full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents T=20 (Algorithmic Enclosure)
   ========================================================================== */

% --- Numerical metrics (End State: Enclosure) ---
domain_priors:base_extractiveness(internet_evolution_lifecycle, 0.64). % Data extraction is the core business model.
domain_priors:suppression_score(internet_evolution_lifecycle, 0.80).   % Network effects and walled gardens suppress alternatives.
domain_priors:theater_ratio(internet_evolution_lifecycle, 0.60).       % "Connecting the world" vs. the reality of engagement farming.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_evolution_lifecycle, extractiveness, 0.64).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(internet_evolution_lifecycle, tangled_rope).
narrative_ontology:human_readable(internet_evolution_lifecycle, "The Lifecycle of the Open Internet").

% --- Binary flags ---
domain_priors:requires_active_enforcement(internet_evolution_lifecycle). % Terms of Service, API rules, algorithmic moderation.

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, platform_corporations). % Monetize data and attention.
narrative_ontology:constraint_victim(internet_evolution_lifecycle, internet_users). % Are the product.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: A USER TRAPPED IN A WALLED GARDEN (SNARE)
% For a user whose social, professional, and economic life is tied to a
% dominant platform, the internet is a Snare. Leaving is not a viable option,
% and the platform extracts maximum value from their data and attention.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: A PLATFORM CORPORATION (ROPE)
% For the corporation, its platform is a Rope used to coordinate a global
% user base and a marketplace of advertisers, creating immense value. The
% data extraction is seen as the cost of providing the "free" service.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: A DIGITAL PRIVACY ADVOCATE (TANGLED ROPE)
% The analytical observer sees the internet's dual nature: a powerful tool for
% global coordination that is now dominated by an extractive business model.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_evolution_lifecycle_tests).

test(perspectival_gap, [nondet]) :-
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, tangled_rope, context(agent_power(analytical), _, _, _)).

test(end_state_is_extractive_and_suppressive) :-
    narrative_ontology:constraint_metric(internet_evolution_lifecycle, extractiveness, E),
    narrative_ontology:constraint_metric(internet_evolution_lifecycle, suppression_requirement, S),
    E > 0.6,
    S > 0.7.

:- end_tests(internet_evolution_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story charts the enclosure of a digital commons. The final base
 *   extractiveness is set to 0.64, a high value reflecting that surveillance
 *   capitalism is the dominant business model, but not so high as to eclipse
 *   the system's profound coordination function from an analytical view.
 *   - T=0-4 (ARPANET): ROPE. A pure tool for research collaboration.
 *   - T=5-9 (Dot-com Boom): SCAFFOLD. Commercial ISPs and search engines build the on-ramps to a still-open web.
 *   - T=10-15 (Web 2.0): TANGLED ROPE. Centralized platforms provide huge coordination benefits, but introduce the extractive model.
 *   - T=16-20 (Algorithmic Enclosure): SNARE (for users). Network effects become so powerful that platforms are non-optional, trapping users.
 *
 * PERSPECTIVAL GAP:
 *   The user, the ultimate source of value, experiences the final state as a Snare where their attention is farmed and their data is sold. The platform corporation experiences its own ecosystem as a fantastic Rope for organizing people and capital, viewing the extraction as a simple cost of doing business. The analyst sees the Tangled Rope, acknowledging that both realities are true simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic candidate for mandatrophy: a system with a
 *   genuine, powerful coordination function (connecting the globe) that also
 *   operates a high-extraction business model (surveillance capitalism). A naive
 *   analysis might label it a "necessary evil" (a Rope with costs) or pure
 *   exploitation (a Snare). The Deferential Realism framework resolves this by
 *   refusing a single, monolithic classification.
 *   - For the beneficiary (platform), whose business model depends on it, the
 *     system is a Rope. The framework validates this perspective via a low `χ`.
 *   - For the victim (user), whose data and attention are the extracted resources,
 *     the system is a Snare. The framework validates this via a high `χ`.
 *   - The analytical `tangled_rope` classification codifies this structural
 *     tension, preventing the beneficiary's "coordination" narrative from
 *     obscuring the reality of the target's extractive experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_internet_decentralization,
    'Can decentralized protocols (Web3) create a viable alternative (a new Rope), or will they be co-opted by the same centralizing forces (becoming new Snares)?',
    'Observation of user adoption rates and capital concentration in "decentralized" systems over the next decade.',
    'If they succeed, a new, open lifecycle begins. If they fail, the current Snare/Tangled Rope paradigm may be a permanent fixture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_evolution_lifecycle, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% T=0-4: ARPANET (Rope)
narrative_ontology:measurement(inet_ex_t0, internet_evolution_lifecycle, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(inet_th_t0, internet_evolution_lifecycle, theater_ratio, 0, 0.0).
narrative_ontology:measurement(inet_su_t0, internet_evolution_lifecycle, suppression_score, 0, 0.05).

% T=5-9: Dot-com Boom (Scaffold)
narrative_ontology:measurement(inet_ex_t5, internet_evolution_lifecycle, base_extractiveness, 5, 0.10). % ISPs charge fees
narrative_ontology:measurement(inet_th_t5, internet_evolution_lifecycle, theater_ratio, 5, 0.05).
narrative_ontology:measurement(inet_su_t5, internet_evolution_lifecycle, suppression_score, 5, 0.20).
narrative_ontology:measurement(inet_ex_t9, internet_evolution_lifecycle, base_extractiveness, 9, 0.15).
narrative_ontology:measurement(inet_th_t9, internet_evolution_lifecycle, theater_ratio, 9, 0.10).
narrative_ontology:measurement(inet_su_t9, internet_evolution_lifecycle, suppression_score, 9, 0.30).

% T=10-15: Web 2.0 (Tangled Rope)
narrative_ontology:measurement(inet_ex_t10, internet_evolution_lifecycle, base_extractiveness, 10, 0.40). % Data extraction begins in earnest
narrative_ontology:measurement(inet_th_t10, internet_evolution_lifecycle, theater_ratio, 10, 0.30). % "Connecting the world"
narrative_ontology:measurement(inet_su_t10, internet_evolution_lifecycle, suppression_score, 10, 0.55). % Network effects begin to lock users in
narrative_ontology:measurement(inet_ex_t15, internet_evolution_lifecycle, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(inet_th_t15, internet_evolution_lifecycle, theater_ratio, 15, 0.50).
narrative_ontology:measurement(inet_su_t15, internet_evolution_lifecycle, suppression_score, 15, 0.70).

% T=16-20: Algorithmic Enclosure (Snare)
narrative_ontology:measurement(inet_ex_t16, internet_evolution_lifecycle, base_extractiveness, 16, 0.60).
narrative_ontology:measurement(inet_th_t16, internet_evolution_lifecycle, theater_ratio, 16, 0.60).
narrative_ontology:measurement(inet_su_t16, internet_evolution_lifecycle, suppression_score, 16, 0.75).
narrative_ontology:measurement(inet_ex_t20, internet_evolution_lifecycle, base_extractiveness, 20, 0.64). % Final value
narrative_ontology:measurement(inet_th_t20, internet_evolution_lifecycle, theater_ratio, 20, 0.60).   % Final value
narrative_ontology:measurement(inet_su_t20, internet_evolution_lifecycle, suppression_score, 20, 0.80).   % Final value

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_evolution_lifecycle, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */