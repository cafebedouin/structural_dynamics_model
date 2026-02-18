% ============================================================================
% CONSTRAINT STORY: ice_safe_departure
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ice_safe_departure, []).

:- use_module(library(plunit)).
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ice_safe_departure
 *   human_readable: "ICE Safe Departure Program"
 *   domain: political
 *
 * SUMMARY:
 *   The "Safe Departure Program" was an ICE initiative allowing certain
 *   immigrants with final deportation orders to leave the U.S. voluntarily.
 *   In exchange for avoiding a formal deportation record (and its severe
 *   future immigration consequences), participants had to waive all rights to
 *   appeal and pay for their own departure. The program framed a coerced
 *   choice as a compassionate alternative to forcible removal.
 *
 * KEY AGENTS (by structural relationship):
 *   - non_criminal_immigrants_with_deportation_orders: Primary target (powerless/trapped) — bears full extraction by relinquishing legal rights and their life in the U.S.
 *   - ice_enforcement_agency: Primary beneficiary (institutional/arbitrage) — benefits from reduced operational costs, lower detention needs, and achieving removal targets efficiently.
 *   - immigration_hardline_political_actors: Secondary beneficiary (organized/mobile) — benefits from the program fulfilling political promises of increased removals.
 *   - analytical_observer: Analytical observer — sees the dual function of cost-saving coordination and coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is the surrender of legal rights, community ties, and future
% prospects, plus the financial cost of self-removal.
domain_priors:base_extractiveness(ice_safe_departure, 0.75).

% Suppression is extremely high; the alternative to participation is being
% designated a high-priority target for raids and forcible removal.
domain_priors:suppression_score(ice_safe_departure, 0.85).

% Theater is present in the "compassionate" framing, but the program has a
% clear, non-performative function.
domain_priors:theater_ratio(ice_safe_departure, 0.30).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_safe_departure, extractiveness, 0.75).
narrative_ontology:constraint_metric(ice_safe_departure, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ice_safe_departure, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ice_safe_departure, tangled_rope).
narrative_ontology:human_readable(ice_safe_departure, "ICE Safe Departure Program").
narrative_ontology:topic_domain(ice_safe_departure, "political").

% --- Binary flags ---
% The program's existence is predicated on the threat of enforcement.
domain_priors:requires_active_enforcement(ice_safe_departure).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ice_safe_departure, ice_enforcement_agency).
narrative_ontology:constraint_beneficiary(ice_safe_departure, immigration_hardline_political_actors).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ice_safe_departure, non_criminal_immigrants_with_deportation_orders).

% Gate requirements check:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement -> MET.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. For them, it is a coerced choice between
% two destructive outcomes, with no viable alternative to maintain their life.
% Engine derives d from victim membership + trapped exit -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. For ICE, the program is a pure coordination
% mechanism to manage resources and achieve deportation quotas efficiently.
% Engine derives d from beneficiary membership + arbitrage exit -> d ≈ 0.05 -> negative χ.
constraint_indexing:constraint_classification(ice_safe_departure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the genuine coordination function (for the state) and
% the severe, asymmetric extraction imposed on the individual. This dual
% nature is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_safe_departure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_safe_departure, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ice_safe_departure, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: powerless -> snare, institutional -> rope~n').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(ice_safe_departure, _),
    narrative_ontology:constraint_victim(ice_safe_departure, _),
    domain_priors:requires_active_enforcement(ice_safe_departure).

:- end_tests(ice_safe_departure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.75): This score reflects the severe cost imposed on the target: the forfeiture of all legal appeal rights, the loss of a life built over years or decades in the U.S., and the financial burden of self-funded removal. The state extracts immense value by externalizing enforcement costs and legal challenges.
 *   - Suppression Score (0.85): The choice architecture is highly coercive. The alternative to accepting the "offer" is not a neutral status quo, but rather designation as a high-priority target for forcible arrest and deportation, which carries even more severe long-term consequences. This structure effectively suppresses all other options.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For an immigrant like the couple in the article, the program is a Snare. It presents an illusion of choice while channeling them towards a predetermined, extractive outcome. They are trapped. For the ICE agency, the program is a Rope. It is a highly effective coordination tool that solves a resource allocation problem, reducing the cost and complexity of achieving mandated removal targets. The agency can deploy its enforcement resources elsewhere (arbitrage).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The beneficiaries are the state enforcement agency (ICE), which saves operational funds and improves its performance metrics, and the political actors who champion high removal numbers. The victims are the immigrants who must dismantle their lives under duress. The beneficiary/victim declarations directly map to this flow of costs and benefits, driving the derivation of d and the resulting perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This case is a canonical example of a Tangled Rope. A naive analysis might label it a pure Snare, focusing only on the victim's experience. Conversely, a state-centric analysis might call it a Rope, focusing only on its administrative efficiency. The Deferential Realism framework, by requiring multiple indexed perspectives, avoids both errors. It correctly identifies the Rope from the institutional view and the Snare from the powerless view. The analytical resolution to Tangled Rope correctly synthesizes these facts: it is a system with a genuine coordination function *for the beneficiary* that is implemented through severe, asymmetric extraction *from the target*. This prevents the mislabeling of coercive state action as simple coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ice_safe_departure,
    'Is the programs primary design goal administrative efficiency (coordination) or is it to psychologically coerce self-deportation by removing legal hope (extraction)?',
    'Review of internal DHS/ICE policy memos and cost-benefit analyses that established the program.',
    'If primarily for efficiency, it validates the Tangled Rope classification. If primarily for coercion, it would be a Snare even from the analytical perspective.',
    confidence_without_resolution(high) % The structure is clearly hybrid, regardless of intent.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ice_safe_departure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint. The program was rolled
% out, operated, and then discontinued, giving it a clear lifecycle.
% Extraction is modeled as stable, as the core bargain did not change.
% Theater is modeled as slightly higher at rollout (for PR) and then settling.
%
% Theater ratio over time:
narrative_ontology:measurement(ice_safe_departure_tr_t0, ice_safe_departure, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ice_safe_departure_tr_t5, ice_safe_departure, theater_ratio, 5, 0.30).
narrative_ontology:measurement(ice_safe_departure_tr_t10, ice_safe_departure, theater_ratio, 10, 0.30).

% Extraction over time:
narrative_ontology:measurement(ice_safe_departure_ex_t0, ice_safe_departure, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(ice_safe_departure_ex_t5, ice_safe_departure, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ice_safe_departure_ex_t10, ice_safe_departure, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The program is a mechanism for optimizing the allocation of scarce
% state resources (detention beds, enforcement agents).
narrative_ontology:coordination_type(ice_safe_departure, resource_allocation).

% This program structurally impacts the legal system by creating an off-ramp
% that circumvents the standard appeals process.
narrative_ontology:affects_constraint(ice_safe_departure, immigration_legal_appeals_system).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */