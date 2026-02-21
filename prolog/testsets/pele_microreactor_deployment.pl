% ============================================================================
% CONSTRAINT STORY: pele_microreactor_deployment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_pele_microreactor_deployment, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pele_microreactor_deployment
 *   human_readable: "Pele Mobile Micro-Reactor Deployment Protocol"
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   The constraint is the system of rules, security protocols, and political
 *   agreements governing the deployment of a transportable nuclear reactor
 *   (Project Pele) to power forward military bases. This system solves the
 *   immense logistical burden and danger of fuel convoys for diesel
 *   generators, but introduces significant new nuclear security, safety, and
 *   geopolitical risks, which are asymmetrically distributed.
 *
 * KEY AGENTS (by structural relationship):
 *   - Host Nation Governments: Primary institutional target (institutional/constrained) — bear political and environmental risk under diplomatic pressure.
 *   - Local Populace near base: Primary powerless target (powerless/trapped) - bear direct risk of accident or attack with no agency.
 *   - U.S. Military Logistics Command: Primary beneficiary (institutional/arbitrage) — gains immense strategic and operational advantages.
 *   - Deployed Reactor Personnel: Secondary target (moderate/trapped) — face extreme personal risk as operators and defenders of a high-value target.
 *   - Analytical Observer: Sees the full structure of coordination and risk transfer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pele_microreactor_deployment, 0.60).
domain_priors:suppression_score(pele_microreactor_deployment, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(pele_microreactor_deployment, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pele_microreactor_deployment, extractiveness, 0.60).
narrative_ontology:constraint_metric(pele_microreactor_deployment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pele_microreactor_deployment, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pele_microreactor_deployment, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(pele_microreactor_deployment). % Mandatory for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, us_military_logistics_command).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pele_microreactor_deployment, host_nations).
narrative_ontology:constraint_victim(pele_microreactor_deployment, local_populace).
narrative_ontology:constraint_victim(pele_microreactor_deployment, deployed_reactor_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOCAL POPULACE (SNARE)
% Bears the uncompensated risk of a nuclear incident or attack with no
% ability to refuse. Engine derives d from victim membership + trapped exit
% → d ≈ 0.95 → f(d) ≈ 1.42. The local scope σ(S)=0.8 dampens χ, but the high
% ε ensures it still classifies as a Snare.
% χ = 0.60 * 1.42 * 0.8 = 0.68 (Snare: χ ≥ 0.66)
constraint_indexing:constraint_classification(pele_microreactor_deployment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (U.S. MILITARY COMMAND) (ROPE)
% Experiences a pure coordination solution, eliminating a deadly logistical
% vulnerability. Engine derives d from beneficiary status + arbitrage exit
% → d ≈ 0.05 → f(d) ≈ -0.12, resulting in negative effective extraction.
% χ = 0.60 * -0.12 * 1.0 = -0.072
constraint_indexing:constraint_classification(pele_microreactor_deployment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Perceives both the powerful coordination function and the severe,
% asymmetrically imposed risk. The high base extraction, suppression, and
% need for enforcement, combined with a clear beneficiary, define a
% canonical Tangled Rope.
constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE HOST NATION GOVERNMENT (TANGLED ROPE/SNARE)
% An institutional actor, but one whose sovereignty is constrained. Refusing
% deployment carries significant diplomatic costs. It benefits from the
% security the US base provides, but is coerced into accepting nuclear risk.
% Exit is 'constrained', leading to a higher d than the beneficiary.
% The engine derives a high d from victim status + constrained exit.
% χ = 0.60 * f(d) * 1.0. For d ≈ 0.8, f(d) ≈ 1.25 -> χ ≈ 0.75.
constraint_indexing:constraint_classification(pele_microreactor_deployment, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pele_microreactor_deployment_tests).

test(perspectival_gap) :-
    % Verify the core Rope vs. Snare gap.
    constraint_indexing:constraint_classification(pele_microreactor_deployment, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(pele_microreactor_deployment, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(pele_microreactor_deployment, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_gap) :-
    % Verify that two institutional actors classify differently due to exit options.
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(pele_microreactor_deployment, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeBeneficiary = rope,
    TypeTarget = snare,
    TypeBeneficiary \= TypeTarget.

test(tangled_rope_gate_compliance) :-
    % Verify the structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(pele_microreactor_deployment, _),
    narrative_ontology:constraint_victim(pele_microreactor_deployment, _),
    domain_priors:requires_active_enforcement(pele_microreactor_deployment).

:- end_tests(pele_microreactor_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.60): This value models the severe, non-financial
 *     extraction of imposing significant nuclear and geopolitical risk onto
 *     host nations and local populations. The cost is the potential for
 *     catastrophic failure or attack.
 *   - Suppression Score (0.65): High, because the system is designed to
 *     suppress the highly dangerous alternative (fuel convoys). For host
 *     nations, alternatives to accepting deployment are suppressed by
 *     diplomatic and strategic pressure.
 *   - `requires_active_enforcement`: This is critical. A mobile nuclear
 *     reactor is arguably one of the most enforcement-intensive pieces of
 *     equipment imaginable, requiring constant, high-level security.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the U.S. Military, the constraint is a pure Rope: it
 *   solves a deadly coordination problem (fuel logistics) with immense
 *   benefit. For the host nation and its people, it is a Snare: they are
 *   coerced into accepting a profound risk from which they derive little
 *   direct benefit, with limited ability to refuse. The analytical view sees
 *   both halves of this structure, correctly identifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_military_logistics_command` benefits directly by
 *     securing its energy supply chain, reducing casualties, and increasing
 *     operational freedom.
 *   - Victims: `host_nations` and the `local_populace` bear the externalized
 *     risk. Their land and lives are the collateral for the beneficiary's
 *     logistical security. This is a classic case of risk transfer.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the power asymmetry between two institutional actors.
 *   Both are 'institutional', but their `exit_options` differ dramatically.
 *   The U.S. Military has `arbitrage`—it can choose where and when to deploy
 *   this asset. The host nation government has a `constrained` exit—refusing
 *   the deployment could damage its strategic alliance with a superpower. This
 *   difference in exit options is what drives the different directionality
 *   values (d) and thus the different classifications (Rope vs. Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   A naive analysis might classify this technology as a pure Rope (a "public
 *   good" providing clean, reliable power). This would be a failure of
 *   Mandatrophy, ignoring the coercive risk-transfer at its core. The
 *   Deferential Realism framework, by indexing to the powerless and
 *   constrained actors, correctly identifies the extractive component and
 *   arrives at the more accurate Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pele_microreactor_deployment,
    'What is the true, long-term probability of a catastrophic failure (accident, theft, or successful attack) of a deployed Pele reactor?',
    'Decades of operational data and analysis of near-misses, which are not yet available for this new technology.',
    'If the risk is near-zero, the `base_extractiveness` is overstated, and the constraint is closer to a Rope. If the risk is non-trivial, the Snare classification for victims is strongly justified.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pele_microreactor_deployment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.60 > 0.46), so temporal data is required.
% We model a scenario where, as the technology becomes normalized, the
% political pressure to accept it (extraction) increases over time.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(pele_tr_t0, pele_microreactor_deployment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pele_tr_t5, pele_microreactor_deployment, theater_ratio, 5, 0.10).
narrative_ontology:measurement(pele_tr_t10, pele_microreactor_deployment, theater_ratio, 10, 0.10).

% Extraction over time (increases as deployment becomes standard practice):
narrative_ontology:measurement(pele_ex_t0, pele_microreactor_deployment, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(pele_ex_t5, pele_microreactor_deployment, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pele_ex_t10, pele_microreactor_deployment, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (it's a system for allocating energy).
narrative_ontology:coordination_type(pele_microreactor_deployment, resource_allocation).

% Network relationships: This technology directly impacts the constraint of
% depending on fossil fuels.
narrative_ontology:affects_constraint(pele_microreactor_deployment, military_fossil_fuel_dependency).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation from
% beneficiary/victim declarations combined with the different exit_options
% for institutional actors accurately captures the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */