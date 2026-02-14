% ============================================================================
% CONSTRAINT STORY: greenland_defence_pact_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_greenland_defence_pact_2026, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: greenland_defence_pact_2026
 *   human_readable: Greenland-Denmark-NATO Defence Pact of 2026
 *   domain: geopolitical
 *
 * SUMMARY:
 *   A new, permanent defence agreement between Greenland, Denmark, and NATO
 *   establishes a European troop presence in Greenland. The pact grants
 *   Greenland a direct say in its own defence for the first time, but also
 *   solidifies NATO's strategic control over the Arctic region, creating
 *   asymmetric costs and benefits for the involved parties.
 *
 * KEY AGENTS (by structural relationship):
 *   - Local Greenlandic Communities: Primary target (powerless/trapped) — bear the social, cultural, and environmental costs of an increased foreign military presence.
 *   - NATO Alliance & Danish Government: Primary beneficiaries (institutional/arbitrage) — gain strategic depth against rivals and secure the GIUK gap, fulfilling geopolitical objectives.
 *   - Greenlandic Government (Kalaallit Nunaat): Secondary actor / hybrid role (organized/constrained) — gains nominal sovereignty and a seat at the table, but at the cost of accepting the pact's core extractive terms.
 *   - Analytical Observer: Geopolitical analyst (analytical/analytical) — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greenland_defence_pact_2026, 0.52).
domain_priors:suppression_score(greenland_defence_pact_2026, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(greenland_defence_pact_2026, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greenland_defence_pact_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(greenland_defence_pact_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(greenland_defence_pact_2026, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(greenland_defence_pact_2026, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(greenland_defence_pact_2026).
domain_priors:requires_active_enforcement(greenland_defence_pact_2026). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint type.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(greenland_defence_pact_2026, nato_alliance).
narrative_ontology:constraint_beneficiary(greenland_defence_pact_2026, danish_government).
narrative_ontology:constraint_beneficiary(greenland_defence_pact_2026, greenlandic_government). % Gains nominal power
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(greenland_defence_pact_2026, local_greenlandic_communities).
narrative_ontology:constraint_victim(greenland_defence_pact_2026, greenlandic_government). % Cedes ultimate control
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Local communities who bear the direct costs. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   Calculation: χ = 0.52 * 1.42 * 0.9 (regional) ≈ 0.664. This meets the χ ≥ 0.66 threshold for a Snare.
constraint_indexing:constraint_classification(greenland_defence_pact_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% NATO planners who see a solution to a geopolitical coordination problem. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(greenland_defence_pact_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global). Sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% Calculation: χ = 0.52 * 1.15 * 1.2 (global) ≈ 0.717. This is within the Tangled Rope band [0.40, 0.90].
constraint_indexing:constraint_classification(greenland_defence_pact_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The pact creates different realities for the Danish and Greenlandic governments.

% Perspective 4A: Danish Government (institutional, arbitrage exit) -> ROPE
% As the sovereign power and a primary beneficiary, Denmark perceives the pact as pure coordination.
% Engine derives d from: beneficiary status + arbitrage exit -> low d -> negative χ.
constraint_indexing:constraint_classification(greenland_defence_pact_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4B: Greenlandic Government (organized, constrained exit) -> TANGLED ROPE
% The Greenlandic government is both a beneficiary (gains a voice) and a victim (cedes control).
% Its constrained exit options reflect its limited sovereignty.
% Engine derives d from: (beneficiary+victim) + constrained exit -> medium d -> positive χ.
constraint_indexing:constraint_classification(greenland_defence_pact_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greenland_defence_pact_2026_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the gap between the powerless local view (Snare) and the institutional NATO view (Rope).
    constraint_indexing:constraint_classification(greenland_defence_pact_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(greenland_defence_pact_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the gap between the two governments.
    constraint_indexing:constraint_classification(greenland_defence_pact_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)), % Denmark
    constraint_indexing:constraint_classification(greenland_defence_pact_2026, tangled_rope, context(agent_power(organized), _, exit_options(constrained), _)), % Greenland
    true.

test(analytical_claim_matches) :-
    % The analytical classification must match the declared constraint claim.
    narrative_ontology:constraint_claim(greenland_defence_pact_2026, Claim),
    constraint_indexing:constraint_classification(greenland_defence_pact_2026, Claim, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_passed) :-
    % A Tangled Rope must have beneficiary, victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(greenland_defence_pact_2026, _),
    narrative_ontology:constraint_victim(greenland_defence_pact_2026, _),
    domain_priors:requires_active_enforcement(greenland_defence_pact_2026).


:- end_tests(greenland_defence_pact_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the significant transfer of geopolitical control to NATO and the imposition of social/environmental costs on the local population. It's high enough to be coercive but not absolute, acknowledging the coordination function.
 *   - Suppression (0.65): Greenland has very few viable alternatives. Aligning with non-NATO powers is politically impossible, and true neutrality is not feasible given its strategic location and relationship with Denmark.
 *   - The combination of high extraction and high suppression with a clear coordination goal (Arctic security) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For NATO and Denmark, this is a Rope—an elegant solution to a complex coordination problem (countering Russian/Chinese influence). For local communities who are trapped with the consequences, it is a Snare—a coercive imposition with high costs and no escape. The analytical view of Tangled Rope reconciles these by acknowledging both the coordination function and the asymmetric extraction it enables.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: NATO gains a critical strategic position. Denmark fulfills its security obligations and strengthens its alliance role. The Greenlandic government gains political legitimacy and a formal voice.
 *   - Victims: Local communities bear the direct externalities of military bases. The Greenlandic government is also a victim in that it trades ultimate sovereignty for a seat at the table, locking it into a structure it cannot control. The engine derives directionality (d) from these declared relationships and the agent's exit options, correctly calculating high effective extraction (χ) for victims and low/negative χ for beneficiaries.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both the Danish and Greenlandic governments are formal political actors, but they have vastly different relationships to the constraint.
 *   - Denmark (`institutional`, `arbitrage` exit) acts as a sovereign beneficiary, shaping the rules.
 *   - Greenland (`organized`, `constrained` exit) acts as a junior partner whose consent is required but whose options are limited.
 *   The system correctly models this asymmetry, classifying the pact as a Rope for Denmark but a Tangled Rope for Greenland, even before considering the powerless local communities. This captures the nuance of neocolonial power dynamics within a formal alliance structure.
 *
 * MANDATROPHY ANALYSIS:
 *   A simplistic analysis might label this pact as either pure coordination ("protecting the Arctic") or pure extraction ("NATO imperialism"). The Tangled Rope classification avoids this by mandating the recognition of both functions simultaneously. It forces the question: Does the value of the coordination (regional security) justify the asymmetric extraction imposed on local populations and the Greenlandic polity? This is the core tension the Tangled Rope is designed to expose. The `Dynamic Coalition` extension is also relevant: if local communities organize effectively, their power could shift from `powerless` to `organized`, changing their classification from a Snare to a Tangled Rope, reflecting their increased ability to negotiate the terms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_greenland_defence_pact_2026,
    'Is Greenland''s "direct say" a genuine grant of co-sovereignty or a theatrical concession to legitimize a pre-determined NATO strategic objective?',
    'Observation of how disputes are resolved over the next decade. Specifically, if Greenland objects to a specific deployment or base expansion, is its objection respected or overruled?',
    'If genuine (Rope/Tangled Rope), it represents a novel form of devolved power. If theatrical (Snare), it is a modern form of colonial administration.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(greenland_defence_pact_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This pact represents the culmination of growing geopolitical pressure.
% The initial proposal likely had lower extraction, which increased as
% strategic imperatives hardened.
% Base extraction is > 0.46, so temporal data is required.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(gdp26_tr_t0, greenland_defence_pact_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(gdp26_tr_t5, greenland_defence_pact_2026, theater_ratio, 5, 0.18).
narrative_ontology:measurement(gdp26_tr_t10, greenland_defence_pact_2026, theater_ratio, 10, 0.15).

% Extraction over time (increases as strategic value is priced in):
narrative_ontology:measurement(gdp26_ex_t0, greenland_defence_pact_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gdp26_ex_t5, greenland_defence_pact_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gdp26_ex_t10, greenland_defence_pact_2026, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This pact is a mechanism for enforcing a security architecture.
narrative_ontology:coordination_type(greenland_defence_pact_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% This pact is a direct response to other geopolitical constraints.
narrative_ontology:affects_constraint(russian_arctic_militarization, greenland_defence_pact_2026).
narrative_ontology:affects_constraint(chinese_polar_silk_road, greenland_defence_pact_2026).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (beneficiary/victim + exit_options -> d) correctly captures the asymmetric
% relationships, particularly the inter-institutional gap between Denmark
% (arbitrage exit) and Greenland (constrained exit).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */