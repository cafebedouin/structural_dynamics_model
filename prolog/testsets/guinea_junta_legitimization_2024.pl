% ============================================================================
% CONSTRAINT STORY: guinea_junta_legitimization_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_guinea_junta_legitimization_2024, []).

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
 *   constraint_id: guinea_junta_legitimization_2024
 *   human_readable: Legitimization of Guinea's 2021 Military Coup
 *   domain: political
 *
 * SUMMARY:
 *   This constraint represents the political framework established by Guinea's
 *   military junta to formalize its rule following the 2021 coup. By confirming
 *   junta leader Mamady Doumbouya as president, the framework creates a facade
 *   of legitimacy that suppresses democratic alternatives and consolidates
 *   military control over the state, while facing condemnation from regional
 *   bodies like ECOWAS.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Guinean Civilian Populace & Opposition: Primary target (powerless/trapped) — bears the extraction of political rights and agency.
 *   - The Guinea Military Junta: Primary beneficiary (institutional/arbitrage) — benefits from consolidated power, legitimacy, and state control.
 *   - ECOWAS: Inter-institutional actor (institutional/constrained) — opposes the constraint, attempting to enforce regional democratic norms but with limited power.
 *   - Analytical Observer: Sees the full structure of coercive coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guinea_junta_legitimization_2024, 0.65).
domain_priors:suppression_score(guinea_junta_legitimization_2024, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(guinea_junta_legitimization_2024, 0.35).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, extractiveness, 0.65).
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, theater_ratio, 0.35).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(guinea_junta_legitimization_2024, tangled_rope).
narrative_ontology:human_readable(guinea_junta_legitimization_2024, "Legitimization of Guinea's 2021 Military Coup").
narrative_ontology:topic_domain(guinea_junta_legitimization_2024, "political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(guinea_junta_legitimization_2024). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(guinea_junta_legitimization_2024, guinea_military_junta).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, guinean_civilian_populace).
narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, ecowas). % Regional body whose norms are violated

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE GUINEAN POPULACE)
% Bears the full cost of lost political agency.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.65 * 1.42 * 1.0 (national scope) ≈ 0.92 -> Snare
% NOTE: A critical mass of victims could elevate power to 'organized', but
% without active, effective resistance, the classification remains Snare.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE JUNTA)
% Gains power and legitimacy.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ = 0.65 * -0.12 * 1.0 (national scope) ≈ -0.08 -> Rope
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination function (state control) and asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.65 * 1.15 * 1.2 (global scope) ≈ 0.90 -> Tangled Rope
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL ACTOR (ECOWAS)
% An external institution whose rules are being violated. As a 'victim' with
% 'constrained' exit, its derived directionality `d` is higher than a beneficiary
% but lower than a trapped victim.
% Engine derivation for organized victim with constrained exit -> d ≈ 0.6
% f(d) ≈ 0.85. χ = 0.65 * 0.85 * 0.9 (regional scope) ≈ 0.50 -> Tangled Rope.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_junta_legitimization_2024_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the populace and the junta.
    constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical view must resolve the conflict into Tangled Rope.
    constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(guinea_junta_legitimization_2024, _),
    narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, _),
    domain_priors:requires_active_enforcement(guinea_junta_legitimization_2024).

:- end_tests(guinea_junta_legitimization_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.65) is high, reflecting the near-total transfer of
 *   political power from civilians to the military. It is not higher because
 *   the state must still perform some basic functions. Suppression (0.90) is
 *   extremely high, as the constraint's existence depends on the military's
 *   monopoly on force and the preclusion of any democratic alternative. The
 *   theater ratio (0.35) is non-trivial, acknowledging the performative act
 *   of "confirming" the leader, but is low enough to show this is primarily
 *   about raw power, not just appearance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the junta (beneficiary), this framework is a Rope:
 *   it coordinates state power, provides stability (on their terms), and
 *   formalizes their authority. For the civilian population (target), it is a
 *   Snare: a coercive trap that extracts their political rights with no viable
 *   exit. The analytical and inter-institutional (ECOWAS) views resolve this
 *   dichotomy into a Tangled Rope, recognizing both the (coercive) coordination
 *   function and the deeply asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `guinea_military_junta` directly benefits through power
 *     consolidation. With `arbitrage` exit, they set the rules, giving them a
 *     low directionality (`d`) and a `rope` classification.
 *   - Victim: The `guinean_civilian_populace` bears the cost, losing political
 *     freedom. Their `trapped` status gives them a very high `d`, resulting in a
 *     `snare` classification.
 *   - ECOWAS is also a victim, as its regional order is defied. Its `constrained`
 *     exit options result in a moderate-to-high `d`, classifying the constraint
 *     as a `tangled_rope` from its perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   A simplistic analysis might label this a pure Snare. However, the Deferential
 *   Realism framework, by requiring an analytical perspective, correctly
 *   identifies the dual nature of the constraint. The junta's claim of providing
 *   order and stability is a genuine (if coercive) coordination function.
 *   Classifying it as a Tangled Rope correctly captures that this is not just
 *   chaos or pure predation, but an *ordered system of extraction*, which is a
 *   more precise and useful description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_guinea_junta_legitimization_2024,
    'Will this imposed political order lead to long-term stability (a grim Rope) or collapse into further conflict (a failed Snare)?',
    'Longitudinal data on civil unrest, economic performance, and factional splits within the military over a 5-10 year period.',
    'If stable, the constraint solidifies as a highly extractive Rope from the beneficiary view. If it collapses, its coordination function fails, proving it was only ever a temporary Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(guinea_junta_legitimization_2024, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint evolved from a raw coup (T=0) to a formalized political
% structure (T=10). Extraction solidified while theater increased to
% create a facade of legitimacy. Required since base_extractiveness (0.65) > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gjl24_tr_t0, guinea_junta_legitimization_2024, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gjl24_tr_t5, guinea_junta_legitimization_2024, theater_ratio, 5, 0.30).
narrative_ontology:measurement(gjl24_tr_t10, guinea_junta_legitimization_2024, theater_ratio, 10, 0.35).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gjl24_ex_t0, guinea_junta_legitimization_2024, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(gjl24_ex_t5, guinea_junta_legitimization_2024, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(gjl24_ex_t10, guinea_junta_legitimization_2024, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system coordinates state power via a monopoly on violence.
narrative_ontology:coordination_type(guinea_junta_legitimization_2024, enforcement_mechanism).

% Network relationships (structural influence edges)
% The junta's consolidation of power directly impacts regional stability norms.
narrative_ontology:affects_constraint(guinea_junta_legitimization_2024, ecowas_regional_stability).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics between the junta, the populace, and regional bodies.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */