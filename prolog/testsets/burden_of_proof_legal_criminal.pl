% ============================================================================
% CONSTRAINT STORY: burden_of_proof_legal_criminal
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_burden_of_proof_legal_criminal, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_legal_criminal
 *   human_readable: "Beyond a Reasonable Doubt" (Criminal Legal Burden)
 *   domain: political/social
 *
 * SUMMARY:
 *   In common law criminal systems, the burden of proof rests entirely on the
 *   prosecution to prove every element of a crime "beyond a reasonable doubt."
 *   This is a foundational constraint designed to protect individuals from the
 *   overwhelming power of the state, codifying the principle that it is better
 *   for ten guilty persons to escape than for one innocent to suffer. It functions
 *   as a pure coordination rule (Rope) that standardizes the state's use of
 *   coercive power.
 *
 * KEY AGENTS (by structural relationship):
 *   - Criminal Defendants: Primary beneficiary (powerless/trapped) — protected by the high standard.
 *   - State Prosecutors: Primary target (institutional/mobile) — constrained by the high standard, which limits their ability to secure convictions.
 *   - Civil Society: Secondary beneficiary (organized/mobile) — benefits from the limitation on state power.
 *   - Analytical Observer: Sees the full structure as a coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_legal_criminal, 0.10).
domain_priors:suppression_score(burden_of_proof_legal_criminal, 0.20).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(burden_of_proof_legal_criminal, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, extractiveness, 0.10).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% This constraint is not a mountain; it is a human-designed legal construct.
% The suppression score of 0.20 also exceeds the mountain ceiling of 0.05.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(burden_of_proof_legal_criminal, rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(burden_of_proof_legal_criminal). % Judges must instruct juries, appellate courts review.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(burden_of_proof_legal_criminal, criminal_defendants).
narrative_ontology:constraint_beneficiary(burden_of_proof_legal_criminal, civil_society).

% Who bears disproportionate cost?
% The "cost" is reduced efficiency in prosecution.
narrative_ontology:constraint_victim(burden_of_proof_legal_criminal, state_prosecutors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE DEFENDANT (ROPE)
% Agent who is protected by the constraint. While it feels like an
% unchangeable Mountain, it is structurally a highly stable Rope.
% The suppression score (0.20) is too high for a Mountain (max 0.05).
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PROSECUTOR (ROPE)
% Agent who must abide by the constraint. It is a coordination rule
% defining the requirements for a successful prosecution.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% Default analytical context. Sees the constraint as a pure coordination
% mechanism designed to balance state power and individual liberty.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_legal_criminal_tests).

test(uniform_rope_classification) :-
    % Verify that from all key perspectives, this is a Rope.
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, rope, context(agent_power(analytical), _, _, _)).

test(low_extraction_and_suppression) :-
    % The constraint's function is to prevent extraction, not enable it.
    narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, extractiveness, E),
    narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, suppression_requirement, S),
    E < 0.25,
    S < 0.25.

:- end_tests(burden_of_proof_legal_criminal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.10) is low because the constraint's primary
 *   function is protective, preventing the state from wrongfully extracting
 *   liberty. The suppression score (S=0.20) reflects that while the standard
 *   is high, the legal system itself has coercive elements, but the burden
 *   itself does not suppress alternatives; it mandates a high bar for one
 *   specific action (conviction). The theater ratio (0.05) is low as this is
 *   a core, functional legal doctrine, not a performative one.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification; all agents perceive a Rope.
 *   However, the *experience* of the Rope differs dramatically. For the defendant,
 *   it is a lifeline that feels as fixed and protective as a Mountain. For the
 *   prosecutor, it is a high bar that must be cleared. The analytical observer
 *   sees it as the intended coordination mechanism of a liberal society.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `criminal_defendants` are the direct beneficiaries, as the
 *     constraint protects them from the state's power. `civil_society` is a
 *     broader beneficiary, as the rule reinforces the principle of limited government.
 *   - Victims: `state_prosecutors` are the "victims" in a structural sense. The
 *     constraint extracts prosecutorial efficiency and makes their job harder,
 *     which is the intended design.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear example of a functional Rope. It prevents the
 *   mislabeling of state prosecution as a simple coordination task by imposing
 *   a high friction cost. An attempt to lower this burden (e.g., to a
 *   "preponderance of the evidence" standard) would increase the constraint's
 *   extractiveness, shifting it towards a Tangled Rope or Snare by making it
 *   easier for the state to extract liberty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_burden_of_proof_legal_criminal,
    'Is "Beyond a Reasonable Doubt" an objective standard or an irreducible psychological state of the juror?',
    'Comparative neuro-legal studies on juror brain states during verdict deliberation.',
    'If objective, it is a pure information_standard Rope. If subjective, it has elements of irreducible complexity, making its application less predictable.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_burden_of_proof_legal_criminal, conceptual, 'The objectivity vs. subjectivity of the "reasonable doubt" standard.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(burden_of_proof_legal_criminal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint as its base extractiveness
% (0.10) is below the 0.46 threshold for mandatory lifecycle drift monitoring.
% The constraint has been historically stable.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(burden_of_proof_legal_criminal, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural derivation from beneficiary/victim
% declarations accurately captures the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */