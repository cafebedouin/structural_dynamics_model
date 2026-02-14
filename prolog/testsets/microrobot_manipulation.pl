% ============================================================================
% CONSTRAINT STORY: microrobot_manipulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_microrobot_manipulation, []).

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
 *   constraint_id: microrobot_manipulation
 *   human_readable: Micro-scale Programmable Robotic Manipulation
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the capability provided by microscopic, programmable
 *   robots powered by light. These robots overcome the physical limitations
 *   (a mountain constraint) of manipulating matter at the cellular level.
 *   The constraint is not the robot itself, but the system of control and
 *   capability it enables, which functions as a pure coordination mechanism.
 *
 * KEY AGENTS (by structural relationship):
 *   - microscopic_assemblies_and_organisms: Primary target (powerless/trapped) — the matter being manipulated.
 *   - research_and_development_labs: Primary beneficiary (institutional/arbitrage) — creators and users of the technology.
 *   - downstream_scientific_and_medical_fields: Secondary beneficiary (organized/mobile) — fields that will apply the technology.
 *   - analytical_observer: Analytical observer — sees the full structure of the capability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microrobot_manipulation, 0.05).
domain_priors:suppression_score(microrobot_manipulation, 0.10).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(microrobot_manipulation, 0.08).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microrobot_manipulation, extractiveness, 0.05).
narrative_ontology:constraint_metric(microrobot_manipulation, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(microrobot_manipulation, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(microrobot_manipulation, rope).

% --- Binary flags ---
% The system requires active control (e.g., via light patterns) to function.
% This declaration prevents misclassification as a Scaffold, which requires
% an absence of active enforcement.
domain_priors:requires_active_enforcement(microrobot_manipulation).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(microrobot_manipulation, research_and_development_labs).
narrative_ontology:constraint_beneficiary(microrobot_manipulation, downstream_scientific_and_medical_fields).
%
% Who bears disproportionate cost? (Structurally, the object of manipulation)
narrative_ontology:constraint_victim(microrobot_manipulation, microscopic_assemblies_and_organisms).

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

% This is a UNIFORM-TYPE constraint. The classification is Rope from all
% perspectives because the base extractiveness (ε) is extremely low. Even
% for the powerless/trapped target, effective extraction (χ) remains
% well below the Rope threshold (χ ≤ 0.35).

% PERSPECTIVE 1: THE PRIMARY TARGET (the matter being manipulated)
% Structurally powerless and trapped, but because ε is near zero, the
% classification remains Rope.
% χ ≈ ε × f(d) × σ(S) = 0.05 × f(0.95) × σ(local) = 0.05 × 1.42 × 0.8 = 0.0568
constraint_indexing:constraint_classification(microrobot_manipulation, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (the R&D lab)
% The institutional developer sees it as a pure coordination tool with
% negative effective extraction (i.e., a subsidy).
% χ ≈ ε × f(d) × σ(S) = 0.05 × f(0.05) × σ(global) = 0.05 × -0.12 × 1.2 = -0.0072
constraint_indexing:constraint_classification(microrobot_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees a tool for coordination, not extraction.
% χ ≈ ε × f(d) × σ(S) = 0.05 × f(0.72) × σ(global) = 0.05 × 1.15 × 1.2 = 0.069
constraint_indexing:constraint_classification(microrobot_manipulation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM BENEFICIARY (e.g., medical industry)
% An organized user of the technology perceives it as a coordination tool.
% χ ≈ ε × f(d) × σ(S) = 0.05 × f(0.55) × σ(national) = 0.05 × 0.75 × 1.0 = 0.0375
constraint_indexing:constraint_classification(microrobot_manipulation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microrobot_manipulation_tests).

test(uniformity_of_classification, [nondet]) :-
    % For a uniform-type constraint like a pure Rope, we verify that the
    % classification is stable across different perspectives.
    constraint_indexing:constraint_classification(microrobot_manipulation, TypeTarget,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(microrobot_manipulation, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == rope),
    assertion(TypeBeneficiary == rope),
    assertion(TypeTarget == TypeBeneficiary).

test(low_extraction_and_suppression) :-
    domain_priors:base_extractiveness(microrobot_manipulation, E),
    domain_priors:suppression_score(microrobot_manipulation, S),
    assertion(E =< 0.45), % Rope threshold for base extraction
    assertion(S =< 0.40). % Typical for ropes

:- end_tests(microrobot_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε) is set to 0.05 because the technology's primary
 *   function is to enable new actions (coordination) at the micro-scale,
 *   not to extract value from one group for another. It creates capability
 *   rather than redistributing resources. Suppression is low (0.10) because
 *   it introduces a novel capability, expanding the set of possible actions
 *   rather than restricting them. Theater is minimal (0.08) as it is a
 *   demonstrably functional scientific instrument.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. This story is a canonical example
 *   of a uniform-type constraint. Because the base extractiveness (ε) is so
 *   low, the classification remains 'rope' even from the perspective of the
 *   'powerless' and 'trapped' target (the microscopic matter being manipulated).
 *   The math demonstrates this robustness: the high directionality multiplier `f(d)`
 *   for a trapped victim is insufficient to raise the effective extraction `χ`
 *   out of the Rope category.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are clearly the scientists, labs, and downstream industries
 *   that can now perform tasks previously impossible. The victim is defined
 *   structurally as the object of manipulation. By modeling the inanimate target
 *   as a 'victim', we test the system's ability to distinguish between structural
 *   targeting and actual social extraction. The result confirms the system's
 *   integrity: targeting with near-zero extraction is coordination, not oppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a powerful technological capability
 *   as a Rope (coordination tool). It avoids the error of mislabeling all forms
 *   of power and control as a Snare (extraction). The ε-invariance principle is
 *   key: this story models the *capability*. A future, specific application of
 *   this technology for extractive purposes (e.g., a microscopic surveillance
 *   system) would have a different ε and would be modeled as a separate, likely
 *   Snare, constraint. The addition of `requires_active_enforcement/1` ensures
 *   the engine does not misclassify this permanent capability as a temporary
 *   Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_microrobot_manipulation,
    'Will the primary application of microscopic robotics shift from scientific coordination to extractive surveillance or military functions over a civilizational timescale?',
    'Tracking patent filings, DARPA/IARPA funding allocations, and published dual-use research over the next 50 years.',
    'If True, this Rope becomes the technical foundation for a new family of Snare constraints. If False, it remains a pure enabler of scientific progress.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(microrobot_manipulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this low-extraction constraint is not mandatory but is
% included to model its stability as a pure coordination tool since its inception.
% The flat trajectory indicates no drift towards extraction or theater.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(microrobot_manipulation_tr_t0, microrobot_manipulation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(microrobot_manipulation_tr_t5, microrobot_manipulation, theater_ratio, 5, 0.08).
narrative_ontology:measurement(microrobot_manipulation_tr_t10, microrobot_manipulation, theater_ratio, 10, 0.08).

% Extraction over time (stable and low):
narrative_ontology:measurement(microrobot_manipulation_ex_t0, microrobot_manipulation, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(microrobot_manipulation_ex_t5, microrobot_manipulation, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(microrobot_manipulation_ex_t10, microrobot_manipulation, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: As a foundational capability for a new scale of engineering,
% it functions like infrastructure.
narrative_ontology:coordination_type(microrobot_manipulation, global_infrastructure).

% Network relationships: This technology directly affects (and may overcome)
% existing biophysical constraints, which are often Mountains.
narrative_ontology:affects_constraint(microrobot_manipulation, drug_delivery_blood_brain_barrier).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain (beneficiary/victim + exit options -> d) accurately models the
% structural relationships and produces the correct classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */