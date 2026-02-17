% ============================================================================
% CONSTRAINT STORY: pancreatic_cancer_lethality_v1
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_pancreatic_cancer_lethality_v1, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pancreatic_cancer_lethality_v1
 *   human_readable: Biological Lethality of Pancreatic Adenocarcinoma
 *   domain: technological
 *
 * SUMMARY:
 *   Pancreatic cancer is one of the deadliest malignancies, with a 5-year
 *   survival rate below 10%. This constraint represents the fundamental
 *   biological reality of the disease's resistance to conventional therapies.
 *   Recent research, such as a triple-drug therapy showing success in mice,
 *   represents attempts to overcome this natural barrier, but the constraint
 *   itself remains a Mountain of biology.
 *
 * KEY AGENTS (by structural relationship):
 *   - Patients with Pancreatic Cancer: Primary target (powerless/trapped) — experiences the constraint as an absolute biological limit.
 *   - Medical Researchers / Oncologists: Analytical agents (analytical/analytical) — study the structure of the constraint to identify weaknesses.
 *   - Pharmaceutical Companies: Institutional agents (institutional/arbitrage) — view the constraint as a market opportunity for developing novel therapies.
 *   - Analytical Observer: Sees the full biological and statistical reality of the disease.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pancreatic_cancer_lethality_v1, 0.05).
domain_priors:suppression_score(pancreatic_cancer_lethality_v1, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(pancreatic_cancer_lethality_v1, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, extractiveness, 0.05).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values ensure the constraint passes the
% mountain metric gate.
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pancreatic_cancer_lethality_v1, mountain).
narrative_ontology:human_readable(pancreatic_cancer_lethality_v1, "Biological Lethality of Pancreatic Adenocarcinoma").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(pancreatic_cancer_lethality_v1).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(pancreatic_cancer_lethality_v1). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a product of biology, not human design. This flag
% is required for the mountain metric gate to fire.
domain_priors:emerges_naturally(pancreatic_cancer_lethality_v1).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain, this constraint has no designed beneficiaries or victims.
% The concept is incoherent for a natural law. Declarations are omitted.
%
% narrative_ontology:constraint_beneficiary(pancreatic_cancer_lethality_v1, ...).
% narrative_ontology:constraint_victim(pancreatic_cancer_lethality_v1, ...).

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

% UNIFORM-TYPE CONSTRAINT: This is a Mountain from all perspectives.
% The classification is invariant across all indices because it reflects a
% fundamental limit of nature. The perspectival minimum is relaxed.

% PERSPECTIVE 1: THE PATIENT
% Experiences the constraint as an absolute, inescapable biological fact.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PHARMACEUTICAL INSTITUTION
% Views the constraint as a fixed feature of the landscape, creating a
% market opportunity for a solution.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which sees the constraint as a
% well-established feature of biology.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pancreatic_cancer_lethality_v1_tests).

test(classification_invariance) :-
    % For a uniform-type constraint, verify invariance across perspectives.
    constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_met, [nondet]) :-
    % Verify the constraint meets the low-extraction and low-suppression
    % requirements for a Mountain classification.
    domain_priors:base_extractiveness(pancreatic_cancer_lethality_v1, E),
    domain_priors:suppression_score(pancreatic_cancer_lethality_v1, S),
    assertion(E =< 0.25),
    assertion(S =< 0.05).

test(natural_law_profile_present, [nondet]) :-
    % Verify the constraint has the necessary metrics and flags for the
    % natural law certification chain.
    domain_priors:emerges_naturally(pancreatic_cancer_lethality_v1),
    narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, resistance, R),
    config:param(natural_law_collapse_min, AC_Min),
    config:param(natural_law_resistance_max, R_Max),
    assertion(AC >= AC_Min),
    assertion(R =< R_Max).

:- end_tests(pancreatic_cancer_lethality_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is modeled as a Mountain because the lethality of
 *   pancreatic cancer is a product of its biological characteristics, not
 *   a system designed by humans. The base extractiveness (ε=0.05) and
 *   suppression score (S=0.02) are minimal, reflecting that it is a natural
 *   limit rather than a coercive social structure. The key metrics for this
 *   classification are the Natural Law profile: an extremely high
 *   `accessibility_collapse` (0.98), as escaping the disease's typical
 *   prognosis is nearly impossible, and a low `resistance` (0.10), as one
 *   cannot "oppose" the biology, only seek to circumvent it through science.
 *   The `emerges_naturally` flag is critical for the classification gate.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; this is a uniform-type constraint. A patient
 *   (powerless, trapped), a pharmaceutical company (institutional, arbitrage),
 *   and an analyst (analytical) all classify the constraint as a Mountain.
 *   While their emotional and economic responses differ, their structural
 *   assessment of the constraint as a fixed, natural barrier is identical.
 *   This invariance is the hallmark of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable here. The concepts of "beneficiary" and
 *   "victim" presuppose a designed system. Since this constraint emerges
 *   naturally from biology, it has no beneficiaries. Patients are victims of
 *   the disease's effects, but not of the constraint in a structural sense.
 *   Therefore, beneficiary/victim declarations are omitted.
 *
 * MANDATROPHY ANALYSIS:
 *   This Mountain classification is crucial to avoid misattributing a natural
 *   phenomenon to a human-designed Snare. For example, one could incorrectly
 *   model this as a Snare where "the healthcare system" is the beneficiary.
 *   That would be a different constraint (e.g., `cost_of_cancer_treatment_v1`).
 *   By isolating the biological reality as a Mountain, we can create separate,
 *   linked stories for the human systems built around it, ensuring analytical
 *   clarity and preventing the mislabeling of a natural tragedy as a purely
 *   extractive system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pancreatic_cancer_lethality_v1,
    'Is the extreme difficulty of treating pancreatic cancer a fundamental biological limit (true Mountain), or is it an artifact of historically contingent research paradigms that have missed a simpler underlying mechanism?',
    'Long-term (20+ year) success rates of radically different therapeutic approaches (e.g., metabolic, viral, immunological vs. cytotoxic).',
    'If a true Mountain, progress will be slow and incremental. If an artifact (a scientific Piton), a paradigm shift could cause a rapid collapse in lethality, similar to H. pylori and ulcers.',
    confidence_without_resolution(high) % Confidence that it IS a true Mountain is high.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pancreatic_cancer_lethality_v1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint because its base
% extractiveness (ε=0.05) is below the 0.46 threshold. The metrics of this
% Mountain are considered stable over the modeled interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint has no coordination function.
% narrative_ontology:coordination_type(pancreatic_cancer_lethality_v1, ...).

% Network relationships: This biological Mountain motivates the creation of
% human-designed constraints aimed at overcoming it.
narrative_ontology:affects_constraint(pancreatic_cancer_lethality_v1, cancer_research_funding_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary as this is a Mountain constraint where
% directionality is not a relevant concept.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */