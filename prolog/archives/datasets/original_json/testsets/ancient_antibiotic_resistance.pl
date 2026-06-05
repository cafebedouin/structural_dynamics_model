% ============================================================================
% CONSTRAINT STORY: ancient_antibiotic_resistance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ancient_antibiotic_resistance, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ancient_antibiotic_resistance
 *   human_readable: The Inherent Evolutionary Potential for Antibiotic Resistance
 *   domain: biological/technological
 *
 * SUMMARY:
 *   The discovery of antibiotic resistance mechanisms in ancient bacteria,
 *   isolated from 5,000-year-old ice, reveals that resistance is not a modern
 *   phenomenon created by human antibiotic use. Instead, it is a deeply
 *   embedded, natural feature of microbial evolution. This constrains human
 *   efforts to combat bacterial infections, framing it not as a winnable war
 *   against a static enemy, but as a permanent negotiation with a dynamic,
 *   adaptive evolutionary system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Patients with resistant infections: Primary target (powerless/trapped) — experience the constraint as a fatal, unchangeable natural limit.
 *   - Microbiome (bacteria): Primary beneficiary — the constraint is their evolved survival mechanism.
 *   - Medical & Pharmaceutical Researchers: Secondary beneficiary (institutional/arbitrage) — gain knowledge and develop strategies by understanding the constraint's structure.
 *   - Evolutionary Biologists: Analytical observer — see the full structure as a textbook case of natural selection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_antibiotic_resistance, 0.05). % The intrinsic biological cost imposed on humans.
domain_priors:suppression_score(ancient_antibiotic_resistance, 0.0).   % Structural property (raw, unscaled). No coercion, just a lack of alternatives to evolution.
domain_priors:theater_ratio(ancient_antibiotic_resistance, 0.05).      % Piton detection (>= 0.70). No performative aspect.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, extractiveness, 0.05).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values certify this constraint as a natural law.
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, 0.95). % We cannot access a world where bacteria do not evolve.
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, 0.05).           % Opposition is biologically incoherent.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ancient_antibiotic_resistance, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a direct product of natural selection over geological timescales.
domain_priors:emerges_naturally(ancient_antibiotic_resistance).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint (natural law), beneficiary/victim declarations
% are not required. The constraint's effects are universal and not the
% result of an asymmetric social structure. The "beneficiary" is nature itself.

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

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because its base extractiveness (ε) and
% suppression are extremely low, meeting the Mountain criteria regardless of
% the index. The χ value remains negligible for all observers.

% PERSPECTIVE 1: THE PATIENT WITH A RESISTANT INFECTION
% For this person, the constraint is an absolute, life-threatening physical limit.
% It is an unchangeable fact of their biological reality.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PHARMACEUTICAL RESEARCHER
% For researchers, this is a fundamental law of nature they must understand and
% design around. They cannot change it, only adapt to it. It's a mountain
% to be mapped and navigated, not a rope to be pulled.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EVOLUTIONARY BIOLOGIST (ANALYTICAL OBSERVER)
% The observer sees the constraint as a textbook example of natural selection,
% a fixed and universal process in biology.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_antibiotic_resistance_tests).

test(uniform_type_invariance, [nondet]) :-
    % Verify this is a uniform-type constraint (Mountain) from all key perspectives.
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_validation) :-
    % Verify the constraint's metrics adhere to the Mountain classification thresholds.
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, extractiveness, E),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify it meets the stricter NL profile metrics for Mountain certification.
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, R),
    domain_priors:emerges_naturally(ancient_antibiotic_resistance),
    config:param(natural_law_collapse_min, MinAC),
    config:param(natural_law_resistance_max, MaxR),
    AC >= MinAC,
    R =< MaxR.

:- end_tests(ancient_antibiotic_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents a
 *   fundamental, non-human-authored aspect of the natural world: the
 *   evolutionary capacity of bacteria. The metrics reflect this:
 *   - Base Extractiveness (ε=0.05): Low, representing the intrinsic "cost"
 *     nature imposes on human health, not an extraction by a human agent.
 *   - Suppression (0.0): There is no coercion; alternatives are simply
 *     biologically non-existent.
 *   - The Natural Law (NL) profile metrics are key: high accessibility_collapse
 *     (0.95) and low resistance (0.05), combined with the `emerges_naturally`
 *     flag, certify this as a natural law within the system.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; this is a uniform-type constraint. All
 *   agents, from the powerless patient to the institutional researcher,
 *   perceive the constraint as a Mountain. While their *experience* of it
 *   differs dramatically (a fatal limit vs. a research problem), its
 *   fundamental classification as an unchangeable, external reality is
 *   the same for all. This invariance is a hallmark of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not a relevant concept for a Mountain constraint. Since
 *   base extraction (ε) is near zero, the effective extraction (χ) is also
 *   near zero for all observers, regardless of their directionality (d). The
 *   `constraint_beneficiary` is technically the entire domain of bacteria, an
 *   actor outside the human social system being modeled.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a natural limit, preventing the
 *   error of treating a biological reality as a solvable policy problem (Snare
 *   or Tangled Rope). It clarifies that human policies (e.g., antibiotic
 *   stewardship) are Ropes or Scaffolds built *in response to* this Mountain;
 *   they do not change the Mountain itself. This distinction is critical for
 *   effective policy, focusing efforts on adaptation and mitigation rather
 *   than a futile "war" on evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ancient_antibiotic_resistance,
    'Is the observed evolutionary potential for resistance an absolute biological limit, or could a higher-order mechanism (e.g., phage therapy, CRISPR-based gene drive) effectively nullify it, turning the Mountain into a solvable problem (Tangled Rope)?',
    'Long-term empirical results from advanced synthetic biology and virology research programmes over several decades.',
    'If absolute, humanity is locked into a permanent adaptive race (mitigation). If nullifiable, it becomes a technological and economic problem of scaling the solution (elimination).',
    confidence_without_resolution(high) % High confidence that it's a fundamental limit for now.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ancient_antibiotic_resistance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.05) is below the 0.46 threshold.
% The "lifecycle" of this constraint is on an evolutionary, not human, timescale.
% Placeholder values are included to demonstrate the format.
narrative_ontology:measurement(aar_tr_t0, ancient_antibiotic_resistance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aar_tr_t5, ancient_antibiotic_resistance, theater_ratio, 5, 0.05).
narrative_ontology:measurement(aar_tr_t10, ancient_antibiotic_resistance, theater_ratio, 10, 0.05).

narrative_ontology:measurement(aar_ex_t0, ancient_antibiotic_resistance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aar_ex_t5, ancient_antibiotic_resistance, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(aar_ex_t10, ancient_antibiotic_resistance, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type is not applicable for a Mountain constraint, as it has
% no coordination function.

% Network relationships: This natural law underpins and affects many human-made
% constraints related to public health and medicine.
narrative_ontology:affects_constraint(ancient_antibiotic_resistance, superbug_containment_policy).
narrative_ontology:affects_constraint(ancient_antibiotic_resistance, pharmaceutical_rd_incentives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, directionality is not a significant
% factor in its classification, and there are no complex institutional
% dynamics that the standard derivation would misinterpret.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */