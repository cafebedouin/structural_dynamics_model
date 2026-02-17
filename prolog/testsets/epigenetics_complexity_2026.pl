% ============================================================================
% CONSTRAINT STORY: epigenetics_complexity_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_epigenetics_complexity_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epigenetics_complexity_2026
 *   human_readable: Epigenetic Regulatory Constraint
 *   domain: biological/scientific
 *
 * SUMMARY:
 *   Following the discovery that the human genome contains only ~20,000 protein-coding
 *   genes, epigenetics emerged as the primary mechanism explaining biological complexity.
 *   It describes how molecules interact with DNA/RNA to influence gene activity without
 *   altering the underlying code, often in response to environmental pressures. This
 *   constraint represents the biochemical reality of this regulatory layer.
 *
 * KEY AGENTS (by structural relationship):
 *   - Stressed Organisms: Primary target (powerless/trapped) — bears the physiological cost of adaptation.
 *   - Adapting Organisms: Primary beneficiary (powerless/mobile) — benefits from the adaptive flexibility.
 *   - Evolutionary Biologists: Secondary beneficiary (institutional/mobile) — uses the framework for explanation.
 *   - Biochemists: Analytical observer — sees the immutable physical laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Epigenetics is a fundamental biological function; it doesn't "extract"
% in a social sense, but forces physiological resource reallocation under stress.
% The cost is low but non-zero.
domain_priors:base_extractiveness(epigenetics_complexity_2026, 0.10).

% Rationale: Alternatives like "pure genetic determinism" were naturally suppressed
% by the empirical data of the Human Genome Project. The suppression is near-total
% for viable alternative theories at this level of biology.
domain_priors:suppression_score(epigenetics_complexity_2026, 0.05).

% Rationale: The mechanism is purely functional, with no performative aspect.
domain_priors:theater_ratio(epigenetics_complexity_2026, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epigenetics_complexity_2026, extractiveness, 0.10).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(epigenetics_complexity_2026, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(epigenetics_complexity_2026, mountain).
narrative_ontology:human_readable(epigenetics_complexity_2026, "Epigenetic Regulatory Constraint").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally through biochemical interactions.
domain_priors:emerges_naturally(epigenetics_complexity_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(epigenetics_complexity_2026, adapting_organisms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(epigenetics_complexity_2026, stressed_organisms).

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

% PERSPECTIVE 1: THE STRESSED ORGANISM (MOUNTAIN)
% For an organism in a toxic environment, epigenetics is a harsh reality. The
% environment forces a "silence or die" choice. The mechanism constrains the
% cell's behavior to a narrow, costly survival path. It's an unchangeable
% feature of its biological world.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EVOLUTIONARY BIOLOGIST (ROPE)
% For the scientist, epigenetics is a "Rope"—a functional coordination
% mechanism that allows them to explain the complexity of evolution
% beyond simple protein-coding genes. It's a tool for understanding.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BIOCHEMICAL ANALYST (MOUNTAIN)
% From a molecular perspective, the interaction of molecules with DNA/RNA
% is a "Mountain"—a fixed physical reality of how life operates.
% It is a zero-degree-of-freedom constraint of biological architecture.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epigenetics_complexity_2026_tests).

test(perspectival_gap_biologist_vs_organism) :-
    % Verify the key perspectival gap: the biologist sees a Rope (explanatory tool),
    % while the organism and analyst see a Mountain (immutable reality).
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metric_compliance) :-
    % Verify that the base metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(epigenetics_complexity_2026, extractiveness, E),
    narrative_ontology:constraint_metric(epigenetics_complexity_2026, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(epigenetics_complexity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.10) and suppression (0.05) are set low to reflect
 *   a fundamental, near-immutable biological law, consistent with a Mountain
 *   classification. The suppression score reflects the empirical falsification of
 *   competing theories (e.g., pure genetic determinism) by the Human Genome Project.
 *   The Natural Law profile metrics (accessibility_collapse=0.95, resistance=0.05)
 *   are added to ensure the constraint passes the NL certification chain.
 *
 * PERSPECTIVAL GAP:
 *   The core insight is the gap between the institutional and powerless/analytical
 *   perspectives. For biologists (institutional), epigenetics is a 'Rope'—a powerful
 *   explanatory framework that coordinates research and understanding. For the organism
 *   under stress (powerless) and the biochemist (analytical), it is a 'Mountain'—an
 *   unalterable feature of reality they must operate within.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'adapting_organisms' benefit from the flexibility epigenetics provides.
 *   - Victim: 'stressed_organisms' bear the physiological cost of forced adaptation.
 *   This distinction drives the directionality calculation, though with ε=0.10, the
 *   effective extraction χ remains low from all perspectives, preventing a Snare
 *   classification which would be inconsistent with the metrics (ε-invariance principle).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the dual nature of the constraint.
 *   Viewing it only as a Rope (the biologist's view) would miss the coercive,
 *   Mountain-like reality for the organism. Viewing it only as a Mountain would
 *   miss its function as a coordinating scientific paradigm. The low ε prevents
 *   mislabeling this fundamental mechanism as a high-extraction Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve

% /5 form: narrative detail for story context
omega_variable(
    omega_epigenetic_inheritance,
    "To what extent are epigenetic modifications 'reset' vs 'inherited' in humans across generations?",
    "Long-term multi-generational cohort studies tracking environmental stress and epigenetic markers.",
    "If fully reset: Epigenetics is a biographical Rope/Mountain. If inherited: It can become a generational Snare or Mountain.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_complexity_source,
    "Does epigenetics fully explain the gap between gene count and biological complexity, or are other major mechanisms yet to be discovered?",
    "Complete biochemical modeling of the cell-state space, and discovery of novel regulatory pathways.",
    "If incomplete: Epigenetics remains a partial Rope. If complete: It becomes a foundational Mountain of biology.",
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_epigenetic_inheritance, empirical, "Uncertainty over the generational inheritance of epigenetic markers in humans.").
narrative_ontology:omega_variable(omega_complexity_source, empirical, "Uncertainty whether epigenetics is the complete explanation for biological complexity.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(epigenetics_complexity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.10) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% groups and exit options is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */