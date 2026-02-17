% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-01
% ============================================================================

:- module(constraint_click_chemistry_paradigm_2026, []).

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
 *   constraint_id: click_chemistry_paradigm_2026
 *   human_readable: Click Chemistry Paradigm
 *   domain: scientific/technological
 *
 * SUMMARY:
 *   Click chemistry represents a shift from "messy, sluggish" traditional synthesis
 *   to a modular system of "spring-loaded" reactions that snap molecules together
 *   consistently and efficiently. It prioritizes carbon-heteroatom bonds over the
 *   previous carbon-carbon bond orthodoxy to build complex molecules like Lego bricks.
 *   This paradigm shift offers immense coordination benefits but also devalues
 *   the expertise of chemists trained in older, more complex methods.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Graduate Student (powerless/trapped): Primary target of pedagogical orthodoxy; the paradigm is presented as an immutable law of the lab.
 *   - The Drug Developer (institutional/arbitrage): Primary beneficiary; uses the paradigm for rapid, efficient discovery.
 *   - The Legacy Chemist (powerful/constrained): A victim of skill obsolescence; their expertise is devalued by the new, simpler standard.
 *   - Analytical Observer (analytical/analytical): Sees the full structure as a highly efficient coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.10).
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(click_chemistry_paradigm_2026, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.10).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Required because this constraint appears as a
% mountain from the powerless perspective.
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(click_chemistry_paradigm_2026, rope).
narrative_ontology:human_readable(click_chemistry_paradigm_2026, "Click Chemistry Paradigm").

% --- Emergence flag (required for mountain constraints) ---
% The paradigm emerges from competitive efficiency, not top-down enforcement.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(click_chemistry_paradigm_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, pharmaceutical_researchers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, traditional_chemists).

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

% PERSPECTIVE 1: THE GRADUATE STUDENT (MOUNTAIN)
% For a student, the paradigm is an unquestioned reality of lab work. It is
% taught as the default, correct method. Alternatives are not presented, making
% the constraint appear as an immutable law. The low suppression score (0.05)
% allows for a valid Mountain classification.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE DRUG DEVELOPER (ROPE)
% For the researcher, this is a pure coordination tool (Rope). It enables
% rapid, efficient synthesis, solving a collective action problem in drug
% discovery. As a beneficiary with arbitrage exit, their effective
% extraction (χ) is negative.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE LEGACY ORGANIC CHEMIST (ROPE)
% For a chemist whose expertise is in older, devalued methods, the paradigm
% feels extractive. However, with base extraction ε=0.10, the structure cannot
% be a Snare (which requires χ ≥ 0.66). It is a Rope that has shifted the
% coordination point, leaving them behind. Their negative experience is real,
% but the constraint's structure is one of coordination, not pure extraction.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% The default analytical context, which informs the constraint_claim.
% The observer sees a highly efficient coordination mechanism with low
% extraction and suppression, classifying it as a Rope.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(click_chemistry_paradigm_2026_tests).

test(perspectival_gap) :-
    % Verify the gap between the student (Mountain) and developer (Rope).
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    % Verify that base metrics are consistent with a Mountain/Rope profile.
    narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, E),
    narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(click_chemistry_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics were chosen to reflect a paradigm shift based on superior
 *   efficiency, not coercion. Base extractiveness (ε=0.10) is low because the
 *   primary effect is displacing an old method, not extracting value from a
 *   vulnerable population. Suppression (S=0.05) is very low because traditional
 *   methods are not banned, merely out-competed. This low suppression score is
 *   critical for the powerless perspective's Mountain classification to be valid.
 *   The theater ratio (TR=0.05) is minimal as this is a highly functional, results-
 *   oriented scientific practice.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the graduate student (Mountain) and all other
 *   actors (Rope). For the student, the paradigm is an immutable fact of their
 *   training environment—a natural law of the lab. For experienced chemists,
 *   whether beneficiaries or victims of the shift, it is clearly a human-
 *   constructed coordination tool (Rope). This Mountain/Rope gap is a classic
 *   signature of a successful, deeply embedded coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `pharmaceutical_researchers` gain speed and efficiency.
 *   - Victims: `traditional_chemists` suffer skill obsolescence and status loss.
 *   The original file incorrectly classified the victim's perspective as a Snare.
 *   This was corrected. While their experience is negative, the constraint's
 *   low base extraction (ε=0.10) makes a Snare classification structurally
 *   impossible. It is a Rope that has coordinated the field away from their
 *   area of expertise.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies this as a coordination mechanism (Rope)
 *   that appears as a natural law (Mountain) to newcomers. It avoids mislabeling
 *   the negative impact on legacy chemists as pure extraction (Snare), grounding
 *   the analysis in the constraint's low-extractive structure rather than solely
 *   on the subjective experience of one group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    heteroatom_scaffold_limit,
    "Does the reliance on carbon-heteroatom bonds limit the ultimate topological diversity of synthetic molecules compared to C-C bonds?",
    "Comparative analysis of click-derived libraries vs. natural products over a 50-year timeframe.",
    "If limited: Click chemistry is a partial Rope. If not: It becomes a universal Mountain of synthesis.",
    confidence_without_resolution(medium)
).

omega_variable(
    click_bio_toxicity,
    "Are all 'click' motifs, such as triazoles, truly inert in long-term human biological systems?",
    "Long-term, post-market surveillance of drugs built with click chemistry.",
    "If toxic: The drug discovery 'Rope' becomes a hidden biological 'Snare' for patients.",
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(heteroatom_scaffold_limit, empirical, "Does reliance on C-heteroatom bonds limit molecular diversity?").
narrative_ontology:omega_variable(click_bio_toxicity, empirical, "Are click motifs inert in long-term biological systems?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(click_chemistry_paradigm_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for this constraint as base_extractiveness (0.10) is below
% the 0.46 threshold for mandatory lifecycle drift tracking.
%
% narrative_ontology:measurement(click_chemistry_paradigm_2026_tr_t0, click_chemistry_paradigm_2026, theater_ratio, 0, 0.05).
% narrative_ontology:measurement(click_chemistry_paradigm_2026_tr_t5, click_chemistry_paradigm_2026, theater_ratio, 5, 0.05).
% narrative_ontology:measurement(click_chemistry_paradigm_2026_tr_t10, click_chemistry_paradigm_2026, theater_ratio, 10, 0.05).
%
% narrative_ontology:measurement(click_chemistry_paradigm_2026_ex_t0, click_chemistry_paradigm_2026, base_extractiveness, 0, 0.10).
% narrative_ontology:measurement(click_chemistry_paradigm_2026_ex_t5, click_chemistry_paradigm_2026, base_extractiveness, 5, 0.10).
% narrative_ontology:measurement(click_chemistry_paradigm_2026_ex_t10, click_chemistry_paradigm_2026, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Click chemistry acts as a standard for modular molecular assembly.
narrative_ontology:coordination_type(click_chemistry_paradigm_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The standard derivation from beneficiary/victim
% declarations and exit options accurately models the structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */