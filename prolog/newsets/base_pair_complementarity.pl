% ============================================================================
% CONSTRAINT STORY: base_pair_complementarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_base_pair_complementarity, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: base_pair_complementarity
 *   human_readable: Specific Base-Pair Pairing in DNA
 *   domain: biological/chemical
 *
 * SUMMARY:
 *   The structure of the DNA double helix is stabilized by specific hydrogen
 *   bonds between purine and pyrimidine bases: Adenine (A) pairs with Thymine
 *   (T), and Guanine (G) pairs with Cytosine (C). This complementarity is a
 *   direct consequence of molecular geometry and chemical potential, not a
 *   socially constructed rule. It is a foundational constraint upon which all
 *   terrestrial DNA-based life is built, enabling the high-fidelity storage
 *   and replication of genetic information.
 *
 * KEY AGENTS:
 *   - Replicating Cell: Primary subject (powerless/trapped) — its machinery must obey the chemical law.
 *   - Evolutionary Processes: Organized force (organized/constrained) — operates within the fixed boundaries set by this chemical constraint.
 *   - Molecular Biologists: Analytical observers (analytical/analytical) — seek to understand and describe this natural law.
 *   - Pharmaceutical Companies: Institutional actors (institutional/arbitrage) — leverage the predictability of this law to design drugs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(base_pair_complementarity, 0.01).
domain_priors:suppression_score(base_pair_complementarity, 0.02).
domain_priors:theater_ratio(base_pair_complementarity, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(base_pair_complementarity, extractiveness, 0.01).
narrative_ontology:constraint_metric(base_pair_complementarity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(base_pair_complementarity, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(base_pair_complementarity, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(base_pair_complementarity, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(base_pair_complementarity, mountain).
narrative_ontology:human_readable(base_pair_complementarity, "Specific Base-Pair Pairing in DNA").
narrative_ontology:topic_domain(base_pair_complementarity, "biological/chemical").

domain_priors:emerges_naturally(base_pair_complementarity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The rule is a fundamental, verifiable, and unchangeable law of chemistry. It cannot be exited, only understood and utilized. Its discovery collapsed vast biological complexity into a simple, elegant principle.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE REPLICATING CELL (MOUNTAIN) — The cell's machinery (e.g., DNA polymerase) is completely trapped by this constraint. It is not a choice but a physical reality that enables high-fidelity information transfer. There is no alternative pathway.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PHARMACEUTICAL DESIGNER (MOUNTAIN) — From an institutional perspective seeking to design interventions (e.g., base analog drugs), the pairing rule is a fixed landscape feature. It is not a system of extraction to be fought, but a physical law to be exploited (arbitraged) for therapeutic design.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PROCESS OF EVOLUTION (MOUNTAIN) — Evolution is a powerful, organized search algorithm, but it is fundamentally constrained by the laws of chemistry. While it can explore alternative information carriers (like RNA), for DNA-based life, this pairing rule is an immutable boundary condition.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(base_pair_complementarity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(base_pair_complementarity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(base_pair_complementarity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(base_pair_complementarity, ExtMetricName, E),
    domain_priors:suppression_score(base_pair_complementarity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(base_pair_complementarity),
    narrative_ontology:constraint_metric(base_pair_complementarity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(base_pair_complementarity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(base_pair_complementarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Extractiveness (ε=0.01) is effectively zero; the rule does not asymmetrically benefit one agent at the expense of another but provides a stable substrate for all. Suppression (0.02) is extremely low in the DR sense of coercive enforcement, though it is absolute in the physical sense; alternatives are precluded by chemistry, not policy. The NL-profile metrics confirm this: it emerges naturally from physics (emerges_naturally: true), its discovery collapsed complexity into a simple rule (accessibility_collapse: 0.98), and it is impossible to violate within a stable DNA helix (resistance: 0.01).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The convergence of all perspectives—from the powerless cell to the analytical scientist to the institutional drug designer—on the 'mountain' classification is the primary diagnostic signature of a true natural law. The constraint's properties are invariant regardless of the observer's power, timescale, or exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable here. As a symmetric, foundational law of nature, there are no designated beneficiaries or victims. The constraint precedes the existence of the agents it governs and provides the conditions for their existence. The system derives d based on canonical fallbacks, but with ε near zero, the effective extraction χ remains negligible for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint serves as a baseline for calibrating the system against false naturalization. Any attempt to classify this as a Snare or Tangled Rope would fail immediately due to the near-zero extractiveness and suppression scores. It demonstrates that the framework correctly distinguishes between unchangeable physical realities (Mountains) and contingent, coercive systems (Snares), preventing the mislabeling of natural laws as systems of social extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(base_pair_complementarity, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(base_pair_complementarity, dna_double_helix_structure).
narrative_ontology:affects_constraint(base_pair_complementarity, genetic_code_redundancy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
