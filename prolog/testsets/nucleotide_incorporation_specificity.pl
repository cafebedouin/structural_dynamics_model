% ============================================================================
% CONSTRAINT STORY: nucleotide_incorporation_specificity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nucleotide_incorporation_specificity, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nucleotide_incorporation_specificity
 *   human_readable: Nucleotide Incorporation Specificity in DNA Polymerases
 *   domain: biochemistry/molecular_biology
 *
 * SUMMARY:
 *   Nucleotide incorporation specificity is the physical constraint that
 *   governs how accurately DNA polymerases select the correct nucleotide
 *   during DNA replication. The polymerase active site must discriminate
 *   between nucleotides that differ only in their heterocyclic base
 *   composition—a recognition problem where Watson-Crick geometry provides
 *   the primary selectivity signal. The error rate is determined by the free
 *   energy difference between correct and incorrect nucleotides in the active
 *   site. This constraint is invariant across all organisms and all
 *   polymerase designs: no cell, no evolutionary path, and no engineering
 *   project can escape the quantum mechanical reality that similar geometries
 *   cannot be perfectly distinguished. The constraint is a natural law of
 *   biochemistry, not an institution or policy that could be reformed.
 *
 * KEY AGENTS:
 *   - Mispairing Nucleotide: Target of constraint (powerless/trapped) — subject to absolute physical rejection if geometry does not match
 *   - DNA Polymerase: Executor of constraint (powerful/mobile) — implements specificity through active site architecture but cannot exceed the physical limit
 *   - Genetic System: Beneficiary and subject (institutional/arbitrage) — depends on fidelity but also subject to the same constraint
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the quantum mechanical foundation of the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nucleotide_incorporation_specificity, 0.12).
domain_priors:suppression_score(nucleotide_incorporation_specificity, 0.03).
domain_priors:theater_ratio(nucleotide_incorporation_specificity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, extractiveness, 0.12).
narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nucleotide_incorporation_specificity, mountain).
narrative_ontology:human_readable(nucleotide_incorporation_specificity, "Nucleotide Incorporation Specificity in DNA Polymerases").
narrative_ontology:topic_domain(nucleotide_incorporation_specificity, "biochemistry/molecular_biology").

domain_priors:emerges_naturally(nucleotide_incorporation_specificity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MISPAIRING TARGET (MOUNTAIN) — An incorrect nucleotide at a specific replication fork position has no escape from the physical chemistry of base pairing geometry. The constraint is absolute: Watson-Crick geometry determines whether a nucleotide can fit into the polymerase active site. No exit, no negotiation, no institutional workaround. The mispairing rate is fixed by quantum mechanical tunneling probability and electrostatic geometry.
constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE POLYMERASE ENZYME (MOUNTAIN) — Even with all the power of evolutionary optimization and protein engineering, the polymerase cannot exceed the physical limit of discrimination set by geometry and free energy differences. The constraint is invariant across all polymerases: the error rate follows from thermodynamics, not choice. Faster replication means lower specificity; higher specificity means slower synthesis. This tradeoff is structural.
constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE GENETIC SYSTEM (MOUNTAIN) — Life's replication machinery has no option to reject the fundamental constraint. Cells can evolve proofreading mechanisms, mismatch repair systems, and polymerase selectivity, but these address the constraint's consequences, not the constraint itself. The underlying discrimination limit—that two nucleotides with similar Watson-Crick geometry cannot be perfectly distinguished—remains invariant. Every organism experiences this same physical law.
constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a structure-function perspective, nucleotide incorporation specificity emerges from quantum mechanics of hydrogen bonding and steric constraints in the polymerase active site. The constraint has zero degrees of freedom: Pauling exclusion, van der Waals radii, and electrostatic interactions determine which nucleotides can occupy the active site at measurable rates. No measurement-dependent ambiguity exists—the specificity limit is invariant across all observables.
constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nucleotide_incorporation_specificity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nucleotide_incorporation_specificity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, ExtMetricName, E),
    domain_priors:suppression_score(nucleotide_incorporation_specificity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nucleotide_incorporation_specificity),
    narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nucleotide_incorporation_specificity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nucleotide_incorporation_specificity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. This is a natural law, not an extractive mechanism. The 'extraction' measured here is the inherent entropic and energetic cost of achieving discrimination—not a policy cost but a physical one. The polymerase must invest catalytic energy to achieve discrimination, and this energy cost is the floor of any replication system. Suppression (0.03): Minimal. There is no coercion, no alternative pathway, no negotiation. The constraint is purely physical. Accessibility collapse (0.91): High. The constraint is completely inaccessible to modification—it follows from quantum mechanics. Resistance (0.08): Very low. No organism or system resists this constraint; all systems accept it as inherent. Theater ratio (0.15): Very low. The constraint has no performative component. The polymerase either achieves discrimination or does not; there is no ritual, no theatrical compliance, only measured biochemical reality.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the mountain classification. This constraint exhibits zero perspectival gap because the physical reality is identical for all observers: quantum mechanics is observer-independent. The polymerase cannot claim it experiences the constraint as coordination (Rope) because it is genuinely constrained by physics. A cell cannot claim it experiences this as extractive (Snare) because the constraint benefits accurate replication. The analytical observer cannot naturalize a contingent institution because the constraint IS a natural law. The uniformity across perspectives confirms that this is a true mountain, not a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint in the classical sense. A mountain has no beneficiary or victim—it is invariant across all positions. The polymerase is not 'trapped' by something external; rather, the polymerase's function IS to implement this constraint. The mispairing nucleotide does not face extraction; it faces physical impossibility. All agents (polymerase, genome, cell, organism) are equally subject to the same limit. The d-value is meaningless here because there is no asymmetric extraction to compute. The constraint is universally binding in the same way that gravity is universally binding.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_tunneling_probability_variability,
    'Does the quantum tunneling rate for nucleotide incorporation vary with cellular conditions (temperature, Mg2+ concentration, pH) in ways that change the effective error rate classification?',
    'In vitro kinetic studies across physiological parameter ranges; measurement of catalytic efficiency (kcat/Km) for correct vs incorrect nucleotides as a function of temperature, ionic strength, and pH',
    'If variability is > ±10% of baseline: the constraint may have limited degrees of freedom within physiological range, weakening the mountain classification. If < ±5%: mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_tunneling_probability_variability, empirical, 'Whether quantum tunneling rate varies significantly with cellular conditions').

omega_variable(
    engineered_polymerase_upper_bound,
    'What is the theoretical maximum specificity achievable through protein engineering, and does it approach or exceed the thermodynamic limit?',
    'High-throughput directed evolution experiments; deep mutational scanning of polymerase active site residues; comparison of engineered specificity against theoretical thermodynamic predictions from molecular dynamics simulations',
    'If maximum engineered specificity stays 10-20-fold below thermodynamic limit: mountain classification confirmed (fundamental physical constraint). If achievable within 2-3-fold: constraint may have more engineerable freedom than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineered_polymerase_upper_bound, empirical, 'Maximum achievable polymerase specificity through engineering').

omega_variable(
    alternative_geometry_possibility,
    'Could alternative base pairing geometries (isobases, xeno-nucleic acids, non-Watson-Crick pairs) be incorporated with sufficient selectivity to create a ''richer'' nucleotide alphabet without massive specificity loss?',
    'Characterization of non-standard base pair incorporation rates in modified polymerases; kinetic analysis of polymerase selectivity for expanded nucleotide sets; comparison of error rates across standard and modified alphabets',
    'If alternative geometries can be incorporated with error rates < 10^-4: the mountain classification reflects only standard DNA, not a fundamental discrimination limit. If error rates stay > 10^-2: the mountain constraint generalizes to any nucleotide set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_geometry_possibility, empirical, 'Whether alternative base pair geometries can achieve high specificity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nucleotide_incorporation_specificity, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuc_spec_tr_t0, nucleotide_incorporation_specificity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nuc_spec_tr_t2, nucleotide_incorporation_specificity, theater_ratio, 2, 0.15).
narrative_ontology:measurement(nuc_spec_tr_t4, nucleotide_incorporation_specificity, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(nuc_spec_be_t0, nucleotide_incorporation_specificity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(nuc_spec_be_t2, nucleotide_incorporation_specificity, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(nuc_spec_be_t4, nucleotide_incorporation_specificity, base_extractiveness, 4, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nucleotide_incorporation_specificity, information_standard).
narrative_ontology:affects_constraint(nucleotide_incorporation_specificity, genome_replication_fidelity).
narrative_ontology:affects_constraint(nucleotide_incorporation_specificity, mutation_accumulation_rate).
narrative_ontology:affects_constraint(nucleotide_incorporation_specificity, proofreading_tradeoff).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
