% ============================================================================
% CONSTRAINT STORY: genetic_information_storage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_information_storage, []).

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
 *   constraint_id: genetic_information_storage
 *   human_readable: Genetic Information Storage Capacity
 *   domain: biology/information_theory
 *
 * SUMMARY:
 *   Genetic information storage capacity represents a fundamental natural law
 *   arising from the intersection of chemistry, thermodynamics, and
 *   information theory. All known biological systems — from single-cell
 *   bacteria to complex multicellular organisms — operate under the
 *   constraint that genetic material can encode approximately 2 bits of
 *   information per nucleotide (in ideal conditions) or ~1.8-1.9 bits per
 *   nucleotide in realistic genomic contexts accounting for error correction,
 *   redundancy, and junk DNA. This constraint is not a policy choice,
 *   institutional arrangement, or coordination mechanism. It is a consequence
 *   of the 4-state alphabet of DNA (adenine, thymine, guanine, cytosine), the
 *   thermodynamic cost of information storage and retrieval, and the
 *   molecular dynamics of replication and transcription. No organism can exit
 *   this constraint, no institutional actor can arbitrage around it, and no
 *   amount of power can overcome it. The constraint has been invariant across
 *   billions of years of evolution and will remain invariant under any
 *   biological chemistry recognizable as such.
 *
 * KEY AGENTS:
 *   - All biological organisms: Constrained agents (trapped at universal scope) — cannot exceed the physical limits of DNA storage
 *   - Information theorists: Analytical observers (analytical/analytical) — recognize the constraint as a consequence of Shannon entropy and quantum physics
 *   - Synthetic biology industry: Institutional actors (institutional/arbitrage) — maximize storage within the constraint but cannot escape it, even with xenonucleotides or codon expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_information_storage, 0.08).
domain_priors:suppression_score(genetic_information_storage, 0.02).
domain_priors:theater_ratio(genetic_information_storage, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_information_storage, extractiveness, 0.08).
narrative_ontology:constraint_metric(genetic_information_storage, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(genetic_information_storage, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genetic_information_storage, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(genetic_information_storage, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_information_storage, mountain).
narrative_ontology:human_readable(genetic_information_storage, "Genetic Information Storage Capacity").
narrative_ontology:topic_domain(genetic_information_storage, "biology/information_theory").

domain_priors:emerges_naturally(genetic_information_storage).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BIOLOGICAL ORGANISM — All organisms are constrained by the chemical limits of DNA/RNA storage density. No organism can escape the physical constraint that nucleotide strings have finite information capacity per unit volume and time. This is a hard physical law — zero degrees of freedom.
constraint_indexing:constraint_classification(genetic_information_storage, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INFORMATION THEORIST — From the Shannon entropy perspective, genetic storage capacity is fundamentally limited by the information capacity of a 4-state alphabet (A, T, G, C) operating within thermodynamic constraints. The limit is not a policy choice or institutional arrangement — it is a consequence of statistical mechanics. Approximately 2 bits per nucleotide in ideal conditions; real genomic storage achieves ~1.8-1.9 bits per nucleotide due to redundancy and error-correction overhead. No escape from this constraint exists in any physical substrate that maintains discrete state representation.
constraint_indexing:constraint_classification(genetic_information_storage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SYNTHETIC BIOLOGY INDUSTRY — Even actors with maximum institutional power and technical resources experience the storage capacity constraint as immutable. Attempts to expand storage (using xenonucleotides, synthetic bases, codon expansion) operate within the same physical limits — they rescale the problem but do not eliminate it. The constraint is invariant across all institutional perspectives.
constraint_indexing:constraint_classification(genetic_information_storage, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_information_storage_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(genetic_information_storage, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_information_storage, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genetic_information_storage, ExtMetricName, E),
    domain_priors:suppression_score(genetic_information_storage, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(genetic_information_storage),
    narrative_ontology:constraint_metric(genetic_information_storage, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(genetic_information_storage, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(genetic_information_storage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The constraint imposes no extraction from any agent. It is purely a limitation on capacity, not a mechanism for transferring value from one agent to another. There is no beneficiary or victim relationship — the constraint affects all parties symmetrically. Suppression (0.02): Minimal. The constraint suppresses nothing because there is no coercive mechanism. Organisms are not prevented from something they could otherwise do — they are subject to a physical limit. Theater ratio (0.05): Negligible. There is no performative content. The constraint either applies or does not apply — measurement of genetic storage density either confirms or falsifies the limit. The low theater ratio reflects the constraint's absolute clarity and universal verification.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, genetic information storage produces NO perspectival gap. All observers — from the organism's perspective to the information theorist's to the biotechnology executive's — perceive the same constraint in the same way: as an unchangeable physical limit. This invariance across all perspectives is the diagnostic signature of a true mountain. The classification does not vary with power, time horizon, exit options, or spatial scope. A bacteria and an institution and an analytical observer all reach the same conclusion: DNA stores ~2 bits per nucleotide, period.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis is not applicable because the constraint involves no extraction or asymmetry. The constraint is symmetric — it affects all agents equally and benefits none while harming none. There is no beneficiary-victim structure. All agents experience the same limitation proportional to their dependence on genetic storage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thermodynamic_cost_of_information,
    'Are the observed limits on genetic information density direct consequences of the Landauer principle (thermodynamic cost of information manipulation), or are they primarily constraints of molecular chemistry and diffusion-limited enzymatic rates?',
    'Theoretical derivation of minimum energy cost for DNA replication and transcription at observed fidelity levels; comparison with actual metabolic cost data across organisms',
    'If primarily thermodynamic: the constraint is fundamental to all physical systems (true mountain). If primarily biochemical: the limit is specific to carbon-based biochemistry and could differ with alternative chemistries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thermodynamic_cost_of_information, empirical, 'Whether storage limits are thermodynamic or biochemical in origin').

omega_variable(
    error_correction_necessity,
    'Is the redundancy overhead observed in real genomes (genomes store <2 bits per nucleotide instead of the theoretical 2.0) a fundamental requirement for maintaining information fidelity, or a contingent artifact of specific error rates and evolutionary history?',
    'Comparative genomics analysis of error-correction overhead across domains of life; theoretical calculation of minimum redundancy necessary for given mutation rates and selection thresholds',
    'If fundamental: practical storage capacity is lower than the theoretical Shannon limit (true constraint). If contingent: storage capacity could be higher in systems with lower mutation rates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_correction_necessity, empirical, 'Whether error-correction overhead is necessary or contingent').

omega_variable(
    alternative_polymer_chemistry,
    'Could non-DNA-based polymer systems (XNA variants, protein-based storage, or entirely different chemistries) achieve higher information density while maintaining biological functionality and evolvability?',
    'Laboratory synthesis of alternative polymer systems; testing for information capacity, replication fidelity, and ability to encode functional molecular machines',
    'If alternative chemistries can exceed DNA density: the constraint is specific to DNA chemistry, not universal. If all polymers hit similar limits: constraint is fundamental to information storage via polymer state variation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_polymer_chemistry, empirical, 'Whether alternative chemistries could exceed DNA storage capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_information_storage, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genetic_information_storage, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gene_tr_t100, genetic_information_storage, theater_ratio, 100, 0.05).
narrative_ontology:measurement(gene_tr_t200, genetic_information_storage, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genetic_information_storage, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gene_be_t100, genetic_information_storage, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(gene_be_t200, genetic_information_storage, base_extractiveness, 200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_information_storage, information_standard).
narrative_ontology:affects_constraint(genetic_information_storage, genomic_mutation_rate_equilibrium).
narrative_ontology:affects_constraint(genetic_information_storage, metabolic_cost_of_replication).
narrative_ontology:affects_constraint(genetic_information_storage, dna_replication_fidelity_ceiling).

% DUAL FORMULATION NOTE:
% Genetic information storage capacity is a fundamental constraint in the family of molecular information processing limits. It affects downstream constraints on mutation rates (error correction requires information overhead), metabolic cost (information retrieval has thermodynamic cost), and replication fidelity (faster replication reduces accuracy). Each downstream constraint has its own extractiveness and structural dynamics; the storage capacity constraint itself is the upstream natural law that all others reference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
