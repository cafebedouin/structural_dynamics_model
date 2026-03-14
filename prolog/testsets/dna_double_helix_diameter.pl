% ============================================================================
% CONSTRAINT STORY: dna_double_helix_diameter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dna_double_helix_diameter, []).

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
 *   constraint_id: dna_double_helix_diameter
 *   human_readable: DNA Double Helix Diameter
 *   domain: molecular_biology/structural_biochemistry
 *
 * SUMMARY:
 *   The DNA double helix diameter — approximately 2.0 nanometers — is a
 *   fundamental structural constraint that emerges from molecular chemistry
 *   and quantum electrostatics. This is a constraint that produces identical
 *   classification across all observer perspectives because it is a natural
 *   law rather than an institutional arrangement, policy, or social
 *   structure. The diameter is invariant across all organisms, all cellular
 *   contexts, and all time scales relevant to biology. No agent — biological,
 *   institutional, or analytical — experiences this constraint as extractive,
 *   negotiable, or theater. It is simply a structural boundary that all
 *   biochemistry must respect. This constraint serves as a diagnostic test
 *   case for the Deferential Realism system's ability to recognize and
 *   classify true natural laws while avoiding the false summit fallacy (where
 *   contingent institutional arrangements are mistaken for laws of nature).
 *
 * KEY AGENTS:
 *   - DNA Molecules: Structural target (powerless/trapped) — no agency; diameter is intrinsic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a direct consequence of chemistry and quantum mechanics
 *   - Biotechnology Industry: Powerful institutional actor (institutional/arbitrage) — accommodates the constraint but cannot modify it
 *   - Synthetic Chemists: Attempting to engineer alternative nucleotides (powerful/mobile) — constrained by chemical and thermodynamic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dna_double_helix_diameter, 0.08).
domain_priors:suppression_score(dna_double_helix_diameter, 0.02).
domain_priors:theater_ratio(dna_double_helix_diameter, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dna_double_helix_diameter, extractiveness, 0.08).
narrative_ontology:constraint_metric(dna_double_helix_diameter, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(dna_double_helix_diameter, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dna_double_helix_diameter, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dna_double_helix_diameter, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dna_double_helix_diameter, mountain).
narrative_ontology:human_readable(dna_double_helix_diameter, "DNA Double Helix Diameter").
narrative_ontology:topic_domain(dna_double_helix_diameter, "molecular_biology/structural_biochemistry").

domain_priors:emerges_naturally(dna_double_helix_diameter).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOLECULAR AGENT (MOUNTAIN) — DNA molecules have no agency. The 2.0 nm diameter is an invariant structural property that emerges from quantum electrostatic interactions between base pairs and the sugar-phosphate backbone. No exit option exists — the constraint is physically immutable.
constraint_indexing:constraint_classification(dna_double_helix_diameter, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The DNA helix diameter is a direct consequence of covalent bond geometry, van der Waals radii, and electrostatic repulsion. These constraints are fundamental to chemistry itself. The 2.0 nm diameter emerges necessarily from quantum mechanics and cannot be modified without changing the chemistry of nucleotides entirely. This is a natural law at the molecular scale.
constraint_indexing:constraint_classification(dna_double_helix_diameter, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: BIOENGINEER (MOUNTAIN) — Even powerful actors with advanced molecular engineering capabilities cannot alter the DNA double helix diameter. Attempts to create synthetic nucleotides with different dimensions either produce unstable structures (rapid hydrolysis, loss of base pairing) or violate the fundamental assumptions of the molecular constraint. The diameter is not a design parameter — it is a physical boundary that all chemistry must respect.
constraint_indexing:constraint_classification(dna_double_helix_diameter, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: BIOTECHNOLOGY INDUSTRY (MOUNTAIN) — All commercial DNA-based technologies (PCR, sequencing, gene synthesis, CRISPR) operate within the constraint of the 2.0 nm helix diameter. This constraint is not extractive — it is simply a structural fact that biotechnology must accommodate, not resist. The diameter is universal across all organisms and all synthetic DNA variants. No institutional actor can negotiate or arbitrage around this boundary.
constraint_indexing:constraint_classification(dna_double_helix_diameter, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dna_double_helix_diameter_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dna_double_helix_diameter, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dna_double_helix_diameter, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dna_double_helix_diameter, ExtMetricName, E),
    domain_priors:suppression_score(dna_double_helix_diameter, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dna_double_helix_diameter),
    narrative_ontology:constraint_metric(dna_double_helix_diameter, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dna_double_helix_diameter, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dna_double_helix_diameter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract resources from any agent because no agent experiences asymmetric costs or benefits. DNA diameter is a physical invariant, not a distribution mechanism. The small non-zero value reflects that knowledge of the constraint requires scientific labor (measurement, inference from crystallography), but this is acquisition cost, not extraction. Suppression (0.02): Minimal. The constraint suppresses nothing — it is transparent and universally observable. Laboratory techniques can measure DNA diameter directly via X-ray crystallography, cryo-EM, and AFM. Theater ratio (0.05): Minimal. The constraint produces minimal performative activity. Measurement of DNA diameter is straightforward and uncontroversial; no ritualization is required. The small non-zero value reflects that some communication overhead exists in transmitting the empirical fact across scientists, but this is negligible compared to institutional constraints.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives classify the constraint identically as Mountain because the constraint is a natural law. The constraint is invariant across all possible indexical positions (P, T, E, S). This uniformity is the diagnostic signature of a true natural law: no agent perceives it as extractive, no time horizon reveals it as contingent, no exit option changes its status, no scope modification shifts its classification. The absence of perspectival gap distinguishes this constraint from false summits — institutional arrangements that appear mountain-like from the beneficiary's view but reveal themselves as snares or tangled ropes from other positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not meaningfully defined for this constraint because there is no extraction flow. The d parameter (directionality) measures the agent's structural position along a beneficiary-victim axis. For a mountain constraint with no extracted benefits and no imposed costs, d approaches 0.5 (symmetric) for all agents. The f(d) sigmoid evaluates to approximately 0.65 for all perspectives, but this is not experienced as extraction — it is simply the mathematical floor of the sigmoid at equilibrium. The constraint structure involves no beneficiaries or victims, so no directionality_overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: This constraint demonstrates the mandatrophy resolution by showing that true natural laws have zero mandatrophy. The mandatrophy arises when a single structural phenomenon can be labeled as both 'coordination' and 'extraction' depending on observer perspective — when the tension between these frames cannot be resolved into a single consistent interpretation. The DNA helix diameter has no such tension. It is neither coordination (no agents coordinating) nor extraction (no asymmetric cost distribution). It is simply a physical boundary. All six types collapse to Mountain. This is the marker of a genuine natural law: the manifold of possible interpretations converges to a single point. False summits (constraints misclassified as natural laws) exhibit residual mandatrophy even across consensus perspectives — the beneficiary sees rope, the analytical observer sees mountain, but closer structural inspection reveals asymmetric costs buried in the 'natural' framing. The DNA helix diameter shows no such residual tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_canonical_dna_variants,
    'Do non-canonical DNA forms (Z-DNA, A-form DNA, alternative helical structures) constitute violations of the 2.0 nm constraint or variations within a broader family?',
    'Structural crystallography of non-canonical forms; determination of whether diameter variations are meaningful (>5% deviation) or within measurement error and hydration-dependent fluctuation ranges',
    'If violations: the constraint applies only to B-form DNA, requiring a narrower scope declaration (not universal). If variations: the constraint remains universal but with acknowledged context-dependence on solvent and temperature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_canonical_dna_variants, empirical, 'Whether non-canonical DNA forms violate the 2.0 nm constraint').

omega_variable(
    synthetic_xenonucleic_acids,
    'Can engineered XNA (xenonucleic acids) with fundamentally different backbones (TNA, PNA, XNA) achieve stable double helix structures with diameters substantially different from 2.0 nm?',
    'Experimental synthesis and crystallographic characterization of stable XNA duplexes with modified backbone chemistry; measurement of helix diameters and assessment of structural stability over time and across conditions',
    'If achievable: the constraint is chemistry-specific rather than universal, requiring a narrower scope (global but not universal). If not achievable: the constraint is a deeper structural limit of any stable information polymer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_xenonucleic_acids, empirical, 'Whether engineered XNAs can achieve substantially different helix diameters').

omega_variable(
    quantum_indeterminacy_of_diameter,
    'Is the 2.0 nm diameter a classical geometric property or a quantum average of a distribution with non-zero variance? If quantum mechanical, what is the variance and how does it propagate through larger scales?',
    'High-resolution cryo-EM and X-ray crystallography at sub-angstrom resolution; analysis of structural fluctuations from molecular dynamics simulations; NMR and SAXS measurements of solution-phase diameter distribution',
    'If quantum indeterminacy is large: the constraint is statistical rather than absolute, and the ''diameter'' is a model-dependent average. If variance is small: the constraint remains effectively invariant for practical purposes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_indeterminacy_of_diameter, empirical, 'Quantum mechanical indeterminacy of DNA helix diameter').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dna_double_helix_diameter, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dna__tr_t0, dna_double_helix_diameter, theater_ratio, 0, 0.02).
narrative_ontology:measurement(dna__tr_t50, dna_double_helix_diameter, theater_ratio, 50, 0.05).
narrative_ontology:measurement(dna__tr_t100, dna_double_helix_diameter, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(dna__be_t0, dna_double_helix_diameter, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(dna__be_t50, dna_double_helix_diameter, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(dna__be_t100, dna_double_helix_diameter, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dna_double_helix_diameter, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
