% ============================================================================
% CONSTRAINT STORY: base_pair_complementarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: base_pair_complementarity
 *   human_readable: Specific Base-Pair Pairing in DNA
 *   domain: biological/chemical
 *
 * SUMMARY:
 *   Base-pair complementarity in DNA represents a fundamental structural
 *   constraint operating at the molecular level. Adenine (a purine with mass
 *   ~135 Da) invariably pairs with Thymine (a pyrimidine with mass ~126 Da)
 *   via two hydrogen bonds; Guanine (a purine with mass ~151 Da) invariably
 *   pairs with Cytosine (a pyrimidine with mass ~111 Da) via three hydrogen
 *   bonds. This specificity emerges from the three-dimensional geometry of
 *   the bases: purines are larger and pair with smaller pyrimidines to
 *   maintain uniform DNA helix diameter. The hydrogen bonding geometry is
 *   determined by quantum mechanical properties of nitrogen and oxygen atoms
 *   in the bases — their electronegativity, orbital structure, and lone-pair
 *   availability. This constraint has zero degrees of freedom: no agent can
 *   choose alternative pairing rules while preserving DNA's information
 *   storage and replication functions. It is not enforced by any external
 *   authority; it is simply the structural consequence of organic chemistry
 *   applied to nucleic acid polymers.
 *
 * KEY AGENTS:
 *   - Watson-Crick hydrogen bonding: The chemical mechanism enforcing complementarity (not an agent, but the physical law itself)
 *   - DNA polymerase enzymes: Molecular machinery that implements specificity through template recognition and active site geometry (institutional agent with arbitrage exit — they operate optimally within the constraint)
 *   - Replicating cells: Organisms whose genomes depend absolutely on complementary pairing (powerless/trapped relative to the constraint)
 *   - Evolutionary process: Natural selection operating on organisms with complementary genomes (powerful/analytical perspective)
 *   - Synthetic biologists: Researchers attempting to engineer DNA variants or alternatives (institutional/arbitrage perspective)
 *   - Structural chemists: Analytical observers who model and understand the quantum mechanics of base pairing (analytical/analytical perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(base_pair_complementarity, 0.08).
domain_priors:suppression_score(base_pair_complementarity, 0.02).
domain_priors:theater_ratio(base_pair_complementarity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(base_pair_complementarity, extractiveness, 0.08).
narrative_ontology:constraint_metric(base_pair_complementarity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(base_pair_complementarity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(base_pair_complementarity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(base_pair_complementarity, resistance, 0.08).

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

% PERSPECTIVE 1: CHEMIST'S STRUCTURAL LAW (MOUNTAIN) — From any chemical or structural biology perspective, base-pair complementarity is an immutable consequence of molecular geometry and hydrogen bonding. Adenine (purine) pairs with Thymine (pyrimidine) via two hydrogen bonds; Guanine (purine) pairs with Cytosine (pyrimidine) via three hydrogen bonds. These are not choices or regulations — they follow from the 3D shapes of the bases and the quantum mechanics of hydrogen bond formation. No agent, no time horizon, no spatial scope changes this. Zero degrees of freedom.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: REPLICATING CELL (MOUNTAIN) — A cell engaged in DNA replication cannot choose alternative pairing rules. The constraint is experienced as a pure structural law: DNA polymerase recognizes the template strand and inserts complementary nucleotides according to Watson-Crick rules. This is not coercive in any meaningful sense — it is simply the mechanical operation of enzymatic specificity. The cell has zero exit options and zero degrees of freedom relative to this constraint.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: EVOLUTIONARY PROCESS (MOUNTAIN) — Even at the civilizational scale, evolution cannot violate base-pair complementarity. Mutations occur via substitution, insertion, or deletion — but all within the constraint of specific pairing rules. Natural selection operates on organisms whose genomes respect Watson-Crick complementarity. The evolutionary process has no ability to 'choose' alternative pairing schemes; it is constrained as absolutely as a cell in replication.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MOLECULAR ENGINEER (MOUNTAIN) — Even synthetic biology and DNA nanotechnology — fields that deliberately manipulate DNA sequences — must work WITHIN base-pair complementarity. Xeno nucleic acids (XNA) with alternative pairing rules have been synthesized in vitro, but they form distinct chemical systems, not alternatives to DNA proper. Standard DNA, as a functional molecule in living systems, has no exit from Watson-Crick pairing. Any agent attempting to 'engineer around' complementarity would be creating a different molecule.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.08): Minimal. This constraint does not extract resources, suppress alternatives, or impose coercive overhead. It is a pure structural law. The value 0.08 reflects residual measurement uncertainty at the lower bound — the true extractiveness is approaching the Mountain floor (ε ≤ 0.25). Suppression (0.02): Negligible. There is no suppression of alternatives because alternatives are not possible while maintaining DNA function. Theater ratio (0.05): Negligible. There is no performative content. The constraint operates transparently at the chemical level with no ceremonial or decorative function. All three metrics affirm Mountain classification: the constraint is invariant across all observables, all time horizons, and all agent perspectives.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on Mountain classification. This is not a perspectival gap but perspectival unanimity — the signature of a true natural law. The chemist sees immutable structure. The cell sees mechanical necessity. The evolutionary process sees constraint on variation. The engineer sees boundaries of DNA proper vs. alternative molecules. The structural biologist sees quantum mechanical inevitability. No perspective experiences the constraint as coordination, extraction, or contingent institutional arrangement. This convergence is the definition of a mountain constraint: it appears the same from every possible viewing angle because it is intrinsic to the substrate, not dependent on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is meaningless for this constraint because there are no beneficiaries or victims in any structural sense. The constraint is not extracting from any agent toward any beneficiary. Watson-Crick pairing is symmetric: both adenine and thymine, both guanine and cytosine, are equally constrained. Neither the cell nor the DNA molecule is a victim of the pairing rule; both are simply operating within a structural law. This absence of asymmetric benefit or cost is another signature of Mountain classification. If a constraint could be described with beneficiaries and victims, it would not be a mountain — it would be a rope, snare, or hybrid.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    xna_alternative_pairing,
    'Do synthetic xeno nucleic acids with non-Watson-Crick pairing rules demonstrate that base-pair complementarity is contingent rather than necessary?',
    'Structural comparison: Are XNAs alternative pairing schemes for DNA, or structurally distinct molecules? Can XNA pairing rules replicate the information storage and replication functions of standard DNA in living systems?',
    'If XNAs are truly alternative DNA chemistries: base-pair complementarity becomes contingent (moves from Mountain to Tangled Rope). If XNAs are separate molecular classes: complementarity remains universal law for DNA proper.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(xna_alternative_pairing, empirical, 'Whether xeno nucleic acids represent alternative pairing or distinct molecules').

omega_variable(
    wobble_pairing_scope,
    'Wobble base pairing (non-Watson-Crick pairing at codon third positions) and G-U mismatches in RNA — do these demonstrate that specific pairing is contingent, or are they secondary interactions within a primary constraint?',
    'Functional analysis: Do wobble interactions enable alternative genetic codes or are they tolerance mechanisms within the Watson-Crick primary structure? Can genomes function with relaxed pairing specificity at primary positions?',
    'If wobble represents true alternative pairing: complementarity is softer than claimed (Rope or Tangled Rope). If wobble is secondary tolerance: primary complementarity remains Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wobble_pairing_scope, empirical, 'Whether wobble pairing demonstrates contingency of Watson-Crick rules').

omega_variable(
    origin_of_pairing_rules,
    'Is Watson-Crick base pairing a contingent historical outcome of RNA world chemistry, or is it the inevitable consequence of organic chemistry given replicating polymer constraints?',
    'Prebiotic chemistry simulations; origin-of-life models; analysis of whether alternative pairing schemes could support self-replicating systems under early Earth conditions',
    'If contingent: the constraint might be reclassified as Piton (historical accident maintained by inertia). If inevitable: Mountain status is affirmed at deeper level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(origin_of_pairing_rules, conceptual, 'Whether base pairing is inevitable or contingent outcome of chemistry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(base_pair_complementarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bpcomp_tr_t0, base_pair_complementarity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bpcomp_tr_t5, base_pair_complementarity, theater_ratio, 5, 0.04).
narrative_ontology:measurement(bpcomp_tr_t10, base_pair_complementarity, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(bpcomp_be_t0, base_pair_complementarity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(bpcomp_be_t5, base_pair_complementarity, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(bpcomp_be_t10, base_pair_complementarity, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(base_pair_complementarity, information_standard).
narrative_ontology:affects_constraint(base_pair_complementarity, dna_double_helix_diameter).
narrative_ontology:affects_constraint(base_pair_complementarity, genetic_information_storage).
narrative_ontology:affects_constraint(base_pair_complementarity, nucleotide_incorporation_specificity).

% DUAL FORMULATION NOTE:
% Base-pair complementarity is the upstream structural constraint that determines properties of the DNA double helix (diameter, helical pitch, stability). Downstream constraints on genetic code fidelity, nucleotide incorporation specificity, and replication error rates all depend on Watson-Crick complementarity being maintained. This is a constraint family with base-pair complementarity as the foundational natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
