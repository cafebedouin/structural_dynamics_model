% ============================================================================
% CONSTRAINT STORY: base_pair_complementarity
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Watson & Crick, "Molecular Structure of Nucleic Acids" (1953)
% ============================================================================

:- module(constraint_base_pair_complementarity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: base_pair_complementarity
 * human_readable: Specific Base-Pair Pairing
 * domain: biological/chemical
 * temporal_scope: Permanent (Biological Life)
 * spatial_scope: Universal (Cellular Level)
 * * SUMMARY:
 * Watson and Crick propose a structure for DNA consisting of two helical chains 
 * held together by purine and pyrimidine bases. The core constraint is the 
 * "specific pairing" (Adenine with Thymine, Guanine with Cytosine), which 
 * dictates the molecular geometry and suggests a copying mechanism.
 * * KEY AGENTS:
 * - The DNA Polymerase (Implied): The institutional agent that "reads" and 
 * enforces the pairing during replication.
 * - The Nucleotide: The powerless agent that must conform to the 
 * hydrogen-bond geometry.
 * - Watson & Crick: The analytical observers who identify the "Rope" of 
 * genetic information.
 * * NARRATIVE ARC:
 * The paper critiques previous models (Pauling's three-chain model) for being 
 * chemically "unsatisfactory." By identifying the correct helical structure, 
 * the authors reveal a constraint that is both a "Mountain" (chemical necessity) 
 * and a "Rope" (a functional copying mechanism for heredity).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(dna_structure_discovery, 0, 10).
narrative_ontology:constraint_claim(base_pair_complementarity, mountain).

% Base extractiveness score: 0.2
% Rationale: The pairing coordinates information; it does not "extract" 
% value in a social sense, though it "extracts" thermodynamic stability.
domain_priors:base_extractiveness(base_pair_complementarity, 0.2).

% Suppression score: 1.0
% Rationale: Alternatives (like A-G pairing) are physically impossible 
% within this geometry; the suppression is absolute and dictated by 
% Van der Waals distances.
domain_priors:suppression_score(base_pair_complementarity, 1.0).

% Enforcement requirements: Thermodynamic laws and steric hindrance.
domain_priors:emerges_naturally(base_pair_complementarity).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(base_pair_complementarity, extractiveness, 0.2).
narrative_ontology:constraint_metric(base_pair_complementarity, suppression_requirement, 1.0).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(base_pair_complementarity, [biological_order, heredity]).
constraint_victim(base_pair_complementarity, [entropic_decay]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NUCLEOTIDE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - A single molecule.
   WHEN: immediate - The chemical bond occurs in nanoseconds.
   WHERE: trapped - Bound by the "sugar-phosphate backbone."
   SCOPE: local - Within a single double helix.
   
   WHY THIS CLASSIFICATION:
   For an individual base, the pairing is a "Mountain." It cannot "choose" 
   another partner due to the rigid spacing of the helix and the specific 
   hydrogen bond donor/acceptor sites.
   
   NARRATIVE EVIDENCE:
   "Only specific pairs of bases can bond together... one member of a pair 
   must be a purine and the other a pyrimidine."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    base_pair_complementarity,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(base_pair_complementarity, biological_order),
        constraint_victim(base_pair_complementarity, []),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(base_pair_complementarity, S),
    S > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CELLULAR MACHINERY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - The enzymes (polymerases) that govern replication.
   WHEN: generational - Concerned with the "copying mechanism."
   WHERE: arbitrage - Able to correct errors (proofreading).
   SCOPE: global - Across the entire genome.
   
   WHY THIS CLASSIFICATION:
   For the cell, the double helix is a "Rope"—a functional coordination 
   mechanism. It is the "suggested copying mechanism" that allows life to 
   propagate its code across generations.
   
   NARRATIVE EVIDENCE:
   "It has not escaped our notice that the specific pairing... immediately 
   suggests a possible copying mechanism."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    base_pair_complementarity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        constraint_beneficiary(base_pair_complementarity, heredity),
        constraint_victim(base_pair_complementarity, []),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PAULING & COREY - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - Competing world-class scientists.
   WHEN: immediate - The release of a superior model.
   WHERE: constrained - Their three-chain model is invalidated by geometry.
   SCOPE: national - The scientific consensus.
   
   WHY THIS CLASSIFICATION:
   For those holding the incorrect model, the geometric reality of the 
   Watson-Crick model acts as a "Noose." It "suppresses" their theory 
   by demonstrating that their proposed structure is "unsatisfactory" 
   due to electrostatic repulsion.
   
   NARRATIVE EVIDENCE:
   "Without the acidic hydrogen atoms it is not clear what forces would 
   hold the structure together."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    base_pair_complementarity,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(base_pair_complementarity, watson_crick_consensus),
        constraint_victim(base_pair_complementarity, pauling_model),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(base_pair_complementarity, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dna_structure_tests).

test(geometry_determines_type) :-
    % Verify that for the powerless nucleotide, the constraint is a Mountain
    constraint_indexing:constraint_classification(base_pair_complementarity, mountain, context(agent_power(individual_powerless), _, trapped, _, _, _)).

test(functional_replication) :-
    % Verify that for cellular machinery, the helix is a Rope
    constraint_indexing:constraint_classification(base_pair_complementarity, rope, context(agent_power(institutional), _, arbitrage, _, _, _)).

test(suppression_of_error) :-
    % Mismatched bases face 1.0 suppression (steric hindrance)
    domain_priors:suppression_score(base_pair_complementarity, 1.0).

:- end_tests(dna_structure_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: DNA structure is the ultimate "Mountain" because 
 * chemical bonds and atomic distances leave zero room for alternative 
 * "interpretations."
 * 2. THE REPLICATION ROPE: I identified the "copying mechanism" as a 
 * Rope because it is the functional utility that justifies the 
 * existence of the constraint in a biological sense.
 * 3. SUPPRESSION (1.0): The paper is effectively a proof. It suppresses 
 * incorrect models (Pauling/Fraser) not through political force, but 
 * through the "Analytical" force of chemical consistency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tautomeric_shift,
    "Can bases exist in alternative states (tautomers) that allow 'illegal' pairings?",
    resolution_mechanism("Requires biochemical analysis of mutation rates."),
    impact("If yes: The Mountain of pairing becomes a Noose for the genetic code (mutations)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Pauling & Corey's Three-Chain Model:
 * Viability: Real model proposed by leading scientists of the era.
 * Suppression: Rejected by Watson & Crick because "the negatively charged 
 * phosphates near the axis will repel each other."
 * EVIDENCE: "Their model consists of three intertwined chains... in our 
 * opinion this structure is unsatisfactory."
 * * CONCLUSION: 
 * The physical impossibility of the three-chain model reinforces the 
 * classification of the double helix as a "Mountain."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
