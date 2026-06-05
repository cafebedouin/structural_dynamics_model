% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Mathematical Definition (Stipulative Reading)
 *   domain: philosophy_of_mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   The Church-Turing Thesis is a foundational claim in the theory of
 *   computation, often presented as the assertion that effective
 *   computability (the notion of an algorithm that can be carried out by hand
 *   in finite time) is coextensive with Turing-computability. This constraint
 *   story represents ONE READING of the thesis: the
 *   mathematical_definition_reading. Under this reading, the thesis is not a
 *   claim about empirical reality or about the nature of physical
 *   computation. Instead, it is a stipulative mathematical definition — a
 *   choice to use 'effective computability' as the name for the class of
 *   functions computed by a Turing machine (equivalently, lambda-computable
 *   functions, register machines, etc.). The reading treats the thesis as a
 *   convergence-of-models observation with a stipulative conclusion:
 *   mathematicians observed that multiple independent formal models of
 *   computation (Turing, Church, Gödel, Post, Markov) generated the same set
 *   of computable functions. Rather than treating this as empirical evidence
 *   for a deep truth about nature, the stipulative reading says: we will
 *   define effective computability to mean 'Turing-computable' (or any of the
 *   equivalent models). This definition solves a coordination problem in
 *   mathematical discourse. It allows mathematicians to make precise
 *   statements about decidability, halting, computability classes, and
 *   approximation without ambiguity about what 'algorithm' or 'effective
 *   procedure' means. The constraint is thus a Rope — a pure coordination
 *   mechanism with minimal extraction.
 *
 * KEY AGENTS:
 *   - Mathematical foundations community: Primary beneficiary (institutional/mobile) — benefits from terminological clarity and shared reference point for foundational theorems
 *   - Computational theory researchers: Secondary beneficiary (institutional/mobile) — coordinate on problem definition and theorem statements; share a common language for undecidability proofs
 *   - Computer science students: Individual agents (moderate/constrained) — benefit from learning a canonical definition that enables socialization into the professional discourse
 *   - Applied computing industry: Weak stakeholder (powerful/constrained) — the definition is mandatory in curricula but carries low operational force; theater is high but functional value low
 *   - Analytical observer: Position outside the stipulation — can see the thesis as either a definitions (this reading) or as making a claim about physical or logical reality (sibling readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition (Stipulative Reading)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/philosophy_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '639142b7-8677-408a-9dba-0a0b8ea30d27').
narrative_ontology:cs_kernel_codification('639142b7-8677-408a-9dba-0a0b8ea30d27', formalized).
narrative_ontology:cs_authority_grounding('639142b7-8677-408a-9dba-0a0b8ea30d27', expertise).
narrative_ontology:cs_interpretation_layer_present('639142b7-8677-408a-9dba-0a0b8ea30d27').
narrative_ontology:cs_reading_relation('639142b7-8677-408a-9dba-0a0b8ea30d27', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('639142b7-8677-408a-9dba-0a0b8ea30d27', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('639142b7-8677-408a-9dba-0a0b8ea30d27', foundational, computability_defined_by_convention).
narrative_ontology:cs_axiom_status(computability_defined_by_convention, holdable).
narrative_ontology:cs_axiom_grounding('639142b7-8677-408a-9dba-0a0b8ea30d27', computability_defined_by_convention, conventional).
narrative_ontology:cs_axiom('639142b7-8677-408a-9dba-0a0b8ea30d27', foundational, convergence_of_models_supports_stipulation).
narrative_ontology:cs_axiom_status(convergence_of_models_supports_stipulation, holdable).
narrative_ontology:cs_axiom_grounding('639142b7-8677-408a-9dba-0a0b8ea30d27', convergence_of_models_supports_stipulation, empirically_contingent).
narrative_ontology:cs_reference_frame('639142b7-8677-408a-9dba-0a0b8ea30d27', mathematical_consensus_on_turing_equivalence).
narrative_ontology:cs_drift_state('639142b7-8677-408a-9dba-0a0b8ea30d27', contemporary_hypercomputation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('639142b7-8677-408a-9dba-0a0b8ea30d27', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_foundational_clarity).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computational_theory_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL FOUNDATIONS (ROPE) — From the analytical standpoint at civilizational scope, the thesis is a pure coordination mechanism: a stipulated definition that aligns terminology across formal systems. Effective computability is defined to mean Turing-computable functions (equivalently, lambda-computable, register-machine-computable, etc.). The constraint solves the coordination problem of how mathematicians communicate about computability without ambiguity. Zero extraction, minimal suppression — this is consensus on notation.
constraint_indexing:constraint_classification(church_turing_thesis__mathematical_definition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL THEORY COMMUNITY (ROPE) — The institutional mathematical and CS community benefits from the stipulative definition as a coordination tool. It enables clean theorem statements, reduces ambiguity in foundational work, and provides a stable reference point for research. The community is mobile relative to this constraint — they could adopt alternative definitions if beneficial — but do not because the stipulated definition works. Low extraction, low theater. The constraint is functional.
constraint_indexing:constraint_classification(church_turing_thesis__mathematical_definition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: APPLIED COMPUTING INDUSTRY (PITON) — At the powerful/constrained/biographical level, the definition constraint appears as inert institutional baggage. Industry engineers do not care whether their algorithms are Turing-computable — they care about performance, correctness, and tractability. The thesis is taught as foundational doctrine but carries no operational force. Theater is high (the definition is cited ritually in textbooks and curricula) but functional content is low. Engineers adopt this reading not because it coordinates their work but because it is institutionally mandatory.
constraint_indexing:constraint_classification(church_turing_thesis__mathematical_definition_reading, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPUTER SCIENCE STUDENT (ROPE) — At the individual level, the definition reads as coordination: the student must learn this stipulation to communicate with peers and professors about decidability, halting, and computability classes. The constraint enables entry into the community's discourse. Exit options are constrained (education has high switching costs) but the coordination benefit is real. Low extraction because the definition is genuinely useful for the student's learning and professional socialization.
constraint_indexing:constraint_classification(church_turing_thesis__mathematical_definition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, TR),
    TR >= 0.70.

:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. A pure stipulation creates no extraction. The definition is a convention — a choice to use certain words to mean certain things. Stipulations cannot be violated or extracted from in the way that material constraints extract. Any agent can in principle leave the convention and use alternative terminology; they do not because the convention is useful for coordination. The extractiveness value is non-zero only to capture the weak institutional cost of learning and maintaining the convention (textbook space, pedagogical effort, cognitive load). Suppression (0.02): Minimal. The definition is transparent and non-coercive. There are no barriers to exit — agents adopt it voluntarily because it coordinates their work. Theater ratio (0.15): Low. The constraint is mostly functional and minimally performative. The thesis is taught as foundational truth and cited ritually, but the teaching serves a real coordination function (explaining what 'decidable' means) rather than merely performative purpose. The small performative component reflects that students sometimes memorize the definition without understanding its role as stipulative choice rather than empirical discovery.
 *
 * PERSPECTIVAL GAP:
 *   The gap is not between beneficiary and victim (this constraint has no victims — it is purely coordinative) but between those who see the thesis as a functional coordination mechanism (analytical, institutional, student perspectives) and those who see it as empty institutional ritual (applied industry perspective). The piton classification for the industry perspective shows that a constraint can be structurally Rope (it is coordination) while experientially Piton (it feels like inert baggage). The applied engineer does not question the definition — it is beyond their scope — but also does not feel it coordinating their work. They memorize it for exams and interviews but do not use it in daily practice. This is not extraction (there is no asymmetric cost) but rather non-functional institutional requirement. The perspectival gap reveals that functional coordination for the theoretical community can appear as dysfunctional ritual for the applied practitioner.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is nearly zero across all perspectives because the constraint is stipulative and symmetrical. Beneficiaries and victims do not apply in the traditional sense — a definition cannot harm or help specific agents asymmetrically in the way extraction constraints do. All agents who adopt the convention benefit equally from coordination. The analytical perspective derives d ≈ 0.72 under the canonical formula (analytical power), but the constraint itself has no target-vs-beneficiary asymmetry. The piton perspective shows slightly higher d (0.58 for powerful/constrained) because the powerful agent experiences the constraint as institutional baggage rather than genuine coordination, suggesting some friction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint because there is no tension between claiming coordination and claiming extraction. The constraint is pure coordination (Rope) from all perspectives that acknowledge its function. The piton classification does not represent a mandatrophy — it is the applied industry's accurate perception that the constraint is not coordinating THEIR activity, even though it coordinates mathematical discourse. No misclassification occurs; the constraint genuinely is coordination-only, and those who experience it as inert are simply outside its functional scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stipulation_vs_discovery_ambiguity,
    'Is the Church-Turing Thesis a pure mathematical stipulation about terminology, or does it carry a hidden empirical claim about the nature of computation?',
    'Semantic analysis: if the thesis is purely stipulative, it cannot be falsified by any empirical discovery (definitions are true by convention). If it carries empirical content, it is falsifiable by discovering a more powerful model of computation that exists in principle but is not Turing-computable. The sibling reading (physical_claim_reading) claims there IS empirical content; this reading denies it.',
    'If pure stipulation: this reading (mathematical_definition_reading) is correct, the constraint is Rope, and chi is minimal. If empirical content exists: the constraint is Tangled Rope or Snare, and the reading is incomplete or false. The resolution hinges on whether ''effective computability'' refers to a mind-independent feature of the world or to a human-stipulated convention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stipulation_vs_discovery_ambiguity, conceptual, 'Whether the thesis is purely stipulative or carries hidden empirical content').

omega_variable(
    sister_reading_coexistence,
    'Can the mathematical_definition_reading (the thesis is a stipulation) coexist with the physical_claim_reading (the thesis asserts something about physical systems) in a single coherent framework?',
    'Logical analysis of the two axioms: if the stipulation axiom and the physical claim axiom are mutually exclusive within a single foundational commitment, they foreclose each other. If they address different questions (the stipulation defines the mathematical object; the physical claim asserts that physical systems instantiate it), they coexist. The epistemological_boundary_reading offers a third possibility: that the boundary between stipulation and claim cannot be drawn sharply within the thesis itself.',
    'If the readings foreclose each other: only one can be true (game-theoretic consequence is winner-take-all). If they coexist: the thesis is ambiguous by design, and all three readings are live interpretations of a kernel that does not resolve between them. If the boundary is genuinely unclear: the kernel may be inherently under-determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sister_reading_coexistence, conceptual, 'Whether sibling readings coexist or foreclose within a single framework').

omega_variable(
    foundational_authority_erosion,
    'Has the foundational authority of the Church-Turing Thesis eroded as hypercomputation, quantum computation, and oracle models have proliferated?',
    'Historical and bibliometric analysis: track citation patterns, textbook treatment, and how the thesis is framed (as definitional vs. as empirical claim) across decades. Measure whether newer foundational work treats the thesis as settled canonical doctrine or as a contingent choice among multiple computational paradigms.',
    'If authority has eroded: the stipulation is losing force, and the constraint may degrade from Rope (consensus) to Piton (ritual maintenance). If authority is stable: the mathematical community continues to see the thesis as a functional coordination mechanism. The physical_claim_reading predicts erosion (as evidence for hypercomputation accumulates); the mathematical_definition_reading predicts stability (definitions are not falsified by new models).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_authority_erosion, empirical, 'Whether foundational authority of the thesis has eroded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_mathdef_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ctt_mathdef_tr_t30, church_turing_thesis__mathematical_definition_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(ctt_mathdef_tr_t60, church_turing_thesis__mathematical_definition_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(ctt_mathdef_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(ctt_mathdef_be_t30, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(ctt_mathdef_be_t60, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 60, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing Thesis decomposes into three distinct constraints with different ε values and structural properties. The mathematical_definition_reading (this story) treats the thesis as a stipulation: ε ≈ 0.05, Rope, no victims, coordination-only. The physical_claim_reading treats the thesis as an empirical assertion about the physical world and what is computable in principle: ε ≈ 0.35-0.45, Tangled Rope, potential victims (if false), active research commitment. The epistemological_boundary_reading treats the thesis as marking an epistemic limit independent of stipulation or physical fact: ε ≈ 0.25, Rope or Mountain depending on whether the boundary is seen as discovered or constructed. All three are valid readings of the same kernel (the text and consensus around the Church-Turing Thesis); the kernel does not resolve which reading is correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
