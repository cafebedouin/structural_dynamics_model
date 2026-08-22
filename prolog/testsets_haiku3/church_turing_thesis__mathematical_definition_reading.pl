% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Mathematical Definition of Effective Computability
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   The Church-Turing thesis states that the class of effectively computable
 *   functions is exactly the class of Turing-computable functions. This
 *   constraint story instantiates ONE READING of this contested kernel: the
 *   mathematical-definition reading. Under this reading, the thesis is a
 *   conventional definition of mathematical terminology, true by stipulation
 *   and not empirically testable. The competing readings treat the thesis as
 *   an empirical claim about the physical universe (physical_claim_reading)
 *   or as a boundary marker of formal proof capability
 *   (epistemological_boundary_reading). This story covers only the definition
 *   reading: the thesis is what we decided to call 'effective computability,'
 *   and it is neither true nor false in any empirical sense—it simply
 *   specifies the meaning of a term within formal mathematics. The extraction
 *   and suppression metrics are low because the definition generates no
 *   coercive enforcement or alternative-suppression—it is a purely
 *   coordinative act that all parties (logicians, computer scientists,
 *   mathematicians) accept as beneficial.
 *
 * KEY AGENTS:
 *   - Mathematical community: benefits from agreed-upon terminology; bears no cost; has full analytical exit
 *   - Logicians and proof theorists: use the definition to build formal systems; unaffected by alternative readings
 *   - Philosophers of computation: observe and contest; some adopt this reading, others adopt sibling readings
 *   - Quantum researchers (excluded): would contest the definition if empirical claims about quantum computation were at stake, but are outside the scope of the mathematical-definition reading
 *   - Physical universalists (excluded): hold the empirical claim directly contradicts the definition reading; structurally foreclosed from the same framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.08).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.12).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'c464102b-5617-4497-baf6-0e5fb575ad44').
narrative_ontology:cs_kernel_codification('c464102b-5617-4497-baf6-0e5fb575ad44', formalized).
narrative_ontology:cs_authority_grounding('c464102b-5617-4497-baf6-0e5fb575ad44', expertise).
narrative_ontology:cs_interpretation_layer_present('c464102b-5617-4497-baf6-0e5fb575ad44').
narrative_ontology:cs_reading_relation('c464102b-5617-4497-baf6-0e5fb575ad44', church_turing_thesis__physical_claim_reading, forecloses).
narrative_ontology:cs_reading_relation('c464102b-5617-4497-baf6-0e5fb575ad44', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('c464102b-5617-4497-baf6-0e5fb575ad44', foundational, thesis_is_definitional_convention).
narrative_ontology:cs_axiom_status(thesis_is_definitional_convention, holdable).
narrative_ontology:cs_axiom_grounding('c464102b-5617-4497-baf6-0e5fb575ad44', thesis_is_definitional_convention, conventional).
narrative_ontology:cs_axiom('c464102b-5617-4497-baf6-0e5fb575ad44', foundational, effective_computability_means_turing_computable).
narrative_ontology:cs_axiom_status(effective_computability_means_turing_computable, holdable).
narrative_ontology:cs_axiom_grounding('c464102b-5617-4497-baf6-0e5fb575ad44', effective_computability_means_turing_computable, conventional).
narrative_ontology:cs_reference_frame('c464102b-5617-4497-baf6-0e5fb575ad44', formalist_mathematical_practice).
narrative_ontology:cs_drift_state('c464102b-5617-4497-baf6-0e5fb575ad44', contemporary_computational_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c464102b-5617-4497-baf6-0e5fb575ad44', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, logicians_proof_theorists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formal_computability_equivalence).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, definition_by_convention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the thesis as a stable, agreed-upon definition of what 'effective computability' means in formal contexts. The definition allows mathematicians to reason about computable functions without needing to specify a computational model—Turing machines, lambda calculus, recursive functions all reduce to the same class. The benefit is terminological clarity and a unified framework for discussing decidability, halting problems, and reduction procedures.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_community, beneficiary,
    institutional, generational, analytical, universal).

% Depend on the thesis as a conventional boundary marker: what we can prove computable, via any formalism, is exactly what we call Turing-computable. This reading treats the thesis as definitional, not empirical, allowing them to build proof systems and undecidability results on solid ground without claiming anything about physical reality.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logicians_proof_theorists, beneficiary,
    institutional, generational, analytical, universal).

% Analyze and contest the thesis's interpretation. Some adopt the mathematical-definition reading (endorsed by this constraint story); others adopt the physical-claim or epistemological-boundary readings. They ask whether the thesis is true by convention, by empirical fact, or by the structure of knowledge itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    institutional, generational, analytical, universal).

% Would object if they were in the definitional room: the mathematical-definition reading treats the thesis as closed definitional truth, while quantum researchers ask whether quantum processes compute functions outside the Turing-computable class. Under this reading, their question is misdirected—the thesis is a definition, not an empirical claim about quantum systems.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, quantum_computing_researchers, excluded,
    institutional, biographical, constrained, global).

% Hold that the thesis is an empirical claim about what the physical universe permits. They are excluded from the mathematical-definition reading's conversation because that reading explicitly brackets the physical claim. Their position is instantiated in the physical_claim_reading constraint story.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, physical_universalists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, formal definition of 'effective computability' across multiple mathematical formalisms (Turing machines, lambda calculus, recursive functions, Post machines, Markov algorithms, etc.). All formalisms converge on the same set of functions; the thesis codifies this convergence as the meaning of the term. The coordination solves the problem of talking about computability without ambiguity—mathematicians can reason about computable vs. uncomputable functions without specifying which formal model they are using.
% TRANSFER_FUNCTION: No transfer occurs. This reading treats the thesis as a definition, not an exchange. There is no extraction, no victim set, no asymmetric cost. Clarity and shared terminology are public goods available to all participants in the mathematical community.
% ABSENT_VOICES: Researchers who hold the physical-claim reading (physicalists who believe the thesis makes an empirical claim about the universe) and those who hold the epistemological-boundary reading (who locate the thesis's truth in the structure of formal proof, not convention) are structurally excluded from this reading's conversation. Under the mathematical-definition reading, their positions are on a different axis (empirical vs. definitional, or epistemic vs. conventional) and cannot be reconciled within a single framework that treats the thesis as pure convention.
% DISAPPEARANCE_RATIONALE: If this definition were dropped, the mathematical facts about what functions are Turing-computable would remain unchanged. Only the agreed-upon label for the class would vanish. Mathematicians could still reason about the same phenomena using Church's formulation or Turing's formulation separately, but they would lose the convenient name and the symbolic coordination that 'the Church-Turing thesis' provides. The disappearance would be inconvenient, not catastrophic—the world of computation continues; only the terminology is disrupted.
% FOUNDING_PROBLEM: In the 1930s, Church, Gödel, and Turing independently proposed formal models of computation (lambda calculus, recursive functions, Turing machines). All three captured the same class of functions. The founding problem was: should we define 'effective computability' by reference to one model, or by the convergence across models? The thesis emerged as a answer: we define it by convergence—whichever formalism we use, if a function is computable in one, it is computable in all.
% FOUNDING_PROBLEM_CORROBORATION: The convergence of formalisms remains a live fact: every new model of computation proposed since the 1930s (abstract reduction systems, cellular automata, register machines, etc.) has either reduced to Turing-computability or has been shown to be uncomputable by Turing standards. Independent verification comes from computer science textbooks, mathematical logic, and the empirical track record of algorithm theory. The reading attests the founding problem is solved—we have a stable, working definition—and the thesis is now simply the name we give to that definition.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_unchanged).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the thesis generates no coercive transfer or asymmetric benefit under this reading. All participants—mathematicians, logicians, computer scientists—benefit equally from the shared terminology. There is no rent collection, no exclusion mechanism, no victim set. Suppression is low (0.12) because the definition requires no active enforcement—it is self-stabilizing through consensual use. Theater is minimal (0.05) because the definition's function is purely symbolic; no performative maintenance is required. Accessibility collapse is high (0.92) because once the thesis is understood as a definition (not an empirical claim), alternatives collapse: one cannot simultaneously hold that the thesis is a pure definition AND that it is an empirical claim about physical reality—that is a categorical distinction, not a negotiable one. Resistance is low (0.15) because the definition is not actively defended against; it is simply the standard term mathematicians use. The small resistance reflects philosophical debate about whether the definition reading is the correct reading, not resistance to the definition itself.
 *
 * PERSPECTIVAL GAP:
 *   All seats (logicians, mathematicians, philosophers) compute this constraint identically under the definition reading: they all perceive it as a beneficial, non-coercive, terminological agreement. There is no perspectival divergence because there is no asymmetry. The excluded seats (quantum researchers, physical universalists) compute a DIFFERENT constraint (the physical_claim_reading or epistemological_boundary_reading), not a different seat in the same constraint. This is the key feature of the definition reading: it forecloses the empirical question entirely by reframing what the thesis is ABOUT. Seats that contest the definition reading are not holding a different position within the same constraint—they are holding a different constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply meaningfully under this reading because there is no asymmetric distribution of costs and benefits. The mathematical-definition reading has no victims (definitions cannot be violated) and no targets (there is no extraction). All beneficiaries are beneficiaries equally; the collective-action problem is solved symmetrically. Directionality would arise only if the definition reading were contested—if some mathematicians refused the definition or if the definition forced costs onto some parties. Under the thesis-as-definition, neither is the case. The constraint is purely coordinative, making directionality analytically close to d=0.5 (symmetric) for all parties, but since there is no extraction, the usual directionality scaling does not fire.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply. The thesis-as-definition has no mandate that could outlive its function. The function IS the definition itself—to specify what we mean by 'effective computability.' As long as mathematicians care about computability, the definition's function is live. If mathematics were to abandon the study of computability (an anthropologically remote prospect), the definition would simply cease to be used, not persist as a zombie mandate. The constraint's persistence is pure function-fit, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_discovery_ambiguity,
    'Is the thesis a definition we stipulated by convention, or a discovery about the deep structure of computation that happens to match our definitions?',
    'Historical and philosophical analysis of the intentions of Church, Turing, and Gödel; examination of whether the convergence of formalisms is explained by independent convergence on the same mathematical truth or by the formalisms being formal encodings of a pre-existing concept.',
    'If the thesis is purely definitional (this reading), it is true by convention and cannot be empirically falsified. If it is a discovery, it is subject to empirical challenge (e.g., from quantum computing or hypercomputation), and this reading would collapse into or be superseded by the physical_claim_reading. The reading itself depends on holding the definitional framing stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vs_discovery_ambiguity, conceptual, 'Whether the convergence of formalisms reflects convention or prior mathematical truth.').

omega_variable(
    sibling_reading_boundary,
    'Can the same kernel admit both the definition reading (this constraint) and the physical_claim_reading as valid simultaneous readings, or do they foreclose each other?',
    'Logical analysis of whether a single logical system or framework can simultaneously hold ''the thesis is a definition'' and ''the thesis is an empirical claim about physical computation'' without contradiction.',
    'If they foreclose each other, the two readings are incommensurable and cannot coexist in a single framework—a party holding both would be incoherent. If they coexist, the kernel is genuinely polysemous and the readings are not readings of the same constraint but of different constraints. This affects how the constraint family is structured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Logical compatibility of the definition reading with the physical-claim reading.').

omega_variable(
    quantum_challenge_relevance,
    'If quantum computers demonstrably compute functions outside the Turing-computable set, does this falsify the thesis under the definition reading?',
    'Theoretical and empirical: (1) if quantum systems compute superTuring functions, and (2) if that fact is deemed relevant to the definition of ''effective computability,'' then the definition would need revision. If the definition reading maintains that quantum computation is simply outside the scope of ''effective computability'' by convention, the quantum fact is irrelevant.',
    'If the definition reading can be revised by empirical facts, it is not purely definitional—it becomes responsive to empirical discovery, collapsing the distinction between this reading and the physical_claim_reading. If the definition reading is rigid to empirical revision, it becomes increasingly incoherent as computational practice evolves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_challenge_relevance, empirical, 'Whether quantum computation can falsify the mathematical definition reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement_basis(chur_tr_t1936, projected).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement_basis(chur_tr_t1950, observed).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1970, 0.04).
narrative_ontology:measurement_basis(chur_tr_t1970, observed).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement_basis(chur_tr_t2000, observed).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.05).
narrative_ontology:measurement_basis(chur_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.08).
narrative_ontology:measurement_basis(chur_be_t1936, projected).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1950, 0.07).
narrative_ontology:measurement_basis(chur_be_t1950, observed).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1970, 0.06).
narrative_ontology:measurement_basis(chur_be_t1970, observed).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement_basis(chur_be_t2000, observed).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2026, 0.08).
narrative_ontology:measurement_basis(chur_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.15).
narrative_ontology:measurement_basis(chur_su_t1936, projected).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement_basis(chur_su_t1950, observed).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement_basis(chur_su_t1970, observed).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(chur_su_t2000, observed).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(chur_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel admits three structurally distinct readings: (1) mathematical-definition (this story), (2) physical-claim (sibling), and (3) epistemological-boundary (sibling). The readings are linked by network.affects_constraints because they occupy the same kernel but make different categorical claims. The definition reading forecloses the physical reading within its own frame (by stipulating the thesis is not an empirical claim); the physical reading treats the definition reading as incomplete; the epistemological reading treats both as missing the foundational point about formal proof. All three stories should exist in the corpus as separate constraints with their own ε values, beneficiary/victim structures, and classification dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
