% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Church-Turing Thesis (Mathematical Definition Reading)
 *   domain: philosophy of mathematics/foundations of computation
 *
 * SUMMARY:
 *   The Church-Turing thesis under the mathematical-definition reading is a
 *   stipulative definition: the thesis defines 'effective computability' as
 *   precisely equivalent to Turing-machine computability. This reading treats
 *   the thesis as true by convention, not by empirical discovery or logical
 *   deduction. No party can violate a definition; definitions can only be
 *   accepted, refined, or replaced. The mathematical-definition reading is
 *   one of three core readings of this kernel. The physical-claim reading
 *   treats the thesis as an empirical claim about the universe's
 *   computational limits. The epistemological-boundary reading treats it as
 *   marking the boundary of formally knowable computation. This story
 *   instantiates the mathematical-definition reading only.
 *
 * KEY AGENTS:
 *   - Mathematical logic community: maintains the definition and enforces consensus on its reading
 *   - Theoretical computer scientists: benefit from shared terminology and stable proof frameworks
 *   - Mathematical philosophers: benefit from clarity about the definition-vs-claim distinction
 *   - Quantum researchers: excluded because the definition is stipulated, not empirically contested
 *   - Physical computation researchers: excluded because their empirical framing contradicts the definitional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.08).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis (Mathematical Definition Reading)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy of mathematics/foundations of computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '8c98030a-c15e-4283-b2c3-ee1f20ca3690').
narrative_ontology:cs_kernel_codification('8c98030a-c15e-4283-b2c3-ee1f20ca3690', fixed_text).
narrative_ontology:cs_authority_grounding('8c98030a-c15e-4283-b2c3-ee1f20ca3690', lineage).
narrative_ontology:cs_interpretation_layer_present('8c98030a-c15e-4283-b2c3-ee1f20ca3690').
narrative_ontology:cs_reading_relation('8c98030a-c15e-4283-b2c3-ee1f20ca3690', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c98030a-c15e-4283-b2c3-ee1f20ca3690', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('8c98030a-c15e-4283-b2c3-ee1f20ca3690', foundational, computability_is_stipulated).
narrative_ontology:cs_axiom_status(computability_is_stipulated, holdable).
narrative_ontology:cs_axiom_grounding('8c98030a-c15e-4283-b2c3-ee1f20ca3690', computability_is_stipulated, conventional).
narrative_ontology:cs_axiom('8c98030a-c15e-4283-b2c3-ee1f20ca3690', secondary, formalizations_are_equivalent).
narrative_ontology:cs_axiom_status(formalizations_are_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('8c98030a-c15e-4283-b2c3-ee1f20ca3690', formalizations_are_equivalent, empirically_contingent).
narrative_ontology:cs_reference_frame('8c98030a-c15e-4283-b2c3-ee1f20ca3690', mathematical_definition_of_computability).
narrative_ontology:cs_drift_state('8c98030a-c15e-4283-b2c3-ee1f20ca3690', contemporary_quantum_computing_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8c98030a-c15e-4283-b2c3-ee1f20ca3690', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_practice_and_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_philosophers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, formal_verification_engineers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, student_populations).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formal_computability_thesis).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, recursive_function_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and teaches the definition of 'effective computability' as Turing-machine computable. They adjudicate disputes about what counts as a valid formalization of the concept and coordinate the terminology across logic, computer science, and mathematics. The community accepts the thesis as a stipulative definition and maintains consensus on this reading.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community, agenda_setter,
    institutional, generational, analytical, universal).

% Benefit from a stable, widely-accepted mathematical definition of computability that enables rigorous proofs about what can and cannot be computed. The definition provides a shared framework for research, publication, and education without requiring empirical validation of each claim.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists, beneficiary,
    institutional, generational, analytical, universal).

% Benefit from conceptual clarity about the thesis as a definition rather than an empirical hypothesis, which clarifies the nature of the claims that can be made about computation. They also observe and analyze the reading itself as a positioning choice within a larger kernel of debate.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_philosophers, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__mathematical_definition_reading, mathematical_philosophers, observer).

% Would argue that the empirical boundaries of computability may be contestable if quantum processes yield genuinely novel computational capabilities. They are structurally excluded from the mathematical-definition reading because that reading treats 'effective computability' as stipulated, not discovered, so empirical findings about quantum systems cannot constrain the definition itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, quantum_computing_researchers, excluded,
    institutional, generational, analytical, universal).

% Would argue that the thesis makes an empirical claim about what is physically possible to compute, and therefore can be falsified by physical evidence. They are excluded from the mathematical-definition reading because that reading structurally denies the empirical contestability the physical reading asserts.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, physical_computation_researchers, excluded,
    institutional, generational, analytical, universal).

% Benefit from a definition of computability that grounds formal verification practices. The thesis-as-definition provides the conceptual foundation for proofs about program correctness and computational bounds without requiring empirical validation of the definition itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, formal_verification_engineers, beneficiary,
    organized, biographical, analytical, universal).

% Benefit from learning a stable, universally-taught definition of computability that permits entry into formal computer science and mathematical logic. The reading-as-definition simplifies pedagogy: the thesis is taught as a stipulation, not as an empirical claim requiring experimental evidence.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, student_populations, beneficiary,
    powerless, biographical, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared mathematical definition of 'effective computability' — all instances of Turing-computable functions and only such functions count as effectively computable. This enables the global mathematical and computer science communities to use the same terminology, prove theorems with the same scope, and compare results without ambiguity about what 'computable' means.
% TRANSFER_FUNCTION: No transfer function: the definition is a stipulation, not an extraction. The thesis moves no resources, benefits, or liabilities from one agent to another. What moves is conceptual clarity and shared terminology.
% ABSENT_VOICES: Researchers pursuing quantum computing and hypercomputation are structurally excluded from the mathematical-definition reading because that reading treats the boundary of 'effective computability' as a matter of definition, not empirical discovery. Physical computation researchers who dispute whether the thesis is really a definition or an empirical claim are also excluded from THIS reading, though their objection is the sibling reading itself (physical_claim_reading).
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, mathematical logic and computer science would need to negotiate a new shared definition of computability. Research would not stop, but papers would need to specify which notion of computability they were invoking (Turing, lambda, register machines, etc.), proofs would lose some universality, and textbooks would splinter into multiple pedagogical frameworks. The mathematical communities that depend on the definition would reorganize around alternative definitional choices or fragmented local conventions.
% FOUNDING_PROBLEM: In the 1930s, the concept of 'effective computability' (what a human being with paper and pencil could calculate in finite time following a finite set of rules) was informally understood but lacked a precise mathematical definition. Multiple formalization attempts (Turing machines, lambda calculus, recursive functions, Post systems) were proposed. The founding problem was: how do we mathematically define 'effective computability' such that all intuitive notions of effective procedure are captured?
% FOUNDING_PROBLEM_CORROBORATION: The mathematical logic community (Church, Turing, Gödel, Post, and contemporary logicians) attests that the thesis successfully solves this problem — all proposed formalizations are equivalent and provably capture the informal notion. Philosophers and computer scientists who teach the thesis as a definition (rather than an empirical claim) corroborate that it provides the shared mathematical framework the founding problem sought. No party outside the defining authority (independent corroborators with no stake in the definition) has independently validated this reading; the corroboration is internal to the mathematical community.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.08 at interval end) because a mathematical definition does not extract value in the way constraints usually do. The definition stipulates meaning; it does not coerce actors or allocate scarce resources. The small non-zero value reflects the minimal coordination overhead: the mathematical community must invest effort in teaching, maintaining consensus, and occasionally defending the definition against alternative readings. Suppression is negligible (0.02) because acceptance of a definition is voluntary participation in a shared convention, not enforced compliance. Theater ratio is very low (0.05) because the definition's function — to establish shared terminology — is entirely genuine; very little of the maintenance activity is performative or defensive. Accessibility collapse is very high (0.92) because once the definition is understood and accepted, alternatives (using different formalizations without unified terminology) are nearly completely ruled out as practical options within the mathematical community. Resistance is low (0.08) because the definition meets almost no real opposition within the mathematical community; opposition comes from the sibling readings (physical, epistemological), which contest the reading itself rather than the definition's internal consistency.
 *
 * PERSPECTIVAL GAP:
 *   From the mathematical logic community's perspective, this constraint is pure beneficial coordination — shared terminology enables their work. From the perspective of quantum computing and physical computation researchers, the same constraint enforces a particular reading of the thesis and excludes their empirical framework. The gap is not between positions on the constraint itself but between different framings of what the thesis IS. The mathematical-definition reading computes uniformly across all seats within that reading; the perspectival gap is the gap between readings, not within this one.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents listed have directionality near the beneficiary pole (d ≈ 0.1–0.3 range) because acceptance of the definition is voluntary and profitable for their mathematical work. No agent is trapped or coerced into accepting the definition; exclusion (quantum researchers, physical computation researchers) is structural to the reading itself, not enforced extraction. The community stakes (mathematical logic, theoretical CS) benefit from clarity and shared terminology without bearing extractive costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy. The founding problem (how to define 'effective computability' mathematically) remains live: the definition is continuously re-taught, validated in new contexts, and refined as new formalizations emerge. The definition's founding mandate — to capture all intuitive notions of effective procedure — is still actively pursued and is not obsolete or vestigial. This is a genuine rope constraint without extraction decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_discovery_boundary,
    'Is the Church-Turing thesis genuinely a matter of stipulated definition, or does it express a discovered fact about the structure of computability that happens to be stated in definitional language?',
    'Conceptual analysis of whether the equivalence of Turing machines, lambda calculus, and recursive functions is: (a) a consequence of the definitions chosen (making it definitional), or (b) a substantive fact about computational structure that any adequate definition must capture (making it discovery-like). Philosophical examination of whether accepting the definition commits one to any non-tautological empirical claims.',
    'If the thesis is genuinely definitional (tautological given the definitions), then it is true by convention and carries no empirical content. If it expresses discovered structure, then the mathematical-definition reading mischaracterizes the thesis and the physical-claim reading gains force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_vs_discovery_boundary, conceptual, 'Whether the thesis is purely stipulative or expresses discovered mathematical structure.').

omega_variable(
    definition_stability_across_contexts,
    'Does the mathematical-definition reading of the thesis remain stable and uncontested as new computational paradigms emerge (quantum, hypercomputational, analog)?',
    'Observation of whether the mathematical community maintains the definition''s scope or redefines ''effective computability'' when empirical evidence or new formalizations challenge Turing''s boundaries. If new paradigms force definitional revision, the stability of the ''true by convention'' reading is in question.',
    'If the definition proves stable across new paradigms, the mathematical-definition reading is validated. If the definition is revised or reinterpreted under empirical pressure, the reading''s status shifts — it was a contingent choice, not an inevitable definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_stability_across_contexts, empirical, 'Whether the definition remains stable as computational science evolves.').

omega_variable(
    kernel_reading_distinction_among_pedagogues,
    'Do different teaching communities (logic, computer science, philosophy) teach the thesis under different readings — some as definition, some as empirical claim, some as epistemological boundary — and if so, does this signal that no single reading captures the thesis?',
    'Systematic survey of how the thesis is presented in textbooks across the three disciplines and explicit interviews with instructors about which reading they adopt.',
    'Widespread divergence in how the thesis is taught would suggest that the thesis is genuinely kernel-like (a contested commitment stable under multiple readings) rather than settled into a single mathematical-definition reading. The consensus of the mathematical logic community might reflect institutional dominance rather than truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction_among_pedagogues, empirical, 'Whether pedagogical and disciplinary divergence reveals hidden reading structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(chur_tr_t20, church_turing_thesis__mathematical_definition_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(chur_tr_t40, church_turing_thesis__mathematical_definition_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__mathematical_definition_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(chur_tr_t80, church_turing_thesis__mathematical_definition_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(chur_tr_t100, church_turing_thesis__mathematical_definition_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(chur_be_t20, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(chur_be_t40, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(chur_be_t80, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(chur_be_t100, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 100, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(chur_su_t20, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 20, 0.01).
narrative_ontology:measurement(chur_su_t40, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 40, 0.02).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 60, 0.02).
narrative_ontology:measurement(chur_su_t80, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 80, 0.02).
narrative_ontology:measurement(chur_su_t100, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.03).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel decomposes into three structurally distinct constraint stories, one per reading. Each story has a different ε, beneficiary structure, and type. The mathematical-definition reading (this story) treats the thesis as stipulation (low ε, pure coordination, rope). The physical-claim reading treats it as empirical (higher ε, contestable, likely snare or tangled-rope). The epistemological-boundary reading treats it as marking the boundary of formal knowability (moderate ε, coordination around epistemological limits). All three readings reference the same kernel (the identity of 'effective computability' and Turing computability) but differ in what that identity means. They are linked via network.affects_constraints to indicate that a shift in one reading's status (e.g., falsification of the physical reading) would affect the perceived legitimacy of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
