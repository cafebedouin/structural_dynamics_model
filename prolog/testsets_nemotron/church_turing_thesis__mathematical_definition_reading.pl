% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint story captures the mathematical_definition_reading of the
 *   Church-Turing thesis: the thesis is a stipulative mathematical definition
 *   that fixes the meaning of 'effective computability' by convention, not an
 *   empirical claim about physical reality. As a definition, it cannot be
 *   violated — one either uses the term 'effectively computable' to mean
 *   'Turing-computable' or one uses a different term. The constraint
 *   coordinates terminology across mathematics and computer science,
 *   providing a stable reference point for proofs, algorithms, and complexity
 *   theory. Its extraction is negligible (ε = 0.02) because no party is
 *   coerced; deviation simply means speaking a different language. The thesis
 *   emerged from the natural convergence of multiple formalisms (Turing
 *   machines, lambda calculus, recursive functions) and persists because it
 *   solves a genuine coordination problem: aligning what different
 *   researchers mean by 'computable'.
 *
 * KEY AGENTS:
 *   - mathematical_community: Primary beneficiary (institutional/analytical) — gains terminological clarity and proof coherence
 *   - computer_science_community: Primary beneficiary (institutional/analytical) — gains stable foundation for algorithms and complexity
 *   - physical_claim_reading_proponents: Analytical observers — contest whether the thesis has empirical content
 *   - epistemological_boundary_reading_proponents: Analytical observers — contest whether the thesis marks a boundary of knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.01).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, mountain).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

domain_priors:emerges_naturally(church_turing_thesis__mathematical_definition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '7b0e1e36-8c8f-4320-a572-f9db677180b5').
narrative_ontology:cs_kernel_codification('7b0e1e36-8c8f-4320-a572-f9db677180b5', formalized).
narrative_ontology:cs_authority_grounding('7b0e1e36-8c8f-4320-a572-f9db677180b5', expertise).
narrative_ontology:cs_interpretation_layer_present('7b0e1e36-8c8f-4320-a572-f9db677180b5').
narrative_ontology:cs_reading_relation('7b0e1e36-8c8f-4320-a572-f9db677180b5', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b0e1e36-8c8f-4320-a572-f9db677180b5', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7b0e1e36-8c8f-4320-a572-f9db677180b5', foundational, effective_computability_defined_as_turing_computable).
narrative_ontology:cs_axiom_status(effective_computability_defined_as_turing_computable, holdable).
narrative_ontology:cs_axiom_grounding('7b0e1e36-8c8f-4320-a572-f9db677180b5', effective_computability_defined_as_turing_computable, conventional).
narrative_ontology:cs_axiom('7b0e1e36-8c8f-4320-a572-f9db677180b5', secondary, formalisms_converge_naturally).
narrative_ontology:cs_axiom_status(formalisms_converge_naturally, holdable).
narrative_ontology:cs_axiom_grounding('7b0e1e36-8c8f-4320-a572-f9db677180b5', formalisms_converge_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('7b0e1e36-8c8f-4320-a572-f9db677180b5', turing_1936_convergence).
narrative_ontology:cs_drift_state('7b0e1e36-8c8f-4320-a572-f9db677180b5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7b0e1e36-8c8f-4320-a572-f9db677180b5', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_science_community).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, mathematical_definition_stipulation).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, terminological_coordination_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mathematicians and logicians who use the Church-Turing definition as the standard for 'effective computability' in proofs, recursion theory, and foundations. They gain a shared language that makes results comparable and cumulative. Exit is analytical: a mathematician could adopt a different definition (e.g., for hypercomputation) but would then be speaking a different dialect, excluded from mainstream discourse without translation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_community, beneficiary,
    institutional, generational, analytical, universal).

% Computer scientists who build algorithms, complexity theory, and programming language semantics on the Turing-computable foundation. The definition provides a stable, uncontested basis for 'what can be computed' — enabling curriculum, hiring, publication standards, and interoperable reasoning. Exit is analytical: researchers working on quantum computing, analog computation, or hypercomputation use extended models but must translate back to the standard definition to communicate with the field.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_science_community, beneficiary,
    institutional, generational, analytical, universal).

% Physicists and philosophers who read the Church-Turing thesis as an empirical claim about the universe (e.g., Deutsch's Church-Turing Principle). They analyze whether physical processes can exceed Turing computability. They neither benefit from nor pay for the definition — they contest its empirical scope. Their role is analytical observation of a different reading of the same kernel.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, physical_claim_reading_proponents, observer,
    analytical, biographical, analytical, universal).

% Philosophers and logicians who read the thesis as marking the boundary of what can be formally proven computable (e.g., Kreisel, Gandy). They analyze the relationship between formal provability and the informal notion of effective procedure. Like the physical_claim_reading proponents, they are analytical observers of a different constraint instantiated from the same kernel.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, epistemological_boundary_reading_proponents, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable terminological standard for 'effective computability' across mathematics and computer science, enabling cumulative proof, algorithm design, and complexity classification without perpetual translation between equivalent formalisms (Turing machines, lambda calculus, recursive functions).
% TRANSFER_FUNCTION: Moves nothing of material value. The arrangement transfers terminological authority: the community agrees to use 'effectively computable' to mean 'Turing-computable', and in exchange gains a shared language. No money, work, or status flows coercively.
% ABSENT_VOICES: Researchers in hypercomputation, oracle machines, and analog computation who reject the identification of 'effective computability' with Turing computability. They are not excluded from discourse — they publish in specialized venues — but they are linguistically isolated from the mainstream because they must constantly translate their results into the standard terminology to be understood.
% DISAPPEARANCE_RATIONALE: If the Church-Turing definition vanished overnight, mathematics and computer science would lose their shared foundation for 'what is computable'. Proofs would need to specify their computational model explicitly; complexity classes would fragment; algorithm textbooks would require model parameters. The field would reorganize around multiple competing definitions or a new consensus — a substantial rearrangement of terminology and pedagogy.
% FOUNDING_PROBLEM: In the 1930s, multiple formalisms (Turing machines, lambda calculus, general recursive functions, Post canonical systems) were proposed to capture the intuitive notion of 'effective procedure'. They were proven equivalent, but no single definition had terminological authority. The field needed a convention to stabilize discourse.
% FOUNDING_PROBLEM_CORROBORATION: The convergence of formalisms in the 1930s (Turing 1936, Church 1936, Kleene 1936, Post 1936) is documented in the historical record by participants (Davis 1958, 1982; Gandy 1988) and historians (Copeland 2004; Sieg 1994) outside the benefiting communities. The founding problem — 'which formalism defines effective computability?' — was resolved by the equivalence proofs and community convergence on Turing's formulation as the most intuitive. No living participant attests the problem is still open; the definition is universally adopted in standard curricula and literature.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, ExtMetricName, E),
    domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(church_turing_thesis__mathematical_definition_reading),
    narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The mathematical_definition_reading has ε = 0.02 because the definition itself extracts nothing — it is a convention. Suppression is near zero (0.01) because no one is prevented from working with alternative definitions (e.g., hypercomputation, oracle machines); they simply use different terminology. Theater ratio is low (0.05) because the definition performs its coordination function without performative maintenance. Accessibility collapse is very high (0.95) because once the definition is adopted, alternatives that reject it are linguistically isolated — they cannot participate in standard mathematical discourse without translating. Resistance is negligible (0.03) because the definition was adopted by consensus across mathematical logic and computer science, not imposed. The claimed_type is Mountain because the definition emerges naturally from the convergence of formalisms and functions as a terminological bedrock.
 *
 * PERSPECTIVAL GAP:
 *   From the mathematical_community and computer_science_community seats, the constraint is a Mountain — a stable, natural definition that enables coordination. From the physical_claim_reading_proponents seat, the same kernel produces a constraint with higher extraction (the empirical claim that no physical process exceeds Turing computability). The engine will compute per-seat classifications from the structural data; this reading's structural data reflects the mathematical_definition_reading's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the mathematical_community and computer_science_community — both are institutional/analytical agents who gain terminological coordination without bearing costs. No victims exist because definitions cannot be violated; alternative formalisms (hypercomputation, oracle machines) are not suppressed, they simply operate under different terminology. The directionality derivation yields d ≈ 0.05 for beneficiaries (full beneficiary end) and no target agents. The d_value for analytical observers is near 0.5 (symmetric) because they neither benefit nor pay — they analyze.
 *
 * MANDATROPHY ANALYSIS:
 *   The definition's founding problem — 'what do we mean by effective computability?' — was live in the 1930s when multiple formalisms converged. That founding problem is now dead (the definition is settled), but the arrangement does not persist as a zombie because it continues to serve an active coordination function. The definition is not mandatrophy; it is a genuine Mountain that solved its founding problem and remains the coordination standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_definition,
    'Is the Church-Turing thesis a genuine natural law (as some physical interpretations claim) or a constructed mathematical definition that benefits identifiable communities?',
    'Distinguish the mathematical_definition_reading (this story) from the physical_claim_reading; the former treats the thesis as a convention with ε ≈ 0, the latter as an empirical claim with higher extraction. The existence of competing readings with different ε values over the same kernel is the structural evidence for false-summit ambiguity.',
    'If the thesis is a natural law, the mathematical_definition_reading correctly classifies as Mountain with ε ≈ 0. If it is a constructed definition masquerading as natural law, the mathematical_definition_reading is a false summit and the physical_claim_reading captures the extraction. FSM signature evaluates Mountain constraints with declared beneficiaries — this reading declares beneficiaries, triggering FSM.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_definition, conceptual, 'Whether the mathematical_definition_reading is a genuine Mountain or a false summit masking extraction in the physical_claim_reading.').

omega_variable(
    reading_framing_underdetermination,
    'Does the mathematical_definition_reading represent the only defensible framing of the Church-Turing thesis as a definition, or could an alternative definition (e.g., recursive function theory, lambda calculus) serve the same coordination function with different boundary implications?',
    'Compare the coordination function across different mathematical formalisms that all claim to capture ''effective computability''. If the coordination function is stable across formalisms, the definition is robust; if different formalisms produce different boundary cases, the definition is underdetermined.',
    'If the definition is underdetermined, the constraint''s emerges_naturally claim weakens — the ''natural'' convergence of formalisms (Turing machines, lambda calculus, recursive functions) may be a retrospective narrative rather than a discovered necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the definition''s naturalness is robust across equivalent formalisms or fragile to framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(chur_tr_t2025, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.02).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1970, 0.02).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(chur_be_t2025, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2025, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.01).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the church_turing_thesis constraint family. The mathematical_definition_reading (this story) has ε ≈ 0 and claims Mountain. The physical_claim_reading has substantially higher ε (empirical claim that can be violated by physical discovery) and likely claims Tangled Rope or Snare. The epistemological_boundary_reading sits between — it marks a boundary of proof but not of physical possibility. The three stories share the kernel but instantiate different constraints with different ε, beneficiaries, and types. The mathematical_definition_reading is upstream: its convergence-of-formalisms result is often cited as evidence for the physical_claim_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__mathematical_definition_reading, analytical, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
