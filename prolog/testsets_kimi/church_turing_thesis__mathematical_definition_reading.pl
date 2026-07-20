% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Church-Turing Thesis as Mathematical Definition
 *   domain: philosophy_of_mathematics/computation_foundations
 *
 * SUMMARY:
 *   The Church-Turing thesis, under the mathematical definition reading, is
 *   treated not as an empirical claim about physical machines or an
 *   epistemological limit, but as a stipulative definition: 'effectively
 *   computable' means computable by a Turing machine (or equivalent
 *   formalism). This reading interprets the thesis as a convention that
 *   coordinates mathematical practice, generating shared terminology without
 *   coercion or extraction. As a kernel reading, it is one of three competing
 *   interpretations of the same historical texts; the structural data reflect
 *   only the definitional reading and are independent of the sibling
 *   readings.
 *
 * KEY AGENTS:
 *   - mathematical_community: Primary beneficiary (organized/mobile) â gains terminological alignment across subfields
 *   - computability_theorists: Primary beneficiary (moderate/mobile) â gains precise extension for proofs and classifications
 *   - hypercomputation_advocates: Excluded voice (moderate/constrained) â argues for non-Turing computation but is sidelined by convention
 *   - philosophers_of_computation: Analytical observer (analytical/analytical) â studies meta-status of the thesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.08).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/computation_foundations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'ab11de3d-a982-4c6e-bb68-81e42139e247').
narrative_ontology:cs_kernel_codification('ab11de3d-a982-4c6e-bb68-81e42139e247', fixed_text).
narrative_ontology:cs_authority_grounding('ab11de3d-a982-4c6e-bb68-81e42139e247', expertise).
narrative_ontology:cs_interpretation_layer_present('ab11de3d-a982-4c6e-bb68-81e42139e247').
narrative_ontology:cs_reading_relation('ab11de3d-a982-4c6e-bb68-81e42139e247', church_turing_thesis__physical_claim_reading, forecloses).
narrative_ontology:cs_reading_relation('ab11de3d-a982-4c6e-bb68-81e42139e247', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('ab11de3d-a982-4c6e-bb68-81e42139e247', foundational, effective_computability_is_stipulative).
narrative_ontology:cs_axiom_status(effective_computability_is_stipulative, holdable).
narrative_ontology:cs_axiom_grounding('ab11de3d-a982-4c6e-bb68-81e42139e247', effective_computability_is_stipulative, conventional).
narrative_ontology:cs_axiom('ab11de3d-a982-4c6e-bb68-81e42139e247', secondary, formal_extension_determines_meaning).
narrative_ontology:cs_axiom_status(formal_extension_determines_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ab11de3d-a982-4c6e-bb68-81e42139e247', formal_extension_determines_meaning, conventional).
narrative_ontology:cs_reference_frame('ab11de3d-a982-4c6e-bb68-81e42139e247', stipulative_definition_framework).
narrative_ontology:cs_drift_state('ab11de3d-a982-4c6e-bb68-81e42139e247', contemporary_philosophy_of_computation, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ab11de3d-a982-4c6e-bb68-81e42139e247', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computability_theorists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, classical_computability_theory).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formalism_in_mathematics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses 'effectively computable' as a precise technical term grounded in Turing machines and lambda calculus, avoiding the need to repeatedly stipulate formal definitions in every paper and lecture. Benefits from interoperability of theorems and proofs across computability theory, logic, and theoretical computer science.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_community, beneficiary,
    organized, civilizational, mobile, universal).

% Builds classifications, proofs, and textbooks on the shared definition. Could adopt alternative equivalent formalisms (register machines, general recursion) but these are provably coextensive and serve the same coordinating function; the definition eliminates redundant stipulation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computability_theorists, beneficiary,
    moderate, generational, mobile, universal).

% Argue for computation beyond Turing limits and would prefer that 'computable' be reserved for physically realizable processes or notional hypermachines. They are marginalized in standard computability curricula and textbooks not by active enforcement but by the definitional entrenchment of the conventional reading.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, hypercomputation_advocates, excluded,
    moderate, biographical, constrained, global).

% Analyze whether the thesis is a definition, an empirical claim, or an epistemological boundary. They do not depend on the thesis for technical mathematical work but study its meta-mathematical status and its relationship to physical possibility.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, precise extension for the informal notion of 'effective computability', aligning terminology across logic, mathematics, and computer science so that researchers do not need to restate formal prerequisites in every communication.
% TRANSFER_FUNCTION: Moves the burden of formal specification from individual papers and lectures to a single, community-endorsed convention; transfers epistemic confidence from intuitive calculation to rigorous proof.
% ABSENT_VOICES: Hypercomputation researchers and non-standard computation theorists who treat physical or notional machines beyond Turing limits as relevant to the extension of 'computable'; they are absent from standard curricula and textbooks not by enforcement but by definitional entrenchment.
% DISAPPEARANCE_RATIONALE: If the definitional convention vanished overnight, computability theory would lose its standard reference point; textbooks, proofs, and cross-paper communication would fragment into competing stipulations, forcing a costly re-coordination of the field around a new convention or a return to perpetual explicit formalism.
% FOUNDING_PROBLEM: Before the 1930s, 'effectively computable' was an informal, pre-theoretic notion without a precise extension; researchers lacked a shared formal referent for algorithmic solvability, impeding systematic proof and communication.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics (e.g., Soare, Sieg) and philosophers of computation attest that pre-thesis computability was informal; contemporary textbooks and encyclopedia entries outside the immediate beneficiary community treat the formal definition as settled convention.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.08, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-zero (0.08) because a definition does not extract from its users; it is adopted by convention and can be abandoned or supplemented without penalty. Suppression is negligible (0.05) because no agent is coerced into using the definition; alternatives are technically possible but practically unused. Theater ratio is minimal (0.05) as there is no performative maintenance. Accessibility collapse is high (0.82) because once the convention is understood, alternative informal definitions effectively disappear from standard discourse, though they are not barred. Resistance is negligible (0.05). The flat measurement series confirms stability over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The mathematical community experiences this constraint as pure coordination: a useful abbreviation that simplifies discourse. A philosopher of computation treating it as an empirical claim would experience it as a contested boundary. The engine computes this divergence from structural data; the authored claim does not adjudicate between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The mathematical community and computability theorists are beneficiaries of the convention (low directionality). No victim group is declared because a definition cannot extract; hypercomputation advocates are excluded not by the constraint's operation but by the convention's entrenchment. Directionality is derived from beneficiary status plus mobile exit options, placing both beneficiary seats near the full-beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling because its founding problem (lack of precise definition) is genuinely solved and the arrangement persists only as long as it remains useful. If mathematicians stopped finding it convenient, the convention would atrophy naturally without institutional residue; there is no enforcement machinery to become theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_status_contest,
    'Is the Church-Turing thesis a stipulative definition, an empirical claim, or an epistemological boundary?',
    'Philosophical analysis of the thesis''s use in mathematical practice versus physical theorizing; examination of whether purported counterexamples (hypercomputation) are treated as falsifications or as changes of subject.',
    'Resolves which reading of the kernel is operative; if definitional, the constraint remains a low-extraction rope; if empirical, it becomes a mountain or snare depending on physical possibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_status_contest, conceptual, 'The fundamental ambiguity in the kernel''s epistemic status').

omega_variable(
    definition_vs_empirical_foreclosure,
    'Does treating the thesis as a stipulative definition logically foreclose treating it as an empirical claim about physical computability?',
    'Meta-mathematical analysis of whether the same proposition can be both a convention and a falsifiable physical hypothesis within a single framework.',
    'If foreclosure holds, the mathematical definition reading and physical claim reading are mutually exclusive; if not, they coexist as different emphases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vs_empirical_foreclosure, conceptual, 'Whether the definitional reading excludes the empirical reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_math_def_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ctt_math_def_tr_t30, church_turing_thesis__mathematical_definition_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(ctt_math_def_tr_t60, church_turing_thesis__mathematical_definition_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(ctt_math_def_tr_t90, church_turing_thesis__mathematical_definition_reading, theater_ratio, 90, 0.05).

% Extraction over time
narrative_ontology:measurement(ctt_math_def_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ctt_math_def_be_t30, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(ctt_math_def_be_t60, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(ctt_math_def_be_t90, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 90, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel decomposes into three structurally distinct constraints because the same historical text supports three incompatible epistemic readings: definitional (convention), empirical (physical claim), and epistemological (boundary). Each reading has a distinct epsilon, beneficiary structure, and constraint type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
