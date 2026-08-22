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
 *   human_readable: Church-Turing Thesis: Mathematical Definition Reading
 *   domain: philosophy_of_mathematics/computer_science
 *
 * SUMMARY:
 *   This constraint story captures the Church-Turing thesis under the
 *   mathematical definition reading: the thesis is treated as a stipulative
 *   convention that fixes the meaning of 'effective computability' as
 *   Turing-machine computability, not as an empirical claim about the
 *   physical universe or an epistemological boundary on formal knowledge. As
 *   a convention, it coordinates terminology across mathematics and computer
 *   science without extracting from any party. There are no victims because a
 *   definition cannot be violated; one simply adopts or declines the
 *   convention. The reading is instantiated as a low-extraction rope with
 *   negligible suppression and no active enforcement.
 *
 * KEY AGENTS:
 *   - Mathematical community (organized/universal): Primary beneficiary of terminological coordination â gains clarity and shared vocabulary without bearing extraction.
 *   - Physicalist philosophers and hypercomputation researchers (excluded): Would contest the definitional reading in favor of empirical or physical interpretations; absent from this framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis: Mathematical Definition Reading").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'e055753a-dd9c-42a2-bc59-7fc00b50cfa3').
narrative_ontology:cs_kernel_codification('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', formalized).
narrative_ontology:cs_authority_grounding('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', expertise).
narrative_ontology:cs_interpretation_layer_present('e055753a-dd9c-42a2-bc59-7fc00b50cfa3').
narrative_ontology:cs_reading_relation('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', church_turing_thesis__physical_claim_reading, forecloses).
narrative_ontology:cs_reading_relation('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', foundational, effective_computability_is_stipulative).
narrative_ontology:cs_axiom_status(effective_computability_is_stipulative, holdable).
narrative_ontology:cs_axiom_grounding('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', effective_computability_is_stipulative, conventional).
narrative_ontology:cs_axiom('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', foundational, mathematical_definitions_lack_empirical_content).
narrative_ontology:cs_axiom_status(mathematical_definitions_lack_empirical_content, holdable).
narrative_ontology:cs_axiom_grounding('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', mathematical_definitions_lack_empirical_content, conventional).
narrative_ontology:cs_reference_frame('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', stipulative_definition_framework).
narrative_ontology:cs_drift_state('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e055753a-dd9c-42a2-bc59-7fc00b50cfa3', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_community).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, effective_computability_convention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the Church-Turing thesis as a stipulative definition to align usage of the term 'effectively computable' across logic, mathematics, and computer science. The convention is adopted for clarity and interoperability; members may freely employ equivalent formalisms such as lambda calculus or recursive functions without penalty, so exit from the specific terminological convention is unimpeded.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_community, beneficiary,
    organized, civilizational, mobile, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, precise meaning for 'effective computability' across foundational mathematics and computer science, eliminating terminological ambiguity in proofs, textbooks, and interdisciplinary communication.
% TRANSFER_FUNCTION: No asymmetric transfer; the arrangement circulates conceptual clarity and terminological alignment among all participants without extracting from any party.
% ABSENT_VOICES: Physicalist philosophers of computation and hypercomputation researchers who treat the thesis as an empirical or physically limiting claim are not represented in this definitional framing; they would argue for empirical testability or physical possibility but are excluded by the conventional reading.
% DISAPPEARANCE_RATIONALE: If the definitional convention vanished overnight, foundational textbooks, proofs, and interdisciplinary communication would lose a standard terminological anchor, forcing recourse to longer explicit formal specifications or equivalent alternative formalisms; the coordination function would fragment until a replacement convention stabilized.
% FOUNDING_PROBLEM: The absence of a precise, shared definition for 'effective computability' in the 1930s, which created terminological ambiguity and hindered communication among logicians and early computer scientists.
% FOUNDING_PROBLEM_CORROBORATION: Historians of logic attest that Church, Turing, and Kleene offered competing but equivalent formalizations in the 1930s without prior consensus; contemporary philosophical analysis from outside the beneficiary community (e.g., Copeland, Sieg) distinguishes the definitional convention from empirical and epistemological variants, corroborating that the coordinative problem of shared terminology persists.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.05, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is negligible (0.05) because a stipulative definition does not extract from those who use it; it is a voluntary coordination device. Suppression is near zero (0.02) because no coercion maintains the convention â practitioners adopt it for clarity and may switch to equivalent formalisms. Theater ratio is zero because there is no performative maintenance. Accessibility collapse is low (0.15) because alternative equivalent formalisms (lambda calculus, recursive functions) remain fully accessible and widely taught. Resistance is minimal (0.01) because the convention is not contested within the reading's own framework. The metrics and claimed type are independently authored: the claim is rope (coordination) and the metrics describe a virtually non-extractive, non-coercive arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From within the mathematical definition reading, all participants are beneficiaries of coordination; there is no payer seat. The only divergence would be experienced by parties outside the reading (e.g., physical claim proponents) who are structurally excluded rather than targeted. The engine will compute a uniform beneficiary directionality for the mathematical community and no high-d targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The mathematical community sits near the full-beneficiary end (low d): the constraint subsidizes their communicative coordination by providing a shared term. There are no declared victims and no excluded parties within the constraint's operational scope; the excluded physicalist voices are outside the reading's framework, not targets of extraction. Effective extraction is therefore near zero for all seated agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement is not a candidate for mandatrophy because the founding problem â terminological ambiguity in effective computability â remains live as a perennial coordination need. Even if the original historical ambiguity was resolved, the ongoing need for shared vocabulary in teaching and research keeps the coordination function alive. There is no decay into theatrical maintenance because the convention requires no enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Church-Turing thesis fundamentally a stipulative definition, an empirical claim about physical computation, or an epistemological boundary on formal knowability?',
    'Philosophical analysis of the thesis''s functional role in mathematical practice versus physical and epistemological discourse; comparison of stakeholder and victim structures across the three kernel readings.',
    'A definitional reading yields a low-extraction rope with no victims; a physical claim reading may generate victims (e.g., suppressed hypercomputation research) and raise extractiveness; an epistemological boundary reading may introduce coordination-extraction hybrid dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between definition, physical claim, and epistemological boundary readings').

omega_variable(
    convention_beneficiary_concentration,
    'Does the definitional convention confer differential benefit on any subset of the mathematical community, or is the benefit perfectly diffuse?',
    'Sociological analysis of citation practices, textbook authorship, and terminological gatekeeping in computability theory.',
    'If benefit is concentrated (e.g., among foundational logicians who control the convention''s pedagogy), the constraint may shift toward tangled rope; if perfectly diffuse, it remains a pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convention_beneficiary_concentration, empirical, 'Whether the definitional reading''s benefit is concentrated or diffuse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_def_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(ctt_def_tr_t30, church_turing_thesis__mathematical_definition_reading, theater_ratio, 30, 0.0).
narrative_ontology:measurement(ctt_def_tr_t60, church_turing_thesis__mathematical_definition_reading, theater_ratio, 60, 0.0).
narrative_ontology:measurement(ctt_def_tr_t90, church_turing_thesis__mathematical_definition_reading, theater_ratio, 90, 0.0).

% Extraction over time
narrative_ontology:measurement(ctt_def_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ctt_def_be_t30, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(ctt_def_be_t60, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(ctt_def_be_t90, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 90, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ctt_def_su_t0, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(ctt_def_su_t30, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 30, 0.02).
narrative_ontology:measurement(ctt_def_su_t60, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 60, 0.02).
narrative_ontology:measurement(ctt_def_su_t90, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 90, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing thesis kernel. The mathematical definition reading treats the thesis as a conventional stipulation with negligible extraction, while the physical claim and epistemological boundary readings instantiate structurally distinct constraints with different referents and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
