% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Anthropological Record (Naturalist Reading)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint represents the naturalist reading of the anthropological
 *   record, which asserts that human origins (evolution, migration) are
 *   knowable exclusively through the scientific method and material evidence.
 *   It functions as a gatekeeping mechanism within academia and public
 *   discourse, granting epistemic authority to credentialed scientists while
 *   marginalizing or suppressing alternative, non-materialist, or
 *   non-credentialed interpretations. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates scientific research and understanding,
 *   but does so with significant asymmetric extraction from those whose
 *   epistemologies are excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.75).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Anthropological Record (Naturalist Reading)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '06b682c0-1a76-46da-8e78-908c600d165e').
narrative_ontology:cs_kernel_codification('06b682c0-1a76-46da-8e78-908c600d165e', formalized).
narrative_ontology:cs_authority_grounding('06b682c0-1a76-46da-8e78-908c600d165e', expertise).
narrative_ontology:cs_interpretation_layer_present('06b682c0-1a76-46da-8e78-908c600d165e').
narrative_ontology:cs_reading_relation('06b682c0-1a76-46da-8e78-908c600d165e', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('06b682c0-1a76-46da-8e78-908c600d165e', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('06b682c0-1a76-46da-8e78-908c600d165e', foundational, material_causation_only).
narrative_ontology:cs_axiom_status(material_causation_only, holdable).
narrative_ontology:cs_axiom_grounding('06b682c0-1a76-46da-8e78-908c600d165e', material_causation_only, empirically_contingent).
narrative_ontology:cs_axiom('06b682c0-1a76-46da-8e78-908c600d165e', foundational, scientific_method_sole_epistemic_path).
narrative_ontology:cs_axiom_status(scientific_method_sole_epistemic_path, holdable).
narrative_ontology:cs_axiom_grounding('06b682c0-1a76-46da-8e78-908c600d165e', scientific_method_sole_epistemic_path, conventional).
narrative_ontology:cs_reference_frame('06b682c0-1a76-46da-8e78-908c600d165e', enlightenment_scientific_rationalism).
narrative_ontology:cs_drift_state('06b682c0-1a76-46da-8e78-908c600d165e', contemporary_postmodern_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('06b682c0-1a76-46da-8e78-908c600d165e', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_anthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the methods, interpret the evidence (archaeological, genetic, linguistic), and publish findings on human origins. They benefit from the authority and funding associated with being the recognized experts in this domain. Their careers and professional identity are tied to the scientific method as the sole legitimate path to knowledge.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_anthropologists, agenda_setter,
    institutional, generational, constrained, global).

% Fund research, house collections, and disseminate the naturalist account of human origins. They gain legitimacy and public trust by presenting a unified, evidence-based narrative, reinforcing the authority of science as a knowledge system.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_institutions, beneficiary,
    institutional, generational, constrained, global).

% Attempt to interpret the human past outside of established scientific frameworks, often drawing on alternative evidence or methodologies. They face exclusion from academic discourse, funding, and public platforms, and their interpretations are often dismissed as unscientific or pseudoscientific.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, trapped, local).

% Possess deep, place-based knowledge of human origins and migrations, often transmitted through oral traditions and ceremonies. Their epistemologies are frequently marginalized or dismissed by the naturalist reading, which prioritizes material evidence and scientific peer review. Their identity is often fused with their traditional knowledge systems.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    moderate, civilizational, identity_locked, local).

% Adhere to origin narratives (e.g., creation stories) that conflict with the materialist account. They face pressure to reconcile their beliefs with scientific findings or risk being seen as irrational or anti-science. Their identity is often deeply intertwined with their faith traditions.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religious_communities, payer,
    organized, generational, identity_locked, global).

% Are tasked with teaching human origins, often adopting the naturalist reading as the authoritative scientific consensus. They face political and social pressure from religious communities to include alternative narratives, but are largely bound by scientific curricula standards.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_education_systems, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, evidence-based framework for understanding human origins and migrations, allowing for cumulative research and shared understanding within the scientific community.
% TRANSFER_FUNCTION: Transfers epistemic authority and funding to credentialed scientists and institutions, while transferring marginalization and suppression to alternative knowledge systems and their proponents.
% ABSENT_VOICES: Alternative epistemologies, particularly those rooted in spiritual or non-materialist frameworks, are systematically excluded from the discourse of legitimate knowledge production regarding human origins. Their absence ensures the naturalist reading's dominance.
% DISAPPEARANCE_RATIONALE: If the naturalist reading of the anthropological record vanished, the scientific consensus on human origins would collapse, leading to a fragmentation of research, a loss of epistemic authority for scientific institutions, and a resurgence of diverse, often conflicting, origin narratives in public discourse and education.
% FOUNDING_PROBLEM: To provide a systematic, verifiable, and universally applicable account of human origins and diversity, moving beyond speculative or mythic explanations.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed anthropologists and scientific institutions attest that the problem of understanding human origins remains live, with ongoing discoveries and refinements. Public education systems corroborate the need for a coherent, evidence-based narrative. Religious communities and indigenous knowledge holders, while offering alternative accounts, do not dispute the *existence* of the problem of understanding origins, only the naturalist reading's exclusive claim to its solution.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) due to the concentration of epistemic authority and funding within the scientific establishment, which benefits from being the sole arbiter of 'truth' in this domain. Suppression is also high (0.75) because the constraint actively excludes and delegitimizes non-scientific or non-credentialed interpretations, often through institutional gatekeeping and public dismissal. Theater ratio is low (0.15) as the scientific enterprise is largely functional in its pursuit of knowledge, but a small component of its public presentation involves defending its exclusive authority against perceived threats. Accessibility collapse is moderate (0.60) because while alternative interpretations exist, their access to mainstream platforms and resources is severely limited. Resistance is moderate (0.45) from religious and indigenous communities, but this resistance is largely external to the scientific discourse itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of credentialed scientists, this constraint is a 'rope' or even a 'mountain' – a necessary framework for objective knowledge. From the perspective of indigenous knowledge holders or religious communities, it operates as a 'snare' or 'tangled_rope', actively suppressing their ways of knowing and extracting epistemic legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed anthropologists and scientific institutions are clear beneficiaries and agenda-setters, as they define the terms of knowledge production and reap the rewards of epistemic authority. Non-credentialed interpreters, indigenous knowledge holders, and religious communities are victims, as their alternative epistemologies are suppressed or delegitimized. Public education systems act as agenda-setters in disseminating the naturalist reading, but are also constrained by political pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide a verifiable account of human origins remains live. However, the 'tangled_rope' classification prevents mislabeling it as a pure 'rope' by highlighting the asymmetric extraction and suppression inherent in its operation, particularly concerning the exclusion of alternative epistemologies. The persistence of resistance from victim groups indicates that the coordination function is not universally beneficial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_authority_legitimacy,
    'Is the exclusive epistemic authority granted to the naturalist reading genuinely justified by its superior explanatory power and empirical rigor, or is it partly maintained by institutional power and suppression of alternatives?',
    'Comparative analysis of explanatory power and predictive accuracy across different epistemologies, coupled with an examination of institutional gatekeeping mechanisms and funding biases.',
    'If authority is primarily maintained by power, the constraint''s extractiveness and suppression are higher than warranted by its coordination function, pushing it closer to a ''snare''. If purely justified by rigor, it leans more towards a ''rope'' or even a ''mountain'' of scientific consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_legitimacy, conceptual, 'The true basis of the naturalist reading''s epistemic authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative epistemologies structural (lack of funding, publication barriers) or internalized (self-censorship, belief in scientific superiority by non-scientists)?',
    'Post-funding/platform access trajectory: if alternative epistemologies gain traction and legitimacy after structural barriers are removed, suppression was primarily structural. If marginalization persists, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, removing barriers could significantly reduce extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative epistemologies.').

omega_variable(
    natural_vs_constructed_epistemic_boundary,
    'Is the boundary between ''scientific'' and ''non-scientific'' knowledge a natural, objective distinction, or a socially constructed one that benefits specific institutions?',
    'Philosophical analysis of demarcation criteria and historical sociology of science, examining how the boundary has shifted over time and in different cultural contexts.',
    'If the boundary is largely constructed, the ''emerges_naturally'' claim for the naturalist reading is false, and its ''mountain'' aspects are a ''false summit'', reclassifying it as a ''tangled_rope'' or ''snare'' from the perspective of excluded epistemologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_epistemic_boundary, conceptual, 'The nature of the scientific/non-scientific knowledge boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 1859, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1859, anthropological_record__naturalist_reading, theater_ratio, 1859, 0.05).
narrative_ontology:measurement(anth_tr_t1900, anthropological_record__naturalist_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(anth_tr_t1950, anthropological_record__naturalist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(anth_tr_t2000, anthropological_record__naturalist_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__naturalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(anth_be_t1859, anthropological_record__naturalist_reading, base_extractiveness, 1859, 0.4).
narrative_ontology:measurement(anth_be_t1900, anthropological_record__naturalist_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(anth_be_t1950, anthropological_record__naturalist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(anth_be_t2000, anthropological_record__naturalist_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__naturalist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1859, anthropological_record__naturalist_reading, suppression_requirement, 1859, 0.3).
narrative_ontology:measurement(anth_su_t1900, anthropological_record__naturalist_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(anth_su_t1950, anthropological_record__naturalist_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(anth_su_t2000, anthropological_record__naturalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__naturalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, indigenous_epistemology_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, public_education_curriculum_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'anthropological_record' kernel. Its structural properties and classification differ significantly from sibling readings (creationist, indigenous epistemology) due to distinct axioms and beneficiary/victim structures. All readings are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
