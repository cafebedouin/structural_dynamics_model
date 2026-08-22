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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Naturalist Reading of Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint describes the 'naturalist reading' of the anthropological
 *   record, which asserts that human origins (evolution, migration) are
 *   knowable exclusively through scientific method and materialist
 *   explanations. It functions as a gatekeeping mechanism, granting epistemic
 *   authority to credentialed scientists while suppressing alternative
 *   interpretations from religious or indigenous epistemologies. The
 *   constraint is claimed as a 'rope' by its beneficiaries (academic
 *   anthropologists and scientific institutions) who view it as a necessary
 *   coordination mechanism for scientific progress. However, the authored
 *   metrics reflect a 'tangled_rope' or 'snare' due to high extractiveness
 *   and suppression of non-credentialed voices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.75).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '1f80ae1b-ab9d-4416-9789-cb61958542aa').
narrative_ontology:cs_kernel_codification('1f80ae1b-ab9d-4416-9789-cb61958542aa', formalized).
narrative_ontology:cs_authority_grounding('1f80ae1b-ab9d-4416-9789-cb61958542aa', expertise).
narrative_ontology:cs_interpretation_layer_present('1f80ae1b-ab9d-4416-9789-cb61958542aa').
narrative_ontology:cs_reading_relation('1f80ae1b-ab9d-4416-9789-cb61958542aa', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('1f80ae1b-ab9d-4416-9789-cb61958542aa', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('1f80ae1b-ab9d-4416-9789-cb61958542aa', foundational, materialist_causation_only).
narrative_ontology:cs_axiom_status(materialist_causation_only, holdable).
narrative_ontology:cs_axiom_grounding('1f80ae1b-ab9d-4416-9789-cb61958542aa', materialist_causation_only, empirically_contingent).
narrative_ontology:cs_axiom('1f80ae1b-ab9d-4416-9789-cb61958542aa', foundational, scientific_method_exclusive_truth_path).
narrative_ontology:cs_axiom_status(scientific_method_exclusive_truth_path, holdable).
narrative_ontology:cs_axiom_grounding('1f80ae1b-ab9d-4416-9789-cb61958542aa', scientific_method_exclusive_truth_path, conventional).
narrative_ontology:cs_reference_frame('1f80ae1b-ab9d-4416-9789-cb61958542aa', enlightenment_scientific_rationalism).
narrative_ontology:cs_drift_state('1f80ae1b-ab9d-4416-9789-cb61958542aa', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f80ae1b-ab9d-4416-9789-cb61958542aa', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_anthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_communities).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, general_public).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, scientific_method_supremacy).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, materialist_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the legitimate methods and interpretations of human origins, publish findings, and control academic discourse. Their careers and institutional funding depend on the naturalist framework. They benefit from the exclusion of non-scientific interpretations.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_anthropologists, agenda_setter,
    institutional, generational, identity_locked, global).

% Fund and house academic research, grant credentials, and disseminate scientific consensus. They benefit from the authority and prestige derived from being the sole arbiters of 'truth' regarding human origins, reinforcing their institutional power.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_institutions, beneficiary,
    institutional, generational, constrained, global).

% Attempt to interpret human origins outside of academic scientific frameworks, often drawing on local knowledge or alternative methodologies. They face systematic exclusion from academic platforms, funding, and public legitimacy, effectively paying a cost in suppressed voice and recognition.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, trapped, local).

% Hold origin narratives rooted in oral traditions and deep connection to place, often clashing with scientific materialist accounts. They bear the cost of having their epistemologies dismissed as 'myth' or 'unscientific,' leading to cultural erosion and loss of control over their heritage.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_communities, payer,
    organized, civilizational, identity_locked, local).

% Adhere to origin stories based on sacred texts or divine revelation, which are often incompatible with purely materialist evolutionary accounts. They experience the naturalist reading as a challenge to their worldview and a suppression of their interpretive authority in public discourse.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religious_communities, payer,
    organized, civilizational, identity_locked, global).

% Receives a coherent, evidence-based narrative of human origins that is consistent with broader scientific understanding. They benefit from the clarity and predictive power of this framework, but may also be deprived of alternative, culturally rich interpretations.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, general_public, beneficiary,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, empirically verifiable framework for understanding human origins, enabling cumulative research and consistent educational curricula within scientific disciplines.
% TRANSFER_FUNCTION: Transfers epistemic authority and cultural legitimacy regarding human origins from diverse interpretive communities to credentialed scientific institutions, along with associated funding and prestige.
% ABSENT_VOICES: Interpretive communities whose epistemologies are systematically excluded (e.g., spiritual leaders, traditional knowledge keepers, non-academic historians) would challenge the naturalist reading's claim to exclusive truth and demand recognition for their own ways of knowing.
% DISAPPEARANCE_RATIONALE: If the naturalist reading's authority vanished, the consensus on human origins would fragment. Scientific institutions would lose their exclusive claim to truth in this domain, leading to a proliferation of competing narratives and a significant rearrangement of educational and research priorities.
% FOUNDING_PROBLEM: To provide a coherent, evidence-based, and universally applicable explanation for human origins, moving beyond speculative or religiously dictated accounts.
% FOUNDING_PROBLEM_CORROBORATION: Academic scientists and scientific institutions universally attest that the problem of understanding human origins remains live and requires ongoing scientific inquiry. External corroboration comes from the general public's reliance on scientific consensus for factual understanding, though some religious and indigenous communities contest the exclusivity of this approach.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) stems from the concentration of epistemic authority and resources within academic institutions, effectively extracting legitimacy from alternative knowledge systems. Suppression (0.75) is high due to active exclusion of non-scientific methodologies and interpretations from mainstream discourse, funding, and educational platforms. The theater ratio (0.20) is relatively low, as the scientific method genuinely produces knowledge, but a portion of the effort is performative in defending the exclusivity of the naturalist frame against challenges. Accessibility collapse (0.60) is moderate, as alternative interpretations exist but are systematically devalued. Resistance (0.45) is present from marginalized communities but often lacks the institutional power to significantly alter the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of academic anthropologists, this constraint is a 'rope' that coordinates scientific inquiry and produces reliable knowledge. From the perspective of indigenous communities or religious communities, it operates as a 'snare' that extracts their epistemic authority and suppresses their voices. The engine's classification will likely reflect this divergence, computing a more extractive type for the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic anthropologists and scientific institutions are clear beneficiaries and agenda-setters, as they define and enforce the naturalist reading, gaining prestige and resources. Non-credentialed interpreters, indigenous communities, and religious communities are payers, bearing the cost of epistemic marginalization and suppression of their own origin narratives. The general public is a diffuse beneficiary of a coherent scientific narrative, but also indirectly pays by losing access to diverse interpretive frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_authority_legitimacy,
    'Is the exclusive epistemic authority granted to scientific method for human origins inherently legitimate, or is it a historically contingent social construct?',
    'Philosophical analysis of the foundations of knowledge, cross-cultural studies of epistemology, and historical sociology of science.',
    'If historically contingent, the constraint''s suppression of alternative epistemologies would be reclassified as a more severe form of extraction, rather than a necessary boundary for scientific rigor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_authority_legitimacy, conceptual, 'The nature of scientific epistemic authority in the domain of human origins.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-credentialed interpretations structural (institutional barriers) or internalized (self-censorship, belief in scientific superiority)?',
    'Post-exit suppression trajectory: if non-credentialed interpreters continue to self-censor or defer to scientific authority even after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the targets carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative interpretations.').

omega_variable(
    materialism_vs_supernatural_exclusion,
    'Is the exclusion of supernatural causation from the anthropological record a methodological necessity for scientific inquiry, or an ontological commitment that forecloses certain truths a priori?',
    'Philosophical debate on the limits of scientific explanation and the definition of ''natural'' vs. ''supernatural''.',
    'If an ontological commitment, the constraint''s claim to neutrality is undermined, and its suppression of creationist readings becomes a foundational exclusion rather than a methodological one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(materialism_vs_supernatural_exclusion, conceptual, 'The nature of materialism in scientific inquiry regarding human origins.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1900, anthropological_record__naturalist_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(anth_tr_t1930, anthropological_record__naturalist_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(anth_tr_t1960, anthropological_record__naturalist_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(anth_tr_t1990, anthropological_record__naturalist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__naturalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(anth_be_t1900, anthropological_record__naturalist_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(anth_be_t1930, anthropological_record__naturalist_reading, base_extractiveness, 1930, 0.5).
narrative_ontology:measurement(anth_be_t1960, anthropological_record__naturalist_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(anth_be_t1990, anthropological_record__naturalist_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__naturalist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1900, anthropological_record__naturalist_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(anth_su_t1930, anthropological_record__naturalist_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(anth_su_t1960, anthropological_record__naturalist_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(anth_su_t1990, anthropological_record__naturalist_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__naturalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'anthropological_record' kernel. Its ε value differs significantly from the creationist and indigenous epistemology readings due to its specific claims about knowability and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
