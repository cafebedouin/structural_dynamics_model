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
 *   record, which asserts that human origins are materialist (evolution,
 *   migration) and knowable exclusively through the scientific method. It
 *   functions as a gatekeeping mechanism within academia and public
 *   discourse, granting authority to credentialed scientists while
 *   suppressing alternative, non-materialist, or non-scientific
 *   interpretations. The constraint is claimed as a 'rope' by its proponents,
 *   emphasizing its coordination function in building a shared scientific
 *   understanding. However, the authored metrics reflect its substantial
 *   extractiveness and suppression of alternative epistemologies, leading to
 *   an engine-computed classification that will likely diverge from the
 *   claim.
 *
 * KEY AGENTS:
 *   - credentialed_scientists: Primary agenda-setter (institutional/constrained) — defines and enforces the scientific method for human origins.
 *   - academic_institutions: Primary beneficiary (institutional/constrained) — provides infrastructure and legitimacy for the naturalist reading.
 *   - non_credentialed_interpreters: Primary payer (powerless/identity_locked) — excluded from mainstream discourse.
 *   - indigenous_knowledge_holders: Payer (moderate/identity_locked) — their epistemologies are marginalized.
 *   - religious_communities: Payer (organized/identity_locked) — their non-materialist views are challenged.
 *   - public_education_systems: Beneficiary (institutional/constrained) — disseminates the naturalist reading as authoritative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.65).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.75).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Anthropological Record (Naturalist Reading)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, 'f1f0e75c-f256-4e39-94b5-9e65b274b206').
narrative_ontology:cs_kernel_codification('f1f0e75c-f256-4e39-94b5-9e65b274b206', formalized).
narrative_ontology:cs_authority_grounding('f1f0e75c-f256-4e39-94b5-9e65b274b206', expertise).
narrative_ontology:cs_interpretation_layer_present('f1f0e75c-f256-4e39-94b5-9e65b274b206').
narrative_ontology:cs_reading_relation('f1f0e75c-f256-4e39-94b5-9e65b274b206', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('f1f0e75c-f256-4e39-94b5-9e65b274b206', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('f1f0e75c-f256-4e39-94b5-9e65b274b206', foundational, materialist_causation_only).
narrative_ontology:cs_axiom_status(materialist_causation_only, holdable).
narrative_ontology:cs_axiom_grounding('f1f0e75c-f256-4e39-94b5-9e65b274b206', materialist_causation_only, empirically_contingent).
narrative_ontology:cs_axiom('f1f0e75c-f256-4e39-94b5-9e65b274b206', foundational, scientific_method_sole_epistemic_authority).
narrative_ontology:cs_axiom_status(scientific_method_sole_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('f1f0e75c-f256-4e39-94b5-9e65b274b206', scientific_method_sole_epistemic_authority, conventional).
narrative_ontology:cs_reference_frame('f1f0e75c-f256-4e39-94b5-9e65b274b206', enlightenment_scientific_rationalism).
narrative_ontology:cs_drift_state('f1f0e75c-f256-4e39-94b5-9e65b274b206', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1f0e75c-f256-4e39-94b5-9e65b274b206', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, public_education_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the methods, interpret the data, and control the publication and funding channels for research into human origins. They benefit from the authority and resources granted by this framework, which validates their expertise and excludes alternative epistemologies.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_scientists, agenda_setter,
    institutional, generational, constrained, global).

% House the credentialed scientists, provide research infrastructure, and derive prestige and funding from the scientific consensus on human origins. They enforce the methodological and interpretive norms that uphold the naturalist reading.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_institutions, beneficiary,
    institutional, generational, constrained, global).

% Attempt to interpret evidence of human origins outside of established scientific methods or academic credentials. They are systematically excluded from mainstream discourse, funding, and publication, and their interpretations are dismissed as unscientific or pseudoscientific.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, identity_locked, local).

% Possess rich oral traditions and place-based knowledge about human origins that often conflict with the naturalist reading's materialist and linear narratives. Their epistemologies are often marginalized or dismissed by the dominant scientific framework, leading to a loss of cultural authority and intellectual sovereignty.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    moderate, civilizational, identity_locked, local).

% Hold beliefs about human origins (e.g., divine creation) that are incompatible with the naturalist reading's materialist explanations. They face social and intellectual pressure to reconcile their beliefs with scientific findings, or risk being labeled as anti-science or irrational.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religious_communities, payer,
    organized, civilizational, identity_locked, national).

% Are mandated to teach the scientific consensus on human origins, reinforcing the naturalist reading as the authoritative account. They benefit from a clear, unified curriculum but also face pressure from groups advocating for alternative narratives.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_education_systems, beneficiary,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, evidence-based framework for understanding human origins, enabling cumulative research, shared understanding, and a basis for public education on the topic.
% TRANSFER_FUNCTION: Transfers epistemic authority, funding, and social prestige to credentialed scientists and academic institutions, while marginalizing or suppressing alternative knowledge systems and their proponents.
% ABSENT_VOICES: Alternative epistemologies, particularly those rooted in spiritual or non-materialist frameworks, are systematically excluded from the scientific discourse. Their proponents would challenge the naturalist reading's foundational assumptions and demand recognition for diverse ways of knowing.
% DISAPPEARANCE_RATIONALE: If the naturalist reading of the anthropological record vanished, the entire edifice of scientific anthropology, archaeology, and evolutionary biology would collapse. Funding, academic careers, and public education curricula would be fundamentally disrupted, leading to a profound reorganization of how human origins are understood and taught.
% FOUNDING_PROBLEM: To provide a systematic, verifiable, and universally applicable explanation for human origins, free from supernatural or culturally specific biases, in an era of increasing global interconnectedness and scientific inquiry.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed scientists and academic institutions attest that the problem of understanding human origins remains live and requires ongoing scientific investigation. Public education systems corroborate the need for a unified, evidence-based curriculum. Indigenous knowledge holders and religious communities, while often critical of the naturalist reading's exclusivity, do not deny the existence of the underlying question of human origins.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the naturalist reading concentrates epistemic authority and resources within a specific institutional framework, effectively extracting the right to define 'truth' from other knowledge systems. Suppression is also high (0.75) due to the active exclusion of non-scientific methods and interpretations from academic and public discourse, enforced through credentialing, publication gates, and rhetorical dismissal. Theater ratio is low (0.1) because the scientific process is genuinely functional, but its gatekeeping aspects are increasingly performative in defending its exclusive authority. Accessibility collapse is 0.7, reflecting that while alternative interpretations exist, their pathways to mainstream acceptance are severely constrained. Resistance is 0.4, indicating ongoing, though often marginalized, challenges from religious and indigenous groups.
 *
 * PERSPECTIVAL GAP:
 *   Credentialed scientists and academic institutions experience this as a robust 'rope' that coordinates a shared, verifiable understanding of human origins. For non-credentialed interpreters, indigenous knowledge holders, and religious communities, it operates as a 'snare' or 'tangled rope' that extracts their epistemic authority and suppresses their alternative ways of knowing. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed scientists and academic institutions are clear beneficiaries (d near 0.0) as they control the narrative and resources. Non-credentialed interpreters, indigenous knowledge holders, and religious communities are targets (d near 1.0) as their knowledge systems are devalued or suppressed. Public education systems are beneficiaries as they gain a clear, authoritative curriculum.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide a 'scientific' understanding of human origins remains live. However, the mechanism has drifted from pure coordination (building shared knowledge) to include substantial extraction (monopolizing epistemic authority). The classification as 'tangled_rope' (expected from metrics) prevents mislabeling this as a pure 'rope' (the claimed type), highlighting the embedded extraction within its coordination function. The persistence is not purely inertial; it is actively maintained by beneficiaries who profit from its gatekeeping function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_monopoly_justification,
    'Is the exclusive reliance on the scientific method for understanding human origins a necessary condition for objective knowledge, or an institutionalized monopoly on truth?',
    'Comparative analysis of knowledge outcomes from diverse epistemologies in contexts where they are given equal institutional support; assessment of whether non-scientific methods yield verifiable insights into human origins.',
    'If it''s a necessary condition, the high suppression is justified as defending epistemic integrity. If it''s a monopoly, the suppression is extractive, and the constraint reclassifies towards ''snare'' for excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_monopoly_justification, conceptual, 'Whether scientific exclusivity is epistemically necessary or institutionally constructed.').

omega_variable(
    indigenous_knowledge_integration,
    'To what extent can indigenous epistemologies and oral traditions be integrated into or reconciled with the naturalist reading of human origins without compromising either framework''s integrity?',
    'Longitudinal studies of collaborative research projects between scientists and indigenous communities, assessing mutual recognition of knowledge claims and methodological pluralism.',
    'Successful integration would reduce the extractiveness and suppression experienced by indigenous knowledge holders, potentially shifting the constraint towards a more inclusive ''rope'' for those communities. Failure to integrate would reinforce the current extractive dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_knowledge_integration, empirical, 'Potential for reconciliation between scientific and indigenous knowledge systems.').

omega_variable(
    supernatural_causation_exclusion,
    'Is the exclusion of supernatural causation from the anthropological record a methodological necessity for scientific inquiry, or a philosophical presupposition that forecloses certain interpretations a priori?',
    'Philosophical analysis of the boundaries of scientific explanation and the role of methodological naturalism; examination of whether ''supernatural'' explanations can be reframed in empirically testable ways.',
    'If a methodological necessity, the exclusion is a ''mountain'' for scientific practice. If a philosophical presupposition, it''s a ''snare'' for religious communities, as it pre-emptively invalidates their core claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supernatural_causation_exclusion, conceptual, 'Methodological vs. philosophical basis for excluding supernatural explanations.').


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
narrative_ontology:measurement(anth_tr_t2000, anthropological_record__naturalist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__naturalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(anth_be_t1859, anthropological_record__naturalist_reading, base_extractiveness, 1859, 0.3).
narrative_ontology:measurement(anth_be_t1900, anthropological_record__naturalist_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(anth_be_t1950, anthropological_record__naturalist_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(anth_be_t2000, anthropological_record__naturalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__naturalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1859, anthropological_record__naturalist_reading, suppression_requirement, 1859, 0.4).
narrative_ontology:measurement(anth_su_t1900, anthropological_record__naturalist_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(anth_su_t1950, anthropological_record__naturalist_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(anth_su_t2000, anthropological_record__naturalist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__naturalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'anthropological_record' kernel. Its structural properties and metrics are distinct from the 'creationist_reading' and 'indigenous_epistemology_reading' siblings, which offer alternative interpretations of human origins. This naturalist reading forecloses the creationist reading and influences the indigenous epistemology reading by setting the dominant epistemic standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
