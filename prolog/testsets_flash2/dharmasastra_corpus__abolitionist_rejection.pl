% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus (Abolitionist Rejection Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'abolitionist rejection' reading of the
 *   Dharmasastra corpus, which views the texts as fundamentally oppressive
 *   and lacking legitimate authority, particularly due to their role in
 *   sanctioning the caste system. From this perspective, the constraint is a
 *   snare, actively extracting from and suppressing marginalized groups. The
 *   reading advocates for the complete abandonment of the textual framework
 *   and its associated social structures. This is one reading of the
 *   'dharmasastra_corpus' kernel; sibling readings (orthodox_literalist,
 *   reformist_contextual) offer different interpretations and
 *   classifications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.95).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.9).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus (Abolitionist Rejection Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, 'a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d').
narrative_ontology:cs_kernel_codification('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', fixed_text).
narrative_ontology:cs_authority_grounding('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', extraction).
narrative_ontology:cs_interpretation_layer_present('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d').
narrative_ontology:cs_reading_relation('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', foundational, caste_system_inherently_unjust).
narrative_ontology:cs_axiom_status(caste_system_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', caste_system_inherently_unjust, deontological).
narrative_ontology:cs_axiom('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', foundational, textual_authority_derived_from_social_justice).
narrative_ontology:cs_axiom_status(textual_authority_derived_from_social_justice, holdable).
narrative_ontology:cs_axiom_grounding('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', textual_authority_derived_from_social_justice, deontological).
narrative_ontology:cs_reference_frame('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', universal_human_rights_framework).
narrative_ontology:cs_drift_state('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', contemporary_global_ethics, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a5f041a7-b3aa-4dbf-9c25-7cc56bbd7c5d', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the complete dismantling of the caste system and the rejection of Dharmasastra as a legitimate source of authority. They analyze the texts as instruments of oppression and seek to empower victims to resist and exit the system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_scholars_activists, observer,
    organized, generational, mobile, global).

% Bear the brunt of the caste system's discrimination, economic exploitation, and social exclusion, which they see as directly sanctioned by Dharmasastra. Their identity is often fused with their social position, making exit extremely difficult and costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, biographical, identity_locked, local).

% Experience social and economic disadvantages and restrictions on their life choices, justified by the Dharmasastra framework. While not as severely trapped as Dalits, their opportunities are significantly curtailed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, lower_castes, payer,
    powerless, biographical, constrained, local).

% Face gender-based restrictions on property rights, education, and social roles, often codified in Dharmasastra. Their ability to exit these constraints varies by social context and individual agency.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women, payer,
    moderate, biographical, constrained, local).

% Historically and currently enforce the caste system and other Dharmasastra prescriptions, deriving their authority and social position from the textual framework. They actively resist any reinterpretation or rejection of the texts.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_religious_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Seek to reinterpret Dharmasastra to align with modern ethical standards, separating its 'eternal' principles from 'time-bound' social rules. While they challenge the orthodox reading, their approach is seen by abolitionists as insufficient and still legitimizing an oppressive framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, reformist_theologians, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the Dharmasastra corpus primarily coordinates social hierarchy and resource allocation to maintain the power of dominant groups, rather than solving a genuine collective action problem for all. Any 'coordination' is coercive.
% TRANSFER_FUNCTION: Transfers social status, economic resources, and political power from Dalits, lower castes, women, and religious minorities to upper castes and orthodox religious institutions, through a system of religiously sanctioned discrimination and exclusion.
% ABSENT_VOICES: The voices of those historically and currently oppressed by the caste system, particularly Dalits and lower castes, have been systematically excluded from the interpretive and authoritative structures of Dharmasastra. Their perspectives, when heard, directly challenge the legitimacy of the entire framework.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra corpus and its associated interpretive authority vanished overnight, the social and economic structures of caste would lose their primary religious justification. While inertia would remain, the legal and moral basis for discrimination would collapse, leading to a fundamental rearrangement of social relations and power dynamics, particularly for formerly oppressed groups.
% FOUNDING_PROBLEM: The Dharmasastra corpus was constructed to establish and maintain a divinely ordained social order (dharma) and ritual purity, which included the varna-jati (caste) system, to ensure cosmic and social harmony.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist scholars and human rights organizations attest that the 'founding problem' of maintaining a hierarchical social order is not a legitimate problem to solve, and that the system's persistence serves only to perpetuate oppression. Independent sociological and historical analyses corroborate that the original justifications are no longer ethically or functionally viable in a modern context, and that the system now primarily functions as a mechanism of social control and extraction.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) and suppression (0.9) are extremely high because this reading identifies the Dharmasastra framework as the primary ideological and institutional tool for maintaining a deeply exploitative and coercive social hierarchy. The 'coordination' function is seen as a cover for pure extraction. Resistance is high (0.9) due to ongoing social movements and activism against caste discrimination. Theater ratio is low (0.05) because, from this perspective, the system's oppressive function is direct and overt, with minimal performative justification masking its true nature.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both orthodox and reformist interpretations. While orthodox readings would classify Dharmasastra as a Mountain or Rope (divinely ordained, beneficial order) and reformist readings might see it as a Tangled Rope (imperfect but redeemable coordination), the abolitionist view sees only a Snare. This gap is central to the contest over the kernel's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   From the abolitionist perspective, there are no legitimate beneficiaries of the Dharmasastra corpus; any perceived benefits to dominant groups are seen as illicit gains from an extractive system. Dalits, lower castes, women, and religious minorities are the primary victims, bearing the full weight of the system's oppression. Orthodox religious institutions are identified as the agenda-setters and enforcers of this snare. Reformist theologians are seen as excluded because their attempts at reinterpretation, while well-intentioned, are viewed as ultimately legitimizing a fundamentally flawed system.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the Dharmasastra corpus is a snare whose original 'mandate' (establishing social order) has long atrophied into pure extraction. The constraint persists not because it solves a genuine coordination problem for all, but because it is actively enforced by those who benefit from the existing hierarchy and because victims are suppressed. The classification prevents mislabeling this as coordination by highlighting the extreme asymmetry and coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_textual_authority,
    'Does the Dharmasastra corpus possess any inherent or divinely revealed authority that transcends its historical and social context?',
    'Philosophical and theological debate, combined with empirical analysis of the social consequences of its application. Resolution is likely conceptual/preference-based rather than purely empirical.',
    'If inherent authority is affirmed, the abolitionist reading''s claim of ''no legitimate authority'' is weakened, potentially shifting the constraint towards a Tangled Rope (if coordination is also present) or even a Mountain (if seen as natural law). If denied, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_textual_authority, conceptual, 'Whether Dharmasastra holds intrinsic, context-independent authority.').

omega_variable(
    internalized_suppression_of_victims,
    'To what extent is the suppression experienced by Dalits and lower castes structural (external barriers) versus internalized (cognitive patterns, identity fusion, belief in karma)?',
    'Post-exit suppression trajectory: if suppression persists after structural barriers are removed (e.g., through migration to contexts without caste discrimination), it suggests a significant internalized component. Qualitative sociological studies on identity and belief systems.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as victims carry the suppression with them even after physical exit. This reinforces the ''identity_locked'' exit option for Dalits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_victims, empirical, 'Structural vs. internalized suppression mechanism for caste victims.').

omega_variable(
    alternative_framings_of_dharma,
    'Can the concept of ''dharma'' (righteous conduct) be genuinely separated from the caste-based prescriptions within Dharmasastra, as reformist readings suggest, or is the entire framework inextricably linked to hierarchy?',
    'Analysis of reformist movements'' success in establishing non-hierarchical, universally ethical interpretations of dharma that gain widespread acceptance and lead to concrete social change. If such interpretations consistently fail to dismantle caste, it supports the abolitionist view.',
    'If dharma is separable, the abolitionist rejection of the entire corpus might be seen as overly broad, potentially allowing for a ''scaffold'' or ''tangled_rope'' reading of a reformed Dharmasastra. If inseparable, the Snare classification is further validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_of_dharma, conceptual, 'Separability of ethical dharma from hierarchical prescriptions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t5, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 5, 0.08).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 10, 0.07).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.06).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(dhar_be_t5, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 5, 0.92).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 10, 0.93).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.94).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dhar_su_t5, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 5, 0.86).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dharmasastra_corpus' kernel. This abolitionist reading fundamentally rejects the legitimacy of the corpus, contrasting sharply with orthodox and reformist interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
