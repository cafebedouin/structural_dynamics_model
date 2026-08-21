% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'universalist devotional' reading of the
 *   Bhagavad Gita's Kurukshetra discourse. It interprets the text as teaching
 *   a path-independent devotion (bhakti) accessible to all, regardless of
 *   caste or social status. Dharma, in this reading, is understood as
 *   surrender to divine will rather than adherence to prescribed social
 *   roles. This reading fundamentally challenges traditional caste-based
 *   hierarchies and ritualistic gatekeeping, promoting an egalitarian
 *   approach to spiritual life. The metrics reflect a relatively low
 *   extractiveness and suppression, as this reading primarily functions to
 *   open access rather than restrict it, though it faces resistance from
 *   established authorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.2).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.3).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '0b9070c2-9b0d-40fe-be14-ec6bb38ffab7').
narrative_ontology:cs_kernel_codification('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', fixed_text).
narrative_ontology:cs_authority_grounding('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', practice).
narrative_ontology:cs_interpretation_layer_present('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7').
narrative_ontology:cs_reading_relation('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', foundational, devotion_transcends_caste).
narrative_ontology:cs_axiom_status(devotion_transcends_caste, holdable).
narrative_ontology:cs_axiom_grounding('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', devotion_transcends_caste, deontological).
narrative_ontology:cs_axiom('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', foundational, dharma_is_divine_surrender).
narrative_ontology:cs_axiom_status(dharma_is_divine_surrender, holdable).
narrative_ontology:cs_axiom_grounding('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', dharma_is_divine_surrender, theological).
narrative_ontology:cs_reference_frame('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', egalitarian_bhakti_path).
narrative_ontology:cs_drift_state('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', contemporary_pluralistic_hinduism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0b9070c2-9b0d-40fe-be14-ec6bb38ffab7', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains direct, unmediated access to spiritual liberation, bypassing traditional caste-based hierarchies and ritual requirements. Finds empowerment and validation in a path accessible to all.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    powerless, generational, mobile, global).

% Promotes and interprets the Gita through a devotional lens, emphasizing egalitarian access to spiritual practice. Benefits from the expansion of its adherents and the validation of its theological stance against more exclusive interpretations.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_traditions, agenda_setter,
    organized, civilizational, mobile, continental).

% Experiences a challenge to its traditional gatekeeping authority and caste-based social order. The universalist reading undermines its exclusive claims to scriptural interpretation and ritual mediation, leading to a loss of influence and status.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authorities, payer,
    institutional, generational, constrained, national).

% Analyzes the text for its ethical implications, often aligning with the universalist message of devotion over ritual, but primarily focused on non-violence and social justice rather than purely spiritual liberation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a diverse spiritual community around a shared devotional path, providing a framework for ethical conduct and spiritual practice that transcends social divisions.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from traditional caste-based intermediaries to individual devotees, fostering a direct relationship with the divine. It also transfers social capital and influence to bhakti traditions.
% ABSENT_VOICES: Strict traditionalists who believe in the immutable, divinely ordained nature of caste-based dharma would object, arguing that this reading distorts the text's true meaning and undermines social order. They are often marginalized in contemporary academic and popular discourse.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the spiritual landscape of Hinduism would be significantly altered. Bhakti traditions would lose a foundational text for their universalist claims, potentially leading to a resurgence of more exclusive, caste-based interpretations and practices. Many devotees would lose a path to spiritual fulfillment that resonates with modern egalitarian values.
% FOUNDING_PROBLEM: The problem of spiritual exclusion and social stratification inherent in rigid caste systems, which limited access to religious practice and liberation for vast segments of society.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of religion, social historians, and contemporary spiritual leaders from diverse backgrounds corroborate the ongoing struggle against caste discrimination and the historical role of devotional movements in challenging it. This is attested by academic publications, social reform movements, and the lived experiences of marginalized communities.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this reading primarily functions to dissolve barriers and open access, rather than to extract resources or labor. Suppression is also low (0.3) as its persistence relies on the appeal of its message and the agency of devotees, not active coercion. Theater ratio is minimal (0.1) as the core message is direct and functional in its aim to provide spiritual guidance. Resistance is high (0.7) because this reading directly challenges entrenched social and religious hierarchies, leading to ongoing contestation from traditionalists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the universal devotee class and bhakti traditions, this reading is a liberating 'rope' that provides a clear, accessible path to spiritual fulfillment. From the perspective of orthodox Brahminical authorities, it is a 'snare' that undermines their traditional authority and social order, leading to a loss of influence and control over religious practice.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal devotee class and bhakti traditions are clear beneficiaries (d near 0.0) as they gain spiritual access and influence, respectively. Orthodox Brahminical authorities are targets (d near 1.0) as their traditional power is challenged and diminished by this reading's widespread acceptance. The reading itself acts as a 'rope' for those it coordinates, but its impact on existing power structures is extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the universalist devotional reading as a 'snare' by focusing on its primary function of coordination and liberation for its adherents, while acknowledging its 'extractive' impact on traditional gatekeepers. It highlights that the 'mandate' of this reading is to provide an inclusive spiritual path, which remains highly relevant and 'live' in contemporary society, thus avoiding mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_as_spiritual_barrier_dissolved,
    'To what extent has the universalist devotional reading truly dissolved caste as a spiritual barrier in practice, versus merely offering an alternative framework?',
    'Sociological studies tracking participation in devotional movements across caste lines, and analysis of the actual social mobility and spiritual access of lower-caste individuals within these traditions.',
    'If caste barriers persist significantly despite the reading''s influence, its ''accessibility collapse'' might be lower than currently assessed, indicating a more constrained ''rope'' or even a ''tangled rope'' for some, where the coordination function is partially undermined by persistent social structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_as_spiritual_barrier_dissolved, empirical, 'Empirical impact of the reading on caste-based spiritual exclusion.').

omega_variable(
    dharma_redefinition_acceptance,
    'Is the redefinition of dharma as surrender to divine will, rather than social role, widely accepted across all segments of Hindu society, or is it primarily confined to specific devotional movements?',
    'Textual analysis of contemporary religious discourse, surveys of religious belief among different Hindu communities, and ethnographic studies of dharma''s practical application in daily life.',
    'If the redefinition is not widely accepted, the ''resistance'' to this reading from traditionalists might be higher, and its overall ''scope'' of influence more limited, potentially shifting its classification towards a ''tangled rope'' due to ongoing, unresolved contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_redefinition_acceptance, empirical, 'Societal acceptance of dharma''s redefinition.').

omega_variable(
    kernel_reading_relationship_to_orthodox_literal,
    'Does the universalist devotional reading logically foreclose the ''orthodox_literal_reading'' within a single coherent theological framework, or do they merely represent competing interpretations that can coexist?',
    'Detailed philosophical and theological analysis of the core premises of both readings to identify direct logical contradictions that cannot be reconciled within a broader interpretive meta-framework.',
    'If it forecloses the orthodox reading, the structural impact on traditional authority is more profound, potentially leading to a more rapid ''authority erosion'' for the orthodox view. If they merely coexist, the contestation remains ongoing, and the ''resistance'' metric for this reading might be higher due to persistent, viable alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_to_orthodox_literal, conceptual, 'Conceptual relationship between universalist devotional and orthodox literal readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(gita_tr_t1930, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(gita_tr_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(gita_tr_t1990, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(gita_tr_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(gita_be_t1930, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement(gita_be_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(gita_be_t1990, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(gita_be_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(gita_su_t1930, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement(gita_su_t1960, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(gita_su_t1990, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(gita_su_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gita_kurukshetra_discourse' kernel, focusing on universalist devotion. It is distinct from the 'orthodox_literal_reading' and 'gandhian_allegorical_reading', which offer different interpretations of caste, duty, and violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
