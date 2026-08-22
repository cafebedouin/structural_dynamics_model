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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading
 *   domain: religious_studies/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'universalist devotional' reading of the
 *   Bhagavad Gita's Kurukshetra discourse. In this reading, the text teaches
 *   that devotion (bhakti) is the supreme path to spiritual liberation,
 *   accessible to all individuals regardless of their caste or social
 *   standing. Dharma is reinterpreted as surrender to divine will rather than
 *   strict adherence to prescribed social roles. This reading fundamentally
 *   challenges traditional Brahminical gatekeeping and caste-based spiritual
 *   exclusion, promoting an egalitarian spiritual access. The constraint's
 *   claimed type is 'rope' because it genuinely coordinates a broad spiritual
 *   community and offers net benefits to its participants, particularly the
 *   historically marginalized, with minimal inherent extraction.
 *
 * KEY AGENTS:
 *   - universal_devotee_class: Primary beneficiary (organized/mobile)
 *   - marginalized_castes: Primary beneficiary (powerless/identity_locked)
 *   - traditional_brahminical_priesthood: Primary payer (institutional/constrained)
 *   - orthodox_scholars: Secondary payer (powerful/constrained)
 *   - gandhian_pacifists: Analytical observer (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.2).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3').
narrative_ontology:cs_kernel_codification('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', fixed_text).
narrative_ontology:cs_authority_grounding('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', lineage).
narrative_ontology:cs_interpretation_layer_present('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3').
narrative_ontology:cs_reading_relation('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', gita_kurukshetra_discourse__orthodox_literal_reading, influences).
narrative_ontology:cs_reading_relation('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', foundational, bhakti_marga_supreme).
narrative_ontology:cs_axiom_status(bhakti_marga_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', bhakti_marga_supreme, deontological).
narrative_ontology:cs_axiom('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', foundational, caste_no_spiritual_barrier).
narrative_ontology:cs_axiom_status(caste_no_spiritual_barrier, holdable).
narrative_ontology:cs_axiom_grounding('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', caste_no_spiritual_barrier, deontological).
narrative_ontology:cs_reference_frame('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', egalitarian_devotional_path).
narrative_ontology:cs_drift_state('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1ac3793f-6da1-4db1-8d3b-be6a1cfaf7b3', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_priesthood).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_marga_supremacy).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, egalitarian_spiritual_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Finds spiritual liberation and purpose through direct devotion, unmediated by traditional caste hierarchies. This reading empowers them by validating their spiritual path and dissolving traditional barriers to salvation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    organized, generational, mobile, global).

% Are explicitly granted access to spiritual paths previously denied or restricted by caste. This reading offers a powerful counter-narrative to their traditional social and religious subjugation, providing dignity and hope.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_castes, beneficiary,
    powerless, generational, identity_locked, regional).

% Experiences a loss of exclusive spiritual authority and gatekeeping power. Their traditional role as sole interpreters and mediators of dharma is challenged, leading to a reduction in their social and religious influence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, national).

% Find their literal and caste-affirming interpretations of the Gita undermined. Their academic and religious careers may be built on defending traditional readings, making this universalist interpretation a direct challenge to their intellectual and social capital.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars, payer,
    powerful, biographical, constrained, national).

% While not directly benefiting or paying, they observe this reading as a partial ally in de-emphasizing violence, though they might still find its devotional focus less aligned with their primary ethical concerns than their own allegorical reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_pacifists, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a diverse spiritual community around a shared devotional path, transcending social divisions and providing a common framework for ethical conduct rooted in divine surrender.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from traditional caste-based gatekeepers to individual devotees, regardless of their social standing. It also transfers a sense of agency and dignity to marginalized groups.
% ABSENT_VOICES: Those who benefit from strict caste hierarchy and exclusive spiritual access are structurally excluded from this reading's interpretive community; they would argue for the divine sanction of social stratification and the necessity of ritual mediation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the spiritual landscape for millions would revert to more restrictive, caste-bound interpretations, leading to a loss of agency and spiritual access for marginalized groups, and a re-consolidation of traditional priestly authority.
% FOUNDING_PROBLEM: The problem of spiritual exclusion and social stratification based on birth, where access to divine grace and liberation was denied to vast segments of society.
% FOUNDING_PROBLEM_CORROBORATION: Numerous devotional movements and social reform efforts throughout history, as well as contemporary academic scholarship on subaltern religious practices, corroborate the ongoing problem of spiritual exclusion and the historical role of this reading in challenging it. This corroboration comes from outside the immediate beneficiary groups, from historical records and sociological analyses.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.2) because this reading primarily functions to open access and empower, rather than to extract from its adherents. Any 'cost' is largely the relinquishing of ego or attachment, which is framed as beneficial. Suppression is also low (0.1) as the reading's persistence relies on its appeal and transformative power, not coercion. Theater ratio is minimal (0.05) because its core message is direct and functional in fostering devotion. Accessibility collapse is high (0.8) because once understood, the path of universal devotion is presented as universally available and superior, making alternatives (like strict ritualism or caste-based paths) less appealing. Resistance is moderate (0.15) from those whose traditional authority is challenged.
 *
 * PERSPECTIVAL GAP:
 *   The traditional Brahminical priesthood and orthodox scholars experience this reading as highly extractive, as it directly undermines their social status, interpretive authority, and economic base. For the universal devotee class and marginalized castes, it is profoundly beneficial, offering liberation and dignity. The engine's per-seat classification will reflect this divergence, with payers experiencing a 'snare' or 'tangled_rope' due to the loss of their traditional rents, while beneficiaries experience a 'rope' or even a 'mountain' of spiritual truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal devotee class and marginalized castes are clear beneficiaries (d near 0.0) as the reading grants them spiritual access and agency. The traditional Brahminical priesthood and orthodox scholars are payers (d near 1.0) because this reading directly challenges and diminishes their established authority and social capital. The reading subsidizes the former by dissolving barriers, and extracts from the latter by eroding their exclusive claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resolves a form of mandatrophy present in older, more restrictive interpretations of the Gita. It re-establishes a 'live' founding problem (spiritual exclusion) and offers a solution that is still relevant, preventing the constraint from becoming a 'piton' of outdated ritualism. The classification as 'rope' prevents mislabeling it as pure extraction, acknowledging its genuine coordination and benefit-generating function for a broad base of adherents, while still recognizing the 'extraction' it performs on traditional gatekeepers by dissolving their power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, distinct reading of the Bhagavad Gita kernel, or merely a reformist interpretation within a broader orthodox framework?',
    'Analysis of historical reception and independent theological development: if it generated distinct schools of thought and practice, it''s a distinct reading.',
    'If a distinct reading, its classification stands. If merely an interpretation, its structural independence is weaker, potentially making it a ''tangled_rope'' within the larger orthodox ''snare''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading constitutes a structurally independent constraint.').

omega_variable(
    violence_interpretation_ambiguity,
    'Does this universalist devotional reading fully neutralize the ''righteous violence'' aspect of the Kurukshetra discourse, or does it merely de-emphasize it, leaving an ambiguity that could be re-activated?',
    'Textual analysis of how proponents of this reading address the violence, and historical instances of its re-interpretation in contexts of conflict.',
    'If fully neutralized, the reading''s ''rope'' classification is robust. If merely de-emphasized, a latent ''snare'' potential remains, increasing its effective extractiveness in certain contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_interpretation_ambiguity, empirical, 'Ambiguity regarding the text''s stance on violence within this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditional authority structural (loss of social legitimacy) or internalized (traditionalists adopting devotional practices)?',
    'Sociological studies tracking the actual decline in traditional priestly roles versus the adoption of devotional practices by former gatekeepers.',
    'If structural, the constraint''s impact on traditional authority is external and measurable. If internalized, the ''suppression'' is a transformation, and the overall extractiveness from the traditional seat might be lower than perceived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 500, 0.04).
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1000, 0.03).
narrative_ontology:measurement(gita_tr_t1500, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1500, 0.04).
narrative_ontology:measurement(gita_tr_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(gita_be_t1500, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(gita_be_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(gita_su_t1500, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1500, 0.09).
narrative_ontology:measurement(gita_su_t2024, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gita_kurukshetra_discourse' kernel. Each reading instantiates a different constraint with its own ε and classification. This reading emphasizes universal devotion and egalitarian access, contrasting with the orthodox literal reading (caste-based duty, righteous violence) and the Gandhian allegorical reading (internal struggle, non-violence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
