% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine
 *   domain: religious_authority/social_stratification
 *
 * SUMMARY:
 *   This constraint story describes the 'bhakti devotional' reading of the
 *   Vedic/Dharmic corpus, which asserts that sincere devotion (bhakti) offers
 *   direct access to the divine, bypassing traditional caste-based ritual
 *   requirements. This reading challenges the hereditary monopoly on
 *   spiritual authority, offering a more egalitarian path. While it
 *   significantly reduces the extractiveness and suppression associated with
 *   a purely birth-based system, it operates within a broader social context
 *   where caste hierarchies often persist, leading to a moderate level of
 *   extractiveness and ongoing resistance from traditionalists.
 *
 * KEY AGENTS:
 *   - devotees_of_all_varnas: Primary beneficiary (moderate/mobile) — gains spiritual access
 *   - bhakti_gurus: Agenda setter/beneficiary (organized/mobile) — propagates the path
 *   - traditional_brahminical_priesthood: Payer (institutional/constrained) — loses exclusive authority
 *   - hereditary_monopoly_adherents: Excluded (institutional/identity_locked) — rejects this reading's legitimacy
 *   - reformist_egalitarian_scholars: Observer (analytical/analytical) — analyzes its social impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious_authority/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '0d088011-e6d7-4bd7-8f48-47d146060170').
narrative_ontology:cs_kernel_codification('0d088011-e6d7-4bd7-8f48-47d146060170', fixed_text).
narrative_ontology:cs_authority_grounding('0d088011-e6d7-4bd7-8f48-47d146060170', lineage).
narrative_ontology:cs_interpretation_layer_present('0d088011-e6d7-4bd7-8f48-47d146060170').
narrative_ontology:cs_reading_relation('0d088011-e6d7-4bd7-8f48-47d146060170', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d088011-e6d7-4bd7-8f48-47d146060170', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('0d088011-e6d7-4bd7-8f48-47d146060170', foundational, devotion_supersedes_birth).
narrative_ontology:cs_axiom_status(devotion_supersedes_birth, holdable).
narrative_ontology:cs_axiom_grounding('0d088011-e6d7-4bd7-8f48-47d146060170', devotion_supersedes_birth, deontological).
narrative_ontology:cs_axiom('0d088011-e6d7-4bd7-8f48-47d146060170', foundational, divine_access_is_universal).
narrative_ontology:cs_axiom_status(divine_access_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('0d088011-e6d7-4bd7-8f48-47d146060170', divine_access_is_universal, theological).
narrative_ontology:cs_reference_frame('0d088011-e6d7-4bd7-8f48-47d146060170', universal_spiritual_access).
narrative_ontology:cs_drift_state('0d088011-e6d7-4bd7-8f48-47d146060170', contemporary_pluralism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d088011-e6d7-4bd7-8f48-47d146060170', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals from all social strata who find spiritual fulfillment and community through direct devotional practices, bypassing traditional caste-based ritual requirements. They gain spiritual agency and social recognition within bhakti traditions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas, beneficiary,
    moderate, biographical, mobile, regional).

% Spiritual teachers and leaders who propagate devotional paths, interpret scriptures through a bhakti lens, and establish communities centered on devotion. They gain spiritual authority and influence, often independent of birthright.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus, beneficiary).

% Hereditary priests whose traditional monopoly on ritual performance and scriptural interpretation is challenged by the direct access offered by bhakti. They experience a loss of exclusive authority and, in some cases, material support.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, national).

% Those who strictly uphold the view that ritual and interpretive authority are solely derived from birth into Brahmin lineage. They view bhakti's claims as illegitimate or secondary, and are excluded from the interpretive framework of this reading.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_adherents, excluded,
    institutional, generational, identity_locked, national).

% Academics and activists who analyze religious texts and practices through the lens of modern equality principles, often advocating for a more radical dismantling of caste hierarchy than bhakti traditions typically achieve. They observe the dynamics of bhakti's challenge to tradition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal and accessible path to spiritual fulfillment and community, coordinating diverse individuals around shared devotional practices and bypassing rigid social stratification based on birth.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from birthright and ritual exclusivity to individual devotion and sincere faith; transfers social capital and legitimacy to bhakti movements and their gurus.
% ABSENT_VOICES: Strict adherents to the hereditary monopoly of ritual authority are structurally excluded from the interpretive framework of this reading; they would argue that spiritual access is mediated solely through prescribed rituals performed by qualified (birth-defined) priests.
% DISAPPEARANCE_RATIONALE: If the concept of direct devotional access vanished, spiritual life would largely revert to stricter hierarchical control, potentially leading to widespread spiritual alienation, social unrest, and a significant loss of agency for many individuals seeking spiritual paths outside traditional structures.
% FOUNDING_PROBLEM: The spiritual inaccessibility and social rigidity imposed by a purely birth-based ritual system, leading to widespread spiritual alienation and a sense of exclusion for those outside the privileged castes.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the emergence and growth of bhakti movements, philosophical treatises advocating universal devotion, and contemporary sociological studies documenting the ongoing tension between traditional hierarchy and inclusive spiritual paths, all corroborate the persistence of this problem.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'rope' because bhakti genuinely coordinates spiritual access for a broad base of participants, offering a net benefit to devotees. However, the 'extractiveness' is moderate (0.40) because while it bypasses some caste restrictions, it doesn't fully dismantle the broader social stratification, and traditional authorities still exert influence, leading to a contested spiritual economy. Suppression (0.35) is lower than a strict hereditary system but not zero, as social pressures and traditional norms still exist. Theater ratio (0.15) is low, reflecting the genuine spiritual practice at its core. Resistance (0.50) is moderate, indicating ongoing tension with traditional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of devotees and bhakti gurus, this reading is a liberating 'rope' that opens spiritual paths. From the perspective of traditional Brahminical priests, it represents a challenge to their divinely ordained authority, potentially seen as a 'snare' that undermines their social and religious standing. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Devotees of all varnas and bhakti gurus are beneficiaries, gaining spiritual agency and authority. The traditional Brahminical priesthood acts as a 'payer' in this reading, as their exclusive claims to ritual authority are diminished, leading to a loss of influence and potential material support. The constraint's operation shifts spiritual capital away from birthright to devotion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Vedic/Dharmic corpus, or merely a variant within the broader hereditary framework?',
    'Analysis of scriptural commentaries and historical theological debates: if bhakti interpretations fundamentally reframe core tenets, it''s a distinct reading; if it''s an accommodated variant, it''s a sub-reading of the hereditary kernel.',
    'If a distinct reading, it confirms the kernel''s contestability and the validity of multiple constraint instantiations. If a variant, it suggests the hereditary kernel is more robust and capable of absorbing challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing a distinct kernel reading from a sub-variant.').

omega_variable(
    extent_of_caste_bypass,
    'To what extent does direct devotional access truly bypass caste requirements in practice, versus merely mitigating them or creating parallel, but still stratified, spiritual communities?',
    'Sociological field studies and ethnographic research on contemporary bhakti movements: measure social mobility, inter-caste relations, and access to leadership roles within these communities.',
    'If bypass is extensive, the constraint''s extractiveness and suppression are lower than currently estimated. If bypass is limited, the constraint''s effective extractiveness remains higher, indicating a ''tangled rope'' where coordination benefits are still layered on existing stratification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_caste_bypass, empirical, 'Empirical assessment of the practical efficacy of caste bypass through bhakti.').

omega_variable(
    persistence_of_traditional_authority,
    'How much does the traditional Brahminical priesthood''s authority persist despite the rise of bhakti, and what mechanisms sustain it?',
    'Analysis of patronage networks, institutional endowments, and continued demand for traditional rituals among certain segments of society.',
    'If traditional authority remains strong, the ''payer'' role for the priesthood is less severe, and the overall ''rope'' classification might lean more towards ''tangled rope'' due to the enduring, albeit challenged, extractive structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_of_traditional_authority, empirical, 'Measuring the resilience and mechanisms of traditional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 80, 0.34).
narrative_ontology:measurement(vedi_su_t100, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'vedic_dharmic_corpus' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
