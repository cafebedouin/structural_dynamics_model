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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint represents the 'bhakti devotional' reading of the Vedic
 *   Dharmic corpus, which asserts that direct, sincere devotion to the divine
 *   bypasses traditional caste requirements for spiritual authority. It
 *   emphasizes personal experience and inner feeling over birthright and
 *   ritual formality. This reading functions as a 'rope' by coordinating
 *   spiritual access for a broader population, but it still operates within a
 *   larger system where caste-based authority (the 'hereditary monopoly'
 *   reading) remains a powerful, extractive force. The extractiveness is
 *   moderate (0.40) because while it offers an alternative, it doesn't fully
 *   dismantle the underlying caste system, and some forms of extraction
 *   persist (e.g., through charismatic gurus). Suppression is low (0.30) as
 *   its persistence relies more on voluntary adherence and spiritual appeal
 *   than active coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'a505a1c4-d652-406b-95e6-6f9eed03a4a0').
narrative_ontology:cs_kernel_codification('a505a1c4-d652-406b-95e6-6f9eed03a4a0', fixed_text).
narrative_ontology:cs_authority_grounding('a505a1c4-d652-406b-95e6-6f9eed03a4a0', practice).
narrative_ontology:cs_interpretation_layer_present('a505a1c4-d652-406b-95e6-6f9eed03a4a0').
narrative_ontology:cs_reading_relation('a505a1c4-d652-406b-95e6-6f9eed03a4a0', vedic_dharmic_corpus__hereditary_monopoly_reading, influences).
narrative_ontology:cs_reading_relation('a505a1c4-d652-406b-95e6-6f9eed03a4a0', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('a505a1c4-d652-406b-95e6-6f9eed03a4a0', foundational, sincere_devotion_is_supreme_spiritual_path).
narrative_ontology:cs_axiom_status(sincere_devotion_is_supreme_spiritual_path, holdable).
narrative_ontology:cs_axiom_grounding('a505a1c4-d652-406b-95e6-6f9eed03a4a0', sincere_devotion_is_supreme_spiritual_path, deontological).
narrative_ontology:cs_axiom('a505a1c4-d652-406b-95e6-6f9eed03a4a0', foundational, divine_grace_is_accessible_to_all).
narrative_ontology:cs_axiom_status(divine_grace_is_accessible_to_all, holdable).
narrative_ontology:cs_axiom_grounding('a505a1c4-d652-406b-95e6-6f9eed03a4a0', divine_grace_is_accessible_to_all, theological).
narrative_ontology:cs_reference_frame('a505a1c4-d652-406b-95e6-6f9eed03a4a0', universal_devotional_access).
narrative_ontology:cs_drift_state('a505a1c4-d652-406b-95e6-6f9eed03a4a0', contemporary_bhakti_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a505a1c4-d652-406b-95e6-6f9eed03a4a0', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchy_adherents).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, personal_devotion_as_spiritual_path).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_grace_is_universal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals from all social strata who find spiritual fulfillment and community through direct devotional practices, bypassing traditional caste-based ritual requirements. They benefit from a more accessible and inclusive spiritual path.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_devotees, beneficiary,
    moderate, biographical, mobile, regional).

% Spiritual teachers and leaders who propagate bhakti traditions, emphasizing personal devotion over ritual and birth. They establish and maintain devotional communities and interpret scriptures through a bhakti lens, gaining authority through their spiritual charisma and following.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus, agenda_setter,
    organized, generational, mobile, regional).

% Hereditary priests who traditionally held exclusive rights to ritual performance and scriptural interpretation based on birth. The rise of bhakti challenges their monopoly on spiritual authority and ritual income, forcing them to adapt or lose influence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, national).

% Individuals and communities who uphold the traditional caste system as divinely ordained. They perceive bhakti's egalitarian tendencies as a threat to social order and their established status, experiencing a loss of social capital and legitimacy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchy_adherents, payer,
    organized, generational, constrained, local).

% Advocates for social equality and constitutional principles who see bhakti as a positive, but often insufficient, step towards dismantling caste. They analyze its impact and push for more radical structural changes.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice and community formation around personal devotion, allowing individuals to connect with the divine directly without intermediaries, fostering inclusive spiritual movements.
% TRANSFER_FUNCTION: Transfers spiritual authority and social legitimacy from birth-based caste hierarchies to individual sincerity and devotional practice. It also transfers followers and resources from traditional priestly institutions to bhakti movements and gurus.
% ABSENT_VOICES: Strict traditionalists who believe any deviation from hereditary ritual authority is a sacrilege are often marginalized or dismissed by bhakti movements, which prioritize personal experience over rigid adherence to caste-based ritual codes. Their objections are framed as resistance to spiritual progress.
% DISAPPEARANCE_RATIONALE: If the principle of direct devotional access vanished, spiritual authority would revert entirely to hereditary lines, caste-based discrimination in religious practice would intensify, and many existing spiritual communities would dissolve or be forced underground. The social and religious landscape would fundamentally shift.
% FOUNDING_PROBLEM: The problem of spiritual access being restricted by birth, leading to exclusion and a perceived lack of genuine spiritual experience for many, alongside the ossification of ritual into mere form.
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movements themselves, and many social historians and religious scholars outside the traditional Brahminical priesthood, corroborate that the problem of caste-based spiritual exclusion remains live, even if its forms have evolved. The continued appeal and growth of bhakti traditions across diverse populations attest to this ongoing need.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is moderate because while bhakti offers a path to spiritual liberation, it often creates new forms of authority (gurus, devotional lineages) that can, in turn, become extractive. It also doesn't fully eliminate the social disadvantages of lower castes, even if it offers spiritual equality. Suppression is low because bhakti's spread is largely through voluntary conversion and charismatic appeal, rather than coercive enforcement. The theater ratio is low because the devotional practices are generally sincere and functional for their adherents, with little performative maintenance for its own sake. The decreasing extractiveness and suppression over time reflect the growing acceptance and institutionalization of bhakti traditions, making them less 'resistant' and more integrated into the broader religious landscape, thus reducing the friction and cost of participation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bhakti devotees, this is a liberating 'rope' that offers direct access to the divine. From the perspective of the traditional Brahminical priesthood, it is a 'snare' that undermines divinely ordained social order and their legitimate authority. The engine's classification will reflect the structural reality of this reading, which offers coordination but still exists within a contested, partially extractive system.
 *
 * DIRECTIONALITY LOGIC:
 *   Bhakti devotees and gurus are beneficiaries, as they gain spiritual access and authority, respectively. The traditional Brahminical priesthood and caste hierarchy adherents are payers, as their traditional monopoly on spiritual authority and social status is challenged and eroded by the bhakti movement. Reformist egalitarian activists observe, analyzing its impact on broader social justice goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_as_new_extraction,
    'Does the ''direct access'' offered by bhakti merely replace one form of extraction (caste-based) with another (guru-centric or institutionalized bhakti movements)?',
    'Empirical study of resource flows and power dynamics within established bhakti organizations and guru lineages, comparing them to traditional Brahminical structures.',
    'If new forms of extraction are significant, the extractiveness metric for this reading would need to be adjusted upward, potentially shifting its classification towards a ''tangled_rope'' or even ''snare'' from the perspective of some adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_as_new_extraction, empirical, 'Assesses whether bhakti''s anti-caste stance is fully realized or if new hierarchies emerge.').

omega_variable(
    caste_system_persistence,
    'To what extent does the ''bhakti devotional reading'' genuinely undermine the caste system''s social and economic power, versus merely offering a spiritual bypass that leaves structural inequalities intact?',
    'Sociological analysis of inter-caste relations, economic mobility, and political representation in regions with strong bhakti traditions, compared to regions without.',
    'If structural inequalities persist largely unchanged, the ''suppression'' metric for this reading might be higher than currently assessed, as the underlying coercive force of caste remains, even if spiritual access is broadened. This would highlight the limits of spiritual reform without social revolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_system_persistence, empirical, 'Examines the real-world impact of bhakti on caste-based social stratification.').

omega_variable(
    interpretive_legitimacy_source,
    'Is the authority of bhakti gurus derived from their spiritual charisma and personal experience, or does it implicitly rely on a selective interpretation of the very Vedic texts that also support caste hierarchy?',
    'Textual analysis of bhakti commentaries and philosophical treatises, tracing their hermeneutical strategies for reinterpreting or de-emphasizing caste-affirming passages in the Vedic corpus.',
    'If bhakti''s legitimacy is found to be heavily dependent on re-interpreting traditional texts rather than purely on direct experience, it would highlight a deeper ''tangled rope'' dynamic within the interpretive layer, where the coordination of new spiritual paths is still bound by the authority of the original, contested kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_legitimacy_source, conceptual, 'Clarifies the source of interpretive authority for bhakti traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_dharmic_corpus' kernel. This 'bhakti devotional reading' offers an alternative path to spiritual authority, influencing but not fully foreclosing the 'hereditary monopoly reading' and coexisting with the 'reformist egalitarian reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
