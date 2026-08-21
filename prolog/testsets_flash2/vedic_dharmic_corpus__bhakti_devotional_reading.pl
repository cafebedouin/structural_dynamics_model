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
 *   Dharmic corpus, which asserts that direct devotional access to the divine
 *   bypasses traditional caste requirements, and sincere devotion (bhakti)
 *   rather than birth determines spiritual authority. This reading offers a
 *   more egalitarian path to spiritual realization, reducing the
 *   extractiveness of the traditional caste system but not fully dismantling
 *   it. It is claimed as a Rope because it genuinely coordinates spiritual
 *   access for many, but its moderate extractiveness and suppression reflect
 *   the ongoing friction with entrenched hereditary authority.
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
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'a8ecf094-0998-4896-a6a5-a7ffe734287c').
narrative_ontology:cs_kernel_codification('a8ecf094-0998-4896-a6a5-a7ffe734287c', fixed_text).
narrative_ontology:cs_authority_grounding('a8ecf094-0998-4896-a6a5-a7ffe734287c', practice).
narrative_ontology:cs_interpretation_layer_present('a8ecf094-0998-4896-a6a5-a7ffe734287c').
narrative_ontology:cs_reading_relation('a8ecf094-0998-4896-a6a5-a7ffe734287c', vedic_dharmic_corpus__hereditary_monopoly_reading, influences).
narrative_ontology:cs_reading_relation('a8ecf094-0998-4896-a6a5-a7ffe734287c', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('a8ecf094-0998-4896-a6a5-a7ffe734287c', foundational, sincere_devotion_is_supreme_path).
narrative_ontology:cs_axiom_status(sincere_devotion_is_supreme_path, holdable).
narrative_ontology:cs_axiom_grounding('a8ecf094-0998-4896-a6a5-a7ffe734287c', sincere_devotion_is_supreme_path, deontological).
narrative_ontology:cs_axiom('a8ecf094-0998-4896-a6a5-a7ffe734287c', foundational, divine_grace_is_accessible_to_all).
narrative_ontology:cs_axiom_status(divine_grace_is_accessible_to_all, holdable).
narrative_ontology:cs_axiom_grounding('a8ecf094-0998-4896-a6a5-a7ffe734287c', divine_grace_is_accessible_to_all, theological).
narrative_ontology:cs_reference_frame('a8ecf094-0998-4896-a6a5-a7ffe734287c', universal_devotional_access).
narrative_ontology:cs_drift_state('a8ecf094-0998-4896-a6a5-a7ffe734287c', contemporary_pluralistic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a8ecf094-0998-4896-a6a5-a7ffe734287c', '').
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

% Hereditary priests who traditionally held exclusive rights to ritual performance and scriptural interpretation. The rise of bhakti challenges their monopoly on spiritual authority and ritual income, forcing them to adapt or lose influence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, national).

% Individuals and communities whose social status and identity are deeply intertwined with the traditional caste system. The bhakti reading undermines the divine justification for their social position, creating cognitive dissonance and resistance to change.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchy_adherents, payer,
    organized, generational, identity_locked, local).

% Advocates for social equality who see bhakti as a historical precursor or partial fulfillment of their goals, but argue it doesn't go far enough in dismantling caste. They analyze its impact on social structures and push for more radical reform.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice and community formation around personal devotion, allowing individuals to access the divine directly without intermediaries, fostering inclusive religious communities.
% TRANSFER_FUNCTION: Transfers spiritual authority and social recognition from hereditary Brahminical lineages to individuals based on their sincere devotion and spiritual merit, democratizing access to religious life.
% ABSENT_VOICES: Strict traditionalists who believe any deviation from caste-based ritual is a violation of dharma would object, but their voices are increasingly marginalized in many public spheres, or they retreat to insular communities.
% DISAPPEARANCE_RATIONALE: If the bhakti devotional reading vanished, many inclusive spiritual communities would dissolve, individuals would lose a direct path to the divine, and the traditional caste hierarchy's spiritual claims would regain unchallenged dominance, leading to a significant reorganization of religious and social life.
% FOUNDING_PROBLEM: The problem of spiritual access being restricted by birth and ritual complexity, leading to exclusion and a perceived disconnect between common people and the divine.
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movements across centuries attest to the persistent problem of caste-based exclusion. Contemporary sociological studies and religious scholars (outside the immediate bhakti communities) corroborate that the issue of spiritual access and social hierarchy remains a live concern, even if its manifestations have evolved.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.40) because while it opens spiritual access, it doesn't fully eliminate the social and economic disadvantages of lower castes, nor does it fully dislodge the traditional priesthood. Suppression is low (0.30) as bhakti traditions generally do not rely on coercion, but rather on voluntary adherence and spiritual appeal. Resistance is moderate (0.55) due to the ongoing pushback from traditionalists. Accessibility collapse is moderate (0.45) as it provides a viable alternative, but not one that completely collapses the traditional system. Theater ratio is low (0.10) as the devotional practices are sincere and functional, not performative cover.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bhakti devotees, this is a liberating Rope, offering genuine spiritual coordination. From the traditional Brahminical priesthood, it is a threat to their established authority and a source of extraction from their traditional role. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Bhakti devotees and gurus are beneficiaries (d near 0.0) as they gain spiritual access and authority. The traditional Brahminical priesthood and caste hierarchy adherents are payers (d near 1.0) as their exclusive authority and social status are challenged and diminished by this reading. Reformist egalitarian activists are observers, analyzing its impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_social_impact_ambiguity,
    'To what extent does the ''bhakti devotional reading'' genuinely dismantle caste-based social stratification, versus merely offering a spiritual bypass that leaves secular hierarchies intact?',
    'Longitudinal sociological studies tracking inter-caste relations, economic mobility, and political representation in regions dominated by bhakti traditions, compared to regions with strong hereditary adherence.',
    'If it primarily offers a spiritual bypass, its effective extractiveness (from lower castes) remains higher than measured, as it doesn''t address material conditions. If it drives significant social leveling, its extractiveness is lower, and its coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_social_impact_ambiguity, empirical, 'The actual social impact of bhakti on caste hierarchy beyond spiritual access.').

omega_variable(
    interpretive_authority_ambiguity,
    'Is the ''bhakti devotional reading'' a genuine re-interpretation of the Vedic Dharmic corpus, or a separate, parallel spiritual tradition that merely coexists with the traditional readings?',
    'Textual analysis by independent scholars on the hermeneutical methods used by bhakti gurus to derive their interpretations from canonical texts, and their engagement with traditional commentaries.',
    'If it''s a re-interpretation, it directly challenges the ''hereditary_monopoly_reading'' within the same framework. If it''s a parallel tradition, it merely offers an alternative, reducing direct conflict but also limiting its capacity to structurally alter the traditional reading''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_ambiguity, conceptual, 'Whether bhakti is an internal re-interpretation or an external alternative to traditional Vedic readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_dharmic_corpus' kernel. This 'bhakti devotional reading' emphasizes personal devotion over birthright, offering a more inclusive spiritual path. It coexists with and influences the 'hereditary monopoly reading' (which upholds caste-based ritual authority) and the 'reformist egalitarian reading' (which seeks to dismantle caste entirely based on modern equality principles). Each reading has distinct extractiveness and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
