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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading of Vedic Dharmic Corpus
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint represents the 'bhakti devotional' reading of the Vedic
 *   Dharmic Corpus, which asserts that direct devotional access to the divine
 *   bypasses caste requirements, and sincere devotion (bhakti) rather than
 *   birth determines spiritual authority. This reading challenges the
 *   hereditary monopoly of the Brahmin priesthood, offering a more inclusive
 *   path to spiritual realization. While it reduces extraction and
 *   suppression compared to more rigid interpretations, it does not fully
 *   dismantle the caste system, leading to ongoing contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading of Vedic Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '100deb44-3edc-41c8-82b7-5172c8d9683c').
narrative_ontology:cs_kernel_codification('100deb44-3edc-41c8-82b7-5172c8d9683c', fixed_text).
narrative_ontology:cs_authority_grounding('100deb44-3edc-41c8-82b7-5172c8d9683c', lineage).
narrative_ontology:cs_interpretation_layer_present('100deb44-3edc-41c8-82b7-5172c8d9683c').
narrative_ontology:cs_reading_relation('100deb44-3edc-41c8-82b7-5172c8d9683c', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('100deb44-3edc-41c8-82b7-5172c8d9683c', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('100deb44-3edc-41c8-82b7-5172c8d9683c', foundational, sincere_devotion_is_supreme_path).
narrative_ontology:cs_axiom_status(sincere_devotion_is_supreme_path, holdable).
narrative_ontology:cs_axiom_grounding('100deb44-3edc-41c8-82b7-5172c8d9683c', sincere_devotion_is_supreme_path, deontological).
narrative_ontology:cs_axiom('100deb44-3edc-41c8-82b7-5172c8d9683c', foundational, birth_is_irrelevant_to_spiritual_merit).
narrative_ontology:cs_axiom_status(birth_is_irrelevant_to_spiritual_merit, holdable).
narrative_ontology:cs_axiom_grounding('100deb44-3edc-41c8-82b7-5172c8d9683c', birth_is_irrelevant_to_spiritual_merit, deontological).
narrative_ontology:cs_reference_frame('100deb44-3edc-41c8-82b7-5172c8d9683c', bhakti_inclusive_spiritual_access).
narrative_ontology:cs_drift_state('100deb44-3edc-41c8-82b7-5172c8d9683c', contemporary_globalized_hinduism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('100deb44-3edc-41c8-82b7-5172c8d9683c', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahmin_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchy_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains direct access to spiritual authority and divine connection, bypassing traditional caste-based intermediaries. Experiences liberation from ritual dependency and social stigma, but may still face social pressure from traditionalists.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas, beneficiary,
    moderate, biographical, mobile, regional).

% Establishes and propagates devotional practices, gaining spiritual authority and followers based on their sincerity and teaching, rather than birth. Administers devotional communities and interprets scriptures through a bhakti lens.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_gurus, agenda_setter,
    organized, generational, mobile, regional).

% Loses exclusive control over ritual and spiritual authority, seeing their traditional role diminished and their economic base challenged as devotees seek direct access. Resists the devotional reading, emphasizing scriptural literalism and hereditary rights.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahmin_priesthood, payer,
    powerful, generational, constrained, national).

% Finds the social order challenged by the devotional reading, which undermines the spiritual justification for their inherited status and privileges. May actively suppress or ostracize those who embrace direct devotional access.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchy_adherents, payer,
    organized, generational, constrained, local).

% Observes the bhakti reading as a partial step towards social equality, but argues it does not go far enough in dismantling caste structures, which they see as fundamentally unjust and unconstitutional. Advocates for a more radical reinterpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice and community formation around shared devotion, allowing individuals to connect with the divine directly, fostering inclusive spiritual communities that transcend traditional social barriers.
% TRANSFER_FUNCTION: Transfers spiritual authority from hereditary Brahmin lineages to individuals based on their sincere devotion (bhakti), regardless of birth. This shifts social capital and influence within religious communities.
% ABSENT_VOICES: Those who are deeply invested in the hereditary caste system as a divinely ordained structure, whose worldview is entirely predicated on birth-based spiritual authority, are effectively absent from the conversation that validates devotional access. They are present as a force of resistance, but not as a voice shaping the interpretation.
% DISAPPEARANCE_RATIONALE: If the devotional reading vanished, the spiritual landscape would revert to a more rigid, caste-based system. Many individuals would lose their primary path to spiritual fulfillment and community, and the social dynamics of religious practice would fundamentally shift back towards hereditary control.
% FOUNDING_PROBLEM: The problem of spiritual access being restricted by birth, leading to exclusion and a perceived lack of direct connection to the divine for many sincere seekers.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and independent sociological studies corroborate the historical and ongoing problem of caste-based exclusion in spiritual practice. Many non-Brahmin spiritual leaders and devotees also attest to the continued relevance of this problem, even with the rise of bhakti traditions.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).

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
 *   The extractiveness (0.4) is moderate because while it opens access, it still operates within a broader system where caste retains social and economic power. Suppression (0.3) is relatively low as devotional movements often spread through popular appeal rather than coercion, though traditionalists may exert social pressure. The theater ratio (0.2) is low, indicating that the devotional practices are largely sincere and functional, not performative cover for other agendas. The historical measurements show a trend of decreasing extractiveness and suppression as bhakti traditions gained prominence, though a slight uptick in modern times reflects renewed traditionalist resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bhakti devotees, this is a liberating 'rope' that provides genuine spiritual coordination. From the perspective of traditional Brahmin priests, it is a 'snare' that undermines their divinely ordained role and extracts their legitimate authority. The engine's classification will reflect the overall structural reality, which is a hybrid, but closer to a rope due to its genuine coordination function and reduced suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Devotees of all varnas and bhakti gurus are beneficiaries, gaining spiritual agency and authority. The traditional Brahmin priesthood and caste hierarchy adherents are victims, as their exclusive authority and social status are challenged. Reformist egalitarian activists act as observers, acknowledging the positive impact but pushing for more fundamental change.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_vs_caste_persistence,
    'To what extent does the ''bhakti devotional'' reading genuinely dismantle caste-based spiritual exclusion, versus merely creating an alternative path that coexists with, but does not eliminate, the broader caste hierarchy?',
    'Sociological studies tracking intergenerational mobility and social acceptance of non-Brahmin spiritual leaders in traditionally Brahmin-dominated spaces over time.',
    'If it merely coexists, the effective extractiveness and suppression of the broader system remain higher than this reading suggests, implying a ''tangled_rope'' or ''snare'' for those still bound by caste. If it actively dismantles, the ''rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_vs_caste_persistence, empirical, 'The actual impact of bhakti on caste structures.').

omega_variable(
    legitimacy_of_hereditary_authority,
    'Is the claim of hereditary spiritual authority (as asserted by the ''hereditary_monopoly_reading'') a genuine interpretation of the Vedic corpus, or a later accretion used to justify social power?',
    'Historical-critical textual analysis of early Vedic texts and their commentaries, tracing the evolution of interpretive traditions regarding varna and spiritual access.',
    'If hereditary authority is a later accretion, the ''hereditary_monopoly_reading'' would be reclassified as a ''snare'' built on a false premise. This would strengthen the ''rope'' classification of the ''bhakti devotional'' reading by removing a competing legitimate claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_hereditary_authority, conceptual, 'The historical and textual grounding of hereditary spiritual authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1000, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(vedi_tr_t1300, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 1300, 0.22).
narrative_ontology:measurement(vedi_tr_t1600, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(vedi_tr_t1900, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(vedi_tr_t2024, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1000, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement(vedi_be_t1300, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 1300, 0.45).
narrative_ontology:measurement(vedi_be_t1600, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(vedi_be_t1900, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(vedi_be_t2024, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1000, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(vedi_su_t1300, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 1300, 0.35).
narrative_ontology:measurement(vedi_su_t1600, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 1600, 0.3).
narrative_ontology:measurement(vedi_su_t1900, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 1900, 0.28).
narrative_ontology:measurement(vedi_su_t2024, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
