% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Bhakti Devotional Reading of Vedic-Dharmic Authority
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   This constraint instantiates the bhakti devotional reading of the
 *   vedic_dharmic_corpus kernel: the claim that sincere devotion (bhakti) to
 *   the divine grants spiritual authority independent of birth-status or
 *   caste identity. Historically articulated by marginalized-caste saints and
 *   poet-theologians, this reading challenges the hereditary monopoly reading
 *   that restricts ritual authority to Brahmin birth. It coordinates large
 *   cross-caste devotional communities but leaves the broader social and
 *   economic architecture of caste substantially intact â the victim set
 *   shrinks without disappearing. The constraint is claimed here as rope
 *   (genuine coordination with diffuse benefits) while the metrics
 *   acknowledge moderate residual extraction through persistent social
 *   hierarchy, producing the divergence the engine is meant to measure.
 *
 * KEY AGENTS:
 *   - cross_caste_devotees: Primary beneficiary (moderate/constrained) â gain spiritual authority via devotion
 *   - marginalized_caste_communities: Residual target (powerless/identity_locked) â remain subject to social caste hierarchy despite spiritual inclusion
 *   - hereditary_priesthood: Secondary target (organized/identity_locked) â lose exclusive ritual monopoly but retain social prestige
 *   - devotional_institutions: Agenda setter (institutional/constrained) â administer doctrine and set criteria for authentic bhakti
 *   - scriptural_scholars: Analytical observer (analytical/analytical) â track syncretism and tension between devotional and hereditary claims
 *   - reformist_egalitarian_advocates: Excluded voice (moderate/mobile) â demand full social abolition of caste, not merely spiritual bypass
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading of Vedic-Dharmic Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social/interpretive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7').
narrative_ontology:cs_kernel_codification('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', fixed_text).
narrative_ontology:cs_authority_grounding('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', practice).
narrative_ontology:cs_interpretation_layer_present('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7').
narrative_ontology:cs_reading_relation('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', foundational, bhakti_supersedes_birth_status).
narrative_ontology:cs_axiom_status(bhakti_supersedes_birth_status, holdable).
narrative_ontology:cs_axiom_grounding('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', bhakti_supersedes_birth_status, theological).
narrative_ontology:cs_axiom('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', foundational, divine_access_without_intermediary).
narrative_ontology:cs_axiom_status(divine_access_without_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', divine_access_without_intermediary, theological).
narrative_ontology:cs_reference_frame('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', bhakti_practice_as_legitimacy_source).
narrative_ontology:cs_drift_state('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea4d5ba5-a23f-4f4a-9d9e-d7515f6b50a7', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, cross_caste_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, marginalized_caste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain spiritual authority and communal standing through sincere devotion rather than birth lineage. Their piety is recognized by temples and gurus, yet they continue to face social and economic discrimination outside the religious sphere. Exit means abandoning the devotional community and its spiritual promises.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, cross_caste_devotees, beneficiary,
    moderate, generational, constrained, national).

% Receive spiritual inclusion through bhakti doctrine but remain subject to social and economic caste hierarchy in secular life. They are the residual victim set: the constraint shrinks their subordination in the religious domain without eliminating it in the social domain. Exit from caste identity is structurally unavailable regardless of spiritual attainment.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, marginalized_caste_communities, payer,
    powerless, generational, identity_locked, national).

% Lose exclusive monopoly over ritual performance and textual interpretation as devotional movements elevate non-Brahmin practitioners. They retain significant temple administrative posts and social prestige, but their exclusive claim to divine mediation is contested. Their identity is fused with lineage; exit from the hereditary role is not structurally available.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood, payer,
    organized, generational, identity_locked, national).

% Temples, monasteries, and guru lineages that administer devotional practice and interpret scriptural authority. They set the criteria for authentic bhakti and thus control access to spiritual legitimacy. They do not primarily collect material rents from the constraint; their authority is reputational and doctrinal.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Academic and comparative-religion scholars who analyze the relationship between devotional movements and caste stratification. They observe that bhakti texts frequently originate from lower-caste authors yet are later domesticated by Brahminical redaction.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, scriptural_scholars, observer,
    analytical, civilizational, analytical, universal).

% Advocate for complete abolition of caste in all spheres, arguing that spiritual equality without social equality is insufficient. They are excluded from this reading's framework because it legitimizes partial hierarchy; they would object that the constraint is a palliative rather than a cure.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_advocates, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates access to spiritual authority and communal worship across birth-based divisions, allowing dispersed devotees to share a religious framework without requiring hereditary gatekeepers for every ritual function.
% TRANSFER_FUNCTION: Moves spiritual legitimacy and interpretive authority from hereditary lineages to devotional practitioners; transfers social status within religious communities from birth status to demonstrated piety, while leaving broader economic and social caste privileges largely unmoved.
% ABSENT_VOICES: Reformist egalitarian voices who would demand full social and economic abolition of caste alongside spiritual equality are sidelined in this reading; so are hardline hereditary exclusivists who reject any non-Brahmin authority. Both are present in the broader kernel but not in this reading's optimal set.
% DISAPPEARANCE_RATIONALE: If the bhakti principle vanished, devotional communities would lose their textual and doctrinal basis for cross-caste spiritual authority; hereditary gatekeeping would reassert uncontested control in religious domains, and the social composition of religious leadership would revert toward birth-based stratification.
% FOUNDING_PROBLEM: The hereditary monopoly reading restricted spiritual knowledge and ritual access to a narrow birth-defined elite, creating a supply bottleneck in religious legitimacy and alienating large populations from authoritative practice.
% FOUNDING_PROBLEM_CORROBORATION: Explicitly corroborated by marginalized-caste devotional hagiographies and hymnodies (e.g., Alvar saints, Kabir, Ravidas) from within the beneficiary community. No corroboration from entirely outside the dispute exists â the historical record is partisan, with hereditary institutions contesting the premise. This absence is itself signal: the founding problem is asserted by both sides from within their respective readings.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is set at 0.40 to reflect the moderate residual cost of persistent caste hierarchy that spiritual egalitarianism does not eliminate. Suppression is 0.35 because the constraint does not actively suppress alternatives (other paths remain live), yet social stigma against lower-caste devotional leadership partly suppresses full realization of the doctrine. Theater ratio is 0.25: there is genuine coordination in devotional communities, but a share of practice ritualizes performance of egalitarianism without social transfer. Accessibility collapse is 0.35 because understanding the bhakti framework does not collapse exit options â one can still operate within hereditary or reformist frameworks. Resistance is 0.40 because hereditary institutions and orthodox interpreters actively contest the expansion of non-Brahmin authority. The claim is rope because the reading's structural intent is coordination across caste lines; the metrics register the incompleteness of that coordination without conflating it with pure extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the devotional institution seat, the constraint is a rope: it solves the coordination problem of mass religious participation without requiring everyone to be born into priestly lineage. From the marginalized caste seat, the same constraint is an incomplete promise: spiritual authority is granted, but social and economic subordination persists, so effective extraction remains non-zero. The hereditary priesthood experiences it as a targeted loss of exclusive franchise. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Cross-caste devotees are declared beneficiaries: the constraint subsidizes their spiritual access by removing birth-barriers. Marginalized caste communities are declared victims: they bear the cost of the constraint's failure to dismantle social hierarchy. Hereditary priests are narratively payers but not declared in victims because the 'victim set' in this domain refers to those harmed by hierarchy, not those losing privilege; their structural cost is captured in the stakeholder layer. The diffuse nature of gains supports the rope claim while the presence of any victim set prevents a clean coordination verdict.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the bhakti reading as a snare (pure extraction) because the constraint genuinely coordinates: devotional communities achieve collective worship and textual access that the hereditary monopoly would deny. It also prevents mislabeling it as a pure rope because the victim set is not empty â social caste hierarchy persists. If the founding problem (exclusion from spiritual authority) were fully solved, the remaining social hierarchy would be a separate constraint; because the spiritual and social domains are coupled in practice, the constraint carries residual extraction. The mandatrophy risk is low because the coordination function is live and visible, not a cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_caste_ambiguity,
    'Does the bhakti reading leave caste hierarchy intact as a necessary structural feature, or is the remaining hierarchy a residual social artifact separable from the doctrinal constraint?',
    'Comparative analysis of bhakti-dominated communities: if social hierarchy persists at similar rates, the reading does not dismantle it; if hierarchy is weaker, the residual is separable.',
    'If inseparable, the constraint is a tangled rope (coordination + extraction); if separable, it is closer to a rope with external contamination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_caste_ambiguity, conceptual, 'Whether residual caste hierarchy is intrinsic or separable from the bhakti reading').

omega_variable(
    sibling_reading_boundary,
    'To what extent does the bhakti reading''s acceptance depend on the hereditary monopoly reading it challenges remaining legible as a foil?',
    'Historical analysis of bhakti movements in periods where hereditary authority was weakest â did bhakti doctrine expand or contract?',
    'If bhakti requires the hereditary foil for its own coherence, the two readings are structurally coupled rather than independent constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Structural coupling between bhakti and hereditary readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t200, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement(vedi_tr_t400, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement(vedi_tr_t600, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(vedi_tr_t800, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 800, 0.23).
narrative_ontology:measurement(vedi_tr_t1000, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 1000, 0.25).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vedi_be_t200, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement(vedi_be_t400, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 400, 0.42).
narrative_ontology:measurement(vedi_be_t600, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 600, 0.41).
narrative_ontology:measurement(vedi_be_t800, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 800, 0.4).
narrative_ontology:measurement(vedi_be_t1000, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 1000, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is the bhakti devotional reading of the vedic_dharmic_corpus kernel, which decomposes into three structurally distinct claims: hereditary_monopoly_reading (birth-based authority, high extraction), bhakti_devotional_reading (devotion-based authority, moderate coordination with residual hierarchy), and reformist_egalitarian_reading (rational-constitutional equality, contested emancipation). Each carries a different epsilon and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
