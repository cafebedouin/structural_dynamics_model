% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive (Prayer/Study Replaced Sacrifice)
 *   domain: religious/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus (the Talmudic order on sacrifices) is read in the
 *   substitution_archive framing as a memorial archive: it documents a cultic
 *   order that has been superseded by prayer and Torah study. This reading
 *   acknowledges the replacement as genuine and final — the Torah's
 *   sacrificial system is not 'occupied' through study but 'archived' as a
 *   superseded stage. The constraint is the rabbinic insistence that study of
 *   Kodashim constitutes continuity with the Temple service, which obscures
 *   the fact that a substitution occurred. Beneficiaries are the rabbinic
 *   text-study institutions whose authority rests on this claim of
 *   continuity. Victims are those who seek living sacrificial practice and
 *   are told it is obsolete, forbidden, or deferred. The constraint is a
 *   tangled_rope because it coordinates a dispersed people around a portable
 *   worship system (real coordination) while extracting legitimacy from the
 *   claim that nothing essential was lost (asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.52).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.61).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.52).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.33).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive (Prayer/Study Replaced Sacrifice)").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/rabbinic_judaism/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '53ae9173-b761-48be-93e6-67a7d3dc618a').
narrative_ontology:cs_kernel_codification('53ae9173-b761-48be-93e6-67a7d3dc618a', fixed_text).
narrative_ontology:cs_authority_grounding('53ae9173-b761-48be-93e6-67a7d3dc618a', lineage).
narrative_ontology:cs_interpretation_layer_present('53ae9173-b761-48be-93e6-67a7d3dc618a').
narrative_ontology:cs_reading_relation('53ae9173-b761-48be-93e6-67a7d3dc618a', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('53ae9173-b761-48be-93e6-67a7d3dc618a', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('53ae9173-b761-48be-93e6-67a7d3dc618a', foundational, prayer_and_study_fully_replaced_sacrificial_worship).
narrative_ontology:cs_axiom_status(prayer_and_study_fully_replaced_sacrificial_worship, holdable).
narrative_ontology:cs_axiom_grounding('53ae9173-b761-48be-93e6-67a7d3dc618a', prayer_and_study_fully_replaced_sacrificial_worship, deontological).
narrative_ontology:cs_axiom('53ae9173-b761-48be-93e6-67a7d3dc618a', foundational, kodashim_is_memorial_archive_not_occupied_kernel).
narrative_ontology:cs_axiom_status(kodashim_is_memorial_archive_not_occupied_kernel, holdable).
narrative_ontology:cs_axiom_grounding('53ae9173-b761-48be-93e6-67a7d3dc618a', kodashim_is_memorial_archive_not_occupied_kernel, conventional).
narrative_ontology:cs_reference_frame('53ae9173-b761-48be-93e6-67a7d3dc618a', rabbinic_substitution_paradigm_post_churban).
narrative_ontology:cs_drift_state('53ae9173-b761-48be-93e6-67a7d3dc618a', contemporary_temple_activism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('53ae9173-b761-48be-93e6-67a7d3dc618a', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, seekers_of_living_sacrificial_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, observant_lay_jews).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, observant_lay_jews).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_and_torah_study_fully_replaced_sacrificial_worship).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, kodashim_documents_superseded_order_not_occupied_kernel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivas, kollels, and rabbinic academies whose institutional authority, funding, and curricular center derive from the claim that sustained Talmudic study of sacrificial law (Kodashim) constitutes the living continuation of the cultic order. They collect prestige, donor support, and communal leadership from this framing. Exit is arbitrage-grade: they could pivot to other corpuses but the Kodashim claim is their distinctive franchise.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals and small groups (Temple Institute adherents, messianic activists, some Religious Zionist factions) who seek actual restoration of korbanot. They are told by the dominant rabbinic establishment that study IS the substitute and restoration is either forbidden (halakhic obstacles) or deferred (messianic). Their exit is trapped: leaving the framework means leaving the tradition that defines their identity; staying means accepting the substitution as final.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, seekers_of_living_sacrificial_practice, payer,
    powerless, biographical, trapped, local).

% Receive a coherent, portable, non-violent religious practice (prayer, study) that requires no Temple, priesthood, or animal slaughter. They also pay the cost of a religion that has spiritualized its most concrete rites into text, losing the visceral immediacy the Torah prescribes. Exit is constrained: they remain within the halakhic world but could shift to performance-only or study-as-exercise framings.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, observant_lay_jews, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, observant_lay_jews, payer).

% Hold that the Temple will be rebuilt and sacrifice resumed literally. They are excluded from the substitution_archive reading's consensus because their position is treated as either dangerous (political) or premature (halakhic). Their identity is fused to the expectation of literal restoration; exit would shatter their theological self-concept.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_expectants, excluded,
    moderate, generational, identity_locked, regional).

% Read Kodashim as a literary-historical archive of a discontinued cultic system. They see the substitution narrative as a rabbinic construction post-70 CE. They neither collect nor pay; they map the constraint's genealogy and rhetorical function.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_critical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the Temple's destruction (70 CE), the rabbinic movement needed to preserve Jewish collective identity and divine service without a central cult. The substitution of prayer (tefillah) and Torah study (talmud torah) for sacrifice (avodah) provided a portable, non-violent, universally accessible system that could survive exile.
% TRANSFER_FUNCTION: Moves the locus of divine service from the Temple courtyard (priests, animals, fire, blood) to the synagogue and study hall (any male Jew, words, intellect). Transfers authority from the hereditary priesthood to the rabbinic sages who interpret the textual corpus. Transfers legitimacy from physical performance to intellectual engagement with Kodashim.
% ABSENT_VOICES: The priestly families (kohanim) who lost their ritual function; the early Christian communities who claimed the sacrificial system was fulfilled in Christ; the Qumran sectarians who anticipated a restored Temple with a purified priesthood. All are absent from the rabbinic consensus that study replaced sacrifice.
% DISAPPEARANCE_RATIONALE: If the substitution_archive reading vanished, the rabbinic claim that study IS the continuation of sacrifice would collapse. Either the performance_only reading would dominate (study as provisional placeholder awaiting literal restoration) or the study_as_exercise reading would become explicit (study as independent mitzvah). In either case, Jewish liturgy, curriculum, and self-understanding would reorganize around a different account of what the Temple's absence means.
% FOUNDING_PROBLEM: How to maintain covenantal continuity and divine service after the catastrophic loss of the Temple, the priesthood, and the sacrificial system — the physical center of Israel's worship — without conceding that the covenant was broken or that God had abandoned Israel.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the rabbinic sources themselves (e.g., Berakhot 26b: 'tefillot kenegd tamidin tiknum' — prayers were instituted corresponding to the daily offerings). Historical-critical scholarship (Neusner, Boyarin, Klawans) corroborates that the substitution narrative emerges in the Tannaitic period as a response to 70 CE. The status is contested because the dominant rabbinic establishment treats the problem as solved (substitution complete), while messianic and some Religious Zionist voices treat it as live (restoration pending).
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the substitution delivers real coordination value (portable, non-violent worship) but also extracts by foreclosing the possibility that the substitution is provisional or incomplete. Suppression is higher (0.61) because maintaining the substitution narrative requires active interpretive enforcement: halakhic rulings against Temple Mount activism, curricular centrality of Kodashim despite its practical irrelevance, liturgical language that equates prayer with sacrifice. Theater ratio (0.38) reflects that much Kodashim study is performative — learning laws that cannot be practiced, maintaining the fiction of occupational continuity. Accessibility collapse (0.42) is moderate: alternatives (performance_only, study_as_exercise) exist but are marginalized. Resistance (0.33) is low-moderate: marginalized voices exist but lack institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institution's seat, the constraint is a rope: it solved a genuine collective-action problem (how to worship without a Temple) with minimal coercion. From the seeker's seat, it is a snare: the coordination story is cover for a substitution that forecloses restoration. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) captures the hybrid reality that both seats experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbnic text-study institutions are structural beneficiaries (d ~ 0.15): they collect authority, funding, and curricular dominance from the substitution claim. Seekers of living sacrificial practice are structural targets (d ~ 0.85): they bear the cost of being told their aspiration is halakhically invalid or religiously dangerous, with no exit from the framework that defines their identity. Observant lay Jews sit near symmetric (d ~ 0.5): genuine coordination benefit, diffuse spiritual cost. Messianic expectants are excluded (identity_locked) — their exclusion is the enforcement object. Historical-critical scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to worship without a Temple) was live in 70 CE. By the early medieval period, the substitution was institutionalized and the problem was effectively solved — prayer and study worked. The constraint persists because the rabbinic establishment continues to claim the substitution is not merely pragmatic but theologically complete (study IS avodah). This is mandatrophy: the mandate (preserve covenantal continuity) has been fulfilled, but the constraint (study-as-continuity claim) persists and extracts by denying that restoration is either possible or desired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_completeness,
    'Is the substitution of prayer/study for sacrifice theologically complete (final) or pragmatically provisional (awaiting restoration)?',
    'Halakhic analysis of whether the rabbinic sources treat tefillah/talmud torah as ''keneged'' (corresponding to) or ''chalif'' (replacement of) the tamid offerings; historical reception of Maimonides'' view (Guide III:32) that sacrifices were a concession to ancient practice.',
    'If provisional, the substitution_archive reading collapses into performance_only; if final, the victim class (seekers of living practice) is structurally foreclosed. The tangled_rope classification depends on the substitution being presented as final while the archive maintains the forms of the superseded order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_completeness, conceptual, 'Whether the substitution claim is final or provisional — the core ambiguity the constraint manages.').

omega_variable(
    kodashim_study_function,
    'Does sustained study of Kodashim serve a coordination function independent of its memorial role (e.g., intellectual discipline, communal identity formation)?',
    'Sociological study of yeshiva curricula: what proportion of Kodashim study time is justified by ''continuity with avodah'' vs. ''intellectual training'' vs. ''curricular tradition''? Comparative analysis with other ''impractical'' corpuses (Taharot, Zeraim).',
    'If Kodashim study has independent coordination value, the constraint''s extraction is lower (more rope-like); if its only justification is the substitution claim, extraction is higher (more snare-like). Affects the coordination/extraction balance in the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kodashim_study_function, empirical, 'Whether the archive''s study has autonomous coordination value or depends entirely on the substitution narrative.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kodashim_corpus kernel best framed as (a) the textual corpus itself, (b) the claim of its ongoing halakhic authority, or (c) the institutional structure that transmits it?',
    'Meta-analysis of how each sibling reading implicitly defines the kernel: performance_only treats the kernel as the restorable practice; study_as_exercise treats it as the intellectual mitzvah; substitution_archive treats it as the archived text. The choice of kernel framing changes which readings appear as foreclosures vs. coexistence.',
    'If the kernel is the practice (a), substitution_archive forecloses performance_only. If the kernel is the intellectual mitzvah (b), all three coexist. If the kernel is the institution (c), substitution_archive influences the others by controlling curricular resources. This is a conceptual omega because the kernel''s identity is not empirically settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination of the kernel itself — affects all reading_relations and axiom distinctions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_sub_arch_tr_t70, kodashim_corpus__substitution_archive, theater_ratio, 70, 0.2).
narrative_ontology:measurement(kodashim_sub_arch_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.28).
narrative_ontology:measurement(kodashim_sub_arch_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.33).
narrative_ontology:measurement(kodashim_sub_arch_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.36).
narrative_ontology:measurement(kodashim_sub_arch_tr_t1800, kodashim_corpus__substitution_archive, theater_ratio, 1800, 0.37).
narrative_ontology:measurement(kodashim_sub_arch_tr_t2026, kodashim_corpus__substitution_archive, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(kodashim_sub_arch_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(kodashim_sub_arch_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(kodashim_sub_arch_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.48).
narrative_ontology:measurement(kodashim_sub_arch_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(kodashim_sub_arch_be_t1800, kodashim_corpus__substitution_archive, base_extractiveness, 1800, 0.51).
narrative_ontology:measurement(kodashim_sub_arch_be_t2026, kodashim_corpus__substitution_archive, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_sub_arch_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.45).
narrative_ontology:measurement(kodashim_sub_arch_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.52).
narrative_ontology:measurement(kodashim_sub_arch_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.57).
narrative_ontology:measurement(kodashim_sub_arch_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(kodashim_sub_arch_su_t1800, kodashim_corpus__substitution_archive, suppression_requirement, 1800, 0.61).
narrative_ontology:measurement(kodashim_sub_arch_su_t2026, kodashim_corpus__substitution_archive, suppression_requirement, 2026, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.08).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, liturgical_substitution_tefillah_avodah).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, yeshiva_curriculum_authority).

% DUAL FORMULATION NOTE:
% This constraint is the substitution_archive reading of the kodashim_corpus kernel. It decomposes the colloquial 'Kodashim remains binding' into a structurally precise claim: the corpus is a memorial archive of a superseded order. The sibling readings (performance_only, study_as_exercise) are separate constraints with different ε, beneficiaries, and victims. All three are linked via affects_constraints. The substitution_archive reading has moderate extractiveness (0.52) because it claims continuity while denying restoration; performance_only has lower extractiveness (archive as placeholder); study_as_exercise has variable extractiveness depending on whether study is framed as independent mitzvah or as substitute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, powerless, 0.88).
constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
