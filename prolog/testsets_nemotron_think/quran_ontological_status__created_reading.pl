% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Mu'tazilite Reading)
 *   domain: theological/philosophical/political
 *
 * SUMMARY:
 *   The created reading (Qur'an as makhlūq) is one of three live readings of
 *   the quran_ontological_status kernel. It emerged in 2nd/8th century
 *   Baghdad among Mu'tazilite theologians who argued that God's essence
 *   (dhāt) transcends all temporal artifacts — including revelation. If the
 *   Qur'an is eternal (qadīm), it becomes a second eternal entity alongside
 *   God, compromising divine unity (tawhīd). The created reading reclassifies
 *   revelation from mountain (ontic constraint, coeternal with God) to rope
 *   (coordination artifact: a divinely authored but temporally situated text
 *   that coordinates the community). This shift preserves divine
 *   transcendence, grants rational theology hermeneutic authority, and makes
 *   textual meaning interpretively flexible. The beneficiaries are
 *   rationalist theologians, philosophical schools, and later reform
 *   movements; the victims are traditionalist jurists whose interpretive
 *   monopoly depends on textual fixity, and literalist communities whose
 *   identity requires unmediated divine speech. The constraint requires
 *   active enforcement (mihna, institutional exclusion, social pressure) to
 *   maintain the uncreated doctrine against the created reading's challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.45).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.55).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Mu'tazilite Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/philosophical/political").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '56df2a10-7d2b-429c-8535-202ed7d9fe2c').
narrative_ontology:cs_kernel_codification('56df2a10-7d2b-429c-8535-202ed7d9fe2c', fixed_text).
narrative_ontology:cs_authority_grounding('56df2a10-7d2b-429c-8535-202ed7d9fe2c', lineage).
narrative_ontology:cs_interpretation_layer_present('56df2a10-7d2b-429c-8535-202ed7d9fe2c').
narrative_ontology:cs_reading_relation('56df2a10-7d2b-429c-8535-202ed7d9fe2c', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('56df2a10-7d2b-429c-8535-202ed7d9fe2c', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('56df2a10-7d2b-429c-8535-202ed7d9fe2c', foundational, quran_created_not_eternal).
narrative_ontology:cs_axiom_status(quran_created_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('56df2a10-7d2b-429c-8535-202ed7d9fe2c', quran_created_not_eternal, theological).
narrative_ontology:cs_axiom('56df2a10-7d2b-429c-8535-202ed7d9fe2c', foundational, divine_transcendence_above_text).
narrative_ontology:cs_axiom_status(divine_transcendence_above_text, holdable).
narrative_ontology:cs_axiom_grounding('56df2a10-7d2b-429c-8535-202ed7d9fe2c', divine_transcendence_above_text, theological).
narrative_ontology:cs_reference_frame('56df2a10-7d2b-429c-8535-202ed7d9fe2c', classical_kalam_framework).
narrative_ontology:cs_drift_state('56df2a10-7d2b-429c-8535-202ed7d9fe2c', post_mihna_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56df2a10-7d2b-429c-8535-202ed7d9fe2c', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, caliphal_state).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_transcendence).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, rational_theology_hermeneutic_authority).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, textual_interpretive_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mu'tazilite and allied kalam scholars who argue the Qur'an is created (makhlūq) to preserve divine transcendence. They gain hermeneutic authority for rational theology and interpretive flexibility. Their exit is mobile — they can migrate between patronage courts, shift to philosophical discourse, or operate in scholarly networks beyond any single polity.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, regional).

% Ahl al-hadith and Hanbali jurists whose authority derives from textual fixity — the Qur'an as uncreated eternal speech (kalām Allāh qadīm) is the ontological anchor of their legal derivation and communal leadership. If the text is created, their interpretive monopoly collapses. Their identity is fused with the doctrine; exit means abandoning the self-concept of being 'guardians of revelation.'
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    institutional, generational, identity_locked, regional).

% Communities whose religious identity depends on unmediated access to divine speech — the Qur'an as God's direct, uncreated word. The created reading threatens the immediacy of revelation. Their exit is identity-locked: leaving the framework means losing the coherent worldview that structures daily practice, communal belonging, and eschatological hope.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    organized, biographical, identity_locked, regional).

% The Abbasid caliphate (especially under al-Ma'mun, al-Mu'tasim, al-Wathiq) which patronized the created doctrine to assert caliphal authority over religious interpretation. The state gains political leverage by making theology a policy lever. Its exit is arbitrage-grade: it can switch patronage to traditionalists (as al-Mutawakkil did) when the created doctrine becomes politically costly.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, caliphal_state, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, caliphal_state, beneficiary).

% Falsafa traditions (al-Kindi, al-Farabi, Ibn Sina) that treat revelation as a symbolic/imaginative faculty of the prophet, not eternal speech. They benefit from the created reading's opening for philosophical hermeneutics. Their exit is mobile — they operate in a trans-regional intellectual network with Greek, Persian, and Arabic sources.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, regional).

% Modernist and reformist movements (19th-21st century) that invoke the created doctrine to justify historical-critical Qur'anic studies, legal reform, and gender-egalitarian readings. They benefit from interpretive flexibility but face constrained exit: traditionalist institutions control religious authority structures, and reformers risk apostasy accusations.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, biographical, constrained, global).

% The comparative theology / philosophy of religion seat that sees the full structural field: the created/uncreated dispute as a contest over hermeneutic authority, political legitimacy, and the ontology of language. Neither collects nor pays; observes the coordination-extraction dynamics across all seats.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared textual anchor for communal identity, legal derivation, and theological discourse across the ummah — a single reference point that enables coordination across vast distances and generations without requiring living authority.
% TRANSFER_FUNCTION: Moves interpretive authority from rational theology (kalam/falsafa) to textual literalism (fiqh/hadith); moves political legitimacy from caliphal rationalism to juristic traditionalism; moves epistemic privilege from philosophical demonstration to transmitted text.
% ABSENT_VOICES: Early mutakallimūn whose nuanced positions (e.g., al-Nazzam's 'word' vs. 'speech' distinction) were flattened by the binary; women scholars excluded from the authoritative interpretive tradition (muftiates, madrasas); non-Arab Muslim communities (Persianate, Turkic, Malay) whose vernacular hermeneutics were marginalized by Arabic textual fixation; Sufi orders whose experiential reading (kashf) was subordinated to textualist norms.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the Qur'an would become a historical document subject to critical philology; legal derivation (fiqh) would lose its ontological anchor in eternal speech; communal identity would need new foundations beyond 'people of the book'; the caliphal/state religious legitimacy framework would collapse; the entire edifice of classical Islamic law and theology would require restructuring.
% FOUNDING_PROBLEM: The early Muslim community (7th-8th century) needed a stable, authoritative text to unify legal practice, theological doctrine, and political legitimacy across rapidly expanding territories — the Qur'an as uncreated speech solved the coordination problem of a universal religion without a living prophet or centralized church.
% FOUNDING_PROBLEM_CORROBORATION: Western orientalist scholarship (Wansbrough, Crone, Cook) corroborates the historical contingency thesis: the fixation of the Qur'anic text and its elevation to uncreated status was a 2nd/8th century development, not a 1st/7th century given. Traditionalist scholarship (Ibn Taymiyya, modern Salafi institutions) self-attests persistence. No neutral arbiter exists — the corroboration split mirrors the beneficiary/victim split.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the uncreated doctrine extracts interpretive authority from rational actors and locks communities into a fixed hermeneutic, but the coordination function (shared text for law, theology, identity) is genuine and valuable. Suppression (0.55) is significant but not total: the mihna (inquisition) was intense but brief (833-848 CE); thereafter suppression operated through institutional gatekeeping (madrasa curricula, judicial appointments, fatwa authority) and social marginalization. Theater ratio (0.35) reflects that the uncreated doctrine's ritualized performance (recitation, memorization, physical veneration of the mushaf) exceeds its coordination necessity — the text could coordinate without the ontological claim of eternity. Accessibility collapse (0.5) is partial: rationalist, philosophical, and Sufi hermeneutics persisted as underground or marginal traditions, and modern reform movements have reopened the created reading. Resistance (0.6) is high: the created reading has never been fully extinguished despite centuries of institutional dominance by the uncreated reading.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist_theologians seat (beneficiary, mobile exit), the constraint is a rope: genuine coordination with extractive overhead they can critique and navigate. From the traditionalist_jurists seat (payer, identity_locked exit), the same constraint is experienced as a mountain that must be defended — its collapse would destroy their authority structure. From the caliphal_state seat (agenda_setter, arbitrage exit), the constraint is a tool: enforce uncreated doctrine when it stabilizes rule, sponsor created doctrine when it centralizes religious authority. The analytical_observer sees the full field: a coordination artifact (rope) that has been hardened into a mountain by institutional capture, with extraction concentrated on identity-locked payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rationalist_theologians, philosophical_schools, reform_movements) gain hermeneutic freedom and intellectual authority — their directionality d is low (near 0.2). Victims (traditionalist_jurists, literalist_communities) bear the cost of interpretive closure and identity foreclosure — their d is high (near 0.85 for traditionalist_jurists, 0.9 for literalist_communities). The caliphal_state has d near 0.15 when sponsoring the created reading (beneficiary of centralized authority) but d near 0.3 when enforcing the uncreated reading (paying enforcement costs for stability). The analytical_observer has d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal coordination without living prophet) was live in the formative period but is contested now: traditionalists say the coordination need persists; rationalists/reformers say modern conditions (print, translation, global communication) have changed the coordination problem. The mandate has atrophied for the uncreated reading's enforcement apparatus — the mihna failed, and contemporary enforcement relies on social pressure rather than state inquisition. Yet the constraint persists because the identity_lock of literalist communities and the institutional_lock of traditionalist jurists make exit prohibitively costly. This is not a snare (the coordination function is real) but a tangled_rope that has hardened toward mountain-like fixation — the engine will classify per-seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_primacy,
    'Is the uncreated doctrine primarily a genuine coordination mechanism (rope) that acquired extractive overlay, or was it designed from the start as an authority-capture mechanism (snare)?',
    'Comparative analysis of early hadith transmission, the timing of ''uncreated'' doctrinal formulation (post-2nd century AH), and whether pre-doctrinal coordination existed via other mechanisms (living companions, caliphal decree, consensus).',
    'If designed as capture, the constraint is a snare from origin; if coordination-first, it is a tangled_rope that degraded. Affects mandatrophy verdict and piton risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_primacy, conceptual, 'Whether the uncreated doctrine''s extraction is original or accumulated.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the created reading structural (institutional exclusion, state violence) or internalized (communities believe the uncreated doctrine is constitutive of Muslim identity)?',
    'Post-exit trajectory study: when individuals or communities leave the uncreated framework (e.g., modernist reformers, ex-Muslim critics), does the suppression persist as internalized constraint (fear, guilt, epistemic vertigo) or dissolve?',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. Affects identity_lock classification and omega resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for literalist communities.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''quran_ontological_status'' name a single theological proposition, or a cluster of distinct claims (ontology of speech, authority of text, legitimacy of interpretation) that the readings disaggregate differently?',
    'Formal decomposition of the kernel into sub-claims and testing whether each reading''s ε-invariance holds per sub-claim.',
    'If the kernel is underdetermined, the three readings may not be symmetric alternatives but responses to different sub-questions. Would require re-authoring as a constraint family with finer granularity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a coherent unity or a conflation of distinct structural questions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_created_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(quran_created_tr_t30, quran_ontological_status__created_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(quran_created_tr_t60, quran_ontological_status__created_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(quran_created_tr_t90, quran_ontological_status__created_reading, theater_ratio, 90, 0.33).
narrative_ontology:measurement(quran_created_tr_t120, quran_ontological_status__created_reading, theater_ratio, 120, 0.34).
narrative_ontology:measurement(quran_created_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.35).

% Extraction over time
narrative_ontology:measurement(quran_created_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(quran_created_be_t30, quran_ontological_status__created_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(quran_created_be_t60, quran_ontological_status__created_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(quran_created_be_t90, quran_ontological_status__created_reading, base_extractiveness, 90, 0.42).
narrative_ontology:measurement(quran_created_be_t120, quran_ontological_status__created_reading, base_extractiveness, 120, 0.44).
narrative_ontology:measurement(quran_created_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(quran_created_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(quran_created_su_t30, quran_ontological_status__created_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(quran_created_su_t60, quran_ontological_status__created_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(quran_created_su_t90, quran_ontological_status__created_reading, suppression_requirement, 90, 0.58).
narrative_ontology:measurement(quran_created_su_t120, quran_ontological_status__created_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(quran_created_su_t150, quran_ontological_status__created_reading, suppression_requirement, 150, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.08).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel decomposes into three constraint stories: (1) created_reading — revelation as rope, transcendence preserved, rational hermeneutics empowered; (2) uncreated_reading — revelation as mountain, text as ontic constraint, traditionalist authority secured; (3) state_enforced_creation_reading — created doctrine + mihna enforcement, adding state extraction layer. The ε values differ: created_reading ε≈0.45 (moderate extraction of interpretive authority), uncreated_reading ε≈0.15 (low extraction, high coordination), state_enforced_creation_reading ε≈0.7 (high extraction via inquisition). They share the same referent (the Qur'an's ontological status) but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__created_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
