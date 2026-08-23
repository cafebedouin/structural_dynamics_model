% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Quran 9:5 Contextual-Defensive Reading (Treaty-Breaking Polytheists Only)
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint story models the contextual-defensive reading of Quran
 *   9:5 ('Then when the sacred months have passed, kill the polytheists
 *   wherever you find them...') as a living hermeneutic constraint operating
 *   in contemporary Islamic legal and political discourse. The reading
 *   restricts the verse's application to specific 7th-century Medinan
 *   polytheist tribes who violated treaties, affirms the continued validity
 *   of peaceful verses (e.g., 2:256 'no compulsion in religion,' 8:61 'if
 *   they incline to peace'), and establishes defensive warfare and treaty
 *   fidelity as the only Quranically sanctioned violence. It functions as a
 *   coordination mechanism enabling Muslim-majority states to participate in
 *   the international treaty system, protect minorities, and suppress
 *   extremist legal theology. The claimed_type is 'rope' — a genuine
 *   coordination function with minimal coercive overhead — but the metrics
 *   reveal non-trivial suppression (0.35) and resistance (0.58) because the
 *   reading must be actively maintained against the classical abrogationist
 *   consensus and modern extremist revival. The standing arrangement under
 *   contest (assessed by this reading's lights) is the classical
 *   abrogationist regime, which this reading evaluates as highly extractive
 *   (ε≈0.78 at interval start) — extracting peaceful coexistence, minority
 *   security, and treaty credibility from Muslim societies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.22).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.35).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Quran 9:5 Contextual-Defensive Reading (Treaty-Breaking Polytheists Only)").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'c153749b-a6fb-4bca-a260-946ce37d2bc5').
narrative_ontology:cs_kernel_codification('c153749b-a6fb-4bca-a260-946ce37d2bc5', fixed_text).
narrative_ontology:cs_authority_grounding('c153749b-a6fb-4bca-a260-946ce37d2bc5', lineage).
narrative_ontology:cs_interpretation_layer_present('c153749b-a6fb-4bca-a260-946ce37d2bc5').
narrative_ontology:cs_reading_relation('c153749b-a6fb-4bca-a260-946ce37d2bc5', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('c153749b-a6fb-4bca-a260-946ce37d2bc5', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('c153749b-a6fb-4bca-a260-946ce37d2bc5', foundational, no_abrogation_of_peaceful_verses_by_9_5).
narrative_ontology:cs_axiom_status(no_abrogation_of_peaceful_verses_by_9_5, holdable).
narrative_ontology:cs_axiom_grounding('c153749b-a6fb-4bca-a260-946ce37d2bc5', no_abrogation_of_peaceful_verses_by_9_5, deontological).
narrative_ontology:cs_axiom('c153749b-a6fb-4bca-a260-946ce37d2bc5', foundational, treaty_obligation_supersedes_general_command).
narrative_ontology:cs_axiom_status(treaty_obligation_supersedes_general_command, holdable).
narrative_ontology:cs_axiom_grounding('c153749b-a6fb-4bca-a260-946ce37d2bc5', treaty_obligation_supersedes_general_command, conventional).
narrative_ontology:cs_axiom('c153749b-a6fb-4bca-a260-946ce37d2bc5', secondary, asbab_al_nuzul_delimits_legal_scope).
narrative_ontology:cs_axiom_status(asbab_al_nuzul_delimits_legal_scope, holdable).
narrative_ontology:cs_axiom_grounding('c153749b-a6fb-4bca-a260-946ce37d2bc5', asbab_al_nuzul_delimits_legal_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('c153749b-a6fb-4bca-a260-946ce37d2bc5', classical_tafsir_consensus).
narrative_ontology:cs_drift_state('c153749b-a6fb-4bca-a260-946ce37d2bc5', contemporary_reformist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c153749b-a6fb-4bca-a260-946ce37d2bc5', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_muslim_rule).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, reformist_scholars).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, interfaith_coexistence_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, universalist_extremist_groups).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, classical_abrogationist_jurists).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, offensive_jihad_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_muslim_rule).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, quranic_verses_not_abrogated_by_chronology).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_obligations_bind_muslim_rulers).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, warfare_only_defensive_in_quranic_ethics).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, historical_context_delimits_legal_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Muslim-majority states seeking peaceful pluralism and international legitimacy. They deploy this reading to constrain domestic extremist factions and justify cooperative relations with non-Muslim states. The reading provides hermeneutic cover for treaties, diplomatic recognition, and minority protections. Their exit is arbitrage-grade: they can cite classical precedent (e.g., Treaty of Hudaybiyyah, early caliphal practice) and modern international law simultaneously.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_states, beneficiary,
    institutional, generational, arbitrage, national).

% Christians, Jews, Zoroastrians, and other protected communities (dhimmis or modern citizens) whose security depends on the rejection of universal offensive jihad. They benefit structurally from the reading's constraint on extremist legal claims. They pay indirectly through the persistence of discriminatory structures (jizya legacy, apostasy laws, blasphemy statutes) that this reading does not fully dismantle. Exit is constrained: emigration is possible but costly; internal reform depends on state willingness.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_muslim_rule, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, non_muslim_minorities_under_muslim_rule, payer).

% Contemporary Muslim scholars (e.g., Abdullahi An-Na'im, Khaled Abou El Fadl, Mohammad Hashim Kamali) who build careers on contextualist hermeneutics. They gain professional recognition, funding, and institutional positions by advancing this reading. Their exit is mobile: they can shift to secular academia, interfaith NGOs, or Western universities if institutional support in Muslim-majority contexts collapses.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, reformist_scholars, beneficiary,
    organized, biographical, mobile, global).

% NGO leaders, diplomats, and civil society actors (Muslim and non-Muslim) who use this reading as a theological resource for peacebuilding. They benefit from a Quranic text that can be cited against violence. Exit is mobile: their work transfers across religious and secular frameworks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, interfaith_coexistence_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Groups (e.g., ISIS, Al-Qaeda affiliates, local Taliban-style movements) whose recruitment, legitimacy, and legal theology depend on 9:5 as a universal offensive command. This reading directly undermines their primary proof-text. They are identity-locked: their self-concept, recruitment narrative, and theological coherence fuse with the abrogationist reading; abandoning it dissolves the group's ideological core. They resist through takfir declarations against proponents, violence against reformists, and alternative hermeneutic infrastructures.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, universalist_extremist_groups, payer,
    organized, biographical, identity_locked, global).

% Traditionalist scholars and institutions (e.g., Al-Azhar classical track, Deobandi madrasas, Saudi Wahhabi establishment) whose authority rests on the classical nasikh/mansukh (abrogation) framework. This reading threatens their interpretive monopoly and the legal edifice built on it (offensive jihad, dhimmi subordination, apostasy penalty). They are constrained: they cannot easily abandon the framework without losing institutional identity, but they can issue fatwas against contextualism, control curriculum, and influence state religious apparatus. Their exit is constrained by institutional inertia and state patronage.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_abrogationist_jurists, payer,
    institutional, generational, constrained, global).

% State and non-state actors (historically: early Umayyad/Abbasid expansionists; contemporarily: irredentist militias) who operationalize 9:5 as a standing license for conquest. This reading strips their primary Quranic warrant. They are trapped: their military-political project requires the verse's universal reading; no alternative theology supports offensive expansion. They suppress the reading through censorship, violence, and state enforcement of classical doctrine.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, offensive_jihad_proponents, payer,
    powerful, immediate, trapped, regional).

% Academic scholars of Quranic studies (e.g., Angelika Neuwirth, Nicolai Sinai, Gabriel Said Reynolds) who analyze the verse's historical composition, redaction history, and early reception. They neither benefit nor pay; they map the constraint's genealogical conditions. Their analytical exit is absolute: they can adopt any hermeneutic frame without personal cost.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, quranic_textual_critics, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic settlement that allows Muslim communities to coexist peacefully with non-Muslims under treaty/covenant, coordinate international diplomacy, and maintain internal pluralism without theological civil war. Solves the coordination problem: how to read a text that appears to command universal violence in a way that enables stable peaceful order.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy from classical abrogationist institutions (who claim exclusive right to define Quranic law) to contextualist scholars and integrationist states. Transfers the capacity to declare war/peace from textual literalists to political authorities bound by treaty law. Transfers security from minority communities (who bear the cost of the universal reading) to the state (which guarantees protection).
% ABSENT_VOICES: The 7th-century Medinan polytheists addressed by the verse — their perspective on treaty violation and retaliation is unrecoverable. Early Muslim dissenters (if any) who may have read 9:5 universally — suppressed in hadith transmission. Modern Muslim majorities in non-Muslim states (e.g., India, China, Myanmar) whose security needs may align with universalist readings for self-defense — rarely consulted in this intra-Muslim hermeneutic debate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the abrogationist reading would become the only coherent textual option for literalist Muslims, legitimizing offensive jihad as standing law. Integrationist states would lose their primary Quranic warrant for treaties and minority rights. Extremist recruitment would surge. Interfaith diplomacy would lose its theological anchor. The Muslim world would rearrange toward either universalist conflict or secularist rejection of the text entirely.
% FOUNDING_PROBLEM: The classical juristic consensus (ijma) on naskh (abrogation) established 9:5 as the 'Verse of the Sword' abrogating 124+ peaceful verses, creating a standing legal obligation for offensive jihad until global submission. This created perpetual war as the default inter-civilizational state, endangering Muslim minorities abroad, non-Muslim minorities at home, and treaty obligations. The contextual-defensive reading was constructed to solve: how can the Quran be affirmed as divine while rejecting perpetual offensive warfare?
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists (Ibn Kathir, Al-Tabari, Al-Qurtubi) attest the abrogationist consensus was real and dominant for centuries — corroborating the founding problem's existence. Modern historians (Patricia Crone, Michael Cook, Fred Donner) corroborate that early Islamic expansion was justified through evolving hermeneutics, not a single fixed verse. Integrationist states (Indonesia, Malaysia, Tunisia, Senegal) attest the problem remains live: they actively use this reading against domestic extremists. No corroboration exists from universalist groups, who deny the problem ever existed (they claim the classical reading is simply 'what the Quran says').
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) for this reading itself because it functions as a protective constraint — it extracts from extremist groups their primary proof-text, but the 'extraction' is the removal of a warrant for violence, not a transfer of resources to the reading's proponents. Suppression (0.35) reflects the active effort required: curricular reform, fatwa councils, state censorship of abrogationist literature, protection of reformist scholars. Theater ratio (0.18) is low but rising: some states performatively cite this reading for Western audiences while maintaining blasphemy/apostasy laws domestically. Accessibility collapse (0.42) is moderate: the classical reading remains textually plausible (9:5's surface language is violent), so alternatives don't fully collapse. Resistance (0.58) is high: the abrogationist framework has 1200 years of institutional inertia, state patronage, and madrasa reproduction behind it.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute stark seat divergence: from the integrationist state seat, this is a rope (coordination for peaceful order, net beneficiary). From the universalist extremist seat, this is a snare (pure extraction of their theological capital, identity-locked target, active suppression via state power). From the classical jurist seat, this is a tangled rope (coordinates some state functions but extracts their authority). The claimed_type 'rope' reflects the authoring seat (integrationist state/reformist scholar); the engine's per-seat computation will reveal the constraint's true hybrid nature across the stakeholder field.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist states and reformist scholars are structural beneficiaries (d ≈ 0.15): they gain legitimacy, coherence, and policy space. Non-Muslim minorities are beneficiaries with secondary payer role (d ≈ 0.35): they gain security but pay through residual discriminatory structures. Universalist extremists are identity-locked full targets (d ≈ 0.95): their entire theological-political project collapses without the universal reading. Classical abrogationist jurists are constrained payers (d ≈ 0.70): they lose interpretive monopoly but retain institutional positions. Offensive jihad proponents are trapped payers (d ≈ 1.0): no exit without abandoning their military-political project. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The classical abrogationist reading has undergone mandatrophy: its founding problem (legal unity for early Islamic expansion) is dead, but the constraint persists through institutional inertia. This reading (contextual-defensive) is the scaffold erected to replace it — but it has not yet achieved full rope status because it still requires active enforcement against the zombie constraint. The mandatrophy is unresolved at the civilizational level: the old constraint's ghost (offensive jihad as standing law) still haunts the legal imagination, requiring this reading's continuous maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the contextual_defensive reading a distinct constraint from the abrogating_universal reading, or are they observer-relative perspectives on a single constraint?',
    'Test ε-invariance: if measuring extractiveness from the abrogationist seat yields ε≈0.8 (extracting peaceful coexistence) but from the contextualist seat yields ε≈0.2 (protecting coexistence), they are different constraints with different referents. The ε-invariance principle requires separate stories.',
    'If they are one constraint, the framework must model observer-relative ε; if two, they are linked via network.affects_constraints with distinct ε values. This story assumes the latter per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings instantiate distinct constraints per ε-invariance.').

omega_variable(
    historical_context_recoverability,
    'Can the specific historical referent of 9:5 (which treaty-breaking tribes, which treaties) be recovered with sufficient certainty to ground the contextual restriction?',
    'Convergence of Quranic exegesis (tafsir), sira literature, epigraphic evidence, and critical historical method. If the historical referent is irrecoverable, the contextual restriction becomes a constructive hermeneutic choice, not a recovered fact.',
    'If unrecoverable, the reading''s claim to textual fidelity weakens; it becomes more like progressive_synthesis (ethical override) than contextual_defensive (historical delimitation). This shifts its coordination type toward identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_recoverability, empirical, 'Epistemic status of the historical contextualization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the contextual_defensive reading by classical institutions structural (state censorship, curriculum control) or internalized (scholars genuinely believe the classical reading is correct)?',
    'Post-reform suppression trajectory: if classical institutions relax control but the abrogationist reading persists among scholars and publics, the suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint carries its own enforcement in the beliefs of the constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of the contextual reading.').

omega_variable(
    progressive_synthesis_boundary,
    'Where does the contextual_defensive reading end and the progressive_synthesis reading begin? Both reject universal offensive jihad; the difference is whether 9:5 retains any legal force.',
    'Survey of scholar self-identification: do proponents cite 9:5 as a valid (but contextual) legal source, or as a historical artifact with no legislative force? Track citation patterns in fatwas and constitutions.',
    'If the boundary is porous, the two readings may be a single constraint family with a gradient; if sharp, they are distinct constraints with different victim/beneficiary sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(progressive_synthesis_boundary, conceptual, 'Boundary between contextual restriction and ethical supersession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q9_5_ctx_def_tr_t1900, quran_9_5_scope__contextual_defensive, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(q9_5_ctx_def_tr_t1930, quran_9_5_scope__contextual_defensive, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(q9_5_ctx_def_tr_t1960, quran_9_5_scope__contextual_defensive, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(q9_5_ctx_def_tr_t1980, quran_9_5_scope__contextual_defensive, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(q9_5_ctx_def_tr_t2000, quran_9_5_scope__contextual_defensive, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(q9_5_ctx_def_tr_t2010, quran_9_5_scope__contextual_defensive, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(q9_5_ctx_def_tr_t2020, quran_9_5_scope__contextual_defensive, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(q9_5_ctx_def_be_t1900, quran_9_5_scope__contextual_defensive, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(q9_5_ctx_def_be_t1930, quran_9_5_scope__contextual_defensive, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(q9_5_ctx_def_be_t1960, quran_9_5_scope__contextual_defensive, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(q9_5_ctx_def_be_t1980, quran_9_5_scope__contextual_defensive, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(q9_5_ctx_def_be_t2000, quran_9_5_scope__contextual_defensive, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(q9_5_ctx_def_be_t2010, quran_9_5_scope__contextual_defensive, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(q9_5_ctx_def_be_t2020, quran_9_5_scope__contextual_defensive, base_extractiveness, 2020, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(q9_5_ctx_def_su_t1900, quran_9_5_scope__contextual_defensive, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(q9_5_ctx_def_su_t1930, quran_9_5_scope__contextual_defensive, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(q9_5_ctx_def_su_t1960, quran_9_5_scope__contextual_defensive, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(q9_5_ctx_def_su_t1980, quran_9_5_scope__contextual_defensive, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(q9_5_ctx_def_su_t2000, quran_9_5_scope__contextual_defensive, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(q9_5_ctx_def_su_t2010, quran_9_5_scope__contextual_defensive, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(q9_5_ctx_def_su_t2020, quran_9_5_scope__contextual_defensive, suppression_requirement, 2020, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_2_256_no_compulsion).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_8_61_incline_to_peace).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, classical_naskh_doctrine).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, modern_islamic_international_law).

% DUAL FORMULATION NOTE:
% This story is one member of the quran_9_5_scope constraint family. The abrogating_universal reading (high ε, snare/tangled_rope from this reading's perspective) and progressive_synthesis reading (low ε, rope/scaffold) are separate constraint stories linked here. All three share the kernel_id 'quran_9_5_scope' but instantiate different constraints with different ε, beneficiaries, victims, and types. The contextual_defensive reading structurally influences the progressive_synthesis reading (provides historical grounding for ethical trajectory) and is influenced by the abrogating_universal reading (must actively suppress it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, institutional, 0.15).
constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, organized, 0.95).
constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, powerful, 1.0).
constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
