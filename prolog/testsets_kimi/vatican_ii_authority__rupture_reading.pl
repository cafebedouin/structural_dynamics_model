% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority: Rupture Reading
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   This constraint story instantiates the rupture reading of the
 *   vatican_ii_authority kernel. It models the post-conciliar magisterial
 *   enforcement of the council as a tangled rope: a structure that maintains
 *   genuine ecclesial coordination (sacramental governance, global
 *   institutional unity) while asymmetrically extracting doctrinal and
 *   liturgical compliance from traditional Catholics. The rupture reading
 *   holds that the conciliar documents contain substantive errors and
 *   irreconcilable contradictions with prior magisterial teaching, rendering
 *   the council either invalid or gravely defective. From this seat, the
 *   post-conciliar Church is in crisis: a modernist faction has captured
 *   institutional space, while traditional Catholic identity-bearers and
 *   resistance groups (notably the SSPX) pay the costs through liturgical
 *   suppression, marginalization, and canonical irregularity. The key agents
 *   are identified by structural relationship to the enforcement apparatus,
 *   not by canonical labels.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: Agenda-setter (institutional/universal) â enforces conciliar documents and suppresses traditional practice
 *   - progressive_religious_faction: Primary beneficiary (organized/global) â gains institutional space and doctrinal latitude
 *   - traditional_catholic_identity_bearers: Primary target (powerless/global/identity_locked) â bear costs of liturgical suppression and doctrinal confusion
 *   - sspx_clergy_and_communities: Target payer (moderate/global/constrained) â maintain resistance at cost of canonical irregularity
 *   - pre_conciliar_theologians: Excluded voice (powerless/trapped) â silenced from advisory and academic roles
 *   - canonical_theologian_observer: Analytical observer â evaluates textual and structural continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.82).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.85).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority: Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theological/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '7e400608-553e-425f-a773-ac8a85388a99').
narrative_ontology:cs_kernel_codification('7e400608-553e-425f-a773-ac8a85388a99', formalized).
narrative_ontology:cs_authority_grounding('7e400608-553e-425f-a773-ac8a85388a99', lineage).
narrative_ontology:cs_interpretation_layer_present('7e400608-553e-425f-a773-ac8a85388a99').
narrative_ontology:cs_reading_relation('7e400608-553e-425f-a773-ac8a85388a99', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7e400608-553e-425f-a773-ac8a85388a99', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('7e400608-553e-425f-a773-ac8a85388a99', foundational, conciliar_texts_contain_doctrinal_errors).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_doctrinal_errors, holdable).
narrative_ontology:cs_axiom_grounding('7e400608-553e-425f-a773-ac8a85388a99', conciliar_texts_contain_doctrinal_errors, empirically_contingent).
narrative_ontology:cs_axiom('7e400608-553e-425f-a773-ac8a85388a99', foundational, post_conciliar_magisterium_lacks_full_authority).
narrative_ontology:cs_axiom_status(post_conciliar_magisterium_lacks_full_authority, holdable).
narrative_ontology:cs_axiom_grounding('7e400608-553e-425f-a773-ac8a85388a99', post_conciliar_magisterium_lacks_full_authority, deontological).
narrative_ontology:cs_reference_frame('7e400608-553e-425f-a773-ac8a85388a99', substantive_break_with_tradition).
narrative_ontology:cs_drift_state('7e400608-553e-425f-a773-ac8a85388a99', post_conciliar_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7e400608-553e-425f-a773-ac8a85388a99', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_religious_faction).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity_bearers).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, sspx_clergy_and_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces Vatican II documents and post-conciliar reforms as binding authoritative teaching. Administratively restricts traditional liturgical celebrations, excludes traditionalist candidates from seminary formation and clerical appointments, and imposes canonical penalties on resistance communities. Derives institutional legitimacy from the council; its authority is structurally committed to the conciliar settlement.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, constrained, universal).

% Gained institutional space, academic positions, and pastoral latitude following the council. Liturgical experimentation, theological pluralism, and ecumenical initiatives advanced their preferences. They do not set the magisterial agenda but benefit from the opening of doctrinal and disciplinary boundaries that the conciliar settlement maintains.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_religious_faction, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of suppressed liturgical traditions, doctrinal ambiguity in catechesis, and the erosion of pre-conciliar devotional life. Their religious identity is fused with traditional Latin liturgy, scholastic theology, and pre-conciliar magisterial clarity; remaining in ordinary parishes means accepting practices they view as harmful to faith, while exit to fully traditional structures carries severe canonical and social penalties.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_identity_bearers, payer,
    powerless, biographical, identity_locked, global).

% Operate sacramental and educational ministries under canonical irregularity because they reject post-conciliar doctrinal and liturgical innovations. They bear the costs of exclusion from diocesan structures, the spiritual burden of uncertain canonical status, and institutional marginalization. They maintain pre-conciliar practice despite the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_clergy_and_communities, payer,
    moderate, biographical, constrained, global).

% Theological and spiritual voices formed in pre-conciliar scholastic and mystical traditions who have been systematically excluded from post-conciliar academic appointments, seminary teaching roles, and curial advisory positions. They would object to doctrinal novelties but lack access to the institutional conversation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, pre_conciliar_theologians, excluded,
    powerless, biographical, trapped, global).

% Studies conciliar texts, pre-conciliar magisterial teachings, and post-conciliar disciplinary acts to assess structural continuity or rupture. Occupies an analytical seat outside the beneficiary and payer structures.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, canonical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified global Church under a single sacramental and magisterial authority with common (though reformed) liturgical and doctrinal standards, enabling pastoral coordination across dioceses, curial governance, and ecumenical engagement with non-Catholic Christians.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical authority from pre-conciliar custodial structures to post-conciliar progressive reformers, extracting compliance with novel teachings and liturgical forms from traditional Catholics in exchange for continued canonical standing and ordinary sacramental access.
% ABSENT_VOICES: Pre-conciliar theologians, contemplatives formed in the old spiritual and intellectual traditions, and lay faithful attached to the traditional Latin Mass are structurally excluded from magisterial advisory roles, seminary formation committees, and doctrinal commissions. They would argue for the council's nullity, limitation, or corrective reinterpretation but are absent from the authoritative conversation, often replaced by voices committed to the conciliar settlement.
% DISAPPEARANCE_RATIONALE: If the post-conciliar magisterial enforcement of Vatican II vanished overnight, traditional liturgical and doctrinal practices would reassert themselves in ordinary parishes, progressive theological experiments would lose institutional cover, the progressive faction would lose its platform, and resistance communities like the SSPX would regularize â the visible Church would rearrange around pre-conciliar or corrected forms.
% FOUNDING_PROBLEM: The mid-twentieth-century Catholic Church faced pastoral and ecumenical challenges: presenting the faith to secular modernity, improving relations with separated Christians and non-Christians, and addressing perceived ossification in curial and liturgical structures.
% FOUNDING_PROBLEM_CORROBORATION: The progressive faction attests the problem remains live, citing secularization. Traditional Catholic scholars and the SSPX attest the council was not a necessary or legitimate response. Independent sociological evidence is ambiguous: secularization accelerated in traditionally Catholic regions after the council, suggesting the arrangement failed even on its own terms; corroboration from outside the benefiting party is weak and contested.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint enforces doctrinal novelties and liturgical reforms that traditionalists view as spiritually harmful and coercively imposed. Suppression is higher (0.85) because persistence depends on active canonical penalties, restrictions on the traditional Latin Mass, exclusion of traditional candidates, and marginalization of resistance communities. Theater ratio is substantial (0.65): the hermeneutic of continuity is the primary performative mechanism, rhetorically asserting fidelity to tradition while administratively enforcing rupture. Accessibility collapse is high (0.80) because fully traditional alternatives are structurally barred from ordinary canonical life; the SSPX exists but under irregularity, and diocesan traditional communities face tightening restrictions. Resistance is moderate (0.55): traditionalist movements are vocal and organized but contained by institutional power asymmetry. Measurements trace intensification from the council's close (1965) to the present, showing extraction and suppression ratcheting upward as enforcement machinery matured.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (post-conciliar magisterium) experiences the constraint as necessary coordination for a modern Church; the progressive beneficiary seat experiences it as liberation. The traditionalist payer seats experience the identical structure as doctrinal extraction and identity warfare. The engine computes this divergence from structural data: identical scope and power atoms produce opposite chi when crossed with beneficiary versus victim declarations and identity_locked versus mobile exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium derives low directionality from its agenda-setting and beneficiary-like control of the institutional apparatus, though it is constrained by the conciliar settlement it cannot easily repudiate. The progressive faction derives strongly low d from its mobile exit and collection of institutional openings. Traditional Catholics derive high d from their identity_locked exit â their religious self-concept is fused with pre-conciliar forms, making departure from the Church spiritually unthinkable while remaining under the post-conciliar regime carries the extraction. The SSPX derives moderate-high d: they have exited ordinary structures but remain trapped in canonical irregularity and spiritual peril from the same constraint. No directionality overrides are needed because the structural derivation chain captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading prevents mislabeling the post-conciliar regime as pure extraction (snare) by acknowledging the genuine coordination function of sacramental governance and global ecclesial unity that persists even under doctrinal deviation. It prevents mislabeling it as pure coordination (rope) by naming the asymmetric victimization of traditional Catholics and the active enforcement required to maintain the conciliar settlement against resistance. The R5 genealogy interview shows a contested founding problem with corroboration from outside the benefiting party, signaling that the arrangement persists beyond its legitimate mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_location_ambiguity,
    'Does the rupture reside principally in the conciliar texts themselves, or in the post-conciliar interpretive and administrative apparatus that claims to implement them?',
    'Textual-historical analysis comparing the conciliar documents in their final voted form against the post-conciliar disciplinary acts and catechisms; if the texts are orthodox but implementation deviated, the constraint''s epsilon lowers and classification shifts toward piton/inertial enforcement rather than tangled rope.',
    'If the rupture is text-immanent, the council itself is the defective constraint; if interpretive, the constraint is a later extraction layered onto a kernel that might admit alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_location_ambiguity, conceptual, 'Whether rupture is in the texts or the post-conciliar apparatus').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditional Catholic practice structural (canonical penalties, liturgical restrictions, exclusion from seminaries) or internalized (traditionalists accepting the schismatic label, spiritual guilt, self-censorship)?',
    'Post-exit trajectory study: traditional Catholics who join fully traditional jurisdictions (SSPX, sedevacantist groups) â if psychological suppression persists after structural exit, reclassify effective suppression upward.',
    'If internalized, effective extraction exceeds structural measures because the target carries the constraint after leaving; if purely structural, exit to traditional jurisdictions genuinely reduces chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    modernist_beneficiary_coherence,
    'Is the progressive religious faction a coherent, organized beneficiary capturing the constraint''s gains, or is the post-conciliar reform diffuse and unowned?',
    'Network analysis of post-conciliar institutional appointments, theological commission compositions, and funding flows to identify concentrated capture versus decentralized drift.',
    'If coherent and organized, the directionality derivation holds and the classification stays tangled_rope; if diffuse, gain_flow becomes indeterminate and the extraction profile may compute as piton (inertial performance without capturer).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_beneficiary_coherence, empirical, 'Whether progressive benefit is captured or diffuse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__rupture_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__rupture_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__rupture_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__rupture_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__rupture_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__rupture_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__rupture_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__rupture_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__rupture_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__rupture_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__rupture_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__rupture_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__rupture_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__rupture_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__rupture_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the rupture reading of the vatican_ii_authority kernel, decomposed per the epsilon-invariance principle from the continuity and composite readings because the structural relationships and epsilon profiles differ irreconcilably across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
