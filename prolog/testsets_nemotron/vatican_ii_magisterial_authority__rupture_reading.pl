% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority — Rupture Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the 'rupture reading' of Vatican II as a
 *   structurally distinct constraint instantiated from the contested kernel
 *   'vatican_ii_magisterial_authority'. The rupture reading holds that the
 *   conciliar texts (especially Gaudium et Spes, Dignitatis Humanae, Lumen
 *   Gentium, Sacrosanctum Concilium) encode a new ecclesiology that
 *   contradicts and supersedes the pre-conciliar magisterium. This reading
 *   authorizes radical liturgical reform, doctrinal development through
 *   acknowledged contradiction, and a reorientation of the Church toward the
 *   modern world. It is enforced through institutional control of liturgy,
 *   catechesis, episcopal appointments, and canonical discipline. The
 *   constraint coordinates a genuine adaptation problem (the Church's
 *   modernity crisis) while extracting compliance from those formed in and
 *   committed to the superseded form — a classic tangled rope structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority — Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '5af55ad7-d18d-4b13-a64b-39a9fc935a1c').
narrative_ontology:cs_kernel_codification('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', fixed_text).
narrative_ontology:cs_authority_grounding('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', lineage).
narrative_ontology:cs_interpretation_layer_present('5af55ad7-d18d-4b13-a64b-39a9fc935a1c').
narrative_ontology:cs_reading_relation('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', foundational, conciliar_texts_authorize_rupture_with_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_texts_authorize_rupture_with_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', conciliar_texts_authorize_rupture_with_prior_magisterium, deontological).
narrative_ontology:cs_axiom('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', foundational, doctrinal_contradiction_as_progress).
narrative_ontology:cs_axiom_status(doctrinal_contradiction_as_progress, holdable).
narrative_ontology:cs_axiom_grounding('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', doctrinal_contradiction_as_progress, deontological).
narrative_ontology:cs_axiom('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', secondary, liturgical_form_as_contingent_not_substantial).
narrative_ontology:cs_axiom_status(liturgical_form_as_contingent_not_substantial, holdable).
narrative_ontology:cs_axiom_grounding('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', liturgical_form_as_contingent_not_substantial, instrumental).
narrative_ontology:cs_reference_frame('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', conciliar_texts_as_rupture_authorization).
narrative_ontology:cs_drift_state('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', post_traditionis_custodes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5af55ad7-d18d-4b13-a64b-39a9fc935a1c', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_reformist_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liturgical_experimentation_advocates).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theological_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_ecclesiology_adherents).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, societies_rejecting_religious_freedom_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, secular_religious_freedom_advocates).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, doctrinal_progress_through_contradiction).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, legitimate_liturgical_development_authorized_by_conciliar_texts).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, religious_freedom_as_doctrinal_advance).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, error_has_no_rights_superseded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls conciliar implementation, episcopal appointments, liturgical norms, and catechetical direction. Reads the Council as authorizing radical reform; uses magisterial authority to enforce this reading through institutional channels. Can redirect resources, suppress dissenting publications, and shape seminary formation. Exit is effectively costless — they hold the levers.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_reformist_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain canonical space for liturgical creativity, vernacular worship, and pastoral adaptation. Their projects receive funding, publication outlets, and institutional cover. Exit is available — they could operate in independent communities — but the institutional platform amplifies their reach.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liturgical_experimentation_advocates, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, liturgical_experimentation_advocates, agenda_setter).

% Dominates Catholic universities, theological journals, and advisory bodies. The rupture reading validates their methodological commitments (historical-critical, hermeneutic of discontinuity) and secures their interpretive authority. They face professional risk only if the institutional wind shifts; otherwise they occupy the commanding heights.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theological_establishment, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of liturgical suppression, canonical marginalization, and epistemic disqualification. Their formational investment (seminary, religious life, parish leadership) is tied to the pre-conciliar form; exit means abandoning vocation, community, and identity. Some form parallel structures (FSSP, ICKSP, SSPX) but remain canonically irregular or dependent on indult. The identity lock is vocational and ecclesial — they cannot 'be Catholic' on their terms without the form the constraint suppresses.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_and_laity, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_and_laity, excluded).

% Hold positions (e.g., on religious liberty, ecumenism, collegiality) that the rupture reading declares superseded. They lose teaching posts, publishing access, and pastoral assignments. Unlike traditionalist clergy, they often lack organized alternative structures — they are individual scholars, priests, or laypeople isolated by the new orthodoxy. Exit from the Church is existentially costly; exit within the Church is blocked by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_ecclesiology_adherents, payer,
    powerless, generational, trapped, global).

% Groups (e.g., SSPX, certain traditionalist institutes, some Catholic integralist movements) that reject Dignitatis Humanae as doctrinal error. They face canonical penalties, exclusion from ordinary jurisdiction, and public designation as schismatic or disobedient. Their exit option is canonical irregularity — they maintain parallel structures but lack juridical communion. The constraint extracts their legitimacy and forces them into a choice between conscience and communion.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, societies_rejecting_religious_freedom_doctrine, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, societies_rejecting_religious_freedom_doctrine, excluded).

% Scholars (e.g., Ratzinger pre-pontificate, de Lubac, Congar, later 'hermeneutic of continuity' proponents) who read the Council from within the tradition but contest the rupture interpretation. They are not beneficiaries of the rupture reading; they are not its primary victims — they occupy an analytical seat that sees the structural tension. Their work is cited by both sides but does not capture the extraction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, magisterial_continuity_scholars, observer,
    analytical, civilizational, analytical, universal).

% Non-Catholic actors (states, NGOs, international bodies) who gain a powerful normative ally in the Church's DH teaching. The rupture reading's acknowledgment of contradiction-as-progress strengthens the universal religious freedom framework. They do not administer the constraint but benefit from its doctrinal output. Exit is irrelevant — they are external to the ecclesial constraint but structurally advantaged by its rupture reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, secular_religious_freedom_advocates, beneficiary,
    powerful, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the crisis of Catholic legitimacy in modernity by authorizing a new ecclesiological self-understanding: the Church as pilgrim people of God, collegial episcopate, religious freedom as right, liturgy as communal action. Provides a unified mandate for adaptation that the pre-conciliar framework could not generate.
% TRANSFER_FUNCTION: Moves interpretive authority, liturgical control, catechetical content, episcopal appointments, and canonical standing from the pre-conciliar guardians (traditionalist bishops, Roman Curia of the 1950s, Thomist manualists) to the post-conciliar reformist hierarchy and their theological allies. Moves the cost of dislocation onto traditionalist clergy, laity, and communities who invested in the suppressed form.
% ABSENT_VOICES: The pre-conciliar magisterium itself (Pius XII, the 1917 Code, the papal teaching corpus 1864–1958) — its texts are the object of the rupture claim but it cannot speak. The laity of the 1950s–60s who received the pre-conciliar formation and were never consulted on its supersession. The Eastern Catholic Churches whose liturgical and canonical traditions were overridden by Latin-rite reform. The 'silent majority' of ordinary Catholics in 1965 who experienced the transition as disorientation, not liberation.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight, the interpretive license for radical liturgical experimentation, the doctrinal basis for religious freedom as positive right, the collegial restructuring of episcopal authority, and the hermeneutic legitimizing contradiction-as-progress would lose their conciliar warrant. The post-conciliar institutional order would face a legitimacy vacuum. Traditionalist communities would gain canonical normalization. The global Catholic institutional landscape would reorganize around a continuity reading or fragment.
% FOUNDING_PROBLEM: The Church's credibility collapse in the modern world: loss of intellectual authority, missionary sterility, liturgical fossilization, inability to engage democracy and religious pluralism, the 'Church of the dead' vs 'Church of the living' crisis diagnosed by the ressourcement theologians and the papal opening to aggiornamento.
% FOUNDING_PROBLEM_CORROBORATION: The reformist school (Congar, Rahner, Schillebeeckx, the Rhine Fathers) attests the founding problem was real and the rupture reading solves it. The continuity school (Ratzinger, de Lubac, later Wojtyła/John Paul II, the 'hermeneutic of continuity' magisterium) attests the problem was real but the rupture reading misdiagnoses the solution — the Council solves it by deepening tradition, not breaking it. The traditionalist critique (Lefebvre, Davies, the SSPX corpus) attests the problem was manufactured or exaggerated to justify a pre-determined rupture. No neutral arbiter exists; the founding problem itself is contested territory.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the scale of transfer: liturgical patrimony, catechetical frameworks, canonical standing, and vocational investments of traditionalist communities are extracted to fund the new ecclesial form. Suppression (0.62) is substantial but not total — parallel structures exist (Ecclesia Dei institutes, Summorum Pontificum/Traditionis Custodes regime), but they are dependent on indult and subject to restriction. Theater ratio (0.38) captures the growing performative gap: the rhetoric of 'hermeneutic of continuity' from the magisterium (1985–2022) masks the structural rupture the constraint actually enforces. Accessibility collapse (0.71) is high — the pre-conciliar form is rendered canonically difficult, culturally marginalized, and epistemically disqualified as 'rejecting the Council.' Resistance (0.55) is significant and persistent: traditionalist communities, the Lefebvre schism, the Ecclesia Dei negotiations, the Summorum Pontificum/Traditionis Custodes oscillation, and the ongoing 'hermeneutic wars' demonstrate that the constraint must be actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences this as genuine coordination solving a real crisis (rope-like). The payer/excluded seats experience it as enforced extraction suppressing their form of life (snare-like). The beneficiary seats experience it as liberation (rope-like). The observer seat sees the structural hybrid (tangled rope). The engine computes this divergence from the declared power/exit/role structure; the authored claim (tangled_rope) reflects the observer's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar reformist hierarchy (agenda_setter, institutional, arbitrage exit) sits at the beneficiary pole (d ≈ 0.1) — they control the constraint and capture its gains. Liturgical experimentation advocates and progressive theologians (beneficiary, organized, mobile) are secondary beneficiaries (d ≈ 0.25). Secular religious freedom advocates (beneficiary, powerful, arbitrage) are external beneficiaries (d ≈ 0.0). Traditionalist clergy/laity (payer/excluded, moderate, identity_locked) are primary targets (d ≈ 0.85) — their identity is fused to the suppressed form. Pre-conciliar ecclesiology adherents (payer, powerless, trapped) are total targets (d ≈ 0.95). Societies rejecting DH (payer/excluded, organized, constrained) are high-target (d ≈ 0.8). Magisterial continuity scholars (observer, analytical) sit at the analytical pole (d = 0.5). The identity_lock on traditionalist clergy/laity is vocational-ecclesial: they cannot exercise their vocation in the suppressed form without canonical irregularity, and leaving the Church abandons their ecclesial identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (modernity crisis) was live in 1965. The rupture reading claims it remains live — the Church must continue 'reading the signs of the times' through rupture. The continuity reading claims the problem was solved by the Council rightly understood (development, not rupture). The traditionalist reading claims the problem was manufactured. The mandatrophy question: has the rupture reading's coordination function (adapting the Church to modernity) been achieved or exhausted, leaving only extraction? The rising extractiveness and theater ratio (1965→2025) suggest the coordination function has atrophied into ritual invocation of 'the Spirit of the Council' while the extraction of traditionalist compliance continues. The constraint persists because the agenda-setters capture the gains and the payers are identity-locked — a mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_empirical_adequacy,
    'Does the conciliar text, read in its totality and historical context, actually warrant the rupture reading''s claim of fundamental break, or does it warrant the continuity reading''s claim of development within tradition?',
    'Systematic textual-historical analysis of the conciliar acts, interventions, schemas, and voting records — not selective citation. The ''hermeneutic of continuity'' school has produced extensive documentation; the rupture school has produced its own. A definitive corpus-linguistic and reception-history study could resolve this.',
    'If the continuity reading is textually superior, the rupture reading''s extractiveness is entirely parasitic on a misreading — the constraint is a snare masquerading as a rope. If the rupture reading is textually superior, the extraction is the price of a genuine doctrinal advance — the constraint is a tangled rope with a real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_vs_continuity_empirical_adequacy, conceptual, 'Whether the rupture reading''s textual warrant is adequate or whether it imposes a break the texts do not require.').

omega_variable(
    identity_lock_mechanism_traditionalists,
    'Is the traditionalist clergy/laity identity lock primarily vocational (cannot be a priest in the old rite without canonical irregularity), ecclesial (cannot be Catholic on their terms without the old form), or ideological (their self-concept is constituted by opposition to the rupture)?',
    'Longitudinal sociological study of traditionalist communities: track vocational pathways, retention rates, canonical regularization outcomes, and self-understanding narratives across generations. Compare FSSP/ICKSP (regularized) vs SSPX (irregular) vs lay traditionalist trajectories.',
    'If vocational-ecclesial, the lock is structural — the constraint creates it. If ideological, the lock is partially self-generated — the constraint exploits it but does not create it. This changes the directionality derivation and the moral weight of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_traditionalists, empirical, 'Mechanism of identity lock on the primary victim group.').

omega_variable(
    coordination_function_exhaustion,
    'Has the rupture reading''s coordination function (solving the Church''s modernity crisis) been achieved, exhausted, or inverted — such that the constraint now persists primarily by extracting from the superseded form?',
    'Measure Catholic institutional vitality metrics (vocations, Mass attendance, catechetical literacy, missionary output, cultural influence) in rupture-dominant vs continuity-dominant vs traditionalist-dominant ecclesial segments over 1965–2025. Correlate with the extractiveness/theater measurements.',
    'If coordination function is exhausted, the constraint is drifting toward piton or snare. If still live, the tangled rope classification holds. The rising theater ratio (0.15→0.38) and extractiveness (0.35→0.68) suggest exhaustion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_exhaustion, empirical, 'Whether the constraint''s coordination justification remains operative or has atrophied into pure extraction.').

omega_variable(
    composite_reading_structural_relation,
    'Does the composite_overdetermination_reading structurally foreclose the rupture reading, coexist with it, or influence it?',
    'Analyze whether the composite reading''s claim (the Council texts are irreducibly ambiguous compromises) logically entails that no single coherent reading (rupture or continuity) can be authoritatively imposed — which would foreclose the rupture reading''s agenda-setter claim to authoritative implementation.',
    'If forecloses, the rupture reading''s agenda-setter authority is structurally illegitimate even on its own terms. If coexists, the rupture reading is one viable implementation among others. If influences, the composite reading creates pressure toward interpretive pluralism that undermines the rupture reading''s enforcement monopoly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_reading_structural_relation, conceptual, 'Structural relationship between rupture reading and composite overdetermination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.61).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, dignitatis_humanae_reception).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, traditionalist_canonical_status).

% DUAL FORMULATION NOTE:
% This constraint (rupture_reading) is one of three readings of the kernel 'vatican_ii_magisterial_authority'. The continuity_reading claims the same texts authorize organic development; the composite_overdetermination_reading claims the texts are ambiguously overdetermined. The rupture reading's ε (0.68) is substantially higher than the continuity reading's (est. 0.25) because the rupture reading extracts from the pre-conciliar form while the continuity reading integrates it. The composite reading's ε is indeterminate — it denies a single authoritative implementation. All three stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, organized, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, moderate, 0.85).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
