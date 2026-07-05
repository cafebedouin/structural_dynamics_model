% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority — Traditionalist Rupture Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the traditionalist-rupture reading of the Vatican
 *   II kernel: the claim that the Council's documents contain genuine
 *   ambiguities and compromise-drafted formulations (particularly on
 *   religious liberty, ecumenism, and the nature of the liturgy) that
 *   constitute a rupture with prior magisterial teaching rather than its
 *   organic development, and that this rupture — not merely its subsequent
 *   misreading — is what enabled decades of heterodox implementation. This is
 *   a distinct constraint from the progressive-rupture reading (which affirms
 *   the same textual ambiguity but reads it as liberating rather than
 *   erroneous) and from the continuity reading (which denies the rupture
 *   altogether). The three readings share no ε value; they disagree about
 *   whether the same measured discontinuity is a defect, a virtue, or an
 *   illusion. This file covers only the traditionalist valence.
 *
 * KEY AGENTS:
 *   - postconciliar_episcopal_bureaucracy: agenda_setter (institutional/arbitrage) — administers implementation, controls which reading gets institutional backing
 *   - traditional_latin_mass_communities: payer (powerless/constrained) — bears restriction of prior liturgical forms under successive reinterpretations of the same texts
 *   - missionary_orders_with_lapsed_vocations: payer (moderate/constrained) — bears demographic and institutional cost attributed to softened missionary theology
 *   - sspx_and_allied_traditionalist_clergy: excluded (moderate/identity_locked) — critique structurally kept outside magisterial forums
 *   - roman_curia_doctrinal_offices: observer/agenda_setter (institutional/analytical) — periodically adjudicates specific disputes without resolving underlying ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority — Traditionalist Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '08880abd-1938-45cd-a7bb-a8e1aa9889c0').
narrative_ontology:cs_kernel_codification('08880abd-1938-45cd-a7bb-a8e1aa9889c0', fixed_text).
narrative_ontology:cs_authority_grounding('08880abd-1938-45cd-a7bb-a8e1aa9889c0', extraction).
narrative_ontology:cs_interpretation_layer_present('08880abd-1938-45cd-a7bb-a8e1aa9889c0').
narrative_ontology:cs_reading_relation('08880abd-1938-45cd-a7bb-a8e1aa9889c0', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('08880abd-1938-45cd-a7bb-a8e1aa9889c0', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('08880abd-1938-45cd-a7bb-a8e1aa9889c0', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('08880abd-1938-45cd-a7bb-a8e1aa9889c0', foundational, conciliar_ambiguity_constitutes_doctrinal_error).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_constitutes_doctrinal_error, holdable).
narrative_ontology:cs_axiom_grounding('08880abd-1938-45cd-a7bb-a8e1aa9889c0', conciliar_ambiguity_constitutes_doctrinal_error, deontological).
narrative_ontology:cs_axiom('08880abd-1938-45cd-a7bb-a8e1aa9889c0', foundational, preconciliar_magisterium_binds_without_qualification).
narrative_ontology:cs_axiom_status(preconciliar_magisterium_binds_without_qualification, holdable).
narrative_ontology:cs_axiom_grounding('08880abd-1938-45cd-a7bb-a8e1aa9889c0', preconciliar_magisterium_binds_without_qualification, conventional).
narrative_ontology:cs_reference_frame('08880abd-1938-45cd-a7bb-a8e1aa9889c0', preconciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('08880abd-1938-45cd-a7bb-a8e1aa9889c0', postconciliar_implementation_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('08880abd-1938-45cd-a7bb-a8e1aa9889c0', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, postconciliar_episcopal_bureaucracy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conference_of_bishops_administrators).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders_with_lapsed_vocations).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechized_laity_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, priests_disciplined_for_preconciliar_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers implementation of the conciliar documents through national bishops' conferences, liturgical commissions, and seminary curricula. Cites the ambiguous conciliar texts as authorization for reforms (vernacular liturgy, collegial governance structures, revised catechetical formation) that traditionalist critics say go well beyond, or against, what the texts actually say. Controls appointments, imprimaturs, and canonical discipline, and so controls which readings of the Council get institutional backing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, postconciliar_episcopal_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, postconciliar_episcopal_bureaucracy, beneficiary).

% Built academic careers, publishing programs, and institutional influence on expansive readings of Council ambiguity — ecumenism, religious liberty, collegiality. Benefit directly from the interpretive latitude the traditionalist reading identifies as compromise-induced error; have strong incentive to defend the documents' openness rather than concede the ambiguities were flaws.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties, beneficiary,
    organized, generational, mobile, global).

% National-conference staff and administrators whose offices (liturgy commissions, ecumenical dialogue bureaus, catechetical institutes) exist because of the post-conciliar reform apparatus. Their institutional continuity depends on the reforms being read as legitimate development rather than as rupture requiring reversal.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conference_of_bishops_administrators, beneficiary,
    institutional, biographical, arbitrage, national).

% Communities attached to the pre-conciliar liturgy who experienced (and in various periods continue to experience) restriction, relocation, or suppression of the older rite by diocesan and Roman authority acting on a reading of the Council as requiring liturgical reform. Their access to the older liturgical forms has been granted, restricted, and re-restricted across decades depending on the presiding pontificate's reading of the same conciliar documents; they cannot appeal past the hierarchy that administers the ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities, payer,
    powerless, biographical, constrained, regional).

% Religious orders and missionary societies that trace steep vocational decline and loss of institutional identity to post-conciliar reforms in formation, community life, and missionary self-understanding (particularly reformed readings of Ad Gentes and Nostra Aetate softening the urgency of conversion). They bear the demographic and institutional cost of an ambiguity they did not write and cannot litigate.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders_with_lapsed_vocations, payer,
    moderate, generational, constrained, global).

% Ordinary Catholics catechized after the Council who report receiving doctrinally inconsistent formation depending on diocese, parish, or decade, tracing this directly to unresolved ambiguity in the conciliar texts on matters like religious liberty, ecumenism, and the nature of the Church. Have no mechanism to demand textual clarification and must simply navigate whatever local reading prevails.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechized_laity_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, global).

% Clergy suppressed, removed from ministry, or refused ordination/incardination for adherence to pre-conciliar liturgical or doctrinal practice, treated by diocesan authorities as noncompliant with the 'spirit' or letter of conciliar reform. Their canonical standing depends entirely on the prevailing episcopal reading of documents the traditionalist analysis holds to be internally defective.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, priests_disciplined_for_preconciliar_practice, payer,
    powerless, biographical, trapped, national).

% Clergy and communities who formally or informally broke canonical communion or entered irregular canonical status specifically over the traditionalist rupture reading of the Council, arguing the documents' ambiguities on religious liberty, ecumenism, and collegiality constitute doctrinal discontinuity. Their position is structurally excluded from official magisterial forums even where individual reconciliation processes exist; their critique is treated as a disciplinary problem rather than a hermeneutical one requiring textual answer.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sspx_and_allied_traditionalist_clergy, excluded,
    moderate, generational, identity_locked, global).

% Vatican dicasteries (Doctrine of the Faith, and prior pontifical commissions) that periodically adjudicate specific disputed points arising from conciliar ambiguity — religious liberty, ecumenism, liturgical norms — issuing clarifications that sometimes side with continuity readings, sometimes tacitly concede traditionalist critiques (e.g., restricting the older rite one pontificate, expanding it the next), without ever resolving the underlying textual ambiguity itself.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia_doctrinal_offices, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, roman_curia_doctrinal_offices, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, postconciliar_episcopal_bureaucracy).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council did solve a real coordination problem: it updated the Church's engagement with modernity, other Christian communions, non-Christian religions, and the vernacular liturgical needs of a global Church, replacing scattered ad hoc accommodations with a single conciliar settlement other institutions could reference.
% TRANSFER_FUNCTION: Interpretive and disciplinary authority over what counts as authentic conciliar teaching moves from the plain text (which the traditionalist reading holds to be internally compromised) to whichever institutional actor currently controls implementation — episcopal conferences, curial dicasteries, seminary formators — at the cost of liturgical continuity, doctrinal clarity, and missionary urgency borne by traditionalist communities, catechized laity, and declining missionary orders.
% ABSENT_VOICES: The minority conciliar fathers who voted against or abstained on the most contested documents (Dignitatis Humanae, Nostra Aetate, Gaudium et Spes) are historically documented but structurally absent from ongoing implementation discourse; their objections were procedurally overruled at the Council and have no standing mechanism for reconsideration. Traditionalist clergy in irregular canonical status are formally excluded from magisterial forums where the ambiguity could be textually resolved.
% DISAPPEARANCE_RATIONALE: If the conciliar documents' authority were suspended overnight, the postconciliar bureaucratic apparatus (liturgy commissions, ecumenical offices, revised formation programs) would lose its textual warrant and likely be restructured or reversed — a major rearrangement for the beneficiary institutions. But traditionalist communities dispute that this would constitute loss of anything real, since from their reading the pre-conciliar magisterium remains the operative and unbroken authority; the disagreement about what would change is itself downstream of the disagreement about what the documents did.
% FOUNDING_PROBLEM: The Council was convened to address the Church's relationship to modernity, to other Christians, to other religions, and to a liturgy in a language most laity no longer understood — real, live problems in 1962.
% FOUNDING_PROBLEM_CORROBORATION: Episcopal administrators and progressive faculties attest the founding problem was substantially solved and current practice represents legitimate development. Traditionalist clergy, missionary orders reporting vocational collapse, and independent sociological studies of post-conciliar Mass attendance and vocations decline (cited by scholars outside both the traditionalist and progressive camps, e.g. demographic historians of Catholic institutional decline) attest that whatever the Council solved, it introduced new problems of doctrinal ambiguity and institutional attrition that were not present, or were less acute, before it — corroboration exists on both sides from parties with no stake in the traditionalist reading specifically.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising over the interval because, on the traditionalist reading, the compromise-drafted ambiguity is not incidental but load-bearing: each successive reinterpretation cycle (post-conciliar reform, John Paul II's partial retrenchments, Summorum Pontificum's liberalization of the older rite, Traditionis Custodes's re-restriction) redistributes disciplinary and liturgical costs onto the same traditionalist-identified communities without ever settling the textual question. Suppression is authored as oscillating (0.4 to a peak near 0.7 mid-interval, settling near 0.62) to reflect the documented cyclical tightening and loosening of restrictions on the older liturgical forms and traditionalist clergy standing across successive pontificates — this is not a monotonic ratchet but a contested disciplinary cycle, which the suppression_requirement series is authored to show honestly rather than flattened to a single trend. Theater ratio rises (0.2 to 0.48) reflecting the traditionalist observation that increasing institutional energy goes into managing the appearance of settled conciliar reception (jubilee celebrations, magisterial reaffirmations of 'the Council's true meaning') rather than resolving the underlying textual disputes.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal bureaucracy's seat, the conciliar documents are the coordinated, legitimate settlement its authority rests on, and ambiguity is simply the ordinary texture of any conciliar compromise. From the traditionalist payer seats, the same textual ambiguity is the mechanism by which costs (liturgical loss, vocational collapse, disciplinary exposure) get imposed without any possibility of textual appeal — the engine should compute these seats as experiencing structurally different constraint types from the same base document set, which is exactly the seat-divergence this framework is built to surface rather than resolve by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   The postconciliar bureaucracy, progressive faculties, and conference administrators are declared beneficiaries because their institutional standing, funding, and doctrinal legitimacy derive directly from the ambiguous documents being read as authorizing the reforms they administer or study — d sits near the beneficiary end for these seats. Traditionalist liturgical communities, missionary orders reporting vocational collapse, catechized laity, and disciplined clergy are declared victims because they bear concrete, often career- or community-ending costs (loss of liturgical access, vocational collapse, canonical discipline, catechetical confusion) traceable to the same textual ambiguity, with constrained or trapped exit — they cannot appeal past the hierarchy administering the ambiguity to a settled textual answer. SSPX-aligned clergy are excluded rather than victimized in the ordinary sense: their exit option is identity_locked because their entire canonical and communal identity is organized around the rupture critique itself, making straightforward reconciliation costly to their own self-understanding even where formally offered.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) preserves the genuine coordination function the Council performed — updating the Church's stance on liturgy, ecumenism, and modernity was a real problem requiring some conciliar settlement — while the traditionalist reading insists the specific textual execution introduced asymmetric costs that the coordination function alone does not explain. Classifying this purely as extraction would erase the real coordination the Council achieved (which even most traditionalists concede was partially necessary); classifying it purely as legitimate coordination (rope) would erase the documented, concrete costs borne disproportionately by traditionalist communities across six decades of disciplinary whiplash. The requires_active_enforcement flag reflects that maintaining any particular reading (progressive or reactive-restrictive) requires ongoing curial and episcopal intervention — the ambiguity does not resolve itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_development_fact_of_matter,
    'Is there an objective fact of the matter about whether specific conciliar formulations (religious liberty, ecumenism, collegiality) constitute doctrinal discontinuity with prior magisterial teaching, or is ''rupture vs. development'' itself a framework-relative description with no neutral adjudication procedure?',
    'Would require either (a) a future magisterial act with sufficient authority to be accepted by all current factions as dispositive — historically not achieved by any postconciliar clarification to date — or (b) independent historical-theological consensus outside all three contending camps on the doctrinal status of the specific disputed propositions, which does not currently exist.',
    'If rupture is a mind-independent structural fact, this traditionalist reading, the progressive reading, and the continuity reading are not equally available interpretive postures but competing empirical claims where only one can be correct, dramatically changing how the corpus should weight victim-set legitimacy. If the rupture/development question is genuinely framework-relative, all three readings persist indefinitely as coexisting constraints with no resolution mechanism, which is the assumption this story is authored under.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_vs_development_fact_of_matter, conceptual, 'Whether rupture-vs-continuity has a mind-independent answer or is irreducibly framework-relative.').

omega_variable(
    ambiguity_intentional_vs_accidental,
    'Were the ambiguous conciliar formulations deliberately constructed as compromise language to secure conciliar-majority passage (making the ambiguity a foreseeable structural feature), or are they accidental byproducts of drafting-committee process that no one intended to be exploitable?',
    'Conciliar drafting-history archives (acta synodalia, relatio texts, minority/majority position papers) document some deliberate compromise language explicitly; a fuller archival study distinguishing deliberate compromise clauses from unintentional drafting ambiguity would sharpen this.',
    'If ambiguity was a deliberately engineered compromise to pass contested documents, the traditionalist charge that the drafters bear responsibility for downstream heterodox implementation is strengthened considerably. If ambiguity was substantially accidental, the causal chain from text to subsequent implementation weakens, and more responsibility shifts to the implementing bureaucracy rather than the conciliar text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentional_vs_accidental, empirical, 'Whether conciliar textual ambiguity was deliberate compromise engineering or unintentional drafting artifact.').

omega_variable(
    vocational_decline_causal_attribution,
    'How much of the documented post-conciliar vocational and Mass-attendance decline is causally attributable to the Council''s textual ambiguity and implementation, versus broader mid-20th-century secularization trends that were already underway independent of the Council?',
    'Comparative demographic analysis across denominations and countries with differing exposure to conciliar reform, controlling for secularization trends already visible pre-1962 in comparable Christian communions that underwent no analogous council.',
    'If the decline tracks broader secularization more than conciliar-specific factors, the traditionalist victim-attribution for missionary orders and vocations is substantially weakened. If conciliar-specific factors show a distinguishable, additional effect beyond background secularization, the victim attribution is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vocational_decline_causal_attribution, empirical, 'Whether post-conciliar institutional decline is Council-caused or a background secularization trend the Council merely coincided with.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the vatican_ii_doctrinal_authority kernel (continuity, rupture_progressive, rupture_traditionalist, composite_overdetermination). All four share the same underlying documentary and historical substrate but assign different ε, different beneficiary/victim structures, and different normative valence to the same observed textual ambiguity. Per the ε-invariance principle, they are authored as four separate constraint files linked here rather than as one file with a hidden observer parameter. The continuity_reading is foreclosed by this reading's foundational axiom (conciliar ambiguity constitutes doctrinal error is incompatible with continuity's claim that no real ambiguity/discontinuity exists); the rupture_progressive_reading coexists with this reading, since both affirm the same underlying discontinuity and merely diverge on valence and disagree in ongoing ecclesial politics without either being logically forced out; this reading's insistence on textual fixity and defect creates downstream interpretive pressure on the composite_overdetermination_reading's attempt to decompose the Council into independently assessable structural shifts, without foreclosing that decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
