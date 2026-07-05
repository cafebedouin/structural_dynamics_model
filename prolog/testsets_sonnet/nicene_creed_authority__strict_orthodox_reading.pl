% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_doctrine
 *
 * SUMMARY:
 *   This story instantiates the strict orthodox reading of the
 *   Nicene(-Constantinopolitan) creed: the claim that the creed fixes one
 *   binding metaphysical ontology of God's nature, such that deviation from
 *   its precise technical formulation (homoousios, hypostatic union, etc.)
 *   constitutes heresy warranting ecclesiastical and, historically, civil
 *   sanction. This is deliberately narrower than the natural-language label
 *   'the Nicene Creed' — sibling readings (symbolic_confessional_reading,
 *   liturgical_habituation_reading) describe structurally different
 *   constraints with different beneficiary/victim profiles and different
 *   extraction levels, and are NOT part of this file. Under this reading,
 *   extraction is substantial: the creed's ontological-binding function is
 *   inseparable from an active heresy-policing apparatus that produced
 *   concrete victims (anathematized churches, exiled clergy, suppressed lay
 *   dissent) across the interval from the 4th century councils through the
 *   early modern confessional era.
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: agenda_setter (institutional/arbitrage) — administers sanction apparatus
 *   - creedal_magisterium: beneficiary/agenda_setter (institutional/arbitrage) — monopolizes interpretive authority over technical ontology
 *   - heterodox_communities: payer (powerless/trapped) — anathematized, exiled, sanctioned
 *   - lay_metaphysical_dissenters: payer (powerless/constrained) — must recite unintelligible formula to remain in communion
 *   - non_chalcedonian_churches: payer (organized/constrained) — schismed at immense political cost
 *   - historical_theologians: observer (analytical) — reconstructs the contingent political-theological history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.79).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '38cf85ed-8b15-49ff-b189-3795cad650e2').
narrative_ontology:cs_kernel_codification('38cf85ed-8b15-49ff-b189-3795cad650e2', fixed_text).
narrative_ontology:cs_authority_grounding('38cf85ed-8b15-49ff-b189-3795cad650e2', lineage).
narrative_ontology:cs_interpretation_layer_present('38cf85ed-8b15-49ff-b189-3795cad650e2').
narrative_ontology:cs_reading_relation('38cf85ed-8b15-49ff-b189-3795cad650e2', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('38cf85ed-8b15-49ff-b189-3795cad650e2', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('38cf85ed-8b15-49ff-b189-3795cad650e2', foundational, creed_fixes_one_true_ontology).
narrative_ontology:cs_axiom_status(creed_fixes_one_true_ontology, holdable).
narrative_ontology:cs_axiom_grounding('38cf85ed-8b15-49ff-b189-3795cad650e2', creed_fixes_one_true_ontology, deontological).
narrative_ontology:cs_axiom('38cf85ed-8b15-49ff-b189-3795cad650e2', foundational, deviation_constitutes_culpable_heresy).
narrative_ontology:cs_axiom_status(deviation_constitutes_culpable_heresy, holdable).
narrative_ontology:cs_axiom_grounding('38cf85ed-8b15-49ff-b189-3795cad650e2', deviation_constitutes_culpable_heresy, conventional).
narrative_ontology:cs_reference_frame('38cf85ed-8b15-49ff-b189-3795cad650e2', conciliar_ontological_settlement).
narrative_ontology:cs_drift_state('38cf85ed-8b15-49ff-b189-3795cad650e2', post_reformation_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38cf85ed-8b15-49ff-b189-3795cad650e2', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, creedal_magisterium).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_theological_faculties).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_metaphysical_dissenters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, non_chalcedonian_churches).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, homoousion_doctrine).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, trinitarian_consubstantiality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, ratifies the creed's wording as binding metaphysical doctrine, and administers the sanctioning apparatus (excommunication, deposition, anathema) against those who deviate. Collects institutional authority, doctrinal gatekeeping power, and the capacity to define who counts as a believer in good standing, from the creed's operation as a fixed ontological boundary.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The teaching authority (councils, patriarchates, later papal magisterium in the West) that interprets what the creed's technical terms (homoousios, hypostasis) metaphysically require. Its authority is self-reinforcing: the more the creed is treated as fixing one ontology, the more indispensable the magisterium's interpretive monopoly becomes.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, creedal_magisterium, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, creedal_magisterium, agenda_setter).

% Universities and seminaries whose curricula, credentials, and career paths are built around defending the creed's precise metaphysical formulation. They gain professional legitimacy and institutional funding from the creed's status as settled ontological truth requiring specialist defense against heresy.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_theological_faculties, beneficiary,
    organized, generational, mobile, continental).

% Groups holding alternative Christologies (subordinationist, adoptionist, later miaphysite or dyophysite positions) are formally declared heretical, stripped of clerical office, exiled, or subjected to imperial sanction once the creed is enforced by state power. Their exit options historically amounted to underground practice, flight, or forced conformity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, trapped, regional).

% Ordinary believers who find the technical ontology of the creed unintelligible or unpersuasive but must recite it as a condition of communion, baptism sponsorship, or marriage within the church. Dissent risks social exclusion from the community that structures their local life; genuine exit means leaving the only religious community available to them.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_metaphysical_dissenters, payer,
    powerless, biographical, constrained, local).

% Entire church traditions (e.g., those holding miaphysite Christology) were formally anathematized under strict ontological readings of conciliar creeds, resulting in centuries of schism, loss of imperial patronage, and periodic persecution. Their exit was institutional separation at immense political and demographic cost, not genuine negotiation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, non_chalcedonian_churches, payer,
    organized, civilizational, constrained, continental).

% Scholars who reconstruct the political, linguistic, and philosophical contingencies behind the creed's formulation, without being bound by its sanctioning apparatus. They can document how metaphysical language was fixed under specific imperial and ecclesiastical pressures.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, creedal_magisterium).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, memorizable doctrinal formula that lets geographically dispersed churches confirm mutual recognition of communion partners and exclude teachings judged incompatible with apostolic testimony, solving a real problem of doctrinal fragmentation in the early church.
% TRANSFER_FUNCTION: Moves interpretive authority, clerical office, and social legitimacy from communities holding alternative Christological formulations to the hierarchy and faculties that administer the creed's official, ontologically-binding interpretation; moves social standing and communion access away from lay dissenters who cannot or will not affirm the technical formula.
% ABSENT_VOICES: Non-Chalcedonian and other anathematized communities, and lay believers whose vernacular understanding of the faith differs from the technical metaphysics, were not meaningfully present as equal parties at the councils that fixed the binding language; their objections survive mainly in polemical fragments preserved by the winning side.
% DISAPPEARANCE_RATIONALE: If the strict ontological-binding reading of the creed vanished, communion requirements would loosen, heresy trials and anathemas would lose their doctrinal warrant, and entire schismatic church bodies currently defined by their rejection of or by this formula would lose the boundary that constitutes their separate identity — ecclesiastical structures organized around orthodoxy/heresy would need to reorganize around some other criterion.
% FOUNDING_PROBLEM: The early church faced genuine theological fragmentation (Arian and other Christological disputes) that threatened both doctrinal coherence and imperial political unity; a fixed formula was sought to settle what counted as authentic apostolic teaching about Christ's nature.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity (outside the beneficiary hierarchy) attest the founding dispute was real but was substantially entangled with imperial politics (Constantine's and later emperors' desire for unified state religion) rather than purely doctrinal necessity; contemporary ecumenical theologians and non-Chalcedonian church historians attest that the sanctioning function has persisted long after the specific 4th-5th century political crisis that occasioned it, functioning now primarily as an inherited boundary-maintenance mechanism rather than a live response to an active Arian-scale threat.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the strict ontological-binding reading requires an active magisterial monopoly on metaphysical interpretation whose maintenance transfers real institutional power, communion access, and social legitimacy away from dissenting communities. Suppression (0.79) is high and structurally distinct from extraction: enforcement historically relied on imperial law, conciliar anathema, and social excommunication — coercive machinery independent of whether any individual believer found the formula persuasive. Theater ratio is comparatively low (0.28) because the doctrinal-policing function, however extractive, was largely genuinely operative rather than merely performative for most of the interval; it rises modestly toward the end as enforcement capacity in modern pluralist states weakens relative to formal doctrinal claims still asserted. Accessibility collapse (0.62) and resistance (0.58) reflect that alternative theological framings never fully disappeared — non-Chalcedonian churches persisted continuously, and lay dissent recurred — so collapse is real but not mountain-level complete, consistent with a tangled_rope rather than snare or mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's seat, the strict reading is coordination: a single trusted formula preventing doctrinal chaos across a dispersed communion. From the heterodox or lay-dissenting seat, the identical structure is enforced extraction: a technical ontology imposed as precondition for communion, with real sanctions for noncompliance. The engine computes both from the same structural data — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal hierarchy and magisterium sit near the full-beneficiary end: they set the binding formula, administer sanctions, and derive institutional authority and interpretive monopoly from the ontological-fixing function. Theological faculties benefit secondarily through professional legitimacy built on defending the formula. Heterodox communities, lay dissenters, and non-Chalcedonian churches sit near the full-target end: trapped or constrained exit, direct sanction exposure, and no meaningful voice in the formula's original fixing. The high suppression score is not scaled by scope in this computation; only extractiveness receives the scope amplification the engine applies given the creed's civilizational-scope enforcement history.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling genuine 4th-century Christological disputes that threatened both church unity and imperial political cohesion) was substantially live at the creed's origin but is contested as still-live today: non-Chalcedonian historians and ecumenical theologians attest the acute crisis passed centuries ago, while the sanctioning apparatus and doctrinal-uniformity claims persisted (and in some traditions persist) as inherited boundary-maintenance rather than active crisis response. This is precisely the mismatch the R5 genealogy interview is designed to surface: founding_problem_status=contested against disappearance_verdict=world_rearranges signals a possible zombie-mandate pattern worth flagging for downstream capture analysis, distinct from a genuine live-mandate tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_fixity_vs_communal_discernment,
    'Does the creed''s authority derive from fixing one metaphysically true ontology (this reading), or from the community''s ongoing discernment and personal faith commitment (symbolic_confessional_reading)?',
    'No empirical resolution is available; this is a question of theological epistemology internal to different ecclesial traditions. Comparative analysis of how different communions historically enforced (or did not enforce) technical assent can at least map which institutional structures presuppose which reading.',
    'If the symbolic_confessional_reading is correct, the sanctioning apparatus modeled in this story is a category error applied to a document that was never meant to function as binding ontology, and the extraction measured here reflects institutional overreach rather than the creed''s actual authority structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_fixity_vs_communal_discernment, conceptual, 'Whether creedal authority is metaphysically binding or communally discerned — the central kernel contest.').

omega_variable(
    cognitive_assent_vs_ritual_performance,
    'Is the operative mechanism of creedal boundary-maintenance actually cognitive metaphysical assent (this reading) or liturgical performance independent of belief content (liturgical_habituation_reading)?',
    'Historical and ethnographic study of how ordinary believers across centuries actually related to the creed in worship — whether recitation functioned as assent-testing or as ritual identity marker regardless of interpretive understanding.',
    'If liturgical habituation is the dominant real mechanism, the extractiveness and victim set modeled here (heresy trials targeting metaphysical belief) may overstate the constraint''s actual operative function for most ordinary lay participants, who may have experienced something closer to the liturgical reading''s lower-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_assent_vs_ritual_performance, empirical, 'Whether the creed''s binding function operates through cognitive assent or ritual performance.').

omega_variable(
    sanction_apparatus_causal_necessity,
    'Was the sanctioning/anathematizing apparatus a necessary consequence of the strict ontological reading, or a contingent political tool (imperial religious unification) that could have been decoupled from the theological claim?',
    'Comparative church history: examine periods and regions where strict ontological creedal claims were maintained without state-backed sanction, to test whether extraction and suppression are intrinsic to the reading or contingent on its political deployment.',
    'If sanction is contingent rather than intrinsic, a version of the strict orthodox reading without imperial enforcement machinery would show substantially lower suppression and might reclassify closer to a rope with genuine coordination function and minimal coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_apparatus_causal_necessity, empirical, 'Whether coercive enforcement is intrinsic to strict ontological creedal claims or a contingent political overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nice_tr_t0, observed).
narrative_ontology:measurement(nice_tr_t300, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement_basis(nice_tr_t300, observed).
narrative_ontology:measurement(nice_tr_t600, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 600, 0.19).
narrative_ontology:measurement_basis(nice_tr_t600, observed).
narrative_ontology:measurement(nice_tr_t900, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement_basis(nice_tr_t900, observed).
narrative_ontology:measurement(nice_tr_t1300, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1300, 0.25).
narrative_ontology:measurement_basis(nice_tr_t1300, observed).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.28).
narrative_ontology:measurement_basis(nice_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(nice_be_t0, observed).
narrative_ontology:measurement(nice_be_t300, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement_basis(nice_be_t300, observed).
narrative_ontology:measurement(nice_be_t600, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 600, 0.63).
narrative_ontology:measurement_basis(nice_be_t600, observed).
narrative_ontology:measurement(nice_be_t900, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 900, 0.65).
narrative_ontology:measurement_basis(nice_be_t900, observed).
narrative_ontology:measurement(nice_be_t1300, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1300, 0.66).
narrative_ontology:measurement_basis(nice_be_t1300, observed).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.68).
narrative_ontology:measurement_basis(nice_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(nice_su_t0, observed).
narrative_ontology:measurement(nice_su_t300, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 300, 0.74).
narrative_ontology:measurement_basis(nice_su_t300, observed).
narrative_ontology:measurement(nice_su_t600, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 600, 0.81).
narrative_ontology:measurement_basis(nice_su_t600, observed).
narrative_ontology:measurement(nice_su_t900, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement_basis(nice_su_t900, observed).
narrative_ontology:measurement(nice_su_t1300, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1300, 0.6).
narrative_ontology:measurement_basis(nice_su_t1300, observed).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.79).
narrative_ontology:measurement_basis(nice_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nicene_creed_authority kernel. strict_orthodox_reading (this file) models the creed as fixing one binding metaphysical ontology enforced through heresy sanction — high extractiveness, clear beneficiary/victim structure. symbolic_confessional_reading models the creed as historically contingent witness whose authority derives from communal discernment — expected substantially lower extraction and no clear victim set. liturgical_habituation_reading models the creed's function as ritual identity-boundary marker independent of cognitive assent — expected moderate extraction concentrated in social-belonging costs rather than doctrinal sanction. Each carries its own ε; do not average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
