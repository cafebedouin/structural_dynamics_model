% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Vatican II Rupture Reading — Council Invalidity Constraint
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture reading' of the Vatican
 *   II kernel — the position that the Council represents a substantive break
 *   with Tradition and contains doctrinal errors irreconcilable with prior
 *   magisterial teaching. The reading is held by the SSPX, sedevacantists,
 *   and traditionalist laity. It identifies the modernist faction within the
 *   hierarchy as the beneficiary of the rupture (they gained institutional
 *   control to implement their vision) and traditional Catholic identity and
 *   doctrinal stability as the victims. The constraint is classified as a
 *   snare: pure extraction disguised as legitimate development, maintained by
 *   active suppression of traditionalist alternatives (canonical penalties,
 *   marginalization, liturgical prohibition). The extraction is the transfer
 *   of authority from the deposit of faith to the living magisterium's
 *   reinterpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.88).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Rupture Reading — Council Invalidity Constraint").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'f8837aab-0481-473c-893e-3e28ca3ce017').
narrative_ontology:cs_kernel_codification('f8837aab-0481-473c-893e-3e28ca3ce017', formalized).
narrative_ontology:cs_authority_grounding('f8837aab-0481-473c-893e-3e28ca3ce017', lineage).
narrative_ontology:cs_interpretation_layer_present('f8837aab-0481-473c-893e-3e28ca3ce017').
narrative_ontology:cs_reading_relation('f8837aab-0481-473c-893e-3e28ca3ce017', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f8837aab-0481-473c-893e-3e28ca3ce017', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('f8837aab-0481-473c-893e-3e28ca3ce017', foundational, conciliar_texts_contradict_prior_dogma).
narrative_ontology:cs_axiom_status(conciliar_texts_contradict_prior_dogma, holdable).
narrative_ontology:cs_axiom_grounding('f8837aab-0481-473c-893e-3e28ca3ce017', conciliar_texts_contradict_prior_dogma, empirically_contingent).
narrative_ontology:cs_axiom('f8837aab-0481-473c-893e-3e28ca3ce017', foundational, no_hermeneutic_can_reconcile_contradiction).
narrative_ontology:cs_axiom_status(no_hermeneutic_can_reconcile_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('f8837aab-0481-473c-893e-3e28ca3ce017', no_hermeneutic_can_reconcile_contradiction, deontological).
narrative_ontology:cs_axiom('f8837aab-0481-473c-893e-3e28ca3ce017', secondary, living_magisterium_cannot_override_deposit_of_faith).
narrative_ontology:cs_axiom_status(living_magisterium_cannot_override_deposit_of_faith, holdable).
narrative_ontology:cs_axiom_grounding('f8837aab-0481-473c-893e-3e28ca3ce017', living_magisterium_cannot_override_deposit_of_faith, deontological).
narrative_ontology:cs_reference_frame('f8837aab-0481-473c-893e-3e28ca3ce017', pre_conciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('f8837aab-0481-473c-893e-3e28ca3ce017', post_conciliar_implementation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f8837aab-0481-473c-893e-3e28ca3ce017', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_identity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_laity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, traditionalist_ecclesiology).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls post-conciliar institutional apparatus — seminaries, diocesan structures, publishing, media, academic theology. Uses the rupture to justify liturgical, disciplinary, and doctrinal innovations that consolidate their authority. Collects the gains of institutional control: appointments, curricula, narrative formation, access to Vatican dicasteries. Exit is near-arbitrage: they can move between academic posts, episcopal appointments, and media platforms with institutional backing.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    institutional, generational, arbitrage, global).

% Administers the post-conciliar order: implements liturgical reforms, governs dioceses, appoints bishops aligned with the reading, and enforces canonical discipline against traditionalist resistance. They set the agenda for how the Council is interpreted and applied. Exit is constrained — a bishop cannot easily resign without Vatican acceptance; career path depends on alignment.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Bears the extraction: the lived experience of the faith — liturgy, catechesis, moral formation — is restructured around a rupture reading that renders their formative tradition incoherent. They pay in cognitive dissonance, loss of sacramental continuity, marginalization in parishes, and canonical penalties for non-compliance. Exit is identity-locked: the faith IS their self-constitution; leaving the Church is experienced as ontological loss, not institutional switching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_identity, payer,
    organized, civilizational, identity_locked, global).

% The deposit of faith itself — as an objective doctrinal structure — pays the price of contradiction. When conciliar texts conflict with prior defined dogma, the very possibility of stable teaching is undermined. There is no exit for doctrinal stability; it either holds or it collapses. The extraction is structural: the constraint forces a choice between accepting contradiction or rejecting the Church's teaching authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_stability, payer,
    powerless, civilizational, trapped, universal).

% Ordinary faithful formed in the pre-conciliar Church who experience the rupture as betrayal. They bear the cost of liturgical displacement, catechetical incoherence, and exclusion from the ordinary sacramental life they knew. Some migrate to SSPX or other traditionalist groups, but these exist in canonical irregularity — exit is identity-locked and institutionally penalized.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_laity, payer,
    powerless, biographical, identity_locked, global).

% The Society of St. Pius X instantiates this reading institutionally: they reject the Council's authority, maintain the traditional liturgy and doctrine, and operate outside regular canonical structures. They would object to the rupture reading being called 'schismatic' — they frame it as fidelity. Their exclusion from full communion is the enforcement mechanism that validates the rupture reading's institutional dominance. Exit from exclusion requires accepting the Council, which their reading forbids.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_leadership, excluded,
    organized, generational, constrained, global).

% Protestant, Orthodox, and secular scholars who study the Council as a historical-theological event. They have no stake in Catholic doctrinal authority but observe how the rupture reading shapes ecumenical dialogue, religious liberty debates, and the Church's public witness. Their analysis is unconstrained by canonical discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecumenical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the rupture reading denies the Council has a legitimate coordination function. It views the Council as a mechanism for displacing tradition rather than solving a coordination problem. The pre-conciliar Church coordinated through stable doctrine and liturgy; the rupture reading sees the Council as breaking that coordination.
% TRANSFER_FUNCTION: Moves doctrinal authority, liturgical control, institutional appointments, and narrative sovereignty from the pre-conciliar tradition (embodied in traditionalist clergy/laity) to the modernist hierarchy. Transfers the faithful's trust in continuity into acceptance of novelty. The transfer is from the deposit of faith as objectively received to the living magisterium as subjectively reinterpreted.
% ABSENT_VOICES: The pre-conciliar magisterium itself — the popes and councils whose teaching the rupture reading contradicts — are structurally absent (dead). The faithful of prior centuries who lived and died in the tradition now declared defective are absent. SSPX and traditionalist voices are excluded from synodal processes, Vatican dicasteries, and mainstream Catholic media.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight — i.e., if the Church officially acknowledged the Council contains errors and returned to pre-conciliar doctrine/liturgy as normative — the entire post-conciliar institutional order would rearrange: liturgy, catechesis, seminary formation, episcopal appointments, ecumenical orientation, and canonical discipline would all revert or be radically restructured. The modernist faction would lose its institutional basis.
% FOUNDING_PROBLEM: The rupture reading was not 'built' to solve a problem — it emerged as a diagnostic claim by traditionalists (especially Archbishop Lefebvre and the SSPX) in response to the Council and its implementation. The founding problem it IDENTIFIES is: how can a Catholic accept conciliar texts that contradict prior defined doctrine without losing the Faith? The reading answers: you cannot; the Council is defective.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by pre-conciliar magisterial texts themselves (Quanta Cura, Syllabus of Errors, Pascendi Dominici Gregis, Mortalium Animos, etc.) which the rupture reading cites as contradicted by Dignitatis Humanae, Unitatis Redintegratio, Nostra Aetate, and Gaudium et Spes. The contradiction is textual and objective — no beneficiary attestation needed. Theologians outside the traditionalist camp (e.g., Josef Ratzinger pre-papacy, Roberto de Mattei) have acknowledged the hermeneutical difficulty, though they resolve it differently.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.88) because the constraint demands acceptance of contradiction as development — the faithful must surrender the principle of non-contradiction in doctrinal matters. Suppression (0.72) is structural: the traditional Latin Mass was effectively prohibited (1970–2007), traditionalist groups canonically penalized, seminary formation redesigned to exclude pre-conciliar theology. Theater ratio is low (0.25): the coordination story ('aggiornamento,' 'opening to the world') is thin cover; the real function is institutional capture. Accessibility collapse is high (0.85): once the contradiction is seen, the alternatives are 'accept rupture' or 'leave the Church' — no middle ground. Resistance is moderate (0.35): traditionalist resistance exists but is institutionally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive hierarchy seat, the constraint is a rope or scaffold — legitimate development solving the coordination problem of modernity. From the traditionalist seat, it is a snare — extraction of their faith's integrity. From the analytical observer seat, it is a contested interpretive framework. The engine computes this seat divergence from the declared power/exit/role structure; the claim (snare) reflects the rupture reading's own structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist faction and progressive hierarchy are beneficiaries (d near 0.0) — they control the institutions, collect appointments, define the narrative. Traditional Catholic identity and doctrinal stability are victims (d near 1.0) — they bear the contradiction, loss of continuity, canonical exclusion. Traditionalist laity are identity-locked payers: their faith is their selfhood; exit is ontological loss. SSPX leadership is excluded (trapped/constrained exit) — their institutional existence is the proof of the rupture. The engine computes per-seat directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading prevents mislabeling the Council as pure coordination (rope) by exposing the beneficiary/victim asymmetry: the modernist faction gained institutional power through the rupture, while traditional Catholics lost their formative tradition. The mandate of 'aggiornamento' has atrophied into a permanent revolution — the Council is not a past event but an ongoing hermeneutic that justifies continuous change. The constraint persists not because it coordinates but because the beneficiaries control the enforcement apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_legitimacy_condition,
    'Does the rupture reading''s core premise (the Council contains errors contradicting prior dogma) logically foreclose the continuity reading within a single Catholic framework, or can both be held as live interpretive options by different parties?',
    'Analyze whether the contradiction between conciliar texts and prior dogma is formally demonstrable (foreclosure) or hermeneutically contestable (coexistence). If formally demonstrable, the continuity reading is logically impossible for anyone accepting the rupture reading''s premises. If contestable, both readings coexist as live positions.',
    'If forecloses: the kernel has a structural fault line — no single commitment framework can hold both readings. If coexists_with: the kernel sustains a permanent schism of interpretation. This determines the reading_relation type in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_legitimacy_condition, conceptual, 'Whether the rupture/continuity divide is logical foreclosure or hermeneutic coexistence.').

omega_variable(
    composite_reading_relation,
    'Does the composite_overdetermination_reading foreclose, coexist with, or influence the rupture reading? The composite reading argues the Council is not a single event but an overdetermined composite — does this structural claim undermine the rupture reading''s premise of a unified ''Council'' that can be judged valid/invalid?',
    'Compare the rupture reading''s unitary object (''the Council'') with the composite reading''s fragmented object (''multiple distinct shifts''). If the composite reading''s fragmentation is accepted, the rupture reading''s target dissolves — the constraint''s object changes.',
    'If influences: the composite reading creates downstream pressure on the rupture reading by fragmenting its object. If coexists_with: both are live descriptions of the same historical reality at different analytical levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_reading_relation, conceptual, 'Structural relationship between rupture reading and composite overdetermination reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist alternatives primarily structural (canonical penalties, liturgical prohibition) or internalized (the faithful''s own acceptance that the old rite is ''superseded'')?',
    'Post-restriction trajectory: if traditionalist communities grow and thrive when canonical restrictions are lifted (Summorum Pontificum, Traditionis Custodes), suppression was primarily structural. If adherence collapses without enforcement, internalization is significant.',
    'If internalized, effective suppression is higher than structural measures suggest — the faithful carry the constraint with them. This affects the suppression metric''s interpretation for identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the traditionalist marginalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__rupture_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__rupture_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__rupture_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__rupture_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__rupture_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__rupture_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__rupture_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__rupture_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__rupture_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__rupture_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__rupture_reading, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__rupture_reading, base_extractiveness, 2005, 0.86).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__rupture_reading, base_extractiveness, 2015, 0.88).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__rupture_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__rupture_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__rupture_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__rupture_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__rupture_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__rupture_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__rupture_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__rupture_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, traditional_latin_mass_restriction).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, post_conciliar_catechetical_reform).

% DUAL FORMULATION NOTE:
% Part of the vatican_ii_authority constraint family (kernel_id: vatican_ii_authority). This rupture_reading extracts 0.88 from traditionalist identity; the continuity_reading claims negligible extraction (coordination); the composite_overdetermination_reading claims structural ambiguity prevents clean classification. The three readings share the same historical referent (the Council) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, organized, 0.85).
constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
