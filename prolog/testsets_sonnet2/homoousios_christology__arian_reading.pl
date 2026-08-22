% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Subordinationist Christology — Christ as Created and Subordinate
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This story instantiates the Arian/subordinationist reading of the
 *   fourth-century Christological kernel: Christ as created, begotten, and
 *   subordinate to the Father — not of identical divine substance. From 318
 *   (Arius's initial teaching in Alexandria) through 381 (the Council of
 *   Constantinople's confirmation of the Nicene settlement), this reading
 *   holds significant institutional power at various points — under
 *   Constantius II and later Valens, Arian and semi-Arian bishops controlled
 *   major eastern sees and used imperial machinery to depose pro-Nicene
 *   rivals. The extraction and suppression metrics track the reading's own
 *   institutional operation when it held power: its own bishops deposing and
 *   exiling opponents, forcing subscription to subordinationist formulas, and
 *   controlling sacramental access in contested cities. This is NOT a story
 *   about whether Arianism is theologically true — it is a story about the
 *   structural operation of the ecclesiastical-political arrangement built
 *   around this reading during the period it held real institutional force.
 *   The pro-Nicene reading and the semi-Arian (homoiousian) compromise are
 *   separate constraints (see network links); each has its own ε and its own
 *   beneficiary/victim structure from its own institutional high-water marks.
 *
 * KEY AGENTS:
 *   - Arius and successor bishops (Eusebius of Nicomedia) — agenda-setters administering the doctrine
 *   - Arian-sympathetic bishops holding eastern sees — beneficiaries of continued institutional recognition
 *   - Nicene clergy and laity under Arian-controlled sees — bear deposition, exile, forced compliance
 *   - Athanasius of Alexandria — repeatedly exiled opponent, powerful but structurally constrained
 *   - Roman emperors (Constantine through Valens) — arbitrage-mobile agenda-setters switching doctrinal favor for statecraft
 *   - Later church historians — analytical observers working from asymmetrically preserved sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.62).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.71).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Subordinationist Christology — Christ as Created and Subordinate").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '196b1c54-c061-4299-b2b5-74eea381064a').
narrative_ontology:cs_kernel_codification('196b1c54-c061-4299-b2b5-74eea381064a', formalized).
narrative_ontology:cs_authority_grounding('196b1c54-c061-4299-b2b5-74eea381064a', lineage).
narrative_ontology:cs_interpretation_layer_present('196b1c54-c061-4299-b2b5-74eea381064a').
narrative_ontology:cs_reading_relation('196b1c54-c061-4299-b2b5-74eea381064a', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('196b1c54-c061-4299-b2b5-74eea381064a', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('196b1c54-c061-4299-b2b5-74eea381064a', foundational, father_alone_unoriginate_source).
narrative_ontology:cs_axiom_status(father_alone_unoriginate_source, holdable).
narrative_ontology:cs_axiom_grounding('196b1c54-c061-4299-b2b5-74eea381064a', father_alone_unoriginate_source, deontological).
narrative_ontology:cs_axiom('196b1c54-c061-4299-b2b5-74eea381064a', foundational, son_begotten_therefore_ontologically_subordinate).
narrative_ontology:cs_axiom_status(son_begotten_therefore_ontologically_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('196b1c54-c061-4299-b2b5-74eea381064a', son_begotten_therefore_ontologically_subordinate, conventional).
narrative_ontology:cs_created_at('196b1c54-c061-4299-b2b5-74eea381064a', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_provincial_sees).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arius_theological_lineage).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_clergy_under_arian_sees).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, alexandrian_laity).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, athanasius_and_allies).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, subordinationist_logos_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, monarchian_preservation_of_the_father).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arius and his theological successors (Eusebius of Nicomedia and allied bishops) articulate and defend the position that the Logos was begotten/created by the Father and is therefore not of identical, eternal substance. They administer sees, ordain sympathetic clergy, and petition emperors for recognition, treating the doctrine as the guardian of monotheism against what they see as ditheism.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arius_theological_lineage, agenda_setter,
    institutional, civilizational, constrained, regional).

% Bishops across the eastern provinces (especially Nicomedia, Antioch, and parts of Asia Minor) who hold sees and imperial favor at various points hold and teach the subordinationist position. Their episcopal authority, patronage networks, and theological legitimacy depend on this Christology remaining a live, defensible option rather than a condemned heresy.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_bishops, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, arian_bishops, agenda_setter).

% Local ecclesiastical structures and clergy networks in regions where the subordinationist reading has taken root benefit from continued regional theological autonomy and imperial patronage cycles that favor their bishops. Their institutional standing rises and falls with the doctrine's imperial favor.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, eastern_provincial_sees, beneficiary,
    organized, generational, constrained, regional).

% Clergy who hold the homoousios position but serve within dioceses where Arian-sympathetic bishops hold power face deposition, exile, or forced subscription to subordinationist formulas as a condition of remaining in office. Their exit options are limited to flight, silence, or underground resistance.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_clergy_under_arian_sees, payer,
    moderate, biographical, trapped, regional).

% Ordinary believers in contested sees experience riots, competing bishops claiming the same cathedra, and shifting demands about which creed to confess at baptism and communion — their access to sacraments becomes hostage to the christological dispute's political fortunes.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, alexandrian_laity, payer,
    powerless, biographical, trapped, local).

% Athanasius of Alexandria and pro-Nicene allies are repeatedly deposed and exiled (five times, in Athanasius's case) by imperial and synodal action when Arian-sympathetic emperors or councils hold sway. They bear direct career and physical costs (exile, mob violence, loss of see) for refusing the subordinationist formula.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, athanasius_and_allies, payer,
    powerful, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, athanasius_and_allies, excluded).

% Roman emperors from Constantine through Constantius II and Valens alternately favor Nicene, semi-Arian, or Arian formulas depending on political calculation about imperial unity, convening councils (Nicaea, Antioch, Constantinople, Milan) and exiling bishops whose position falls out of favor. Their exit is effectively unconstrained — they can switch doctrinal favor when it serves statecraft.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, constantinian_and_successor_emperors, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, constantinian_and_successor_emperors, observer).

% Later ecclesiastical historians and theologians reconstruct the controversy from surviving conciliar records, letters, and polemics — largely preserved by the eventual pro-Nicene victors, which shapes what evidence survives about the Arian position's own internal reasoning.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, arian_bishops).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The subordinationist Christology coordinates a coherent answer to a genuine philosophical problem: how to affirm Christ's divine mediating role while preserving strict monotheism and the Father's unique unoriginate status, using the philosophical vocabulary (ousia, hypostasis, genesis) available in the fourth century. It gives eastern episcopal networks a stable, teachable doctrine that unifies catechesis and liturgy within their sees.
% TRANSFER_FUNCTION: The arrangement moves ecclesiastical authority, imperial patronage, control of sees and basilicas, and control of baptismal/eucharistic access from pro-Nicene clergy and laity to Arian-sympathetic bishops during the periods when the subordinationist reading holds imperial favor — and reverses when favor shifts.
% ABSENT_VOICES: The laity in contested cities (Alexandria, Antioch, Constantinople) have no vote in the councils that determine which creed they must confess; ordinary believers experience the doctrinal contest as street violence and forced sacramental compliance without formal standing to be heard. Non-Greek-speaking or non-elite Christian communities in the wider empire likewise have no seat at Nicaea, Antioch, or the imperial court.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading and its institutional backers vanished, the eastern sees that held it would lose their theological rationale for episcopal independence from Alexandria and Rome, imperial councils would lose a major axis of contest for allocating patronage, and the pro-Nicene formula would face no organized ecclesiastical counterweight — the entire fourth-century pattern of shifting imperial councils and exiled bishops depends on this reading remaining a live institutional option.
% FOUNDING_PROBLEM: How can Christian theology affirm the full divinity and mediating role of the Logos/Son without collapsing strict Jewish-derived monotheism into what looks like two gods, given that Scripture calls the Father the sole unoriginate source and calls the Son 'begotten' and sent?
% FOUNDING_PROBLEM_CORROBORATION: Arian bishops and their theological heirs attest the problem (preserving monotheism against ditheism) remains live and unresolved by homoousios language. Pro-Nicene sources — Athanasius's own polemical writings, and the acts of councils convened by rival factions — attest that the subordinationist solution creates a worse problem (a demoted, creature-like mediator) and that the founding monotheistic concern is better served by consubstantiality; no source outside the two contending theological camps survives to adjudicate independently, since surviving conciliar records are overwhelmingly preserved and edited by the eventual pro-Nicene institutional victors.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises through the 325-360 period (0.35 to 0.68) as Arian-sympathetic emperors (especially Constantius II) increasingly used imperial synods and exile as tools to install subordinationist bishops in contested sees, extracting institutional control from pro-Nicene incumbents. It recedes somewhat by 381 (to 0.62) as the political tide turns and enforcement capacity for Arian formulas erodes ahead of Constantinople I. Suppression tracks similarly and peaks slightly later (375) reflecting the compliance-enforcement machinery (forced creedal subscription, exile orders) built up under Valens before its collapse. Theater ratio is moderate-high (0.4) reflecting that a substantial share of conciliar activity in this period was performative jockeying for imperial favor rather than substantive theological resolution — councils multiplied (Antioch, Sirmium, Rimini, Seleucia) without settling anything, a classic sign of proxy-goal displacement.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an Arian bishop administering a see, the doctrine is a coherent, well-reasoned theological settlement defending monotheism — pure coordination. From the seat of a Nicene priest deposed and exiled for refusing to subscribe, the same structure is experienced as coercive extraction of office and voice, enforced by imperial power rather than won by argument. The engine should compute these as structurally different seat classifications from the same underlying data, which is exactly the divergence the tangled_rope classification is meant to hold open rather than resolve by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Arian bishops and Arius's theological lineage sit at the beneficiary end: they gain sees, patronage, and doctrinal legitimacy when the reading holds imperial favor. Nicene clergy under Arian sees and Alexandrian laity sit at the target end: trapped exit options, direct costs of deposition or forced compliance falling on them through the same synodal and imperial machinery that installs Arian bishops. Athanasius is a powerful but constrained target — his ecclesiastical rank does not translate into exit options, since deposition and exile are imposed by the same imperial-synodal structure regardless of his personal standing. Emperors sit outside the ordinary beneficiary/victim frame: their exit options are effectively arbitrage-grade, since they can switch doctrinal favor whenever unity or political stability recommends it, and they collect legitimacy and stability from whichever formula serves imperial cohesion at a given moment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading answers — how to preserve strict monotheism against apparent ditheism while affirming Christ's mediating divine role — was a live and serious theological question in 318. By 381, with Constantinople I's confirmation of Nicene homoousios language (as refined via the Cappadocian settlement), the institutional question of which formula the empire would enforce was substantially resolved in the pro-Nicene direction, even though the underlying philosophical question the Arian reading tried to answer was never simply dissolved — semi-Arian and Arian communities persisted for centuries, especially among Germanic peoples evangelized by Arian missionaries. Classifying this as tangled_rope rather than pure snare captures that the doctrine did real coordination work (a coherent, teachable answer to a real philosophical tension) for its adherent communities, while also functioning as the vehicle for concrete institutional extraction (deposition, exile, forced compliance) when its backers held imperial power. Neither a pure-coordination 'rope' label nor a pure-extraction 'snare' label would be honest to the mixed structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arian_reading_sibling_relations,
    'How does the Arian reading''s institutional operation relate structurally to the pro-Nicene and semi-Arian readings of the same kernel — are they mutually exclusive commitments, or can a single ecclesiastical-political framework hold more than one simultaneously?',
    'Trace conciliar history: does adoption of the Arian formula at a given council (e.g., Antioch 341, Sirmium 357) logically exclude simultaneous adoption of the Nicene formula within the same imperial jurisdiction, or do jurisdictions and sees hold competing formulas concurrently (as the historical record in fact shows, e.g., competing bishops of the same see)?',
    'If the readings are logically mutually exclusive within one framework (a single see cannot simultaneously confess both), the relationship is properly foreclosing; if regions and factions hold both simultaneously across the empire without either being dissolved, the relationship is coexisting — which is what the historical record shows through most of the fourth century.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arian_reading_sibling_relations, conceptual, 'Whether Arian and pro-Nicene readings logically exclude each other or merely compete for institutional dominance.').

omega_variable(
    natural_theology_vs_constructed_faction,
    'Is the subordinationist Christology best understood as a sincere theological conclusion from available philosophical premises (Middle Platonist emanationism, biblical subordination language) that happened to acquire institutional backers, or as a faction constructed and sustained primarily by the political interests of eastern episcopal networks seeking independence from Alexandrian and Roman authority?',
    'Compare the doctrine''s content and persistence in regions with minimal imperial patronage stakes (e.g., among Gothic and other Germanic Christian communities converted via Arian missionaries centuries after the imperial contest ended) against its behavior in politically contested sees — persistence absent political stakes would support the sincere-theology reading.',
    'If sincere theological reasoning dominates, the extraction metrics measure only the incidental institutional vehicle, not the doctrine''s core function; if political construction dominates, the extraction is closer to the doctrine''s actual generative logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_theology_vs_constructed_faction, conceptual, 'Whether the doctrine''s persistence reflects genuine theological conviction independent of institutional stakes, or primarily political faction-building.').

omega_variable(
    surviving_source_asymmetry,
    'How much does the near-total loss of Arius''s own writings and the overwhelming survival of pro-Nicene polemical sources (Athanasius, later conciliar records compiled by the eventual winners) distort the reconstructed extraction/suppression metrics for this reading?',
    'Cross-reference surviving fragmentary Arian sources (letters preserved in hostile quotation, later Gothic Arian liturgical texts) and independent civil/legal sources (imperial edicts, court records) against the polemical narrative to check whether the suppression exercised by Arian-controlled sees is corroborated outside pro-Nicene testimony.',
    'If the suppression metric rests substantially on hostile-source testimony with no independent corroboration, the authored value may overstate this reading''s own coercive operation relative to what an even-handed contemporary record would show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surviving_source_asymmetry, empirical, 'Whether asymmetric source survival biases the reconstructed metrics against the Arian reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 318, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t318, homoousios_christology__arian_reading, theater_ratio, 318, 0.2).
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__arian_reading, theater_ratio, 325, 0.3).
narrative_ontology:measurement(homo_tr_t337, homoousios_christology__arian_reading, theater_ratio, 337, 0.38).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__arian_reading, theater_ratio, 350, 0.42).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__arian_reading, theater_ratio, 360, 0.45).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t318, homoousios_christology__arian_reading, base_extractiveness, 318, 0.35).
narrative_ontology:measurement(homo_be_t325, homoousios_christology__arian_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement(homo_be_t337, homoousios_christology__arian_reading, base_extractiveness, 337, 0.55).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__arian_reading, base_extractiveness, 350, 0.6).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__arian_reading, base_extractiveness, 360, 0.68).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t318, homoousios_christology__arian_reading, suppression_requirement, 318, 0.3).
narrative_ontology:measurement(homo_su_t325, homoousios_christology__arian_reading, suppression_requirement, 325, 0.5).
narrative_ontology:measurement(homo_su_t337, homoousios_christology__arian_reading, suppression_requirement, 337, 0.6).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__arian_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__arian_reading, suppression_requirement, 360, 0.75).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the homoousios_christology kernel. pro_nicene_reading, semi_arian_reading, and this arian_reading share the same contested textual/creedal kernel (the fourth-century dispute over the Son's relation to the Father's ousia) but instantiate structurally distinct constraints: each has its own institutional high-water mark, its own beneficiary/victim set drawn from when that reading held real ecclesiastical-political power, and its own epsilon. The arian_reading forecloses the pro_nicene_reading within a single confessional framework (a see cannot simultaneously confess both identical and non-identical substance) while coexisting with semi_arian_reading, which occupies a live compromise position (homoiousios) that some factions treated as reconcilable with subordinationist concerns. Do not average epsilon across the three readings; each file stands as its own measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
