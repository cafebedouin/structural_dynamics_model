% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Settlement (Enforced Confessional Regime)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE, interval point 0) defined the Son as
 *   homoousios with the Father and anathematized contrary formulas; the
 *   following half-century (interval runs to 381 CE, point 56, the Council of
 *   Constantinople) was spent imposing, losing, and re-imposing that
 *   definition through imperial law, episcopal deposition, and crowd
 *   violence. The arrangement solves a real coordination problem — one
 *   baptismal confession across autonomous sees — while simultaneously
 *   transferring office, property, and legality from dissenters to
 *   conformists, sustained by continuous enforcement. Claim and metrics are
 *   authored independently: the claimed type (tangled_rope) states my
 *   structural judgment that both coordination and asymmetric extraction are
 *   present; the metrics describe the arrangement's actual operation,
 *   including its mid-century collapse and Theodosian re-ratchet. Committer
 *   structure (kernel, reading, siblings) is routed to the omega variables
 *   and kernel_context per the committer-frame rules.
 *
 * KEY AGENTS:
 *   - roman_imperial_administration: agenda-setter (institutional/arbitrage) — convokes, ratifies, bans; converts doctrine into law and can redefine it
 *   - nicene_episcopal_hierarchy: primary beneficiary (institutional/constrained) — holds sees and teaching authority under the formula; drafted and polices it
 *   - monastic_ascetic_movement: secondary beneficiary (moderate/identity_locked) — supplies mobilization; identity fused with orthodoxy defense
 *   - arian_clergy: primary target (moderate/trapped) — deposed, exiled, legally disabled
 *   - eastern_anti_nicene_congregations: target (organized/constrained) — lose buildings, alms networks, legal standing
 *   - homoiousian_bishops: target with formula-mobility (moderate/mobile) — the negotiable middle, ultimately absorbed
 *   - hellenic_philosophical_establishment: excluded voice (powerful/mobile) — observes as outsider, barred from the process by definition
 *   - modern_historical_theologians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.72).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.82).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Settlement (Enforced Confessional Regime)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'f702b0d1-e359-418d-af61-d50a8d9b8abf').
narrative_ontology:cs_kernel_codification('f702b0d1-e359-418d-af61-d50a8d9b8abf', formalized).
narrative_ontology:cs_authority_grounding('f702b0d1-e359-418d-af61-d50a8d9b8abf', lineage).
narrative_ontology:cs_interpretation_layer_present('f702b0d1-e359-418d-af61-d50a8d9b8abf').
narrative_ontology:cs_reading_relation('f702b0d1-e359-418d-af61-d50a8d9b8abf', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('f702b0d1-e359-418d-af61-d50a8d9b8abf', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('f702b0d1-e359-418d-af61-d50a8d9b8abf', foundational, son_homoousios_with_father).
narrative_ontology:cs_axiom_status(son_homoousios_with_father, holdable).
narrative_ontology:cs_axiom_grounding('f702b0d1-e359-418d-af61-d50a8d9b8abf', son_homoousios_with_father, theological).
narrative_ontology:cs_axiom('f702b0d1-e359-418d-af61-d50a8d9b8abf', foundational, conciliar_formula_universally_binding).
narrative_ontology:cs_axiom_status(conciliar_formula_universally_binding, holdable).
narrative_ontology:cs_axiom_grounding('f702b0d1-e359-418d-af61-d50a8d9b8abf', conciliar_formula_universally_binding, conventional).
narrative_ontology:cs_reference_frame('f702b0d1-e359-418d-af61-d50a8d9b8abf', apostolic_consubstantial_confession).
narrative_ontology:cs_drift_state('f702b0d1-e359-418d-af61-d50a8d9b8abf', post_constantinopolitan_settlement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f702b0d1-e359-418d-af61-d50a8d9b8abf', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, roman_imperial_administration).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, monastic_ascetic_movement).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, eastern_anti_nicene_congregations).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, homoiousian_bishops).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, ecumenical_conciliar_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convokes councils, ratifies or discards their formulas, and converts doctrinal settlement into law (banishment rescripts, the Edict of Thessalonica's penalties on dissenting assemblies). Collects administrative uniformity, episcopal loyalty, and a single religious interlocutor for provincial governance. Because successive emperors redefined the settlement at will (Constantius's formula-shopping, Theodosius's ratchet), its relationship to the formula is instrumental rather than confessional.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, roman_imperial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Holds sees, teaching authority, and ordination power under the formula; receives transferred allegiance, confiscated properties, and the legal standing that dispossessed rivals lose. Drafted the definition at Nicaea and staffed the commissions that policed it. Its position inside the arrangement is conditional on imperial favor — during the Homoian ascendancy many of its members were themselves deposed and exiled — and resignation from the episcopate ends the vocation outright.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, agenda_setter).

% Supplies mass mobilization and moral legitimacy for the settlement (crowds that shielded Athanasius, ascetics whose endorsement certified orthodoxy to urban populations). Gains standing and protection through the alliance; its prestige is fused with defense of the confession, so withdrawing support would dissolve the movement's self-concept rather than merely change its politics.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, monastic_ascetic_movement, beneficiary,
    moderate, biographical, identity_locked, regional).

% Presbyters, deacons, and bishops teaching that the Son is a creature ('there was once when he was not'). Deposed from sees, exiled by rescript, their writings proscribed; after 380 legally barred from assembling in cities. Exit means recantation of the teaching that constitutes their clerical identity, or permanent exile outside the law's protection.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Urban Christian communities in Alexandria, Constantinople, and Antioch whose preferred communion loses buildings, alms networks, and legal standing as the settlement hardens. Their dissent registers as riot and street conflict rather than representation in the councils that define them. Leaving means joining the winning communion against conscience or schism outside the law.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, eastern_anti_nicene_congregations, payer,
    organized, biographical, constrained, regional).

% The large middle party seeking a similarity formula (homoiousios) that preserves the Father's monarchy without reducing the Son to a creature. Repeatedly renegotiated formulas as imperial winds shifted, holding councils and drafting creeds of their own; the anathema's reach covers their position as insufficient. Their exit is formula-adjustment — most were absorbed into the Nicene camp by 381 — at the cost of abandoning their distinguishing claim.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, homoiousian_bishops, payer,
    moderate, biographical, mobile, continental).

% Pagan philosophers and civic elites observing the dispute as internecine Christian strife over unintelligible subtleties. Barred from the conciliar process by definition — the councils define the church's faith, and they are outside it. Would argue the dispute demonstrates the question is unresolvable and politically corrosive; their voice enters the record mainly as outside commentary during the Julian interlude.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, hellenic_philosophical_establishment, excluded,
    powerful, biographical, mobile, continental).

% Reconstruct the settlement from conciliar acts, exile correspondence, sermons, and imperial rescripts; classify its enforcement patterns and distinguish doctrinal content from coercive vehicle. Neither collect from nor pay into the arrangement.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, modern_historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one binding answer to how the baptized threefold name names one God: a shared confession aligning liturgy, baptismal recognition, and episcopal communion across hundreds of autonomous sees, and supplying the empire a single criterion for which clergy count as legitimate interlocutors.
% TRANSFER_FUNCTION: Moves ecclesiastical office, church buildings, alms networks, and communal allegiance from clergy and congregations that will not confess homoousios to those that will; moves imperial coercive capacity (exile, confiscation, legal disability) into the service of doctrinal conformity; moves definitional authority upward to conciliar pronouncement.
% ABSENT_VOICES: The anathematized: Arius (dead 336, reconciled only on paper), the Homoian clergy of the Danubian military establishment, and the Homoiousian middle party enter the settlements chiefly as objects of definition rather than as authors. Non-Christian civic elites watch from wholly outside the process. Ordinary laity are addressed by the formulas rather than consulted; their dissent appears as riot, not testimony.
% DISAPPEARANCE_RATIONALE: Overnight removal in 381 reopens the question the settlement closed: sees realign around competing formulas, the Edict of Thessalonica's legal architecture loses its referent, confiscated properties and jurisdictions are disputed anew, and the imperial-church alignment that shaped the subsequent constitution of Christendom takes a different path.
% FOUNDING_PROBLEM: Whether the Son is divine in the same sense as the Father or a creature — forced by Arius's teaching against Bishop Alexander of Alexandria, threatening on one side the unity of God and on the other the divinity that baptismal practice presupposes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the anathematized parties themselves treated the question as real and urgent — Arius's Thalia and Eusebius of Nicomedia's petitions dispute the answer, not the question's seriousness — and contemporaries outside the dispute (Ammianus Marcellinus's baffled accounts of Constantinopolitan doctrinal riots) attest that the conflict was genuine. Modern patristic historiography confirms the pre-Nicene church lacked a settled account of the Son's relation to the Father.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the settlement's operation stripped identifiable parties of office, property, and legal existence while concentrating those goods on conformists; suppression is higher (0.82) because persistence depended on exile rescripts, assembly bans, and anathema rather than voluntary uptake. Theater is low (0.16): the liturgical and catechetical function is real throughout, and the Cappadocian-era theological work was substantive, though the formula-shopping era (points 16-24) shows a genuine performative peak as bishops signed serially contradictory creeds. Accessibility_collapse is moderate-low (0.45): alternatives never fully collapsed — Homoian and Homoiousian positions remained live for decades and Arianism flourished beyond the frontier. Resistance is high (0.78): fifty years of depositions, riots, and negotiated counter-creeds. The series oscillate rather than drift monotonically: enforcement capacity tracked imperial succession (Constantine's initial ratchet, Constantius's capture of the machinery by the constraint's opponents, the Theodosian re-ratchet). The mid-interval dip in suppression and extraction is enforcement capture, not constraint modification — the arrangement's holders were deposed and the machinery inverted against them. The oscillation is exogenous (imperial succession), not intermittent reinforcement, but each re-imposition landed harder than the last (suppression endpoints 0.70 rising to 0.82), a ratchet pattern. All three metric series share one time grid (points 0-56 at intervals of 8); end-state values match the base_properties snapshot.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal beneficiary seat the anathema is necessary boundary maintenance protecting a coherent baptismal confession; from the arian_clergy seat the same sentence is career destruction and legal death; from the imperial seat it is an instrument of provincial administration, adopted or discarded as governance requires. The engine computes divergent per-seat classifications from these structural positions; the divergence — not any single seat's verdict — is the finding this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (episcopal hierarchy, imperial administration, ascetic movement) derive low d — the arrangement subsidizes them with office, uniformity, and standing. The ascetic movement's identity_locked exit pins it near the beneficiary pole and makes its support enforcement-free. Payers derive high d: trapped exit places arian_clergy nearest the full-target end; eastern_anti_nicene_congregations sit close behind with constrained exit; homoiousian_bishops' formula-mobility moderates their d somewhat, though the anathema's reach covers them regardless. Suppression is authored raw and unscaled; only extractiveness is scaled by directionality and scope — the settlement's continental scope amplifies effective extraction on the target seats because verification of conformity across hundreds of sees requires the enforcement machinery itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live and the arrangement's disappearance would rearrange the world, so no zombie flag arises. The tangled_rope claim prevents two mislabels: reading the settlement as pure snare ignores the real coordination good — a single confession enabling mutual recognition of baptism and ministry across autonomous sees — which outlived the coercion that carried it; reading it as pure rope ignores that the same structure deposed, exiled, and disinherited identifiable parties and required continuous imperial enforcement to hold. Mandatrophy here is not obsolescence but consolidation: the mandate (definitional closure) succeeded, and enforcement hardened around the victory rather than decaying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the pro_nicene_reading of kernel homoousios_christology: what structural differences follow if the arian_reading or semi_arian_reading were instantiated instead?',
    'Author the sibling stories separately (each with its own epsilon, beneficiary/victim sets, and enforcement profile) and compare computed classifications across the kernel family; do not average readings inside one story.',
    'Under the arian_reading the victim/beneficiary sets invert (Nicene hierarchs become the suppressed party during the mid-century Homoian ascendancy) and the enforcement center relocates; under the semi_arian_reading suppression drops (the middle position is negotiable) and the arrangement trends toward rope. Cross-reading comparison, not intra-story hedging, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one kernel, three readings; this file is the pro-Nicene instantiation only.').

omega_variable(
    doctrine_vs_enforcement_separability,
    'Is the measured extraction a property of the homoousios confession itself or of the imperial enforcement vehicle carrying it?',
    'Compare enforced and unenforced phases within the interval (the mid-century Homoian ascendancy, when the constraint''s holders were deposed rather than deposing): if extraction tracks enforcement capacity rather than doctrinal content, the vehicle carries the extraction.',
    'If separable, the confession could operate as a low-extraction rope under non-coercive conditions and the tangled_rope verdict indicts the vehicle; if inseparable, boundary-definition of this kind structurally requires exclusion and the extraction is internal to the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_enforcement_separability, empirical, 'Whether epsilon attaches to the doctrine''s content or to its imperial carrier.').

omega_variable(
    coerced_assent_sincerity,
    'How much of the measured compliance with the formula reflects conviction rather than duress, given bishops who signed successive and mutually contradictory creeds as imperial favor shifted?',
    'Track individual signatories across formula changes (serial signers versus consistent refusers); correlate signatures with the physical presence and disposition of the reigning emperor at each council.',
    'A high duress share inflates apparent consent, depresses true resistance, and predicts collapse under enforcement withdrawal; a high sincerity share supports treating the settlement as durable coordination rather than imposed compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_assent_sincerity, empirical, 'Sincerity composition of measured assent to the formula.').

omega_variable(
    counterfactual_fragmentation_need,
    'Would the church have fragmented irreparably absent a binding metaphysical formula — that is, how much of the measured extraction is the price of a coordination good that only coercion could supply?',
    'Comparative analysis of non-coercively coordinated doctrinal communities (pre-Constantinian pluralism, later failed conciliar settlements) and of the settlement''s durability after enforcement withdrawal in regions beyond imperial reach.',
    'If fragmentation was the realistic counterfactual, part of the extraction is coordination cost and the tangled_rope reading strengthens; if pluralist coexistence was viable, the enforcement was a choice rather than a necessity and the snare component grows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_fragmentation_need, conceptual, 'Counterfactual necessity of a coercively enforced formula for church-wide cohesion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_pronicene_tr_t0, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t0, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t8, homoousios_christology__pro_nicene_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t8, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t16, homoousios_christology__pro_nicene_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t16, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t24, homoousios_christology__pro_nicene_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t24, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t32, homoousios_christology__pro_nicene_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t32, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t40, homoousios_christology__pro_nicene_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t40, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t48, homoousios_christology__pro_nicene_reading, theater_ratio, 48, 0.18).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t48, observed).
narrative_ontology:measurement(homoousios_pronicene_tr_t56, homoousios_christology__pro_nicene_reading, theater_ratio, 56, 0.16).
narrative_ontology:measurement_basis(homoousios_pronicene_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(homoousios_pronicene_be_t0, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t0, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t8, homoousios_christology__pro_nicene_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t8, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t16, homoousios_christology__pro_nicene_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t16, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t24, homoousios_christology__pro_nicene_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t24, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t32, homoousios_christology__pro_nicene_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t32, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t40, homoousios_christology__pro_nicene_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t40, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t48, homoousios_christology__pro_nicene_reading, base_extractiveness, 48, 0.69).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t48, observed).
narrative_ontology:measurement(homoousios_pronicene_be_t56, homoousios_christology__pro_nicene_reading, base_extractiveness, 56, 0.72).
narrative_ontology:measurement_basis(homoousios_pronicene_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_pronicene_su_t0, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t0, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t8, homoousios_christology__pro_nicene_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t8, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t16, homoousios_christology__pro_nicene_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t16, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t24, homoousios_christology__pro_nicene_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t24, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t32, homoousios_christology__pro_nicene_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t32, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t40, homoousios_christology__pro_nicene_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t40, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t48, homoousios_christology__pro_nicene_reading, suppression_requirement, 48, 0.78).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t48, observed).
narrative_ontology:measurement(homoousios_pronicene_su_t56, homoousios_christology__pro_nicene_reading, suppression_requirement, 56, 0.82).
narrative_ontology:measurement_basis(homoousios_pronicene_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__semi_arian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (homoousios_christology), three readings emitted as separate stories per the epsilon-invariance principle — the colloquial label 'the Nicene question' conflates structurally distinct arrangements with distinct victim sets and enforcement profiles. This member (highest enforcement, imperial-church alignment) links to both siblings; the arian_reading is downstream-contested, its holders becoming the persecuted party after 380. Each story carries its own epsilon, beneficiaries, and victims; nothing is averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
