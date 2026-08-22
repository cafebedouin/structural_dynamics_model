% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Asset Seizure (Political Swap Reading)
 *   domain: historical/religious/political
 *
 * SUMMARY:
 *   This reading frames the Reformation as a political swap: secular rulers
 *   (German princes, imperial estates, city councils) exploited theological
 *   disputes to break papal authority and seize church lands, revenues, and
 *   jurisdictional rights. Theology served as post-hoc rationalization and
 *   mobilization tool — a scaffold for power consolidation that partially
 *   sunset once political settlement stabilized at Westphalia (1648). The
 *   Catholic Church (papal states, monastic institutions, dispossessed
 *   clergy) is the primary victim of asset seizure and authority transfer.
 *   The constraint is the standing arrangement of transferred ecclesiastical
 *   sovereignty and property, assessed by this reading's lights: high
 *   extraction (0.78) from the Church to secular rulers, active suppression
 *   of Catholic restoration attempts, and moderate theater as theological
 *   justification persisted beyond its instrumental utility.
 *
 * KEY AGENTS:
 *   - secular_princes: Primary beneficiaries (institutional/powerful) — seized ecclesiastical territories, revenues, and episcopal appointments
 *   - imperial_estates: Beneficiaries (organized/powerful) — gained Reichsstand status and church property through secularization
 *   - city_councils_reformed: Beneficiaries (organized/moderate) — seized monastic property and assumed ecclesiastical jurisdiction
 *   - papal_states: Primary victims (institutional/powerless) — lost territorial sovereignty and revenue streams in Germany
 *   - monastic_institutions: Victims (organized/powerless) — dissolved, assets confiscated across Protestant territories
 *   - catholic_clergy_dispossessed: Victims (moderate/powerless) — lost benefices, livelihoods, legal protections
 *   - imperial_free_cities_catholic: Victims (organized/moderate) — pressured to conform or lose status
 *   - theologians_reformers: Excluded from this reading's beneficiary structure — treated as instrumental agents, not independent drivers
 *   - historians_composite: Observers (analytical) — see overdetermined causality this reading reduces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.72).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Asset Seizure (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/religious/political").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '7158b70c-9c97-4e81-b11a-c831bd15b1b0').
narrative_ontology:cs_kernel_codification('7158b70c-9c97-4e81-b11a-c831bd15b1b0', formalized).
narrative_ontology:cs_authority_grounding('7158b70c-9c97-4e81-b11a-c831bd15b1b0', lineage).
narrative_ontology:cs_interpretation_layer_present('7158b70c-9c97-4e81-b11a-c831bd15b1b0').
narrative_ontology:cs_reading_relation('7158b70c-9c97-4e81-b11a-c831bd15b1b0', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7158b70c-9c97-4e81-b11a-c831bd15b1b0', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('7158b70c-9c97-4e81-b11a-c831bd15b1b0', foundational, reformation_primarily_political_asset_transfer).
narrative_ontology:cs_axiom_status(reformation_primarily_political_asset_transfer, holdable).
narrative_ontology:cs_axiom_grounding('7158b70c-9c97-4e81-b11a-c831bd15b1b0', reformation_primarily_political_asset_transfer, empirically_contingent).
narrative_ontology:cs_axiom('7158b70c-9c97-4e81-b11a-c831bd15b1b0', foundational, theology_as_instrumental_scaffold_for_power_consolidation).
narrative_ontology:cs_axiom_status(theology_as_instrumental_scaffold_for_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('7158b70c-9c97-4e81-b11a-c831bd15b1b0', theology_as_instrumental_scaffold_for_power_consolidation, empirically_contingent).
narrative_ontology:cs_axiom('7158b70c-9c97-4e81-b11a-c831bd15b1b0', secondary, westphalia_as_definitive_settlement_stabilization).
narrative_ontology:cs_axiom_status(westphalia_as_definitive_settlement_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('7158b70c-9c97-4e81-b11a-c831bd15b1b0', westphalia_as_definitive_settlement_stabilization, conventional).
narrative_ontology:cs_reference_frame('7158b70c-9c97-4e81-b11a-c831bd15b1b0', imperial_ecclesiastical_order_pre_1517).
narrative_ontology:cs_drift_state('7158b70c-9c97-4e81-b11a-c831bd15b1b0', westphalian_settlement_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7158b70c-9c97-4e81-b11a-c831bd15b1b0', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, imperial_estates).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, city_councils_reformed).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_states).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_institutions).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_clergy_dispossessed).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, imperial_free_cities_catholic).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, secular_sovereignty_over_ecclesiastical_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German territorial princes (electors, dukes, margraves) who seized episcopal territories, monastic lands, and church revenues. They enforced cuius regio eius religio to consolidate sovereignty. Exit options were high — they could shift confessions for political advantage (e.g., Brandenburg 1613, Palatinate shifts). Gains were direct and massive: ~40% of ecclesiastical territory secularized.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_princes, beneficiary,
    powerful, generational, arbitrage, regional).

% Imperial knights, counts, and prelates who gained Reichsstand status and church property through secularization. They operated within the Imperial legal framework (Reichstag, Kreis structures) but used Reformation to expand autonomy. Exit was constrained by Imperial law but mobile within the estate system.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, imperial_estates, beneficiary,
    organized, generational, mobile, regional).

% Urban magistrates (Nuremberg, Strasbourg, Ulm, etc.) who seized monastic property, assumed ecclesiastical jurisdiction, and redirected church revenues to civic uses. Gains were substantial but exit was constrained by Imperial politics and confessional alliances (Schmalkaldic League).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, city_councils_reformed, beneficiary,
    organized, biographical, constrained, local).

% The Papacy's temporal sovereignty in Germany (episcopal territories, monastic networks, revenue streams from annates/pallia/Peter's Pence). Lost permanently — no exit, no recovery. The constraint extracted territorial sovereignty and fiscal rights through military and legal means.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_states, payer,
    powerless, generational, trapped, regional).

% Monasteries, convents, and cathedral chapters across German lands. Dissolved en masse; assets confiscated; members expelled or pensioned. No collective exit — the institution itself was the target. Individual monks/nuns had limited personal exit (secularization, migration) but the corporate entity was destroyed.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_institutions, payer,
    powerless, generational, trapped, local).

% Parish priests, canons, and bishops in territories that adopted Reformation. Lost benefices, legal protections, and livelihoods. Some migrated to Catholic territories; others conformed outwardly. Exit was individually possible but collectively catastrophic — the Catholic clerical structure in Protestant lands was dismantled.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_clergy_dispossessed, payer,
    moderate, biographical, constrained, regional).

% Free Imperial Cities that remained Catholic (e.g., Cologne, Aachen, Regensburg) but faced pressure to conform, lost influence in Imperial diets, and saw surrounding territories secularize. Exit was constrained by geography and Imperial politics — they could not relocate.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, imperial_free_cities_catholic, payer,
    moderate, biographical, constrained, local).

% Luther, Melanchthon, Bucer, Calvin, Zwingli and their networks. In this reading they are instrumental agents — their theological innovations were real but were mobilized by political actors. They did not capture the gains of asset seizure. Some gained patronage positions but exit was mobile (they moved between cities/territories).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theologians_reformers, excluded,
    moderate, biographical, mobile, regional).

% Scholars who reject single-driver narratives (Moeller, Oberman, Brady, Dixon). They see the Reformation as overdetermined: theological, political, social, and economic causes operating simultaneously. Their analysis is not captured by this reading's constraint but provides the comparative frame.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historians_composite, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solved the coordination problem of fragmented Imperial authority by transferring ecclesiastical sovereignty to secular rulers, creating a new legal framework (cuius regio) for confessional coexistence that stabilized at Westphalia. It coordinated the dissolution of a transnational ecclesiastical property system into territorial state assets.
% TRANSFER_FUNCTION: Transferred ecclesiastical lands, revenues (tithes, annates, monastic rents), jurisdictional rights (episcopal appointments, ecclesiastical courts), and legislative authority over religion from the Papacy and Catholic Church to secular princes, imperial estates, and city councils — roughly 40% of German ecclesiastical territory by 1648.
% ABSENT_VOICES: The Catholic clergy and laity in Protestant territories who had no representation in the secularization decisions; the monastic communities dissolved without consent; the papal curia excluded from Imperial legal processes after 1555. They would have objected to the legitimacy of the transfers but were structurally excluded from the negotiation (Reichstag, religious colloquies, Westphalia).
% DISAPPEARANCE_RATIONALE: If the political swap constraint vanished overnight (i.e., if the secularization of church property and transfer of ecclesiastical sovereignty were reversed), the territorial map of Central Europe would reorganize: ~40% of land would revert to ecclesiastical rule, the cuius regio principle would collapse, confessional boundaries would decouple from political borders, and the Westphalian sovereignty framework would lose its foundational settlement. The modern state system in Germany is built on this transfer.
% FOUNDING_PROBLEM: The Holy Roman Empire's fragmented authority structure allowed the Papacy to extract fiscal resources (annates, pallia, Peter's Pence, reservation of benefices) and appoint foreign clerics to German benefices, while Imperial estates lacked full sovereignty over their territories. Secular rulers sought to seize these resources and jurisdictions for state-building.
% FOUNDING_PROBLEM_CORROBORATION: Imperial diet records (Reichsabschiede 1521-1555) document princely grievances against papal fiscal extraction. The Gravamina Nationis Germanicae (1522-1523) — a petition from German estates to the Diet — attests the founding problem from the beneficiaries' side. Catholic historians (Jedin, Hubbard) corroborate the fiscal grievances but contest the legitimacy of the seizure. The problem is dead: papal fiscal extraction from Germany ended permanently; the territorial sovereignty transfer was consolidated at Westphalia.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78 at 1648) reflects the massive, permanent transfer of ecclesiastical wealth and sovereignty to secular rulers — roughly 40% of German ecclesiastical territory secularized by 1648. Suppression (0.72) reflects active enforcement: military suppression of Catholic restoration (Schmalkaldic War, Thirty Years' War), legal exclusion of Catholic clergy from secularized territories, and the cuius regio principle denying exit to Catholic subjects. Theater ratio (0.45) captures the dual character: genuine theological coordination existed (Lutheran/Calvinist confessionalization) but was instrumentalized for political consolidation — the Peace of Augsburg (1555) and Westphalia (1648) formalized the political settlement while freezing confessional boundaries as political borders. Accessibility collapse (0.35) is moderate: alternative arrangements (imperial reform councils, conciliarism, Erasmian reform) were suppressed but not unthinkable. Resistance (0.68) is high: Catholic Counter-Reformation, Jesuit missions, Habsburg restoration attempts, and the Thirty Years' War itself constitute sustained resistance.
 *
 * PERSPECTIVAL GAP:
 *   From secular princes' seat (beneficiaries, powerful, arbitrage exit): the constraint is a successful coordination mechanism that solved the problem of fragmented imperial authority and unlocked resources for state-building — a rope/tangled_rope from their view. From the papal states and monastic institutions (victims, institutional/powerless, trapped exit): the constraint is a violent expropriation enforced by military power and legal fiction — a snare. From city councils (beneficiaries, organized, constrained exit): mixed — gained autonomy and assets but locked into confessional politics. The engine computes this divergence from power/exit/role structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular princes and imperial estates are structural beneficiaries (d near 0.0-0.15): they collected the extraction (lands, revenues, appointments) and controlled enforcement. The papal states and monastic institutions are structural victims (d near 0.85-1.0): they bore the full extraction with no exit (territorial sovereignty cannot be moved). Catholic clergy and Catholic imperial cities are secondary victims (d near 0.7-0.85): some mobility for individuals but collective dispossession. Theologians are excluded from the beneficiary structure in this reading — their role was instrumental, not that of independent drivers capturing gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented imperial authority, papal fiscal extraction from Germany) was substantially resolved by 1555 (Augsburg) and definitively by 1648 (Westphalia). The arrangement persisted as a settled political order, not as an active extraction machine — but the asset transfer was irreversible. The mandate (breaking papal authority) was resolved; the extraction (seized assets) was consolidated. This is not classic mandatrophy (function atrophied but constraint persists) — it is a completed swap that became the new baseline. The constraint's type reflects the active enforcement phase (1517-1648); post-1648 it becomes the structural background of the Westphalian system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the political_swap_reading of the contested kernel reformation_event_boundary. What structural elements distinguish it from the theological_climb_reading and composite_overdetermination_reading?',
    'Comparative periodization: political_swap extends to 1648 (Westphalia) as settlement stabilization; theological_climb centers 1517-1555 (Augsburg); composite rejects single periodization. Cross-reading ε-invariance test: each reading must produce stable ε on its own referent.',
    'If readings share referent but author different ε, the kernel decomposition is validated. If ε drifts with observable choice within one reading, the reading itself conflates constraints and must split further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel identity and reading distinction for reformation_event_boundary').

omega_variable(
    theology_as_scaffold_or_independent,
    'Is the theological dimension of the Reformation a genuine coordination function (scaffold with sunset) or pure post-hoc rationalization (theater on a snare)?',
    'Trace doctrinal commitments that persisted after political settlement stabilized (post-1648). If core theological innovations survived political utility, they carried independent coordination weight. If they dissolved or were purely instrumental, the scaffold characterization holds.',
    'If theology was genuine scaffold, the constraint has a coordination function that partially sunset; if pure rationalization, extraction is higher and suppression is the primary persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_as_scaffold_or_independent, empirical, 'Whether theological commitments were independent coordination or instrumental cover').

omega_variable(
    asset_seizure_vs_secularization_legitimacy,
    'Did secular rulers seize church assets primarily through force/expropriation, or through negotiated secularization with legal frameworks that conferred legitimacy?',
    'Comparative analysis of Reichstag decrees, Peace of Augsburg (1555) provisions, and Westphalia (1648) treaties on ecclesiastical property. Examine whether transfers followed legal forms or were imposed by conquest.',
    'If primarily legalized secularization, the constraint has stronger coordination character (rope/tangled_rope); if primarily forcible seizure, it trends toward snare with higher suppression and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_seizure_vs_secularization_legitimacy, empirical, 'Mechanism of ecclesiastical asset transfer: legal vs. coercive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.25).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__political_swap_reading, theater_ratio, 1525, 0.35).
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__political_swap_reading, theater_ratio, 1535, 0.4).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.42).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.44).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__political_swap_reading, base_extractiveness, 1525, 0.52).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__political_swap_reading, base_extractiveness, 1535, 0.65).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.72).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.75).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__political_swap_reading, suppression_requirement, 1525, 0.55).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__political_swap_reading, suppression_requirement, 1535, 0.65).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.72).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalian_sovereignty_settlement).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, confessionalization_state_formation).

% DUAL FORMULATION NOTE:
% This reading (political_swap) and theological_climb_reading are dual formulations of the same kernel. Political_swap centers authority transfer and asset seizure (high ε, victims = Church); theological_climb centers doctrinal innovation (lower ε, beneficiaries = reformers/communities). They differ in referent periodization, beneficiary/victim structure, and what counts as the constraint's coordination function. The composite reading rejects the duality as false.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, institutional, 0.12).
constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
