% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Eius Religio Territorial Settlement
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the political realignment reading of the
 *   Reformation: emerging territorial states in the Holy Roman Empire and
 *   Scandinavia used religious differentiation (Lutheranism, Calvinism) to
 *   assert full sovereignty against the layered authority of the Habsburg
 *   Emperor and the Papacy. The observable is the cuius regio eius religio
 *   principle codified at Augsburg (1555) and confirmed at Westphalia (1648).
 *   The constraint operates as a tangled rope: it coordinates legitimate
 *   authority within territories (genuine coordination function for rulers
 *   and bureaucracies) while extracting jurisdiction, property, and obedience
 *   from imperial/papal structures and nonconforming populations (asymmetric
 *   extraction). Active enforcement is required — visitation articles,
 *   consistory courts, expulsion orders, and ultimately the Thirty Years'
 *   War.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Eius Religio Territorial Settlement").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'e2aa1717-444d-4ba2-81be-d8a33995285a').
narrative_ontology:cs_kernel_codification('e2aa1717-444d-4ba2-81be-d8a33995285a', formalized).
narrative_ontology:cs_authority_grounding('e2aa1717-444d-4ba2-81be-d8a33995285a', lineage).
narrative_ontology:cs_interpretation_layer_present('e2aa1717-444d-4ba2-81be-d8a33995285a').
narrative_ontology:cs_reading_relation('e2aa1717-444d-4ba2-81be-d8a33995285a', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2aa1717-444d-4ba2-81be-d8a33995285a', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('e2aa1717-444d-4ba2-81be-d8a33995285a', foundational, territorial_sovereignty_absorbs_ecclesiastical_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_sovereignty_absorbs_ecclesiastical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('e2aa1717-444d-4ba2-81be-d8a33995285a', territorial_sovereignty_absorbs_ecclesiastical_jurisdiction, conventional).
narrative_ontology:cs_axiom('e2aa1717-444d-4ba2-81be-d8a33995285a', foundational, religious_uniformity_is_instrument_of_political_order).
narrative_ontology:cs_axiom_status(religious_uniformity_is_instrument_of_political_order, holdable).
narrative_ontology:cs_axiom_grounding('e2aa1717-444d-4ba2-81be-d8a33995285a', religious_uniformity_is_instrument_of_political_order, instrumental).
narrative_ontology:cs_reference_frame('e2aa1717-444d-4ba2-81be-d8a33995285a', imperial_universalist_christendom).
narrative_ontology:cs_drift_state('e2aa1717-444d-4ba2-81be-d8a33995285a', westphalian_settlement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e2aa1717-444d-4ba2-81be-d8a33995285a', '2026-08-20T14:32:11Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_state_bureaucracies).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, state_aligned_clergy).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, nonconforming_populations).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, transnational_religious_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German princes and Scandinavian monarchs adopt Lutheranism or Calvinism to seize church lands, control ecclesiastical appointments, and eliminate appeals to Rome. They enforce religious uniformity within their territories through visitation articles, consistory courts, and militia. The arrangement pays them directly in confiscated assets and indirectly in consolidated legislative authority.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_rulers, beneficiary).

% Chanceries, consistories, and revenue offices expand their competence as they absorb ecclesiastical jurisdiction: marriage, inheritance, poor relief, censorship, education. Officials gain career ladders and patronage networks anchored in the new state church. Their exit is constrained because their professional identity fuses with the confessional state they administer.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_state_bureaucracies, beneficiary,
    organized, generational, constrained, regional).

% Pastors and superintendents who accept the territorial settlement receive stipends, legal protection, and social status as agents of the godly commonwealth. Their theological vocation becomes inseparable from their office in the state church; leaving means abandoning their calling and community. They police doctrinal boundaries from the pulpit and the visitation protocol.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, state_aligned_clergy, beneficiary,
    moderate, biographical, identity_locked, local).

% The Habsburg Emperor loses the capacity to enforce religious uniformity across the Empire. The Peace of Augsburg (1555) and Westphalia (1648) formalize the fracture: imperial courts can no longer compel Protestant territories to restore Catholic practice. The institution persists but its religio-political reach is permanently truncated.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, generational, constrained, continental).

% Rome loses direct jurisdiction over half of Western Christendom. The papacy retains spiritual claims but cannot enforce them in Protestant territories; nuncios are expelled, appeals are forbidden, revenues cease. The Curia adapts by hardening doctrinal definitions (Trent) and cultivating Catholic powers, but the structural loss is irreversible within the Westphalian order.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Anabaptists, spiritualists, Jews, and crypto-Catholics in Protestant lands — or Protestants in Catholic lands — face exile, imprisonment, or execution. The cuius regio principle grants them no legal personality; their only exits are flight (leaving property and kinship behind) or conformity. The constraint extracts their labor, taxes, and obedience while denying them the coordination benefit of recognized worship.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, nonconforming_populations, payer,
    powerless, biographical, trapped, local).

% Jesuits, Franciscans, Dominicans, and other orders lose monasteries, schools, and mission fields in Protestant territories. Their supranational governance model collides with territorial sovereignty; they survive by retreating to Catholic lands and becoming agents of the Counter-Reformation, but their universalist structure is permanently damaged.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, transnational_religious_orders, payer,
    organized, generational, constrained, continental).

% Analyze the Reformation through competing lenses: confessional, political, economic, technological. This reading emphasizes the political realignment lens; the scholar sees the full structure but does not occupy a seat within it.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate authority in fragmented polities: territorial rulers need a unified legal and moral order to tax, legislate, and command obedience without competing ecclesiastical jurisdiction. The state church provides a single hierarchy of courts, a single catechism, and a single loyalty oath.
% TRANSFER_FUNCTION: Moves ecclesiastical property (lands, tithes, patronage), jurisdictional authority (courts, appointments, censorship), and loyalty obligations from imperial/papal structures to territorial state apparatuses. The rulers and their bureaucracies receive; the old authorities and nonconforming populations lose.
% ABSENT_VOICES: Peasant communities whose traditional festivals, pilgrimage routes, and parish autonomy were dismantled by visitation articles; women whose lay religious associations (beguinages, confraternities) were suppressed as superstitious; merchants whose cross-confessional trade networks were severed by confessional borders. They were not represented at Augsburg or Westphalia.
% DISAPPEARANCE_RATIONALE: If the territorial settlement vanished, the legal basis for state churches, confessional education systems, and the Westphalian sovereignty order would dissolve. Property titles derived from secularized church lands would be contested. The map of Europe would require renegotiation.
% FOUNDING_PROBLEM: The Holy Roman Empire's layered sovereignty — emperor, estates, pope, councils — produced chronic jurisdictional conflict, fiscal leakage to Rome, and legislative paralysis. Territorial rulers needed a unified authority structure to extract resources and enforce order within defined borders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — fragmented imperial sovereignty blocking state-building — is attested as resolved by the very existence of the Westphalian system (sovereign territorial states with recognized borders). Non-beneficiary corroboration: diplomatic historians of the Westphalian order (e.g., Croxton, Osiander) and political theorists of sovereignty (e.g., Bodin's successors) confirm the imperial fragmentation problem was solved by the territorial state, not by the religious settlement per se. The religious settlement was the instrument; the political problem is dead.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the transfer of ecclesiastical property and jurisdiction to territorial states was massive and permanent. Suppression (0.72) is high because the settlement's persistence depended on active coercion: nonconformists were expelled or executed, rival jurisdictions were suppressed by force, and the Peace of Augsburg required military enforcement. Theater ratio (0.42) reflects that the coordination function (unified legal/moral order) was real but increasingly performed — by 1648 the confessional state's religious legitimation was becoming ceremonial as raison d'état took over. Accessibility collapse (0.48) is moderate: alternative arrangements (confessional coexistence, imperial reform) existed but were foreclosed by the settlement's logic. Resistance (0.58) is significant: the Empire resisted for 130 years, nonconformists resisted continuously, and the papacy never accepted the loss.
 *
 * PERSPECTIVAL GAP:
 *   The ruler/bureaucracy seat experiences this as genuine coordination solving the fragmentation problem — the constraint is a rope from their position. The nonconforming population seat experiences it as a snare — pure extraction with no coordination benefit. The imperial/papal seats experience it as a tangled rope — they lose coordination capacity (imperial courts, papal jurisdiction) but the extraction is asymmetric and enforced. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers are agenda_setters and primary beneficiaries (d ~ 0.15): they set the rules, collect the assets, and face arbitrage-grade exit (they could and did switch confessions for political advantage). State bureaucracies and aligned clergy are beneficiaries with constrained/identity_locked exit (d ~ 0.3-0.4): they gain status and career but cannot leave without losing their professional identity. Imperial and papal authorities are victims with constrained/trapped exit (d ~ 0.8-0.9): they bear the extraction but cannot exit the system that extracts from them. Nonconforming populations are trapped victims (d ~ 0.95): no legal personality, no exit but flight or conformity. Transnational orders are constrained victims (d ~ 0.7): they retreat but adapt.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial fragmentation blocking state-building) is dead — Westphalia solved it. The arrangement persists because the confessional state became the template for modern sovereignty, not because the religious coordination function remains necessary. This is mandatrophy: the mandate (religious uniformity as political glue) outlived its function, but the constraint (territorial confessional settlement) persisted by becoming the foundation of the international order. The theater ratio rise after 1555 tracks this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_ambiguity,
    'Where does the political realignment reading end and the theological fragmentation reading begin? Can cuius regio be separated from the doctrinal commitments that made it intelligible to contemporaries?',
    'Comparative analysis of territorial reforms: where rulers adopted Reformation without doctrinal conviction (e.g., Henry VIII, some German princes switching for alliance), vs. where doctrinal conviction drove political action (e.g., Elector Frederick the Wise, Calvinist nobles in France). The boundary is empirical but contested.',
    'If the readings are inseparable, the kernel is a single constraint with observer-dependent ε. If separable, the political reading has lower ε (coordination dominant) and the theological reading has higher ε (extraction dominant via doctrinal enforcement). This story assumes separability per the kernel decomposition protocol.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the political and theological readings are structurally distinct constraints or observer perspectives on one constraint.').

omega_variable(
    extraction_measurement_referent,
    'Does ε = 0.68 measure extraction from imperial/papal authority (the standing arrangement under contest) or from nonconforming populations? The reading''s referent is the territorial settlement, but its victims are heterogeneous.',
    'Decompose the constraint: the settlement''s extraction from imperial/papal structures (jurisdiction, revenue) vs. its extraction from nonconformists (liberty, property, life). The former is coordination-adjacent; the latter is pure snare. This story authors a single ε for the composite; an omega documents the internal heterogeneity.',
    'If extraction from nonconformists dominates, the constraint trends toward snare. If extraction from old authorities dominates, tangled_rope holds. The measurement series shows extraction peaking at 1618 (war) and falling slightly at 1648 (formalization) — consistent with war extraction being the driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_referent, empirical, 'Which victim group''s extraction drives the ε value, and whether the composite obscures a snare component.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (political_realignment_reading) of the contested kernel reformation_composite. Sibling readings: theological_fragmentation_reading, technological_mediation_reading. What structural elements differ across readings?',
    'The engine will compare this story''s ε, beneficiary/victim sets, and cs_structure against sibling stories when they are generated. The kernel''s contestation is mapped by the divergence in these authored fields.',
    'If sibling readings produce substantially different ε values or beneficiary/victim structures, the kernel decomposition is validated. If they converge, the kernel may be a false composite. This omega routes the committer-frame metadata through the omega system per Rule 2.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commiter-frame metadata: kernel_id=reformation_composite, reading_id=political_realignment_reading, siblings=theological_fragmentation_reading,technological_mediation_reading').

omega_variable(
    suppression_mechanism_heterogeneity,
    'The suppression metric (0.72) aggregates structural coercion (laws, courts, armies) and internalized conformity (catechesis, social pressure, identity fusion). For nonconforming populations, how much suppression is carried internally after the structural threat recedes?',
    'Longitudinal study of post-Reformation conformity: where state enforcement relaxed (e.g., post-1648, Enlightenment era), did nonconformity re-emerge or stay suppressed? Persistence suggests internalization.',
    'If internalized suppression is high, the constraint''s effective suppression exceeds the structural measure — the target carries the constraint after exit. This would raise the computed χ for nonconforming populations beyond what structural suppression alone predicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_heterogeneity, empirical, 'Structural vs. internalized suppression for nonconforming populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_pol_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(reformation_pol_tr_t1525, reformation_composite__political_realignment_reading, theater_ratio, 1525, 0.25).
narrative_ontology:measurement(reformation_pol_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.35).
narrative_ontology:measurement(reformation_pol_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.4).
narrative_ontology:measurement(reformation_pol_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.45).
narrative_ontology:measurement(reformation_pol_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.42).

% Extraction over time
narrative_ontology:measurement(reformation_pol_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(reformation_pol_be_t1525, reformation_composite__political_realignment_reading, base_extractiveness, 1525, 0.35).
narrative_ontology:measurement(reformation_pol_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement(reformation_pol_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement(reformation_pol_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.7).
narrative_ontology:measurement(reformation_pol_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(reformation_pol_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(reformation_pol_su_t1525, reformation_composite__political_realignment_reading, suppression_requirement, 1525, 0.55).
narrative_ontology:measurement(reformation_pol_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.65).
narrative_ontology:measurement(reformation_pol_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement(reformation_pol_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.78).
narrative_ontology:measurement(reformation_pol_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty_order).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, confessional_state_formation).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, counter_reformation_institutionalization).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three constraint stories linked by mutual affects_constraints. This reading (political_realignment) emphasizes territorial sovereignty and cuius regio as the primary observable; the theological reading emphasizes doctrinal incompatibility and denominational boundary enforcement; the technological reading emphasizes print-mediated network effects. Each has distinct ε, beneficiaries, and victims. The political reading's ε (0.68) is lower than the theological reading's expected ε (doctrinal enforcement is more extractive) but higher than the technological reading's expected ε (print infrastructure has lower inherent extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.15).
constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, organized, 0.35).
constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, moderate, 0.4).
constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
