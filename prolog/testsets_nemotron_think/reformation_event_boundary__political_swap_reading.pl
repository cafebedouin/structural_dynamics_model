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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Reformation as Political Swap: Secular Seizure of Papal Authority and Assets
 *   domain: historical/religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates the political_swap_reading of the
 *   Reformation kernel: the Reformation was primarily a political realignment
 *   in which secular rulers exploited theological disputes to break papal
 *   authority and seize church assets, with theology functioning as post-hoc
 *   rationalization. The constraint is the historical mechanism of authority
 *   transfer and asset seizure (1517–1648), not the theological debates
 *   themselves. Beneficiaries are secular rulers (German princes,
 *   Scandinavian monarchs, English crown); victims are Catholic Church
 *   institutions (papacy, monastic orders, bishoprics). The coordination
 *   story (church reform) is cover; the extraction (land, revenue,
 *   jurisdiction) is the persistent function. Active enforcement via princely
 *   armies, imperial diets, and legal secularization maintained the transfer.
 *   Periodization extends to Westphalia (1648) when the political settlement
 *   stabilized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.72).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, snare).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Swap: Secular Seizure of Papal Authority and Assets").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/religious/political").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '7d442c93-7d7b-4028-a46d-86c3f1367535').
narrative_ontology:cs_kernel_codification('7d442c93-7d7b-4028-a46d-86c3f1367535', distributed).
narrative_ontology:cs_authority_grounding('7d442c93-7d7b-4028-a46d-86c3f1367535', distributed).
narrative_ontology:cs_reading_relation('7d442c93-7d7b-4028-a46d-86c3f1367535', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('7d442c93-7d7b-4028-a46d-86c3f1367535', reformation_event_boundary__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('7d442c93-7d7b-4028-a46d-86c3f1367535', foundational, secular_sovereignty_primary_driver).
narrative_ontology:cs_axiom_status(secular_sovereignty_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('7d442c93-7d7b-4028-a46d-86c3f1367535', secular_sovereignty_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('7d442c93-7d7b-4028-a46d-86c3f1367535', foundational, theology_as_instrumental_cover).
narrative_ontology:cs_axiom_status(theology_as_instrumental_cover, holdable).
narrative_ontology:cs_axiom_grounding('7d442c93-7d7b-4028-a46d-86c3f1367535', theology_as_instrumental_cover, empirically_contingent).
narrative_ontology:cs_reference_frame('7d442c93-7d7b-4028-a46d-86c3f1367535', political_swap_periodization).
narrative_ontology:cs_drift_state('7d442c93-7d7b-4028-a46d-86c3f1367535', post_revisionist_turn, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d442c93-7d7b-4028-a46d-86c3f1367535', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, german_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, scandinavian_monarchs).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, english_crown).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church_institutions).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papacy).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_orders).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, bishoprics_and_church_lands).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, protestant_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasant_populations).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_principle).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German princes, Scandinavian monarchs, and the English crown exploited theological dissent to seize church lands, appointments, and jurisdictional authority. They initiated and controlled the legal-political machinery of confiscation (e.g., dissolution of monasteries, secularization of bishoprics). Their exit from papal obedience was a strategic choice with high payoff; they faced no structural barriers to seizing assets once theological cover was available.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_rulers, beneficiary).

% The papacy, monastic orders, bishoprics, and church landholders lost vast territories, revenues, and legal immunities. The Council of Trent and Counter-Reformation were reactive attempts to stem losses, but the structural transfer of assets and authority to secular rulers was irreversible in Protestant territories. Exit from the loss was impossible — the assets were physically seized and legally transferred.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church_institutions, payer,
    institutional, generational, constrained, continental).

% Luther, Calvin, and other reformers gained institutional platforms, state protection, and influence over new church structures. However, their theological agenda was frequently overridden by princely interests (e.g., Luther's reliance on Saxon protection, Calvin's Geneva as a city-state project). They were instrumentalized: their doctrines provided the legitimating cover for asset seizure, but they did not control the political outcomes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_theologians, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, protestant_theologians, payer).

% Bore the costs of war, dislocation, increased taxation to fund princely armies, and disruption of traditional charitable networks (monastic poor relief). The German Peasants' War (1524–25) showed their attempted exit was crushed by the same princes who used Reformation rhetoric. They had no voice in the theological-political settlement and no structural exit from its consequences.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasant_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, peasant_populations, excluded).

% Figures like Erasmus, Cardinal Contarini, and the Spirituali advocated internal reform without schism. Their project was structurally excluded by the dynamic the political_swap_reading describes: once secular rulers saw asset seizure as profitable, the middle ground of conciliar reform lost its constituency. They were not beneficiaries of the swap and had no leverage to stop it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_reformers, excluded,
    organized, biographical, constrained, continental).

% Historians and historical sociologists who evaluate the Reformation's causal structure. This seat includes Marxist historians (who emphasized class interest), confessional historians (who emphasized theology), and revisionists (who emphasize contingency). They do not collect rents or pay costs from the historical event itself, but their interpretive frameworks shape how the constraint is classified in the present.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Breaking the papal monopoly on spiritual authority and ecclesiastical appointments to enable secular rulers to consolidate sovereign control over territory, law, and revenue within their domains — solving the coordination problem of how to exit the universal church without triggering uncontrollable fragmentation.
% TRANSFER_FUNCTION: Massive transfer of land (monastic estates, bishopric territories), wealth (annates, tithes, church silver), and jurisdictional authority (episcopal courts, canon law, appointment rights) from Catholic Church institutions to secular rulers, financed by the theological rhetoric of reform.
% ABSENT_VOICES: Peasant populations who fought and died in the resulting wars (e.g., German Peasants' War, Wars of Religion) and lost traditional safety nets; Catholic reformers (Erasmus, Contarini, the Spirituali) who sought reform without schism and were structurally crowded out by the princely seizure dynamic; urban artisans and merchants in imperial cities who faced princely coercion to conform.
% DISAPPEARANCE_RATIONALE: If the political swap — the legal-political mechanism allowing secular rulers to seize church assets and assert cuius regio eius religio — vanished overnight, the map of European sovereignty, the property basis of the early modern state, the confessional geography of the continent, and the legal architecture of Westphalia would all be unrecognizable. The modern state system's religious settlement is a direct downstream product.
% FOUNDING_PROBLEM: Fragmented secular authority under papal supremacy: rulers lacked full control over appointments, taxation, and law within their nominal territories because the Church claimed independent jurisdiction, immunity from secular courts, and a share of revenues. The founding problem was how to consolidate sovereign authority over a territory that the Church legally and financially partitioned.
% FOUNDING_PROBLEM_CORROBORATION: Political historians of early modern Europe (e.g., Heinz Schilling, Thomas Brady, Christopher Clark) attest that the sovereignty driver was central and that Westphalia (1648) resolved it by entrenching princely control over religion. Confessional historians (Catholic and Protestant) contest this, arguing theological conviction was primary; however, the corroboration for 'dead' comes from secular state-formation scholarship outside the benefiting confessional traditions.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is high (0.78) because the asset transfer was massive, one-directional, and permanent — monastic lands alone represented 20–30% of arable land in many German territories. Suppression is high (0.72) because the swap required crushing alternatives: the Peasants' War, the Schmalkaldic War, and the Thirty Years' War were enforcement actions against those who resisted the new property and authority regime. Theater ratio is moderate (0.42): theological disputation was real and consequential, but an increasing share of the machinery (visitations, confiscations, cuius regio enforcement) served extraction, not reform. Accessibility collapse (0.58) reflects that conciliar reform and Catholic renewal were viable alternatives until princes made seizure irreversible. Resistance (0.68) captures the Catholic Reformation, Jesuit missions, and confessional wars — real but ultimately unable to reverse the asset transfer in Protestant zones.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the secular_ruler seat, the constraint appears as rope/scaffold (coordination of sovereignty transition); from the catholic_church_institutions seat, it appears as snare (pure extraction with theological cover); from the peasant seat, it appears as snare with trapped exit; from the protestant_theologian seat, it appears as tangled_rope (genuine theological coordination hijacked by extraction). This divergence is the measurement — the constraint is not one type but a structure that presents differently to each positioned agent.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers are structural beneficiaries (d near 0.0): they initiated the constraint, wrote its laws, and collected its rents. Catholic Church institutions are structural targets (d near 1.0): they lost assets, jurisdiction, and immunity with no exit. Protestant theologians sit near symmetric (d ~ 0.5): they gained platforms but lost control of the political outcome — their doctrines became the scaffold. Peasants are trapped targets (d ~ 1.0, exit_options: trapped): they paid in blood and tax with zero leverage. Catholic reformers are excluded (d ~ 0.7): they had a reform agenda but were structurally locked out by the princely dynamic. Historical analysts are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consolidating sovereign authority over church-partitioned territory) was solved by Westphalia — the mandate is dead. Yet the constraint's descendant structures (state control of religious property, established churches, secular jurisdiction over marriage/education) persist. This is mandatrophy: the arrangement outlived its founding problem and now serves as the inherited architecture of church-state relations. The political_swap_reading treats the Reformation as the origin point of this mandatrophic structure; the theological_climb_reading would locate the origin in doctrinal rupture instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_conviction_vs_opportunism,
    'To what extent did secular rulers genuinely hold Protestant convictions versus purely instrumentalizing theology for asset seizure?',
    'Comparative analysis of princely correspondence, policy consistency before/after theological adoption, and patterns of enforcement where theological conformity conflicted with fiscal interest (e.g., Catholic princes who suppressed Protestantism but seized church lands anyway).',
    'If rulers were predominantly opportunistic, the constraint is snare (coordination story is pure cover). If conviction was mixed but instrumentalization systematic, it remains snare with a thicker coordination veneer. If conviction was primary for key actors, the constraint shifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_conviction_vs_opportunism, empirical, 'Whether the political_swap_reading''s claim of ''post-hoc rationalization'' holds across the ruler population or only for a subset.').

omega_variable(
    coordination_function_genuineness,
    'Was the coordination problem (church corruption, pastoral neglect, doctrinal confusion) genuinely solved by the new arrangements, or was the ''solution'' entirely a byproduct of extraction?',
    'Measure pastoral outcomes (clergy education, lay literacy, poor relief continuity) in territories that adopted Reformation vs. those that remained Catholic but implemented Trent reforms, controlling for pre-existing conditions.',
    'If Protestant territories showed measurable improvement in the coordination targets (pastoral care, doctrinal clarity, institutional accountability) independent of asset seizure, the constraint has a genuine coordination component (tangled_rope). If outcomes were worse or identical to Catholic reform territories, the coordination story is falsified (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether the theological scaffold performed any real coordination work or was purely theatrical.').

omega_variable(
    kernel_reading_foreclosure_theological_climb,
    'Does the political_swap_reading''s core premise (theology as post-hoc rationalization) logically foreclose the theological_climb_reading''s core premise (theology as primary driver) within a single analytical framework?',
    'Formal analysis of the causal claims: if political_swap_reading asserts ''theological disputes were exploited by rulers for political ends'', this entails that theology was not the primary causal driver. A framework holding political_swap_reading cannot simultaneously hold theological_climb_reading as a description of the same event. The foreclosure is logical, not merely historiographical.',
    'Confirms forecloses relation in cs_structure.reading_relations. If foreclosure holds, the two readings cannot be combined in a single constraint story without internal contradiction — they must remain separate constraint stories linked by kernel membership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_theological_climb, conceptual, 'Structural relationship between political_swap_reading and theological_climb_reading: foreclosure of primary causal driver.').

omega_variable(
    kernel_reading_foreclosure_composite,
    'Does the political_swap_reading''s claim of ''primarily political'' foreclose the composite_overdetermination_reading''s claim that ''no single causal driver captures the phenomenon''?',
    'Logical analysis: ''primarily political'' asserts a single dominant driver; ''no single driver captures it'' asserts irreducible multiplicity. These are contradictory truth claims about the same event. A framework committing to ''primarily political'' is logically committed to rejecting ''no single driver''.',
    'Confirms forecloses relation for composite_overdetermination_reading. The composite reading functions as a meta-reading that denies the possibility of the political_swap_reading''s core claim. They cannot coexist in one framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_composite, conceptual, 'Structural relationship between political_swap_reading and composite_overdetermination_reading: foreclosure of monocausal vs. overdetermined causal structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.25).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1525, reformation_event_boundary__political_swap_reading, theater_ratio, 1525, 0.32).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1535, reformation_event_boundary__political_swap_reading, theater_ratio, 1535, 0.38).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.4).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.41).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.42).

% Extraction over time
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1525, reformation_event_boundary__political_swap_reading, base_extractiveness, 1525, 0.52).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1535, reformation_event_boundary__political_swap_reading, base_extractiveness, 1535, 0.65).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.72).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.76).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.45).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1525, reformation_event_boundary__political_swap_reading, suppression_requirement, 1525, 0.58).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1535, reformation_event_boundary__political_swap_reading, suppression_requirement, 1535, 0.63).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.68).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.71).
narrative_ontology:measurement(reformation_event_boundary__political_swap_reading_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalian_sovereignty_settlement).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, secularization_of_church_property_regime).

% DUAL FORMULATION NOTE:
% This constraint is one member of the reformation_event_boundary kernel family. The political_swap_reading treats the Reformation as a political swap (authority/asset transfer 1517–1648) with theology as scaffold. The theological_climb_reading treats it as a doctrinal climb (justification by faith as genuine breakthrough requiring separation). The composite_overdetermination_reading treats it as an overdetermined composite with no single driver. All three share the kernel_id 'reformation_event_boundary' but instantiate distinct constraints with different ε, beneficiaries, victims, and periodizations. This decomposition follows the ε-invariance principle: the label 'the Reformation' conflates structurally distinct causal claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, institutional, 0.1).
constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, organized, 0.45).
constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
