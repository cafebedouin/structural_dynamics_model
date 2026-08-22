% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Princely Appropriation Settlement (Political-Swap Reading of the Reformation)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   Under the political-swap reading, the standing arrangement under contest
 *   is the princely appropriation settlement of 1517–1648: secular rulers
 *   used the theological dispute as entry point and legitimation to break
 *   papal jurisdiction, confiscate ecclesiastical property, and consolidate
 *   territorial sovereignty, with the settlement stabilized at Westphalia. On
 *   this reading theology functions as scaffold — real doctrinal content,
 *   deployed and frozen where it served the transfer. The ε referent is that
 *   standing arrangement assessed by this reading's own lights:
 *   seizure-driven transfer (high extractiveness), theology increasingly
 *   performative relative to its settlement function (rising theater), and
 *   enforcement machinery that ratcheted through war and relaxed once the
 *   order stood on its own. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (real dual-jurisdiction
 *   coordination plus real asymmetric transfer) while the metrics are
 *   authored from the historical record independently; the engine computes
 *   per-seat classifications from the structural data. This file is one
 *   reading of the reformation_event_boundary kernel; the theological-climb
 *   and composite-overdetermination readings are separate constraints with
 *   their own ε, beneficiaries, and periodization.
 *
 * KEY AGENTS:
 *   - - secular_territorial_princes: Primary beneficiary and agenda-setter (powerful/arbitrage) — set, enforced, and collected from the settlement
 *   - - catholic_church_hierarchy: Primary target (institutional/trapped) — lost jurisdiction and fixed endowment it could not relocate
 *   - - monastic_orders: Target (powerless/trapped) — dissolved by decree without recourse
 *   - - territorial_subjects: Target with incidental coordination benefit (powerless/trapped) — bound to the ruler's confession, taxed for the new church
 *   - - peasant_leagues: Target, eliminated from the conversation (powerless/trapped) — demands crushed before any settlement
 *   - - reformist_theologians: Secondary beneficiary paying in autonomy (moderate/constrained) — supplied the legitimating doctrine under princely supervision
 *   - - free_imperial_cities: Secondary beneficiary (organized/constrained) — civic consolidation via adopted reform
 *   - - imperial_authority: Restoring agenda-setter turned payer (institutional/trapped) — every reversal attempt cost more than concession
 *   - - modern_historians: Analytical observer (analytical/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.58).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.69).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.69).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Princely Appropriation Settlement (Political-Swap Reading of the Reformation)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'ae867f2c-3c8f-4746-9492-3fb77c725df8').
narrative_ontology:cs_kernel_codification('ae867f2c-3c8f-4746-9492-3fb77c725df8', fixed_text).
narrative_ontology:cs_authority_grounding('ae867f2c-3c8f-4746-9492-3fb77c725df8', extraction).
narrative_ontology:cs_interpretation_layer_present('ae867f2c-3c8f-4746-9492-3fb77c725df8').
narrative_ontology:cs_reading_relation('ae867f2c-3c8f-4746-9492-3fb77c725df8', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('ae867f2c-3c8f-4746-9492-3fb77c725df8', reformation_event_boundary__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('ae867f2c-3c8f-4746-9492-3fb77c725df8', foundational, theology_is_posthoc_rationalization).
narrative_ontology:cs_axiom_status(theology_is_posthoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('ae867f2c-3c8f-4746-9492-3fb77c725df8', theology_is_posthoc_rationalization, empirically_contingent).
narrative_ontology:cs_axiom('ae867f2c-3c8f-4746-9492-3fb77c725df8', foundational, princely_fiscal_jurisdictional_interest_primary).
narrative_ontology:cs_axiom_status(princely_fiscal_jurisdictional_interest_primary, holdable).
narrative_ontology:cs_axiom_grounding('ae867f2c-3c8f-4746-9492-3fb77c725df8', princely_fiscal_jurisdictional_interest_primary, empirically_contingent).
narrative_ontology:cs_axiom('ae867f2c-3c8f-4746-9492-3fb77c725df8', secondary, confessional_formulae_calibrated_to_settlement_needs).
narrative_ontology:cs_axiom_status(confessional_formulae_calibrated_to_settlement_needs, holdable).
narrative_ontology:cs_axiom_grounding('ae867f2c-3c8f-4746-9492-3fb77c725df8', confessional_formulae_calibrated_to_settlement_needs, conventional).
narrative_ontology:cs_reference_frame('ae867f2c-3c8f-4746-9492-3fb77c725df8', territorial_sovereignty_consolidation).
narrative_ontology:cs_drift_state('ae867f2c-3c8f-4746-9492-3fb77c725df8', contemporary_pluralist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae867f2c-3c8f-4746-9492-3fb77c725df8', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, free_imperial_cities).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, reformist_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_orders).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, territorial_subjects).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasant_leagues).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_subjects).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, reformist_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, imperial_authority).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, secularization_legal_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted territorial church ordinances, confiscated episcopal revenues and monastic property, assumed the right to appoint clergy, and enforced the settlement through visitations and police ordinances. Land, tithe streams, and jurisdictional competence accrued to their treasuries and courts. Their dynastic position let them shift confessional alignment and alliance blocs as circumstances favored, playing emperor against pope and league against league.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_territorial_princes, agenda_setter,
    powerful, generational, arbitrage, continental).

% City councils adopted the new doctrines in ways that consolidated civic autonomy against resident bishops and patrician rivals, absorbing former church properties and courts into municipal administration. They gained governing coherence and revenue; they paid in confessional discipline imposed on guilds, minorities, and neighboring countryside.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, free_imperial_cities, beneficiary,
    organized, biographical, constrained, regional).

% Supplied the doctrinal justifications — scripture alone, justification by faith — that legitimated the transfers of property and jurisdiction. They received princely protection, university posts, and salaries funded in part from redirected church wealth. They paid with subordination: princes took the title of supreme bishop, supervised consistories, and dictated confessional formulae calibrated to diplomatic needs, as the drafting history of the Augsburg Confession records.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reformist_theologians, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, reformist_theologians, payer).

% Lost jurisdictions, appointment rights, and endowments across northern Europe. Its wealth was land and buildings fixed in the converting territories — it could not relocate its balance sheet. Its responses (council, reformed orders, armed recovery) were costly attempts at reversal that failed; by 1648 it accepted confessional parity as settled fact.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Houses dissolved by ordinance, libraries dispersed, members pensioned, expelled, or absorbed into territorial churches. Corporate structures gave them voice in principle but no recourse inside territories whose rulers abolished them; their property passed by decree without compensation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_orders, payer,
    powerless, civilizational, trapped, regional).

% Received a unified territorial confession with parish provision, schooling, and consistory discipline — a real local coordination good. They lost the ability to choose confession: the 1555 settlement bound each subject to the ruler's faith and narrowed emigration rights, and they bore the taxes that funded the new territorial church establishment.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, territorial_subjects, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, territorial_subjects, beneficiary).

% Raised the Twelve Articles in 1525 demanding evangelical reform joined to economic relief. Their bands were destroyed militarily within months; their demands appear in no settlement document, and the aftermath brought tightened serfdom and stricter territorial discipline. Their objection survives in petitions and interrogation records, not in the arrangement.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasant_leagues, payer,
    powerless, immediate, trapped, local).

% Convened the diets, issued the Worms ban and the Augsburg Interim, and led the armed attempts to restore uniformity. Concession at Augsburg 1555 and exhaustion by 1648 left it with permanently ceded control over religion inside the estates. Bound by coronation oath and electoral structure, it could not abandon its own restoring project, and every reversal attempt cost more than acquiescence.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, imperial_authority, payer,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, imperial_authority, agenda_setter).

% Adjudicate among competing accounts of the period using fiscal records, princely correspondence, diet protocols, and printing data. They can compare the chronology of seizures against the chronology of doctrinal formulation and weigh which drove which; they collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, modern_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_territorial_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved the late-medieval dual-jurisdiction problem: within each territory, two authorities claimed overlapping allegiance, revenue, and courts. The settlement consolidated jurisdiction, taxation, clerical appointment, and legal authority under a single territorial ruler, replacing transnational ecclesiastical governance with territorial governance.
% TRANSFER_FUNCTION: Moved land, tithe streams, judicial competence, and appointment rights from ecclesiastical corporations — papacy, bishoprics, abbeys — to princely treasuries and territorial churches; moved subjects' confessional allegiance from a transnational church to the ruler's chosen confession.
% ABSENT_VOICES: Peasant leagues (destroyed in 1525, before any settlement), monastic communities facing dissolution, and Catholic laity in converted territories held no seat at the diets that produced the settlements; Augsburg 1555 was negotiated among emperor and estates. Their objections are preserved in petitions, trial records, and polemic — outside the arrangement, not within it.
% DISAPPEARANCE_RATIONALE: Without the swap, ecclesiastical jurisdiction and endowments remain with Rome and the bishoprics; no territorial-church system and no cuius regio principle emerge, and the Westphalian sovereignty order — built on the confessional-territorial settlement — does not take its recorded form. The fiscal history of northern Europe rearranges around retained church wealth, and the map of Europe's state formation changes.
% FOUNDING_PROBLEM: The arrangement was built to resolve the fiscal-jurisdictional conflict between universal ecclesiastical authority and emerging territorial sovereignty: princes faced papal taxation, appeals to Roman courts, and immune church lands inside their borders while needing revenue and unified administration.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the benefiting parties attest the grievance: imperial diet protocols (the Gravamina lists of 1522–1530, compiled across confessional lines) record estate complaints against Roman taxation and jurisdiction; Catholic controversialists such as Cochlaeus and Eck accused the princes of using the Gospel as cover for confiscation — hostile testimony converging on the asset motive. No source outside this reading's own framework corroborates the stronger claim that doctrine was mere rationalization; that inference rests on the reading itself.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.78 because the transfer was large, one-directional, and decoupled from any service rendered to the paying side: land and jurisdiction moved by decree. Suppression is authored at 0.58 as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic — reflecting the post-Westphalian steady state: subjects still bound to the ruler's confession, dissenters still excluded, but war-scale enforcement ended. Theater ends at 0.69 because, on this reading, an increasing share of confessional activity served settlement maintenance rather than doctrinal function: formulae were calibrated to diplomatic position (Augsburg Confession, Formula of Concord), and by 1648 confessional labels operated as constitutional categories. Accessibility collapse sits at 0.60: within a converted territory alternatives collapsed almost completely, but at imperial level confessional coexistence persisted, so exit and alternatives never fully closed. Resistance is high (0.70): the Peasants' War, the Schmalkaldic War, the Dutch revolt, and the Thirty Years' War are all recorded attempts to resist or reverse the arrangement. The measurement series run on one shared seven-point grid (1517, 1525, 1546, 1555, 1580, 1618, 1648) with every tracked metric authored at every point. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: a ratchet from excommunication (0.34) through the crushing of 1525 and the Schmalkaldic War to the wartime maximum (0.86), then deliberate relaxation at Westphalia (0.58) once the order no longer needed active defense — an enforcement-decay tail, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the princes' seat the settlement is a state-building achievement they authored and profited from, with arbitrage-grade exit that kept them near the beneficiary end throughout. From the church's seat the same structure is dispossession of a fixed, irreplaceable endowment — trapped exit places it near the full-target end. Territorial subjects and monastic orders occupy the sharpest asymmetry: powerless, trapped, and bearing costs, with only diffuse incidental benefit. The theologians straddle: patronage received, autonomy paid. The imperial authority experienced the arrangement as a losing defensive campaign — an agenda-setter whose agenda failed. The observer seat registers all of these simultaneously; no participant seat does.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the princes (arbitrage exit pushes them further toward the beneficiary end), the cities, and the theologians; victim declarations drive high directionality for the church, the orders, the subjects, and the peasants, with trapped exit amplifying each toward the full-target end. The subjects' secondary beneficiary role keeps them off the extreme — they received the parish-and-discipline coordination good — but their powerlessness and trapped exit dominate the computation. The vindicated propositions (cuius regio, territorial sovereignty, secularization precedent) are listed as vindicated propositions, not beneficiaries: doctrines collect no rents; the princes who wielded them do.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the whole episode as pure seizure (snare) erases the genuine coordination function: the dual-jurisdiction problem was real, and territorial governance solved a collective-action problem that the empire's own Gravamina lists show was felt across confessional lines. Reading it as benign coordination (rope) erases the asymmetry: the same structure that coordinated the territory transferred its largest endowment to the coordinators and bound the subjects without their consent — hence tangled_rope with active enforcement. The R5 interview adds the lifecycle finding: the founding problem (fiscal-jurisdictional conflict) is dead — resolved by 1555 and sealed at 1648 — while the disappearance verdict is world_rearranges, because the sovereign order built on the settlement still structures the world. That status-by-verdict mismatch is the honest output here: the arrangement completed its function and persists as inherited structure in established-church forms, and the mismatch flag should fire rather than be tuned away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story authors the political_swap_reading of kernel reformation_event_boundary; if the composite_overdetermination_reading were adopted instead, do the beneficiary/victim sets and the 1517–1648 periodization survive?',
    'Re-author the arrangement under the composite framing and test whether the same agents retain the same directionalities when theology, institutional collapse, and denominational emergence are promoted from scaffold to irreducible strands.',
    'If the composite framing holds, the single-driver classification collapses, epsilon becomes indexical across strands, and the Westphalian endpoint loses its privileged status; if the political framing holds, this file stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would restructure the agent surface and periodization.').

omega_variable(
    theology_agency_counterfactual,
    'Would the doctrinal innovations have developed and persisted without princely sponsorship — that is, was theology mere post-hoc rationalization, or an independent causal strand?',
    'Compare sponsored versus unsponsored movements: Anabaptist and Spiritualist communities sustained doctrinal programs under persecution without territorial adoption, while sponsored movements froze formulae at settlement needs. Survival curves, martyrdom records, and printing data discriminate the hypotheses.',
    'If unsponsored movements sustained doctrine under lethal pressure, the post-hoc-rationalization axiom weakens and this reading drifts toward the climb or composite siblings; if doctrine spread only where princes carried it, the swap reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_agency_counterfactual, empirical, 'Whether the theological strand had causal independence from princely interest.').

omega_variable(
    secularist_narrative_self_benefit,
    'Does the political-swap reading itself function as legitimation for modern secular statehood — an origin myth in which religion was always cover for power — and if so, does the reading persist partly because identifiable modern audiences collect narrative rents from it?',
    'Trace the reading''s citation and deployment history: where it is invoked in state-church disestablishment debates, secularist polemic, and national historiography versus where it is held on archival grounds alone.',
    'If the reading serves present-day legitimation, its persistence mirrors the structure it describes — a scaffold maintained for the benefit of its deployers — and its apparent evidential strength should be discounted accordingly; if it survives in purely academic circulation, the discount does not apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularist_narrative_self_benefit, conceptual, 'Whether the reading operates as a constructed constraint serving modern narrators.').

omega_variable(
    westphalia_endpoint_contest,
    'Is 1648 the correct terminal boundary for this arrangement, or does the settlement''s stabilization extend later (execution ordnances, 1655 retroactive clauses) or dissolve entirely under the composite reading''s rejection of single periodizations?',
    'Test classification stability under alternative endpoints: recompute the measurement series truncated at 1555 and extended to 1660 and compare drift datings.',
    'A later endpoint raises the measured theater share (postwar confessional formulae were increasingly ceremonial); an earlier one truncates the enforcement-decay tail and dates the type transition differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westphalia_endpoint_contest, conceptual, 'Periodization boundary is itself a reading-dependent choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_swap_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.28).
narrative_ontology:measurement_basis(reformation_swap_tr_t1517, observed).
narrative_ontology:measurement(reformation_swap_tr_t1525, reformation_event_boundary__political_swap_reading, theater_ratio, 1525, 0.36).
narrative_ontology:measurement_basis(reformation_swap_tr_t1525, observed).
narrative_ontology:measurement(reformation_swap_tr_t1546, reformation_event_boundary__political_swap_reading, theater_ratio, 1546, 0.46).
narrative_ontology:measurement_basis(reformation_swap_tr_t1546, observed).
narrative_ontology:measurement(reformation_swap_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.56).
narrative_ontology:measurement_basis(reformation_swap_tr_t1555, observed).
narrative_ontology:measurement(reformation_swap_tr_t1580, reformation_event_boundary__political_swap_reading, theater_ratio, 1580, 0.61).
narrative_ontology:measurement_basis(reformation_swap_tr_t1580, observed).
narrative_ontology:measurement(reformation_swap_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.66).
narrative_ontology:measurement_basis(reformation_swap_tr_t1618, observed).
narrative_ontology:measurement(reformation_swap_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.69).
narrative_ontology:measurement_basis(reformation_swap_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(reformation_swap_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement_basis(reformation_swap_be_t1517, observed).
narrative_ontology:measurement(reformation_swap_be_t1525, reformation_event_boundary__political_swap_reading, base_extractiveness, 1525, 0.54).
narrative_ontology:measurement_basis(reformation_swap_be_t1525, observed).
narrative_ontology:measurement(reformation_swap_be_t1546, reformation_event_boundary__political_swap_reading, base_extractiveness, 1546, 0.63).
narrative_ontology:measurement_basis(reformation_swap_be_t1546, observed).
narrative_ontology:measurement(reformation_swap_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.74).
narrative_ontology:measurement_basis(reformation_swap_be_t1555, observed).
narrative_ontology:measurement(reformation_swap_be_t1580, reformation_event_boundary__political_swap_reading, base_extractiveness, 1580, 0.76).
narrative_ontology:measurement_basis(reformation_swap_be_t1580, observed).
narrative_ontology:measurement(reformation_swap_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.82).
narrative_ontology:measurement_basis(reformation_swap_be_t1618, observed).
narrative_ontology:measurement(reformation_swap_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).
narrative_ontology:measurement_basis(reformation_swap_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(reformation_swap_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.34).
narrative_ontology:measurement_basis(reformation_swap_su_t1517, observed).
narrative_ontology:measurement(reformation_swap_su_t1525, reformation_event_boundary__political_swap_reading, suppression_requirement, 1525, 0.58).
narrative_ontology:measurement_basis(reformation_swap_su_t1525, observed).
narrative_ontology:measurement(reformation_swap_su_t1546, reformation_event_boundary__political_swap_reading, suppression_requirement, 1546, 0.66).
narrative_ontology:measurement_basis(reformation_swap_su_t1546, observed).
narrative_ontology:measurement(reformation_swap_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.71).
narrative_ontology:measurement_basis(reformation_swap_su_t1555, observed).
narrative_ontology:measurement(reformation_swap_su_t1580, reformation_event_boundary__political_swap_reading, suppression_requirement, 1580, 0.73).
narrative_ontology:measurement_basis(reformation_swap_su_t1580, observed).
narrative_ontology:measurement(reformation_swap_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.86).
narrative_ontology:measurement_basis(reformation_swap_su_t1618, observed).
narrative_ontology:measurement(reformation_swap_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.58).
narrative_ontology:measurement_basis(reformation_swap_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Reformation' decomposes into three epsilon-invariant readings of the reformation_event_boundary kernel. This file carries the political-swap reading (epsilon 0.78, referent: the princely appropriation settlement). theological_climb_reading carries the doctrinal-breakthrough reading with its own epsilon, beneficiaries, and failure modes; composite_overdetermination_reading carries the irreducible-plurality reading, which denies any single-driver account including this one. The upstream/downstream pressure runs from this reading toward the composite sibling: the archival record of seizures and diet politics is the strongest single body of evidence any reading can cite, which is why the composite reading must absorb rather than refute it. Per the epsilon-invariance principle, the decomposition lives in the linked files, not in hedged values inside any one of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
