% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: The Reformation as Political Realignment: Princely Seizure of Papal Authority and Church Assets
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This story instantiates the political_swap_reading of the
 *   reformation_event_boundary kernel: the Reformation classified as
 *   primarily a jurisdictional and fiscal transfer of authority from Rome to
 *   territorial rulers, with theological dispute functioning as legitimating
 *   scaffold rather than causal driver. Under this reading the Catholic
 *   Church, its monastic orders, and its fiscal apparatus are the
 *   payer/victim seats; secular princes, the nobility who received
 *   secularized land, and the territorial treasuries that absorbed redirected
 *   revenue are the beneficiary seats. The period boundary extends to 1648
 *   (Peace of Westphalia) because that is when the political-territorial
 *   settlement — cuius regio, eius religio and its aftermath — stabilizes,
 *   not when a particular doctrine is finalized. This is a deliberately
 *   different constraint from the sibling readings: theological_climb_reading
 *   treats Luther's doctrinal breakthrough as the primary driver with
 *   different beneficiaries (reforming theological communities) and a
 *   different natural endpoint (doctrinal consolidation, not political
 *   settlement); composite_overdetermination_reading treats no single driver
 *   as separable at all. Each reading has its own stable ε; this file does
 *   not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.71).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "The Reformation as Political Realignment: Princely Seizure of Papal Authority and Church Assets").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e').
narrative_ontology:cs_kernel_codification('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', distributed).
narrative_ontology:cs_authority_grounding('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', distributed).
narrative_ontology:cs_reading_relation('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', foundational, theology_is_instrumentalized_scaffold).
narrative_ontology:cs_axiom_status(theology_is_instrumentalized_scaffold, holdable).
narrative_ontology:cs_axiom_grounding('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', theology_is_instrumentalized_scaffold, empirically_contingent).
narrative_ontology:cs_axiom('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', foundational, territorial_sovereignty_is_dispositive_variable).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_dispositive_variable, holdable).
narrative_ontology:cs_axiom_grounding('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', territorial_sovereignty_is_dispositive_variable, empirically_contingent).
narrative_ontology:cs_axiom('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', secondary, periodization_tracks_political_settlement_not_doctrine).
narrative_ontology:cs_axiom_status(periodization_tracks_political_settlement_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', periodization_tracks_political_settlement_not_doctrine, conventional).
narrative_ontology:cs_reference_frame('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', papal_universal_jurisdiction).
narrative_ontology:cs_drift_state('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('5ac7685f-a7a3-43b3-bfa5-b2f8d7c2554e', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_princes_and_kings).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_state_treasuries).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, noble_beneficiaries_of_secularized_land).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, roman_catholic_church_institution).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_fiscal_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, reforming_theologians_and_clergy).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, reforming_theologians_and_clergy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, territorial_sovereignty_over_ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rulers of German territories, England, Scandinavia, and other realms adopt reformed confessions or negotiate settlements that transfer ecclesiastical jurisdiction, tithe revenue, and monastic land into territorial or royal hands. They fund the theological dispute's institutionalization, provide military protection to reforming clergy, and set the terms under which a given territory's religion is fixed. Their calculus is legibly fiscal and jurisdictional: dissolution of monasteries funds treasuries and rewards loyal nobility; breaking Rome's appellate jurisdiction consolidates domestic legal supremacy.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_princes_and_kings, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_princes_and_kings, beneficiary).

% Represent the fiscal apparatus that directly absorbs confiscated monastic land, suppressed benefice income, and redirected tithes. Not an actor in itself but the mechanism through which princely gain is realized and made durable across successive reigns.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, territorial_state_treasuries, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__political_swap_reading, territorial_state_treasuries).

% Regional aristocrats purchase, are granted, or simply occupy dissolved monastic estates at favorable terms once ecclesiastical title is broken. They lend military and political support to the ruler's break with Rome in direct proportion to the land and office they expect to receive, and can shift allegiance between confessions if a rival ruler offers better terms.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, noble_beneficiaries_of_secularized_land, beneficiary,
    powerful, biographical, mobile, regional).

% Loses appellate jurisdiction, tithe income, monastic property, and clerical appointment power across an entire tier of European territories within roughly a century. Cannot compel restitution once secular military and legal authority backs the seizure; papal excommunications and interdicts lose practical force once a prince's local monopoly on coercion is aligned against Rome. The institution's only remaining leverage is doctrinal condemnation, which does not recover assets.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, roman_catholic_church_institution, payer,
    institutional, civilizational, trapped, continental).

% Monks, nuns, and lay brothers are dispossessed of their houses, endowments, and communal livelihood when dissolution proceeds. They have no independent military or fiscal power to resist territorial seizure and are dispersed, pensioned at the ruler's discretion, or absorbed into changed religious settlements with no say in the process.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_communities, payer,
    powerless, biographical, trapped, local).

% The system of annates, indulgence revenue, and benefice fees that funded the Roman curia collapses territory by territory as princes redirect the underlying revenue streams to their own treasuries; represents the extraction channel being severed rather than an actor.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_fiscal_apparatus, payer,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__political_swap_reading, papal_fiscal_apparatus).

% Provide the doctrinal vocabulary and moral legitimacy that rulers deploy to justify jurisdictional breaks, and in turn receive princely protection, salaried positions, and institutional platforms they would not otherwise hold. Some genuinely believe the doctrine; all depend materially on continued princely favor, which constrains how far they can push theological positions that would embarrass or inconvenience their patron.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reforming_theologians_and_clergy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, reforming_theologians_and_clergy, payer).

% Live under whatever confession their ruler adopts, absorb the social and economic disruption of monastic dissolution (loss of charitable and medical services previously provided by religious houses), and have no formal voice in the princely-papal jurisdictional settlement despite bearing its downstream costs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasant_and_urban_populations, excluded,
    powerless, biographical, trapped, local).

% Reconstruct the sequence of asset transfers, legal breaks, and treaty settlements from archival and fiscal records, and debate whether the theological or political dimension was causally primary — the same evidentiary record this reading interprets as showing political motive with theological instrumentalization.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, later_historians_of_confessionalization, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_princes_and_kings).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides secular rulers a coordinated mechanism for asserting territorial sovereignty over ecclesiastical jurisdiction, consolidating fragmented feudal authority into unified state control over law, land, and religious practice within their domains — a genuine state-building coordination problem that decentralized medieval Christendom left unsolved.
% TRANSFER_FUNCTION: Moves jurisdictional authority, tithe revenue, monastic land titles, and clerical appointment power from the papal-ecclesiastical apparatus (Rome, the curia, monastic orders) to territorial rulers and the nobility they reward, using theological legitimation as the transfer mechanism's public justification.
% ABSENT_VOICES: Monastic communities and lower clergy dispossessed by dissolution had no forum to contest the settlements that stripped their livelihoods; peasant and urban populations who depended on monastic charitable infrastructure absorbed the social cost without being party to the princely-papal negotiations that produced it.
% DISAPPEARANCE_RATIONALE: If the political realignment component were absent — if theological dissent had occurred without princely sponsorship, military protection, and asset appropriation — the doctrinal disputes of the 1520s would likely have followed the pattern of earlier heresies (Hussite, Waldensian): regionally suppressed, absorbed, or extinguished without producing a permanent institutional and territorial rupture. The durability of the break tracks the durability of the property and jurisdiction transfer, not the durability of the doctrine alone.
% FOUNDING_PROBLEM: Territorial rulers faced a structural problem of divided sovereignty: significant land, revenue, legal jurisdiction, and clerical appointment power within their nominal domains answered to a foreign authority (Rome) rather than to them, limiting fiscal capacity and legal supremacy at precisely the moment early modern state-building required both.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal historians examining post-dissolution treasury records (independent of both Protestant and Catholic confessional historiography) document the land and revenue transfers as the stable, non-reversed outcome across territories with otherwise divergent theological trajectories; the jurisdictional problem the settlement solved for princes was fully resolved by the Peace of Westphalia (1648) and has not recurred, corroborating that the founding problem was structural-political rather than narrowly doctrinal. This reading's corroboration set explicitly includes secular administrative and fiscal records, not only confessional theological sources.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steeply from a low baseline (0.15 at 1517, before large-scale dissolution) to a plateau around 0.71 by 1600–1648, tracking the actual chronology of monastic dissolution and revenue redirection (concentrated in the 1530s–1550s across German territories and England) rather than tracking any doctrinal timeline. Theater ratio rises in parallel (0.20 to 0.58) because as the asset transfer becomes the dominant activity, theological argument increasingly functions as public justification for settlements already substantially decided by military and fiscal position — the disputation literature continues, but the underlying land and jurisdiction transfers it purports to justify were often already accomplished or in train. Suppression requirement climbs from 0.25 to 0.62 as princely enforcement apparatus (garrisons, confiscation commissions, oaths of religious conformity) hardens to lock in the territorial settlement and foreclose reversal, then plateaus once Westphalia stabilizes the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular princes and the nobility who receive secularized land are structural beneficiaries: they collect land, revenue, and jurisdiction, and their exit options (arbitrage — they can play confessions against each other, switch alliances, or use theological cover selectively) reflect genuine agenda-setting power. The Catholic Church, monastic communities, and the papal fiscal apparatus are structural targets: they are trapped (no coercive counter-leverage once local secular military power aligns against Rome) and bear the asset loss directly. Reforming theologians occupy an intermediate position — real beneficiaries of princely patronage, but also constrained payers insofar as their doctrinal freedom is bounded by what their patron finds useful. This is the key structural claim of the political_swap_reading: theology is instrumentalized rather than autonomous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (divided sovereignty between territorial rulers and a foreign ecclesiastical authority) is coded dead by 1648 — Westphalia resolves it durably, and no equivalent papal-secular jurisdictional conflict recurs at that scale afterward. The disappearance_verdict (world_rearranges) reflects that the settlement's stability tracks the durability of the underlying property and jurisdiction transfer, not the durability of any specific doctrine, which is the operational signature of a tangled_rope: a real coordination function (state consolidation of fragmented sovereignty) bundled with asymmetric extraction (the Church and monastic communities pay, secular rulers and nobility collect) sustained by active enforcement (confiscation commissions, garrisons, conformity oaths) rather than persisting on voluntary participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_as_genuine_cause_vs_instrument,
    'Was theological conviction (Luther''s doctrine of justification, sacramental disputes) an independent causal driver of the institutional break, or purely an instrument that princes deployed once it became politically useful?',
    'Comparative analysis of cases where theological dissent occurred WITHOUT princely sponsorship (e.g., earlier medieval heresies that were suppressed) versus cases with sponsorship (the German territories, England) — if the presence/absence of princely backing is the dispositive variable in outcome rather than doctrinal content or intensity, this supports the political_swap_reading; if doctrinally similar movements diverge in outcome independent of princely backing, this undercuts it.',
    'If theology is found to have had independent causal force even absent political sponsorship, this reading''s core premise (theology as post-hoc rationalization) would be substantially weakened, though not necessarily foreclosed — the composite_overdetermination_reading would gain support at this reading''s expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_as_genuine_cause_vs_instrument, conceptual, 'Whether theology functioned as genuine cause or convenient instrument is not fully resolvable from the historical record and is this reading''s central interpretive commitment.').

omega_variable(
    periodization_boundary_choice,
    'Is 1648 (Westphalia, political-territorial stabilization) the correct closing boundary for this event, versus an earlier doctrinal-consolidation date (e.g., 1555 Peace of Augsburg, or the finalization of confessional creeds) that the theological_climb_reading would prefer?',
    'Track which boundary better predicts subsequent institutional stability: does confessional/doctrinal identity stabilize before or after the underlying property and jurisdictional settlement? Evidence of continued doctrinal fluidity after Augsburg but political settlement holding through Westphalia would support the later boundary.',
    'Choosing a different endpoint changes both the measured extractiveness trajectory (a shorter interval would show a steeper, less plateaued curve) and the founding_problem_status determination, since papal-secular jurisdictional conflict recurrence would need to be checked against a different closing date.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_boundary_choice, conceptual, 'The periodization boundary is a reading-specific commitment, not a neutral historical fact, and this reading''s choice of 1648 is a direct consequence of treating political settlement as the dispositive variable.').

omega_variable(
    beneficiary_homogeneity_across_territories,
    'Did secular rulers uniformly benefit from breaking with Rome, or did some territories experience net costs (war, schism, loss of legitimacy) that complicate treating ''secular princes'' as a uniform beneficiary class?',
    'Territory-by-territory fiscal and political outcome analysis distinguishing rulers who gained net revenue/jurisdiction from those who incurred net costs from war or instability (e.g., some Thirty Years'' War participants).',
    'If a substantial subset of rulers experienced net costs, the beneficiary declaration would need refinement to distinguish successful from failed appropriators, which would affect the aggregate extractiveness measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_homogeneity_across_territories, empirical, 'The beneficiary class ''secular princes and kings'' may not be homogeneous in realized gain across the full territorial scope of the Reformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__political_swap_reading, theater_ratio, 1530, 0.35).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__political_swap_reading, theater_ratio, 1545, 0.48).
narrative_ontology:measurement(refo_tr_t1560, reformation_event_boundary__political_swap_reading, theater_ratio, 1560, 0.55).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.58).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.58).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__political_swap_reading, base_extractiveness, 1530, 0.38).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__political_swap_reading, base_extractiveness, 1545, 0.55).
narrative_ontology:measurement(refo_be_t1560, reformation_event_boundary__political_swap_reading, base_extractiveness, 1560, 0.66).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.71).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__political_swap_reading, suppression_requirement, 1530, 0.45).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__political_swap_reading, suppression_requirement, 1545, 0.58).
narrative_ontology:measurement(refo_su_t1560, reformation_event_boundary__political_swap_reading, suppression_requirement, 1560, 0.6).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reformation_event_boundary kernel. theological_climb_reading treats doctrinal breakthrough as the primary causal driver with reforming clergy as principal beneficiaries and an earlier doctrinal-consolidation endpoint. composite_overdetermination_reading treats the political, theological, institutional, and denominational-proliferation strands as irreducibly simultaneous, declining to name a single dispositive variable or a single beneficiary/victim structure. This political_swap_reading commits to political realignment as primary, secular rulers as beneficiaries, the Catholic Church/monastic orders as victims, and 1648 as the closing boundary. Each file carries its own ε and stakeholder structure per the ε-invariance principle; none is a hedge or average of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
