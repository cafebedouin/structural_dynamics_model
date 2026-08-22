% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: The Reformation as Political Realignment (Papal-Authority Asset Seizure)
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the political-swap reading — of the
 *   contested Reformation-event-boundary kernel. On this reading, the
 *   theological dispute over justification is real as doctrine but
 *   structurally secondary: it functions as the legitimating vocabulary that
 *   secular rulers use to execute a transfer already made attractive by
 *   fiscal and jurisdictional incentives (monastic land, tithe revenue,
 *   appointment power, judicial independence from Rome). The ε authored here
 *   (0.78 at interval end) is high because, under this reading's own lights,
 *   the standing arrangement is a sustained asset/authority transfer from the
 *   Church to territorial princes, backed by legal and (where resisted)
 *   military enforcement. This is NOT a hedge across the theological-climb or
 *   composite-overdetermination siblings — those are separate constraint
 *   files with their own ε, their own beneficiary/victim sets, and their own
 *   periodization. Do not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "The Reformation as Political Realignment (Papal-Authority Asset Seizure)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'a0057d22-5d07-43c9-8d8c-8fbe4a316123').
narrative_ontology:cs_kernel_codification('a0057d22-5d07-43c9-8d8c-8fbe4a316123', fixed_text).
narrative_ontology:cs_authority_grounding('a0057d22-5d07-43c9-8d8c-8fbe4a316123', extraction).
narrative_ontology:cs_interpretation_layer_present('a0057d22-5d07-43c9-8d8c-8fbe4a316123').
narrative_ontology:cs_reading_relation('a0057d22-5d07-43c9-8d8c-8fbe4a316123', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0057d22-5d07-43c9-8d8c-8fbe4a316123', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('a0057d22-5d07-43c9-8d8c-8fbe4a316123', foundational, political_motive_causally_prior_to_doctrine).
narrative_ontology:cs_axiom_status(political_motive_causally_prior_to_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a0057d22-5d07-43c9-8d8c-8fbe4a316123', political_motive_causally_prior_to_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('a0057d22-5d07-43c9-8d8c-8fbe4a316123', secondary, theology_functions_as_legitimating_instrument_not_independent_cause).
narrative_ontology:cs_axiom_status(theology_functions_as_legitimating_instrument_not_independent_cause, holdable).
narrative_ontology:cs_axiom_grounding('a0057d22-5d07-43c9-8d8c-8fbe4a316123', theology_functions_as_legitimating_instrument_not_independent_cause, instrumental).
narrative_ontology:cs_reference_frame('a0057d22-5d07-43c9-8d8c-8fbe4a316123', papal_plenitudo_potestatis_framework).
narrative_ontology:cs_drift_state('a0057d22-5d07-43c9-8d8c-8fbe4a316123', westphalian_settlement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a0057d22-5d07-43c9-8d8c-8fbe4a316123', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_nobility).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, roman_catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_orders).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_curia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasant_and_urban_populations).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, territorial_sovereignty_over_ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Territorial rulers within the Holy Roman Empire and beyond who adopt reformed doctrine as legal cover to nationalize monastic lands, redirect tithe revenue to territorial treasuries, and remove papal veto power over clerical appointments. They fund reformers, write the religious settlement into territorial law, and enforce conformity within their domains. The theological dispute gives them a vocabulary for a transfer of authority they were already positioned to make.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_princes, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_princes, beneficiary).

% Loses direct jurisdiction, tithe revenue streams, and property across large swaths of northern and central Europe as princes reclassify church assets as territorial patrimony. Excommunication and doctrinal condemnation no longer compel compliance once secular power backs the break; the Church's enforcement mechanism (interdict, excommunication) depends on princes' willingness to honor it, and that willingness collapses precisely where political interest diverges.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, roman_catholic_church, payer,
    institutional, civilizational, constrained, continental).

% Monasteries and convents are dissolved by princely decree, their lands and endowments transferred to the crown or redistributed to loyal nobility. Individual monks and nuns are pensioned off, expelled, or absorbed into secular life with little recourse; their institutional home is liquidated as a line item in a territorial balance sheet.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_orders, payer,
    moderate, biographical, trapped, regional).

% Loses the capacity to tax, appoint bishops, or adjudicate disputes across formerly obedient territories. Attempts at negotiation (legates, councils, diplomatic pressure) are increasingly bypassed once princes have already seized the assets and built domestic legal structures around the seizure; by the time Trent convenes, the political facts are largely settled.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_curia, payer,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, papal_curia, excluded).

% Theologians and preachers who supply the doctrinal justification princes need, and who benefit from princely protection against Rome. Some genuinely believe the doctrine; on this reading their sincerity is not in question, but their institutional survival depends entirely on aligning with a prince who has already decided to break with Rome for other reasons — reformers who displease their patron lose protection fast.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_reformers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, protestant_reformers, excluded).

% Bear the costs of confessional war, forced conformity to whichever confession their ruler adopts, and the loss of monastic charitable functions (hospitals, poor relief, schooling) that dissolved institutions previously provided. Have essentially no voice in whether their territory turns Protestant or stays Catholic — that decision is made above them and imposed by cuius regio, eius religio.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasant_and_urban_populations, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, peasant_and_urban_populations, excluded).

% Reconstruct the causal weighting between theological, political, and economic drivers from archival records, correspondence, and property transfer documents. This reading is their claim: that the balance of evidence favors political motive as primary and theology as instrumentalized rationale, contestable against sibling readings that weight theology or composite causation more heavily.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, later_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides secular rulers a legally and doctrinally coherent framework for consolidating fragmented ecclesiastical authority into unified territorial sovereignty — solving the genuine coordination problem of a fragmented, foreign-controlled institutional layer sitting inside otherwise consolidating states.
% TRANSFER_FUNCTION: Moves land, tithe revenue, appointment power, and adjudicative authority from the papacy and monastic institutions to territorial princes and the nobility allied with them; theology supplies the transfer's public justification without itself being the mechanism of transfer.
% ABSENT_VOICES: Peasant and urban populations, whose religious affiliation is decided by cuius regio eius religio, have no seat in the settlement. Reformers who lose princely favor are excluded once their doctrinal utility is spent. The papal curia is progressively excluded from adjudication as territorial legal facts outrun diplomatic process.
% DISAPPEARANCE_RATIONALE: On this reading, if the political realignment had not occurred — if princes had lacked motive or opportunity to seize ecclesiastical assets and jurisdiction — theological dissent alone (per the composite/theological-primacy siblings) would plausibly have been contained as a heresy dispute, as earlier reform movements were. The territorial state system, the confessional map of Europe, and the post-Westphalian sovereignty doctrine all depend on the asset/authority transfer having occurred; remove it and the political map of early modern Europe reorganizes substantially.
% FOUNDING_PROBLEM: Secular rulers faced a structural problem: extensive ecclesiastical landholding, tax exemption, and papal jurisdictional override sat inside their territories as a foreign-controlled institutional layer that limited fiscal and legal sovereignty, precisely as territorial consolidation was becoming possible.
% FOUNDING_PROBLEM_CORROBORATION: Property and tax records compiled by later economic historians (outside both princely and reformist interests) corroborate the scale and timing of asset transfer independent of confessional affiliation — Catholic princes engaged in comparable jurisdictional consolidation against Rome without adopting Protestant doctrine. This is the strongest outside corroboration for the political-primacy reading; the theological-climb reading's own tradition (confessional church historians) contests it, so corroboration here comes specifically from state-formation historians rather than from either confessional camp.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily from 1517 to roughly 1618-1648 as ad hoc territorial seizures during the early Reformation harden into codified legal transfers (monastic dissolution acts, peace treaties recognizing territorial religious authority) and then into the settled sovereignty doctrine of Westphalia. Theater ratio climbs in parallel: theological argument increasingly functions as public justification for transfers whose legal and fiscal substance is decided elsewhere — by 1618 (Thirty Years' War outbreak) the confessional language often masks what participants themselves understood as dynastic and territorial calculation. Suppression requirement rises through the confessional wars and recedes modestly after Westphalia as the settlement stabilizes and coercive enforcement of a single arrangement gives way to a negotiated multi-confessional order — hence the slight downturn in the final measurement points rather than a monotonic climb.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (secular princes), the arrangement reads as legitimate territorial consolidation vindicated by genuine doctrinal conviction — a rope, even a mountain, if the reading is self-serving. From the payer seat (the Church, monastic orders), the same structural facts read as coerced asset seizure using theology as cover — a tangled rope shading toward snare. The engine computes this divergence from the declared power/exit/scope data; this story does not resolve it by fiat, and the divergence IS the seat-level classification the framework is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular princes are near-full beneficiaries: they set the territorial legal terms, collect the transferred assets and jurisdiction, and retain mobility to renegotiate confessional alignment as political interest shifts (several princes reconverted or switched sides across the period). The Roman Catholic Church, monastic orders, and the papal curia are near-full targets: they lose assets and jurisdiction through a mechanism (territorial law backed by military force) they cannot exit from or effectively litigate against once the political facts are established. Reformers occupy an intermediate position — real beneficiaries of protection and platform, but structurally dependent on princely favor, which is why their exit options are 'constrained' rather than 'mobile.' Peasant and urban populations are victims of a decision made entirely above them.
 *
 * MANDATROPHY ANALYSIS:
 *   The political-swap reading resists collapsing into either pure coordination (rope) or pure extraction (snare) by requiring BOTH a real coordination function (consolidating fragmented, foreign-controlled ecclesiastical authority into coherent territorial sovereignty — a genuine state-formation problem) AND asymmetric extraction (the Church and monastic orders bear costs through the same structure that solves the princes' coordination problem) — hence tangled_rope rather than snare. Declaring the founding problem 'dead' (fragmented ecclesiastical jurisdiction inside consolidating territorial states has been resolved for centuries) while the doctrinal and institutional arrangement (state churches, confessional territorial law) persisted well past 1648 in many jurisdictions is exactly the kind of founding-problem/persistence mismatch the R5 interview is designed to surface — though on this reading the mismatch resolves at Westphalia rather than lingering indefinitely, which is why the interval closes there.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_as_cause_or_cover,
    'Did theological conviction (Luther''s doctrine of justification) independently motivate the break with Rome, or did it function primarily as available legitimating vocabulary for a transfer princes were already positioned and motivated to make?',
    'Comparative analysis of princely correspondence and territorial legal instruments: if asset/jurisdiction transfer timing and scope track fiscal-political opportunity more closely than doctrinal conversion timing (e.g., princes who seized assets before publicly adopting reformed doctrine, or Catholic princes who pursued comparable jurisdictional consolidation without doctrinal change), the political-primacy reading is corroborated; if doctrinal conversion consistently precedes and is causally prior to asset transfer across cases, the theological-climb reading gains support.',
    'If theology is shown to be causally prior and independently sufficient, this reading''s core premise (theology as post-hoc rationalization) fails, and the constraint should be reclassified toward the theological_climb_reading''s structure rather than tangled_rope with the Church as primary victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_as_cause_or_cover, empirical, 'Whether theological conviction was causally prior to or instrumentalized by political motive.').

omega_variable(
    periodization_boundary_choice,
    'Is 1648 (Westphalia) the correct closing boundary for this reading''s constraint, given that confessional state-church arrangements persisted in many jurisdictions well beyond that date, and some historians would extend the political settlement''s consolidation into the 18th century?',
    'Track whether the marginal rate of new territorial asset/jurisdiction transfer approaches zero by 1648 versus continuing at comparable pace afterward; a sharp deceleration supports the Westphalia boundary, a continued comparable rate suggests the true closing point is later.',
    'A later closing boundary would extend the measurement interval and likely show continued or renewed extractiveness (e.g., 18th-century state absorption of remaining church properties), strengthening rather than weakening this reading''s ε trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_boundary_choice, conceptual, 'Whether Westphalia is the structurally correct periodization boundary for the political-swap reading.').

omega_variable(
    cs_framing_kernel_vs_institution,
    'Is the correct commitment-system framing the papal institution itself (as the kernel under contest), or the doctrine of papal supremacy layered above the institution (the legitimacy claim the institution depends on)? The political-swap reading treats the institution as the object of seizure, but a rival framing would treat the doctrine of supremacy as the actual kernel being repudiated, with institutional asset transfer as a downstream consequence.',
    'Examine whether princes who seized assets continued to acknowledge papal doctrinal supremacy in any residual form (some Catholic princes did) versus reformed princes who repudiated the doctrine explicitly — if asset seizure and doctrinal repudiation track together tightly, the doctrine-as-kernel framing is favored; if they dissociate, the institution-as-kernel framing (used in this story) is favored.',
    'Adopting the doctrine-as-kernel framing would shift the cs_structure authority_grounding analysis and could reclassify some Catholic-prince asset seizures as belonging to this constraint family even absent doctrinal break, widening the victim/beneficiary set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_institution, conceptual, 'Whether the institution or the supremacy doctrine above it is the more defensible kernel framing.').


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
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.48).
narrative_ontology:measurement(refo_tr_t1580, reformation_event_boundary__political_swap_reading, theater_ratio, 1580, 0.52).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.6).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.55).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__political_swap_reading, base_extractiveness, 1530, 0.52).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.68).
narrative_ontology:measurement(refo_be_t1580, reformation_event_boundary__political_swap_reading, base_extractiveness, 1580, 0.74).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.8).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__political_swap_reading, suppression_requirement, 1530, 0.45).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.58).
narrative_ontology:measurement(refo_su_t1580, reformation_event_boundary__political_swap_reading, suppression_requirement, 1580, 0.63).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.72).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'the Reformation' per the ε-invariance principle. political_swap_reading (this file) authors high ε (0.78) with the Catholic Church/monastic orders/papal curia as victims and secular princes as beneficiaries, periodized to 1648. theological_climb_reading authors a structurally distinct claim (doctrinal breakthrough as primary event) with a different beneficiary/victim structure and likely different, lower extraction since genuine doctrinal reform is not itself extractive. composite_overdetermination_reading treats all causal strands as irreducibly co-present and does not assign primary causal weight to any single driver, which structurally changes its ε referent. All three are linked via affects_constraints; none averages or reconciles with the others — each is generated as a clean, independent constraint per Rule 1.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
