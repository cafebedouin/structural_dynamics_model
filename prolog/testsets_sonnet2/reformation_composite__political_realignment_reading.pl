% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Cuius Regio Eius Religio — Territorial Sovereignty via Religious Differentiation
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This story instantiates the political-realignment reading of the
 *   Reformation kernel: the claim that religious differentiation functioned
 *   primarily as the legal and legitimating vocabulary through which emerging
 *   territorial states asserted sovereignty against the overlapping authority
 *   of Holy Roman Emperor and papal curia. The observable this reading
 *   foregrounds is cuius regio eius religio and its institutional descendants
 *   (territorial church confiscation, princely appointment of clergy, the
 *   Peace of Augsburg settlement) rather than theological content or
 *   print-technology diffusion, which are separate constraints
 *   (theological_fragmentation_reading, technological_mediation_reading)
 *   sharing this kernel. Extraction here tracks the transfer of
 *   ecclesiastical revenue and jurisdictional authority from empire/papacy to
 *   princes and cities, not the doctrinal merits of any confession.
 *
 * KEY AGENTS:
 *   - territorial_princes: Primary beneficiary and agenda-setter (institutional/arbitrage) — use confessional adoption to consolidate sovereignty
 *   - holy_roman_emperor: Primary institutional payer (institutional/constrained) — loses coercive religious unity as a tool of imperial cohesion
 *   - papal_curia: Primary institutional payer (institutional/trapped) — loses direct revenue and juridical reach territory by territory
 *   - religious_dissenters_within_territories: Diffuse individual payer (powerless/trapped) — bears the cost of a settlement negotiated entirely above their level
 *   - modern_historians_of_political_economy: Analytical observer — assesses fiscal and jurisdictional record independent of confessional partisanship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Eius Religio — Territorial Sovereignty via Religious Differentiation").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '2f5fbf91-0491-4e9c-b9f0-7881b915cc64').
narrative_ontology:cs_kernel_codification('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', distributed).
narrative_ontology:cs_authority_grounding('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', distributed).
narrative_ontology:cs_reading_relation('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', foundational, territorial_sovereignty_supersedes_universal_ecclesiastical_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_sovereignty_supersedes_universal_ecclesiastical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', territorial_sovereignty_supersedes_universal_ecclesiastical_jurisdiction, conventional).
narrative_ontology:cs_axiom('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', secondary, confessional_choice_is_instrumentally_selected_for_political_ends).
narrative_ontology:cs_axiom_status(confessional_choice_is_instrumentally_selected_for_political_ends, holdable).
narrative_ontology:cs_axiom_grounding('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', confessional_choice_is_instrumentally_selected_for_political_ends, empirically_contingent).
narrative_ontology:cs_reference_frame('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', unified_christendom_dual_authority).
narrative_ontology:cs_drift_state('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2f5fbf91-0491-4e9c-b9f0-7881b915cc64', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, urban_magistracies).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_emperor).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_curia).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, religious_dissenters_within_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, reformist_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt Reformation confessions to break fiscal, judicial, and appointment ties to Rome and to resist imperial consolidation. Confiscate church property, appoint territorial clergy, and use confessional identity to bind subjects to the prince's authority rather than to distant imperial or papal jurisdiction. Set the enforcement machinery that makes cuius regio eius religio operative within their lands.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_princes, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_princes, beneficiary).

% Loses the capacity to compel uniform religious allegiance as a lever of imperial unity; princes who convert use confessional difference as legal cover to withhold troops, taxes, and obedience. Repeated military campaigns (Schmalkaldic War) fail to reverse the territorial fragmentation; the Peace of Augsburg formalizes the loss.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, payer,
    institutional, generational, constrained, continental).

% Loses direct revenue streams (annates, indulgence sales, benefice appointments) and juridical authority over entire territories in a single stroke wherever a prince converts. Cannot appeal to a competing secular power to enforce papal claims once the territorial ruler has aligned religious and political authority under one office.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_curia, payer,
    institutional, civilizational, trapped, continental).

% Subjects whose personal confessional commitments diverge from their prince's chosen confession face exile, forced conversion, or persecution under the territorial settlement — Anabaptists, and religious minorities on the losing side of a given territory's choice, bear the cost of the ruler's geopolitical calculation regardless of the emperor-versus-prince question.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_dissenters_within_territories, payer,
    powerless, biographical, trapped, local).

% Free imperial cities and city councils use Reformation adoption to assert autonomy from both episcopal oversight and imperial taxation demands, consolidating civic control over parish appointments, poor relief, and moral regulation formerly mediated by the Church.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, urban_magistracies, beneficiary,
    organized, generational, mobile, regional).

% Provide the doctrinal content princes adopt, but their theological commitments are frequently subordinated to the ruler's political calculus — clergy who resist territorial control (state church subordination, princely authority over doctrine and discipline) find their voice absorbed into the political settlement rather than determinative of it. Their own account of the movement (a theological dispute about salvation) is displaced by this reading's framing.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformist_theologians, excluded,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, reformist_theologians, beneficiary).

% Analyze archival records of territorial fiscal transfers, confiscated church property registers, and diplomatic correspondence to assess how far confessional choice tracked sovereignty calculation versus genuine doctrinal conviction among ruling elites.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, modern_historians_of_political_economy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides emerging territorial states a legitimating vocabulary and legal mechanism (cuius regio eius religio) to consolidate jurisdiction — fiscal, judicial, and ecclesiastical — under a single sovereign authority, resolving the coordination problem of competing overlapping jurisdictions between empire, papacy, and territory.
% TRANSFER_FUNCTION: Moves ecclesiastical revenue, land, judicial authority, and the power to define orthodoxy from the papal curia and the imperial center to territorial princes and urban magistracies; the cost is borne by the emperor's capacity to compel unity and by subjects whose confessional dissent from the territorial settlement is now criminalized rather than merely doctrinally contested.
% ABSENT_VOICES: Ordinary religious dissenters within each territory — Anabaptists, crypto-Calvinists in Lutheran lands, Catholics in newly Protestant territories — are structurally absent from the settlement that determines their confession; the Peace of Augsburg negotiates among princes and emperor, not among believers. Reformist theologians who intended a doctrinal reform, not a sovereignty transfer, are also sidelined once princely adoption occurs.
% DISAPPEARANCE_RATIONALE: If the political-realignment mechanism had not operated — if confessional choice carried no sovereignty payoff for princes — the fragmentation of imperial authority into competing sovereign states plausibly proceeds far more slowly or through different (purely dynastic/military) channels; the territorial state system that culminates in Westphalia is substantially built on the legal and fiscal precedent this reading identifies.
% FOUNDING_PROBLEM: Territorial rulers within the Holy Roman Empire faced an unresolved jurisdictional conflict: overlapping claims of imperial suzerainty, papal ecclesiastical authority, and local princely sovereignty left no settled answer to who could tax, appoint clergy, or adjudicate within a given territory.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians of the Empire (outside both princely and papal interests) attest via the documentary record of the Peace of Augsburg (1555) and the Peace of Westphalia (1648) that the jurisdictional question this arrangement addressed was formally and durably resolved in favor of territorial sovereignty; the arrangement's confessional machinery persisted afterward as an inherited administrative and identity structure rather than as an active solution to a live jurisdictional dispute.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 at Luther's initial theses to 0.68-0.70 through the Schmalkaldic War, Peace of Augsburg, and Thirty Years' War, tracking the escalating transfer of ecclesiastical wealth and jurisdiction to territorial control and the entrenchment of confessional identity as a tool of state consolidation. Suppression climbs sharply (0.40 to 0.80 by 1618) as territorial settlements harden into enforced uniformity within each principality, criminalizing confessional dissent that was tolerable, or at least merely contested, in the movement's earlier years. Theater ratio rises moderately (0.20 to 0.50) as the doctrinal justification for territorial religious control increasingly performs a legitimating function for what is, on this reading, a jurisdictional power transfer already substantially achieved by treaty.
 *
 * PERSPECTIVAL GAP:
 *   From the territorial prince's seat, the arrangement reads as legitimate defense of sovereign prerogative against illegitimate foreign (imperial/papal) interference — closer to rope or tangled_rope with genuine coordination value (settling centuries of overlapping jurisdiction). From the emperor's and curia's seats, the same mechanism is extraction of authority they held as a matter of settled constitutional and canonical order. From the dissenter's seat at the bottom, it is pure suppression with no coordination benefit reaching them at all — the coordination problem the arrangement solves is a problem for princes, not for them.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial princes and urban magistracies are structural beneficiaries: they gain revenue, jurisdiction, and the capacity to define religious life within their domains — d near the beneficiary end. The emperor and papal curia are structural targets: authority and revenue streams are extracted from their prior position — d near the full-target end, amplified further by their inability to appeal to any higher enforcing power once the territorial principle is established. Religious dissenters within territories are targets of a different, secondary extraction: their confession is decided for them by the settlement between princes and empire, and their exit options are trapped (emigration was possible for some but costly and limited).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unresolved three-way jurisdictional conflict between empire, papacy, and territory — is genuinely dead by 1648: Westphalia settles it. Because a genuine, resolvable coordination problem existed at the outset (this is not pure invented cover), the classification as tangled_rope rather than pure snare is warranted: there was real coordination value in resolving overlapping jurisdiction, but it was achieved through asymmetric extraction from the empire/papacy and continued suppression of dissenters even after the coordination problem for princes was solved. The persistence of confessional enforcement machinery after 1648, when the jurisdictional question was settled, is itself evidence of theater/inertia layered onto the original coordination-extraction hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genealogical_omega_political_vs_theological_priority,
    'Is the political-realignment mechanism (princes using religion for sovereignty) the PRIMARY driver of the Reformation''s trajectory, or a downstream opportunistic exploitation of a genuinely theological movement whose primary causal engine lies elsewhere (in doctrine or in print technology)?',
    'Comparative case analysis of princely conversion timing against documented theological conviction (private correspondence, confessor records) versus documented fiscal/jurisdictional motive (treasury records, timing relative to imperial tax demands); a genuinely resolvable historiographical question given sufficient archival access, though contested interpretation is likely to remain.',
    'If political motive dominates, this reading''s high extractiveness and tangled_rope classification are well-founded as the primary structural account. If theological conviction dominates and political consolidation is a secondary consequence, this reading''s ε may overstate the extraction attributable to the political mechanism specifically, and the theological_fragmentation_reading sibling would carry more of the true causal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genealogical_omega_political_vs_theological_priority, conceptual, 'Whether political sovereignty-seeking or theological conviction is the primary causal engine behind princely Reformation adoption — the central genealogical dispute between kernel readings.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the political-realignment reading''s causal claim stop and the theological_fragmentation_reading''s claim begin, given that most historical princes who converted plausibly held some mixture of political calculation and genuine religious conviction?',
    'Documenting the specific structural element the two readings differ on: this reading treats confessional choice as instrumentally selected FOR sovereignty ends; the theological reading treats confessional choice as driven by doctrinal commitment with political consequences flowing FROM it, not motivating it. The disagreement is located in the direction of the causal arrow between confession and sovereignty-claim, not in whether both phenomena co-occurred.',
    'Resolving this affects which reading is treated as upstream in network analysis (does political motive drive theological adoption, or does theological adoption enable political consolidation as a byproduct) — currently authored as coexisting, non-foreclosing accounts of the same historical events.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Locates the precise structural disagreement between the political and theological kernel readings: direction of the causal arrow between confession and sovereignty.').

omega_variable(
    sincere_conviction_among_beneficiary_princes,
    'Among the territorial princes coded here as beneficiaries, what proportion held sincere theological conviction independent of the sovereignty payoff, and does sincere conviction among some beneficiaries undermine the tangled_rope/extraction framing for those specific cases?',
    'Case-by-case biographical and correspondence-based assessment of individual princes (e.g., contrast John Frederick of Saxony''s documented theological seriousness against more clearly opportunistic converts).',
    'If sincere conviction was widespread among converting princes, the beneficiary designation still holds structurally (they still gained sovereignty regardless of motive) but the moral/interpretive weight of ''extraction'' softens for those cases — the structural transfer occurred regardless of intent, but intent affects how culpable the extraction reading treats the beneficiary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincere_conviction_among_beneficiary_princes, empirical, 'Whether individual princely sincerity affects the moral valence, though not the structural fact, of the sovereignty-transfer this reading identifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.28).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__political_realignment_reading, theater_ratio, 1546, 0.35).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.4).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__political_realignment_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.5).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__political_realignment_reading, base_extractiveness, 1546, 0.58).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__political_realignment_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.7).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1546, reformation_composite__political_realignment_reading, suppression_requirement, 1546, 0.68).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.72).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__political_realignment_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.8).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.1).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the Reformation' per the ε-invariance principle: political_realignment_reading (this file, tangled_rope, ε=0.68, observable = cuius regio eius religio / territorial jurisdictional transfer), theological_fragmentation_reading (separate file, observable = doctrinal incompatibility across denominations), and technological_mediation_reading (separate file, observable = printing-press-driven diffusion velocity). The three share a kernel (reformation_composite) but are NOT the same constraint measured three ways — each has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type, linked here via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
