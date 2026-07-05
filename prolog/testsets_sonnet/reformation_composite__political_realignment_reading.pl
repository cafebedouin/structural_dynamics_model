% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Eius Religio — Territorial Sovereignty via Confessional Differentiation
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This story instantiates the political-realignment reading of the
 *   Reformation kernel: the claim that confessional differentiation is best
 *   understood as an instrument by which emerging territorial states and
 *   urban polities asserted sovereignty against overlapping imperial and
 *   papal jurisdiction. The primary observable is the principle cuius regio
 *   eius religio (Peace of Augsburg, 1555) — the formal linkage of confession
 *   to territorial rule. On this reading, territorial princes and
 *   consolidating nation-states are the structural beneficiaries; the Holy
 *   Roman Emperor and the papal curia are the structural victims, losing
 *   jurisdiction, revenue, and coercive religious authority. This is a
 *   distinct constraint from the theological-fragmentation reading (which
 *   locates the causal engine in genuinely incompatible soteriological
 *   commitments) and the technological-mediation reading (which locates it in
 *   the printing press's capacity to scale local dissent). Each reading has a
 *   different epsilon, a different beneficiary/victim structure, and a
 *   different failure mode; they are linked as siblings in the same kernel
 *   contest, not merged into one composite claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Eius Religio — Territorial Sovereignty via Confessional Differentiation").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '004b61a0-24f2-47db-8c64-d79530477396').
narrative_ontology:cs_kernel_codification('004b61a0-24f2-47db-8c64-d79530477396', distributed).
narrative_ontology:cs_authority_grounding('004b61a0-24f2-47db-8c64-d79530477396', extraction).
narrative_ontology:cs_interpretation_layer_present('004b61a0-24f2-47db-8c64-d79530477396').
narrative_ontology:cs_reading_relation('004b61a0-24f2-47db-8c64-d79530477396', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('004b61a0-24f2-47db-8c64-d79530477396', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('004b61a0-24f2-47db-8c64-d79530477396', foundational, confessional_choice_is_sovereignty_instrument).
narrative_ontology:cs_axiom_status(confessional_choice_is_sovereignty_instrument, holdable).
narrative_ontology:cs_axiom_grounding('004b61a0-24f2-47db-8c64-d79530477396', confessional_choice_is_sovereignty_instrument, conventional).
narrative_ontology:cs_axiom('004b61a0-24f2-47db-8c64-d79530477396', foundational, territorial_ruler_supersedes_transnational_ecclesiastical_authority).
narrative_ontology:cs_axiom_status(territorial_ruler_supersedes_transnational_ecclesiastical_authority, holdable).
narrative_ontology:cs_axiom_grounding('004b61a0-24f2-47db-8c64-d79530477396', territorial_ruler_supersedes_transnational_ecclesiastical_authority, conventional).
narrative_ontology:cs_axiom('004b61a0-24f2-47db-8c64-d79530477396', secondary, doctrinal_content_is_secondary_to_jurisdictional_outcome).
narrative_ontology:cs_axiom_status(doctrinal_content_is_secondary_to_jurisdictional_outcome, holdable).
narrative_ontology:cs_axiom_grounding('004b61a0-24f2-47db-8c64-d79530477396', doctrinal_content_is_secondary_to_jurisdictional_outcome, instrumental).
narrative_ontology:cs_reference_frame('004b61a0-24f2-47db-8c64-d79530477396', corpus_christianum_universal_jurisdiction).
narrative_ontology:cs_drift_state('004b61a0-24f2-47db-8c64-d79530477396', peace_of_augsburg_1555, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('004b61a0-24f2-47db-8c64-d79530477396', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, urban_magistracies).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_emperor).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_curia).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, religious_minorities_within_territories).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, peasant_and_urban_dissenters).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, state_confessional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt or reject the new confession within their territory, using the doctrine of cuius regio eius religio to bind religious identity to political jurisdiction. This lets them expropriate church lands, end tithe remittance to Rome, subordinate clergy appointment to the crown, and refuse imperial religious mandates on theological grounds. Exit from the old order is total; they set the terms of confessional membership for everyone under their rule.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_princes, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_princes, beneficiary).

% Consolidate legal, fiscal, and administrative authority previously shared with or subordinate to a transnational ecclesiastical structure. Religious differentiation supplies a legitimating vocabulary — conscience, doctrine, reform — for what is structurally a claim to exclusive internal sovereignty. The state that controls confession controls courts, marriage law, education, and revenue that once flowed to Rome.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, civilizational, arbitrage, national).

% City councils in free imperial cities and Swiss cantons adopt reform to escape episcopal courts, taxation, and the political leverage of resident bishops or imperial appointees. Confessional choice functions as a charter renegotiation — the city gains self-governance over morals regulation, poor relief, and clerical appointment that previously ran through the diocesan hierarchy.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, urban_magistracies, beneficiary,
    organized, generational, mobile, local).

% Loses the capacity to compel religious uniformity across the Empire and, with it, a key instrument of imperial cohesion. Must repeatedly negotiate settlements (Augsburg, later Westphalia) that formalize princely religious autonomy as a fait accompli. Military suppression is attempted (Schmalkaldic War) but cannot be sustained against a coalition of self-interested territorial rulers; the imperial office is structurally weakened regardless of the war's tactical outcomes.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, payer,
    institutional, generational, constrained, continental).

% Loses tithe revenue, appointment rights (benefices), judicial jurisdiction over marriage and morals, and the political leverage that came from being indispensable to imperial legitimacy. Excommunication and interdict — the traditional coercive tools — lose force once a prince's own subjects no longer depend on Rome for salvation-adjacent goods administered locally by a reformed clergy answerable to the territorial ruler.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_curia, payer,
    institutional, civilizational, trapped, continental).

% Catholics living in newly Protestant territories, or Protestants living in territories that remain or re-become Catholic, bear the cost of a settlement that treats confession as a territorial property right rather than an individual conscience matter. Cuius regio eius religio formally offers the right to emigrate, but in practice this means loss of home, property, and community — the sovereignty gain for princes is a mobility cost for dissenting subjects.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_minorities_within_territories, payer,
    powerless, biographical, trapped, local).

% Radical reformers (Anabaptists, peasants invoking evangelical liberty against feudal dues) sought religious change that threatened the property and authority claims of BOTH princes and the old church. They are suppressed by the same territorial rulers who benefit from magisterial reform — the 1525 Peasants' War is crushed by Protestant and Catholic princes alike, because their claims for social leveling are incompatible with confession being annexed to sovereignty rather than to conscience or economic justice.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, peasant_and_urban_dissenters, excluded,
    powerless, biographical, trapped, local).

% Assess the Reformation's causal structure across political, theological, and technological registers, and note that the political-sovereignty reading explains outcomes (settlement patterns, which territories reformed, timing tied to imperial diets) that a purely doctrinal account struggles to explain on its own.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides emerging territorial states with a legitimating framework to consolidate exclusive jurisdiction over law, revenue, and clerical appointment within defined borders, resolving the coordination problem of a multi-layered medieval authority structure (emperor, pope, local ruler) into a single locus of sovereign decision — genuinely reducing the transaction costs of overlapping and competing jurisdictions.
% TRANSFER_FUNCTION: Moves ecclesiastical revenue (tithes, benefice income, monastic land), judicial authority (marriage, morals, probate courts), and political legitimacy from the papal-imperial axis to territorial rulers and emerging national administrations; the cost is borne by religious minorities forced to conform or emigrate, and by radical reformers whose more egalitarian claims are suppressed by the same princes who benefit from the settlement.
% ABSENT_VOICES: Anabaptists, radical peasant reformers, and religious minorities within each territory are the parties who would object most forcefully — they wanted either genuine freedom of individual conscience or social-economic reform, not a swap of one confessional overlord for another. They are not present in the negotiations at Augsburg or in the princely diets; the settlement is negotiated entirely among the powerful.
% DISAPPEARANCE_RATIONALE: Absent the political-sovereignty function of confessional choice, the map of early modern Europe reorganizes: without cuius regio eius religio, either imperial-papal authority retains coercive religious uniformity (blocking the fiscal and jurisdictional consolidation that funded early modern state-building) or confessional plurality is negotiated on some other basis (individual conscience, guild/city charter, ethnic identity) that would not have delivered princes the same concentrated sovereignty gains. The territorial state system of the following centuries depends on this settlement having occurred in this particular form.
% FOUNDING_PROBLEM: Emerging territorial rulers and city magistracies needed a legitimate basis to refuse imperial and papal authority over taxation, court jurisdiction, and clerical appointment without appearing as naked usurpation of established power — theological reform supplied a conscience-based vocabulary that converted a jurisdictional power grab into a defensible moral stance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by comparative historians and constitutional scholars analyzing the Peace of Augsburg (1555) and Westphalia (1648) as instruments of territorial sovereignty consolidation independent of doctrinal content — notably by historians of state formation (e.g., in the Tilly/Skocpol tradition of political sociology) who treat confessional choice as one lever among several (fiscal, military, administrative) used in the same period for the same consolidation project, and who note that the settlement's core political architecture persisted into the fully secularized Westphalian state system long after the original doctrinal disputes lost most contemporaries' attention. The papal curia and Habsburg imperial apologists, by contrast, continued for generations to assert the founding problem as live (heresy, salvation) — but this corroboration comes from the losing/victim party, not an independent source, and is treated accordingly.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises steadily from 0.28 at Luther's initial protest to 0.68 by Westphalia, tracking the transformation of what began as jurisdictional friction into an entrenched territorial settlement in which princes extract fiscal and legal sovereignty using confession as the vehicle. Theater ratio climbs more modestly (0.15 to 0.42) — a meaningful share of doctrinal argument was genuinely contested theology even on this reading, but an increasing share, particularly after Augsburg, is performative doctrinal justification for what had already become settled political fact. Suppression is non-monotonic, spiking around the Schmalkaldic War (1546) and again sharply during the Thirty Years' War (1618), reflecting periods when the political settlement had to be defended militarily rather than merely administered.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a territorial prince this looks like principled religious reform validating a legitimate sovereignty claim; from the seat of the papal curia or the Emperor the identical structure is extraction of jurisdiction and revenue under theological cover; from the seat of a peasant dissenter it is a betrayal — the reformist rhetoric that seemed to promise liberation was captured by the same territorial authorities against whom the rhetoric was first deployed. The engine's per-seat computation should reproduce this divergence directly from the declared power/exit/beneficiary data without any need for narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial princes and urban magistracies sit at the beneficiary end: they set confessional policy, retain arbitrage-grade exit from imperial/papal authority, and capture the fiscal and jurisdictional transfer directly. The Emperor and the papal curia sit at the target end: both are institutionally powerful but structurally trapped or severely constrained — the papacy in particular has no exit option, since Rome cannot simply relocate its claim to universal jurisdiction. Religious minorities within territories and radical/peasant dissenters are powerless and trapped; their situations differ in that minorities are at least nominally permitted the (costly) exit of emigration under the Augsburg settlement, whereas peasant and radical dissenters are excluded from the settlement's terms altogether and suppressed by the very princes who benefit from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rulers needing a legitimating vocabulary to resist imperial/papal jurisdiction) is dead as a live crisis by the time of Westphalia — territorial sovereignty is by then an accomplished, internationally recognized fact — yet the confessional-territorial apparatus (state churches, established clergy answerable to the crown) persists for centuries afterward as institutional inertia layered on top of an already-completed political consolidation. This is not itself classified here as fully resolved mandatrophy, since active enforcement (suppression of dissenting minorities) continues well past 1648, but the founding_problem_status of 'dead' flags the gap between the settlement's original justificatory function and its much longer institutional afterlife.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Is territorial sovereignty consolidation the primary causal driver of the Reformation''s confessional settlement, or is it a downstream consequence that political actors opportunistically captured after genuinely independent theological dissent had already destabilized ecclesiastical authority?',
    'Comparative case analysis of territories where reform occurred without prior political motive to defy imperial/papal authority (or conversely, cases of political defiance that did not produce lasting confessional change) would help isolate whether political motive precedes, follows, or co-emerges with theological commitment.',
    'If theological dissent is shown to be causally prior and largely autonomous, this reading''s beneficiary/victim structure (princes as extractors of an opportunistically captured movement) is undermined in favor of the theological_fragmentation_reading''s structure, where princes are secondary beneficiaries of an independently generated doctrinal schism rather than its primary architects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Whether political sovereignty-seeking or theological conviction is the primary causal engine of the settlement.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three sibling readings (political, theological, technological) disagree structurally, versus where are they compatible layers of the same causal process?',
    'Formal decomposition of the historical causal chain into technology-enablement (printing press scaling), theological-content (doctrinal incompatibility), and political-capture (sovereignty consolidation) stages, tested against the timing and geography of specific territorial conversions.',
    'If the readings are compatible layers rather than competing primary-cause claims, the kernel''s three-way contest resolves into a compatible multi-causal account rather than genuinely rival readings — though each would retain its own epsilon and beneficiary/victim structure as a distinct constraint under the ε-invariance principle, since the observable each foregrounds still differs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the three kernel readings are genuinely rival or complementary/layered causal accounts.').

omega_variable(
    peasant_exclusion_counterfactual,
    'Would a Reformation settlement centered on individual conscience rather than territorial sovereignty have produced better outcomes for radical/peasant dissenters, or would princely suppression of egalitarian claims have occurred regardless of the confessional settlement''s structure?',
    'Comparative analysis of radical reform movements suppressed by both Protestant and Catholic authorities (Münster, the Peasants'' War) against contexts with different sovereignty structures to assess whether suppression tracks political consolidation independent of confessional content.',
    'If suppression is invariant to the confessional settlement''s structure, the peasant/radical victim classification here reflects a general feature of early modern state formation rather than something specific to the political-sovereignty reading of the Reformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_exclusion_counterfactual, empirical, 'Whether radical dissenter suppression is specific to this reading''s political structure or a general feature of state consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__political_realignment_reading, theater_ratio, 1546, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1546, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.34).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__political_realignment_reading, theater_ratio, 1600, 0.38).
narrative_ontology:measurement_basis(refo_tr_t1600, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.4).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.42).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.28).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.42).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__political_realignment_reading, base_extractiveness, 1546, 0.55).
narrative_ontology:measurement_basis(refo_be_t1546, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.6).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__political_realignment_reading, base_extractiveness, 1600, 0.63).
narrative_ontology:measurement_basis(refo_be_t1600, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.66).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.35).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.48).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1546, reformation_composite__political_realignment_reading, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement_basis(refo_su_t1546, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.58).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__political_realignment_reading, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement_basis(refo_su_t1600, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.75).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.71).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the reformation_composite kernel, decomposed per the ε-invariance principle rather than authored as a single composite claim with an averaged epsilon. political_realignment_reading foregrounds territorial sovereignty consolidation (cuius regio eius religio) as the primary observable, with princes/states as beneficiaries and imperial/papal authority as victims — epsilon rises to 0.68 by 1648. theological_fragmentation_reading foregrounds genuinely incompatible soteriological commitments as generating structurally incompatible denominations, with a different beneficiary/victim structure centered on rival clerical/confessional authorities rather than territorial rulers. technological_mediation_reading foregrounds the printing press's capacity to scale local dissent into continental movement, with a beneficiary/victim structure centered on print capital and information-access asymmetry rather than political sovereignty. All three link to each other via affects_constraints since they describe causally interacting (not merely alternative) aspects of the same historical episode: territorial rulers' sovereignty claims (this story) were enabled by print-scaled doctrinal dissent (technological reading) which was itself substantively shaped by genuine theological disagreement (theological reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
