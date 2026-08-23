% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Reformation as Political Authority-Swap (Political-Swap Reading)
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the political_swap_reading of the
 *   reformation_event_boundary kernel: the Reformation enters history as an
 *   authority transfer from Rome to territorial princes, in which theological
 *   dispute supplied the legitimation for a pre-existing political and fiscal
 *   objective. The epsilon referent is the standing arrangement under contest
 *   — the Reformation-era transfer-and-settlement order as this reading sees
 *   it — never the theological or composite alternatives, which are separate
 *   constraints in the same family. The claim and the metrics are independent
 *   authored facts: the reading itself asserts a genuine coordination core
 *   (the territorial settlement dissolved a chronically conflict-generating
 *   overlapping-jurisdiction order and stabilized at Westphalia) alongside
 *   asymmetric, actively enforced extraction (confiscated property, broken
 *   jurisdiction, crushed dissent), and the metrics are authored to describe
 *   that mixed operation rather than to reconcile with any predicted engine
 *   output. Suppression is authored as a raw structural property and is not
 *   scaled; only extractiveness is scaled by directionality and scope in the
 *   engine's computation. KEY AGENTS (by structural relationship):
 *
 * KEY AGENTS:
 *   - - secular_rulers_princes: Primary beneficiary and agenda-setter (powerful/arbitrage) — chose territorial confessions, administered confiscated estates, collected the transferred revenues, and enforced the settlement by arms when challenged
 *   - - catholic_church_hierarchy: Primary target (institutional/trapped) — lost annates, courts, appointments, and whole classes of property across half of Latin Christendom with no route of recovery
 *   - - monastic_orders: Target (powerless/trapped) — dissolved corporations whose accumulated property passed to lay owners
 *   - - german_peasantry: Target (powerless/trapped) — bore the armed suppression of 1525 and the continuing tithe burden under new masters
 *   - - radical_reformers_anabaptists: Target (powerless/trapped) — exterminated by both confessional camps for refusing princely-controlled religion of either kind
 *   - - magisterial_reformers: Dual-positioned (moderate/identity_locked) — gained protection, office, and salary; lost ministerial autonomy to princely consistories
 *   - - imperial_city_magistrates: Secondary beneficiary (organized/constrained) — absorbed urban church property into civic treasuries within bounds set by princely settlements
 *   - - analytical_historians: Analytical observer (analytical/analytical) — sees the full structure and the sibling readings without occupying any seat in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.68).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Authority-Swap (Political-Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'f042bc13-b704-49ca-afc0-e5d0d4aad670').
narrative_ontology:cs_kernel_codification('f042bc13-b704-49ca-afc0-e5d0d4aad670', distributed).
narrative_ontology:cs_authority_grounding('f042bc13-b704-49ca-afc0-e5d0d4aad670', expertise).
narrative_ontology:cs_interpretation_layer_present('f042bc13-b704-49ca-afc0-e5d0d4aad670').
narrative_ontology:cs_reading_relation('f042bc13-b704-49ca-afc0-e5d0d4aad670', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('f042bc13-b704-49ca-afc0-e5d0d4aad670', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('f042bc13-b704-49ca-afc0-e5d0d4aad670', foundational, theology_as_legitimation_instrument).
narrative_ontology:cs_axiom_status(theology_as_legitimation_instrument, holdable).
narrative_ontology:cs_axiom_grounding('f042bc13-b704-49ca-afc0-e5d0d4aad670', theology_as_legitimation_instrument, empirically_contingent).
narrative_ontology:cs_axiom('f042bc13-b704-49ca-afc0-e5d0d4aad670', secondary, territorial_authority_supersedes_canonical_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_authority_supersedes_canonical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('f042bc13-b704-49ca-afc0-e5d0d4aad670', territorial_authority_supersedes_canonical_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('f042bc13-b704-49ca-afc0-e5d0d4aad670', westphalian_territorial_settlement).
narrative_ontology:cs_drift_state('f042bc13-b704-49ca-afc0-e5d0d4aad670', contemporary_revisionist_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f042bc13-b704-49ca-afc0-e5d0d4aad670', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, imperial_city_magistrates).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, magisterial_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_orders).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, german_peasantry).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, radical_reformers_anabaptists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, magisterial_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Territorial princes and electors decided which confession their lands would follow, administered the former church estates through newly created consistory courts and treasury chambers, and collected the tithes, rents, and fees that previously flowed to Rome and the monasteries. They convened the diets, signed the settlements, and called out the armies when the arrangement was challenged. Their confessional choices tracked dynastic and fiscal advantage closely enough that several switched allegiance when inheritance or alliance made it profitable; movement within the arrangement was cheap, and leaving it was never on the table.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers_princes, agenda_setter,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_rulers_princes, beneficiary).

% Councils of imperial cities took over urban parish property, hospitals, and endowments, redirecting income to civic treasuries and poor-relief systems under municipal control. They gained a durable revenue base and patronage power, but sat inside the Empire's legal order and were bound by settlements their princes negotiated; their room to maneuver was real but bounded.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, imperial_city_magistrates, beneficiary,
    organized, generational, constrained, regional).

% Theologians such as Luther and Melanchthon supplied the doctrinal justifications the princes enacted, and in return received protection, university chairs, and superintendency of the new territorial churches. The same princes who sheltered them subordinated them: visitations, consistory ordinances, and catechism mandates placed their ministries under state supervision, and their social-radical allies were abandoned to the armies in 1525. Recanting would have destroyed the movement and their life's work; they could neither walk away from what they had built nor fully steer it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, magisterial_reformers, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, magisterial_reformers, payer).

% The hierarchy lost annates, dispensation fees, and jurisdictional courts across whole kingdoms, and saw bishoprics, abbeys, and parishes transferred to lay lords with no compensation and no route of recovery. Diplomatic protest and counter-reformation campaigning failed to reverse the transfers; within Protestant territories its personnel were expelled, its offices abolished, and its liturgy criminalized. Its institutional scale survived the era, but the specific assets and authority taken were gone permanently.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Abbeys and convents were dissolved wholesale and their lands sold or granted to noble families; monks and nuns were pensioned, dispersed, or, where they resisted, expelled. Communities that had accumulated property over centuries had no legal standing to contest the seizures and no armed recourse; individual members sometimes continued religious life quietly, but the corporate bodies were extinguished.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_orders, payer,
    powerless, biographical, trapped, continental).

% Villagers paid tithes to princely treasuries instead of Roman collectors, and when in 1525 they invoked reformist arguments against dues and landlord privileges, their risings were crushed by allied princely armies at a cost of roughly a hundred thousand lives. Thereafter they worshiped in whatever confession their ruler had chosen, on pain of penalty, with no migration option that did not mean abandoning land and community.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, german_peasantry, payer,
    powerless, immediate, trapped, regional).

% Baptist and spiritualist groups rejected both the old church and the new princely churches, refusing oaths, tithes, and infant baptism. Both confessional camps hunted them: thousands were executed in the 1530s and after, and surviving congregations went underground or emigrated. Their elimination was pursued with equal energy by rulers on each side of the confessional divide, which places their fate in this account rather than on either confession's martyr-list alone.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, radical_reformers_anabaptists, payer,
    powerless, immediate, trapped, continental).

% Historians working on the period weigh the political, theological, and social strands against one another, compare the rival interpretive frameworks, and can see the full structure — who moved first, who paid, who signed — without occupying any seat in it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, analytical_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_rulers_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement replaced an overlapping-jurisdiction order — papal, imperial, and local authorities claiming the same territories, courts, and revenues — with unitary territorial governance in which each sovereign regulated religion, law, and finance within a bounded territory; the Augsburg and Westphalian settlements generalized that formula into a stable interstate order that ended decades of confessional warfare by freezing the division.
% TRANSFER_FUNCTION: Moved land, rental income, tithe streams, and judicial appointments from the Catholic Church's corporations (monasteries, bishoprics, chapters, parishes) to princes and city councils; moved religious allegiance and clerical obedience from a transnational center to territorial sovereigns; and shifted the operating cost of poor relief and education onto reused church endowments under lay control.
% ABSENT_VOICES: No peasant, townsman, or woman sat at Augsburg or Westphalia; the tables seated princes and imperial estates only. The laity whose worship was reassigned, the villagers who died in 1525 asking that reformist principles be applied to social dues, the Anabaptists excluded by both camps, the displaced women religious, and the Catholic minorities left inside Protestant lands would all have objected; they appear in the record chiefly as petitioners, rebels, or casualties.
% DISAPPEARANCE_RATIONALE: Every territorial border, state-church structure, and property title in Central and Western Europe descends from this settlement; overnight removal would unravel princely landholdings, national church establishments, and the sovereignty formula of the entire Westphalian order.
% FOUNDING_PROBLEM: A transnational church claimed taxes, courts, and appointment rights inside emerging territorial states, generating chronic fiscal and jurisdictional conflict between Rome and secular rulers who could not govern coherently under overlapping claims.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Catholic and conciliarist historiography itself documents the late-medieval jurisdictional crisis (grievance literature, pragmatic sanctions, and conciliar theory all predate Luther), and diplomatic historians independently attest that the Westphalian settlement closed the universal-jurisdiction question. No serious party proposes restoring the pre-1517 overlapping order.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.68) because the transfer moved productive property, recurring revenue, and jurisdictional authority from one set of corporate owners to another with no compensation, and because the gains were frozen by treaty rather than returned. Suppression is higher (0.78) because the arrangement's persistence depended on closing alternatives structurally: dissent criminalized, Catholic office abolished in Protestant lands, property transfer made legally irrevocable — a closure that persisted even in peace phases. Theater is moderate (0.52): from this reading's lights a large share of visible confessional activity (disputations, confessional documents, visitation rhetoric) functioned as legitimation for moves whose timing tracked fiscal and dynastic opportunity, but the reading does not claim the theology was empty — only that it was not the driver. Accessibility collapse is low-moderate (0.40): alternative framings of the period survive and compete rather than collapsing. Resistance is substantial (0.62): confessional historiography, the Catholic counter-narrative, and the composite school all actively contest the reduction of theology to rationalization. CYCLICAL PATTERN: the series oscillate rather than drift monotonically — open enforcement phases (1525, 1546, 1618-1630) alternate with legalized-consolidation phases (1555, 1577, 1648). The oscillation is partly an extraction mechanism in itself: each reopening of the settlement question (Schmalkaldic War, Thirty Years' War) created fresh seizure and restitution opportunities, and each treaty froze accumulated gains at a higher baseline than the last, so extractiveness ratchets across cycles while deployed enforcement sawtooths. Base_properties characterize the arrangement's operating profile across the whole interval; the terminal suppression_requirement value (0.44) is lower than the scalar suppression (0.78) because Westphalia demilitarized enforcement while leaving the structural closure of alternatives — irrevocable titles, disenfranchised Catholic minorities, criminalized dissent — fully intact. All three series share one nine-point grid so the engine samples a complete row at every examined year.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently, and the structural data is authored to produce that divergence. From the princes' position the arrangement is an order they built, funded, and defended — a coordination achievement they experience as sovereignty realized. From the hierarchy's and the monasteries' position the same structure is uncompensated dispossession enforced by arms and statute. The reformer seat splits internally: material benefit and institutional protection on one side, subordination of ministry to state supervision on the other, which is why a directionality override is declared for it. The observer seat sees all of this plus the fact that two rival readings of the same span would redistribute every role in the ledger.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: princes and city magistrates sit near the beneficiary pole (arbitrage-grade exit — several rulers switched confessional allegiance when dynastic advantage made it cheap — pushes them further toward subsidy), while the hierarchy, monastic orders, peasantry, and radicals sit near the full-target pole (trapped exit, no recovery route for seized assets, no tolerated dissent). The magisterial reformers carry an explicit override (moderate, d=0.5): the derivation would read them as near-pure beneficiaries from the beneficiary declaration alone, but the consistory system made them salaried instruments of princely administration — their autonomy loss offsets their gains, placing them near symmetric. Scope amplification applies modestly: the arrangement operated at continental scale, where verification of terms was hardest and enforcement most expensive.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying from this reading's own lights guards against two opposite errors. A pure-extraction verdict would erase the real coordination achievement: the overlapping-jurisdiction order it replaced generated chronic fiscal and legal conflict, and the territorial formula — generalized at Westphalia — produced a workable interstate settlement that ended confessional warfare. A pure-coordination verdict would launder armed dispossession as the price of order. The tangled_rope structure holds both halves: genuine coordination function, asymmetric extraction, active enforcement. On obsolescence: the founding problem (universalist versus territorial jurisdiction) was closed at Westphalia, so founding_problem_status is authored dead while the arrangement demonstrably rearranges the world — the R5 mismatch is recorded honestly rather than smoothed over, and the receipt surface explains why nothing corrected it: the gains accrued to exactly the seats that wrote the treaties, and reversal was priced at general war (the Edict of Restitution attempt is the observed cost quote).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformation_kernel_reading_contest,
    'This constraint instantiates the political_swap_reading of the reformation_event_boundary kernel; would instantiating the theological_climb_reading or the composite_overdetermination_reading instead yield a different epsilon, beneficiary/victim structure, and classification for the same historical span?',
    'Author the sibling readings as separate constraint stories over the same interval and shared grid where possible, then compare computed classifications; divergence in epsilon and victim sets locates where the readings actually disagree.',
    'Under the theological_climb_reading the hierarchy''s losses read as schism-cost of a doctrinally necessary separation rather than asset-seizure extraction, and the princes shift from beneficiary toward defensive enforcer; under the composite reading no single victim/beneficiary polarity holds and attribution confidence drops across every seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_kernel_reading_contest, conceptual, 'Committer structure: this file is one reading of a three-reading kernel; the siblings are separate constraints, not parameters of this one.').

omega_variable(
    theology_causal_status_underdetermination,
    'Can the causal status of theology in the break — post-hoc rationalization versus operative driver — be settled from surviving evidence, or is it permanently underdetermined by the single, unrepeated character of the event?',
    'Comparative archival timing analysis: sequencing of princely moves against church property relative to doctrinal commitments; confiscation-without-doctrinal-change cases (Henry VIII before 1534, Gustav Vasa in Sweden); reformer correspondence showing initiative flowing upward to princely patrons.',
    'If theology proves causally load-bearing, epsilon falls (doctrinal activity stops functioning as cover and becomes functional coordination) and the classification drifts toward rope-like; if the rationalization premise is confirmed, the current high-theater tangled_rope profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_causal_status_underdetermination, empirical, 'Whether this reading''s foundational premise is empirically decidable at all.').

omega_variable(
    swap_settlement_decomposition,
    'Does the 1517-1648 span bundle two structurally distinct constraints — the confiscation/transfer phase (to roughly 1555) and the stabilization/settlement phase (Augsburg to Westphalia) — whose epsilon values differ enough to violate epsilon-invariance under one label?',
    'Decomposition test: author the confiscation phase and the settlement phase as separate stories; if their epsilon values diverge widely, split the family and link the parts via network edges.',
    'Splitting would likely raise epsilon for the confiscation phase (open seizure, maximal victim exposure) and lower it for the settlement phase (genuine interstate coordination), moving the latter toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swap_settlement_decomposition, conceptual, 'Possible two-constraint structure hidden inside the extended periodization this reading adopts.').

omega_variable(
    victim_attribution_framing,
    'Do the peasantry''s and the radicals'' losses belong to this constraint''s victim ledger, or to adjacent social conflicts (landlord reaction, urban oligarchic consolidation) that merely coincided with the authority transfer?',
    'Counterfactual tracing: whether the same suppression would have occurred absent the transfer — for instance, whether the fiscal apparatus built to administer confiscated assets is what financed and motivated the crushing of the 1525 risings.',
    'Excluding peasant and radical deaths from the victim set lowers measured extraction and softens the payer-seat classifications; including them, as this reading does, keeps epsilon high and the enforcement asymmetry visible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_attribution_framing, conceptual, 'Framing-dependence of the victim set under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.28).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__political_swap_reading, theater_ratio, 1525, 0.44).
narrative_ontology:measurement(refo_tr_t1534, reformation_event_boundary__political_swap_reading, theater_ratio, 1534, 0.52).
narrative_ontology:measurement(refo_tr_t1546, reformation_event_boundary__political_swap_reading, theater_ratio, 1546, 0.56).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.59).
narrative_ontology:measurement(refo_tr_t1577, reformation_event_boundary__political_swap_reading, theater_ratio, 1577, 0.62).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.58).
narrative_ontology:measurement(refo_tr_t1630, reformation_event_boundary__political_swap_reading, theater_ratio, 1630, 0.55).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.52).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.34).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__political_swap_reading, base_extractiveness, 1525, 0.61).
narrative_ontology:measurement(refo_be_t1534, reformation_event_boundary__political_swap_reading, base_extractiveness, 1534, 0.69).
narrative_ontology:measurement(refo_be_t1546, reformation_event_boundary__political_swap_reading, base_extractiveness, 1546, 0.73).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.67).
narrative_ontology:measurement(refo_be_t1577, reformation_event_boundary__political_swap_reading, base_extractiveness, 1577, 0.65).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.7).
narrative_ontology:measurement(refo_be_t1630, reformation_event_boundary__political_swap_reading, base_extractiveness, 1630, 0.79).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.22).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__political_swap_reading, suppression_requirement, 1525, 0.56).
narrative_ontology:measurement(refo_su_t1534, reformation_event_boundary__political_swap_reading, suppression_requirement, 1534, 0.63).
narrative_ontology:measurement(refo_su_t1546, reformation_event_boundary__political_swap_reading, suppression_requirement, 1546, 0.74).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.58).
narrative_ontology:measurement(refo_su_t1577, reformation_event_boundary__political_swap_reading, suppression_requirement, 1577, 0.55).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.76).
narrative_ontology:measurement(refo_su_t1630, reformation_event_boundary__political_swap_reading, suppression_requirement, 1630, 0.83).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Reformation' conflates three structurally distinct claims about one event-span; per the epsilon-invariance principle each is authored as its own constraint with its own epsilon, victim set, and periodization. This file carries the political-swap instantiation (epsilon high, hierarchy as victim, princes as beneficiaries, boundary extended to 1648). The theological_climb instantiation shares the span but reassigns causal credit and victim status; the composite instantiation refuses single-driver attribution altogether. Edges here link the family for contamination-propagation analysis; no upstream/downstream ordering is asserted between rival readings, since their competition is evidential rather than derivational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
