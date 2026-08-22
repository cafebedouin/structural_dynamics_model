% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Settlement - Enforced Same-Substance Reading
 *   domain: religious/political (late antique ecclesiastical authority)
 *
 * SUMMARY:
 *   Between the Council of Nicaea (325) and the second Council of
 *   Constantinople (553), the claim that Christ is homoousios - of the same
 *   substance - with the Father was transformed from a contested
 *   philosophical formula into the legally enforced religion of the Roman
 *   Empire. This story authors ONE reading of the
 *   nicene_christological_kernel: the same-substance reading as it actually
 *   operated under imperial enforcement. The sibling reading (Christ of
 *   similar substance) is a separate constraint story; per the
 *   epsilon-invariance principle it is not averaged in, hedged, or described
 *   here beyond the family link. The claim/metric relationship is deliberate:
 *   the formula is CLAIMED as tangled_rope from the authoring seat - it
 *   carries a genuine coordination function (interoperable communion
 *   criteria, one baptismal grammar) AND asymmetric extraction (anathema,
 *   exile, confiscation) through the same structure - while the metrics are
 *   authored descriptively from the enforcement record. The engine computes
 *   per-seat types from the structural data; divergence between the claim and
 *   computed output is the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - nicene_episcopal_establishment: Primary beneficiary and co-administrator (institutional/identity_locked) - collects sees, endowments, and confiscated property; chairs the councils that define and enforce
 *   - - theodosian_imperial_administration: Agenda-setter and secondary beneficiary (institutional/mobile) - converts the formula into standing law; could in principle have entrenched a rival formula
 *   - - gothic_arian_churches: Primary target (organized/trapped) - a century-old vernacular church disestablished on contact with imperial enforcement
 *   - - homoian_bishops_and_laity: Primary target (moderate/constrained) - subscribe, flee, or lose office and property
 *   - - north_african_arian_communities: Target (organized/trapped) - former establishment church reversed into proscribed minority after 533
 *   - - imperial_lay_congregations: Excluded seat (powerless/trapped) - bear coerced-allegiance costs with no voice at any defining council
 *   - - ecclesiastical_historians: Analytical observer (analytical/analytical) - sees the full structure across victors' and losers' records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.81).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.84).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Settlement - Enforced Same-Substance Reading").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "religious/political (late antique ecclesiastical authority)").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '08a43703-a6ce-4aa7-aeb8-87eae49c750e').
narrative_ontology:cs_kernel_codification('08a43703-a6ce-4aa7-aeb8-87eae49c750e', fixed_text).
narrative_ontology:cs_authority_grounding('08a43703-a6ce-4aa7-aeb8-87eae49c750e', lineage).
narrative_ontology:cs_interpretation_layer_present('08a43703-a6ce-4aa7-aeb8-87eae49c750e').
narrative_ontology:cs_reading_relation('08a43703-a6ce-4aa7-aeb8-87eae49c750e', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('08a43703-a6ce-4aa7-aeb8-87eae49c750e', foundational, father_son_same_essence_without_interval).
narrative_ontology:cs_axiom_status(father_son_same_essence_without_interval, holdable).
narrative_ontology:cs_axiom_grounding('08a43703-a6ce-4aa7-aeb8-87eae49c750e', father_son_same_essence_without_interval, theological).
narrative_ontology:cs_axiom('08a43703-a6ce-4aa7-aeb8-87eae49c750e', foundational, only_fully_divine_christ_effects_salvation).
narrative_ontology:cs_axiom_status(only_fully_divine_christ_effects_salvation, holdable).
narrative_ontology:cs_axiom_grounding('08a43703-a6ce-4aa7-aeb8-87eae49c750e', only_fully_divine_christ_effects_salvation, theological).
narrative_ontology:cs_axiom('08a43703-a6ce-4aa7-aeb8-87eae49c750e', secondary, conciliar_definition_binds_the_baptized).
narrative_ontology:cs_axiom_status(conciliar_definition_binds_the_baptized, holdable).
narrative_ontology:cs_axiom_grounding('08a43703-a6ce-4aa7-aeb8-87eae49c750e', conciliar_definition_binds_the_baptized, conventional).
narrative_ontology:cs_reference_frame('08a43703-a6ce-4aa7-aeb8-87eae49c750e', apostolic_faith_nicenely_articulated).
narrative_ontology:cs_drift_state('08a43703-a6ce-4aa7-aeb8-87eae49c750e', contemporary_pluralist_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('08a43703-a6ce-4aa7-aeb8-87eae49c750e', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_episcopal_establishment).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, theodosian_imperial_administration).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_churches).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, homoian_bishops_and_laity).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_arian_communities).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, homoousios_formula).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, conciliar_definition_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conciliar and metropolitan bishops of the victorious party receive sees, endowments, and confiscated properties as the settlement consolidates; they chair councils, draft anathemas, and certify which clergy may officiate. A bishop's office and the confession are fused - a bishop who publicly retracts the same-substance formula ceases to be a bishop, so leaving the arrangement and leaving office are the same act.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_episcopal_establishment, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, nicene_episcopal_establishment, agenda_setter).

% Issues the edicts that make one christological formula the empire's legal religion, disqualifies dissenters from office and inheritance, and moves enforcement from ad hoc imperial intervention to standing law. Religious uniformity serves administrative cohesion and court legitimation; the administration could in principle have entrenched a rival formula - it nearly did under Constantius II - so its position is chosen, not fused.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theodosian_imperial_administration, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, theodosian_imperial_administration, beneficiary).

% Received a vernacular scripture and episcopate through Ulfilas' mission in the Homoian tradition; for two centuries their church is the Christianity of the successor kingdoms. After crossing into imperial territory, and after Justinian's reconquest of Africa and Italy, their clergy face deposition, their buildings pass to the Nicene hierarchy, and their congregations face conversion on pain of exclusion.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_churches, payer,
    organized, generational, trapped, continental).

% Hold sees and congregations across the eastern provinces under the compromise formulas; after 380 they must subscribe the Nicene anathema, surrender their churches, accept exile, or relocate beyond the frontier. Conformity preserves livelihood at the price of teaching what many regard as falsehood.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, homoian_bishops_and_laity, payer,
    moderate, biographical, constrained, continental).

% Live a century as the established church of the Vandal kingdom, taxing and excluding the Catholic majority in turn; after the Byzantine reconquest of 533 the polarity reverses - their clergy are exiled, their property transferred, and their worship proscribed.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_arian_communities, payer,
    organized, biographical, trapped, regional).

% Attend, baptize, marry, and bury under whichever formula the current administration enforces; no layperson sits at any council that defines the terms. They learn of doctrinal reversals when their presbyter changes or their church changes hands, and their recourse is conformity, flight, or quiet dissent.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_lay_congregations, excluded,
    powerless, biographical, trapped, continental).

% Reconstruct the settlement from conciliar acts, imperial codes, exile lists, and polemical literature on all sides; they can compare enforcement intensity across reigns and regions and identify where the record survives only through the victors' transcripts.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, nicene_episcopal_establishment).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Specifies one answer, binding across the empire's churches, to the questions that determine communion: who Christ is, therefore what is taught, how baptism is performed, and which bishops are in fellowship. Before the settlement, baptismal formulas and communion tables varied by city and bishop; the settlement makes membership criteria interoperable across the whole church.
% TRANSFER_FUNCTION: Moves sees, endowments, and legal privilege to clergy who subscribe the same-substance formula; moves dissenting clergy into exile and their congregations' buildings into conforming hands; and moves doctrinal allegiance from vernacular and regional traditions toward the imperial-patriarchal center.
% ABSENT_VOICES: Lay congregations had no seat at any council; Gothic and African Arian communities spoke only as defendants after 380; Syriac-speaking frontier churches stood outside the conversation entirely. Their objections survive mainly in hostile transcripts and imperial legal records, which is where the ecclesiastical_historians seat reads them.
% DISAPPEARANCE_RATIONALE: If the enforced settlement vanished overnight, communion boundaries, sees, and endowments would redistribute immediately; the empire would lose the uniformity instrument it legislated in 380; Gothic and African Arian churches would reopen; and the baptismal and liturgical unity of the Mediterranean church would fragment back into regional confessions.
% FOUNDING_PROBLEM: How to confess Christ's saving divinity without collapsing into two gods, and without reducing him to a creature - which would void salvation. Arius' teaching forced the question; Constantine additionally needed a single faith for a reunified empire.
% FOUNDING_PROBLEM_CORROBORATION: Fourth-century sources outside the winning party attest the original problem: Arius' Thalia states the soteriological stakes, and Eusebius of Caesarea's letter to his diocese reports the emperor's uniformity motive. Modern patristics scholars outside ecclesiastical institutions corroborate that the metaphysical question remains live in academic theology, while historiography of the Theodosian code confirms the political rationale expired with the empire that enforced it. No single authority settles which half of the founding problem governs the arrangement's present function.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.81, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the settlement's operation moved sees, endowments, and legal privilege to conforming clergy while exiling and dispossessing dissenters - a transfer decoupled from any service the formula's content performs. Suppression (0.84) is a raw structural property, unscaled by power or scope: persistence depended on standing legal machinery (the Theodosian code's heresy provisions, Justinian's novels), not voluntary assent. Theater ratio (0.41) reflects coerced conformity: subscriptions extracted under penalty generate nominal compliance, visible in the rapid relapse of formerly conforming populations wherever enforcement lapsed. Accessibility collapse (0.62): inside the empire alternatives collapsed almost completely by the sixth century; outside imperial reach (Gothic, Vandal, Visigothic territories) they persisted for generations, keeping the figure below natural-law grade. Resistance (0.68): the settlement met roughly seventy years of open episcopal warfare, urban rioting, and finally armed defense by Arian kingdoms before breaking. CYCLICAL PATTERN: the series oscillates twice before locking - enforcement swings follow imperial successions (Constantius II's Homoian ascendancy produces the 356 trough in this reading's extractive operation; Julian's recall of all exiles reopens the contest in 361; Theodosius locks the ratchet in 380; the Arian successor kingdoms contract the settlement's reach 476-529; Justinian restores and exceeds it). Until 380 the oscillation itself functioned as intermittent reinforcement: each reversal taught clergy that position, not argument, determined survival. All three series share one nine-point grid; the base_properties scalars report the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal establishment's seat the settlement is truth's victory and the church's unity: the same structure that deposes a Homoian bishop certifies the sacrament that saves. From the Gothic and Homoian seats the identical structure is the dismantling of their churches - their vernacular scripture, their episcopate, their buildings. SAME-LEVEL DYNAMICS: Nicene and Homoian bishops held nominally equal office, education, and standing; what differentiated them was proximity to the court and consecration timing - a Homoian bishop under Constantius II wielded the machinery that ruined his Nicene counterpart, and the positions reversed after 380. Exit options diverge accordingly: the establishment's exit is identity_locked (office and confession are fused - retract the formula and cease to be a bishop), while Homoian clergy faced constrained exit (subscribe, flee beyond the frontier, or lose everything). INTER-INSTITUTIONAL: the imperial administration and the episcopal hierarchy are distinct institutional principals that jointly administer the settlement; the administration's interest is cohesion and could in principle have entrenched a rival formula, while the hierarchy's interest is the formula's content itself - the alliance is instrumental on one side and constitutive on the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to near-beneficiary directionality: the episcopal establishment collects sees, endowments, and confiscated property (d near 0.0); the imperial administration collects administrative cohesion and legitimation (low d, slightly raised by its exposure to the civil strife the policy risked). Victim declarations map to near-target directionality: Gothic Arian churches, Homoian clergy, and African Arian communities bear deposition, dispossession, and forced conversion (d near 1.0), with trapped exit pushing them to the full-target end. Imperial lay congregations sit nearer symmetric - they received genuine liturgical coordination and bore coerced-allegiance costs diffusely. The engine derives these values from the declared structure; no directionality overrides are needed because role, power, and exit already differentiate every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Calling the settlement pure extraction would erase the real coordination function: before Nicaea, communion criteria varied by city, baptismal grammar was non-interoperable, and the soteriological question (whether a creature saves) was genuinely unresolved; the settlement solved a real collective-action problem for the church. Calling it pure coordination would erase the asymmetric transfer: the same structure that coordinated communion moved property and office along doctrinal lines under penal sanction. MANDATROPHY: the founding problem is contested - the metaphysical question (how Christ saves without ditheism) remains live for the tradition's believers and in academic theology, while the political problem the enforcement answered (imperial religious uniformity) died with the empire. Because the status is contested rather than dead, the mismatch consumer finds no zombie flag - correctly: the arrangement is not maintained by inertia alone; a concentrated beneficiary (the episcopal establishment) actively maintains it, which also bars the degraded-inertial reading by definition, since a seat profits enough to defend it. Identity-lock note: the establishment's lock is institutional - the organization has become its confession; if that fusion broke (clergy treating the formula as revisable policy rather than constitutive truth), the enforcement coalition would fragment immediately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta_homoiousios,
    'This constraint instantiates the homoousios reading of the nicene_christological_kernel; what would the sibling homoiousios_reading change structurally if it had been the entrenched formula?',
    'Author the sibling story and compare victim sets, enforcement intensity, and kernel precision: ''of similar substance'' leaves a wider semantic range, plausibly lowering the precision an enforcement apparatus must maintain and shifting the victim set from Homoian and Gothic communities to whichever stricter party the vaguer formula excludes.',
    'Under the sibling reading the beneficiary seat may remain the episcopal establishment while the victim set and epsilon shift; the foreclosure edge and axiom set differ, and per-seat classifications must be recomputed on the sibling''s own structural data rather than imported from this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_delta_homoiousios, conceptual, 'Committer-frame delta: what the sibling reading of the same kernel would change structurally.').

omega_variable(
    doctrine_vs_enforcement_decomposition,
    'Is the measured extraction a property of the same-substance claim itself, or of the imperial enforcement apparatus that captured it after 380?',
    'Compare enforcement intensity where the claim held without imperial monopoly (pre-380 western synods, Gothic Homoian counter-establishments) against the post-Thessalonica legal regime; if extraction tracks the apparatus rather than the doctrine, decompose into a doctrinal coordination constraint plus a separate enforcement constraint linked by network edges.',
    'Decomposition would move this story''s epsilon toward the coordination floor and assign the excess to a distinct enforcement-layer constraint; failing to decompose would attribute imperial rent-seeking to the creed''s content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_enforcement_decomposition, conceptual, 'Whether the epsilon belongs to the doctrine or to the regime that enforced it.').

omega_variable(
    coerced_conformity_sincerity_ratio,
    'How much post-380 subscription reflects conviction versus coercion, and how does that ratio vary by region and decade?',
    'Prosopography of subscribing bishops (subsequent conduct and writings), exile and martyrdom records, and archaeological evidence distinguishing church seizure from voluntary transfer.',
    'A high coerced share supports the elevated theater ratio and predicts rapid relapse wherever enforcement lapsed (as in the 476-529 Arian kingdoms); a low coerced share supports treating the settlement as durable consensus rather than enforced performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_conformity_sincerity_ratio, empirical, 'Sincerity composition of the conforming population behind the theater-ratio estimate.').

omega_variable(
    counterfactual_settlement_floor,
    'Would some christological settlement have been enforced regardless of content - that is, how much of the suppression is attributable to the empire''s generic demand for religious uniformity rather than to this formula''s specificity?',
    'Compare enforcement profiles across successive imperial doctrinal campaigns (Nicaea, Constantinople, Chalcedon, the Monothelete controversies): if the machinery recurs at constant intensity across changing content, the uniformity demand is the invariant and the formula is substitutable.',
    'If substitutable, the formula-level constraint inherits most of its suppression from reusable imperial infrastructure, and its own specific extraction is smaller than the aggregate record suggests - moving the formula-level classification toward the coordination end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_settlement_floor, empirical, 'How much suppression is formula-specific versus inherited uniformity machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 553).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t341, nicene_christological_kernel__homoousios_reading, theater_ratio, 341, 0.24).
narrative_ontology:measurement_basis(nice_tr_t341, observed).
narrative_ontology:measurement(nice_tr_t356, nicene_christological_kernel__homoousios_reading, theater_ratio, 356, 0.2).
narrative_ontology:measurement_basis(nice_tr_t356, observed).
narrative_ontology:measurement(nice_tr_t361, nicene_christological_kernel__homoousios_reading, theater_ratio, 361, 0.16).
narrative_ontology:measurement_basis(nice_tr_t361, observed).
narrative_ontology:measurement(nice_tr_t380, nicene_christological_kernel__homoousios_reading, theater_ratio, 380, 0.26).
narrative_ontology:measurement_basis(nice_tr_t380, observed).
narrative_ontology:measurement(nice_tr_t415, nicene_christological_kernel__homoousios_reading, theater_ratio, 415, 0.31).
narrative_ontology:measurement_basis(nice_tr_t415, observed).
narrative_ontology:measurement(nice_tr_t481, nicene_christological_kernel__homoousios_reading, theater_ratio, 481, 0.28).
narrative_ontology:measurement_basis(nice_tr_t481, observed).
narrative_ontology:measurement(nice_tr_t529, nicene_christological_kernel__homoousios_reading, theater_ratio, 529, 0.38).
narrative_ontology:measurement_basis(nice_tr_t529, observed).
narrative_ontology:measurement(nice_tr_t553, nicene_christological_kernel__homoousios_reading, theater_ratio, 553, 0.41).
narrative_ontology:measurement_basis(nice_tr_t553, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.38).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t341, nicene_christological_kernel__homoousios_reading, base_extractiveness, 341, 0.45).
narrative_ontology:measurement_basis(nice_be_t341, observed).
narrative_ontology:measurement(nice_be_t356, nicene_christological_kernel__homoousios_reading, base_extractiveness, 356, 0.28).
narrative_ontology:measurement_basis(nice_be_t356, observed).
narrative_ontology:measurement(nice_be_t361, nicene_christological_kernel__homoousios_reading, base_extractiveness, 361, 0.33).
narrative_ontology:measurement_basis(nice_be_t361, observed).
narrative_ontology:measurement(nice_be_t380, nicene_christological_kernel__homoousios_reading, base_extractiveness, 380, 0.74).
narrative_ontology:measurement_basis(nice_be_t380, observed).
narrative_ontology:measurement(nice_be_t415, nicene_christological_kernel__homoousios_reading, base_extractiveness, 415, 0.79).
narrative_ontology:measurement_basis(nice_be_t415, observed).
narrative_ontology:measurement(nice_be_t481, nicene_christological_kernel__homoousios_reading, base_extractiveness, 481, 0.66).
narrative_ontology:measurement_basis(nice_be_t481, observed).
narrative_ontology:measurement(nice_be_t529, nicene_christological_kernel__homoousios_reading, base_extractiveness, 529, 0.83).
narrative_ontology:measurement_basis(nice_be_t529, observed).
narrative_ontology:measurement(nice_be_t553, nicene_christological_kernel__homoousios_reading, base_extractiveness, 553, 0.81).
narrative_ontology:measurement_basis(nice_be_t553, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.32).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t341, nicene_christological_kernel__homoousios_reading, suppression_requirement, 341, 0.4).
narrative_ontology:measurement_basis(nice_su_t341, observed).
narrative_ontology:measurement(nice_su_t356, nicene_christological_kernel__homoousios_reading, suppression_requirement, 356, 0.18).
narrative_ontology:measurement_basis(nice_su_t356, observed).
narrative_ontology:measurement(nice_su_t361, nicene_christological_kernel__homoousios_reading, suppression_requirement, 361, 0.22).
narrative_ontology:measurement_basis(nice_su_t361, observed).
narrative_ontology:measurement(nice_su_t380, nicene_christological_kernel__homoousios_reading, suppression_requirement, 380, 0.76).
narrative_ontology:measurement_basis(nice_su_t380, observed).
narrative_ontology:measurement(nice_su_t415, nicene_christological_kernel__homoousios_reading, suppression_requirement, 415, 0.8).
narrative_ontology:measurement_basis(nice_su_t415, observed).
narrative_ontology:measurement(nice_su_t481, nicene_christological_kernel__homoousios_reading, suppression_requirement, 481, 0.6).
narrative_ontology:measurement_basis(nice_su_t481, observed).
narrative_ontology:measurement(nice_su_t529, nicene_christological_kernel__homoousios_reading, suppression_requirement, 529, 0.86).
narrative_ontology:measurement_basis(nice_su_t529, observed).
narrative_ontology:measurement(nice_su_t553, nicene_christological_kernel__homoousios_reading, suppression_requirement, 553, 0.84).
narrative_ontology:measurement_basis(nice_su_t553, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Nicene settlement' conflates two structurally distinct claims: the same-substance formula (this story) and the similar-substance formula (homoiousios_reading). They differ in kernel precision, victim set, and enforcement burden, so each is authored as its own epsilon-invariant constraint and linked here per the constraint-family rule. The upstream claim (full deity, higher soteriological stakes) supplied the enforcement precedent that the sibling's brief ascendancy reused; the sibling story should carry the reciprocal edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
