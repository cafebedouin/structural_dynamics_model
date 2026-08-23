% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousios Doctrinal Boundary - Similar-Substance Reading of the Nicene Christological Kernel
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   Mid-fourth-century Christendom needed a working answer to what the Son is
 *   relative to the Father: Arian teaching denied the Son's true divinity,
 *   while Nicaea's identical-substance formula (325) struck much of the Greek
 *   East as collapsing the two into one person. The arrangement under study
 *   is the similar-substance settlement: a creedal boundary declaring Christ
 *   like the Father in substance - genuinely divine, ontologically distinct -
 *   policed by synods and pressed intermittently by imperial courts, reaching
 *   its apex around the Synod of Ancyra (358) and dissolving after
 *   Constantinople (381) when the substance/person distinction let the
 *   identical-substance reading absorb its concerns. This is ONE reading of
 *   the nicene_christological_kernel; its sibling (identical-substance) is
 *   authored separately and linked via network.affects_constraints. The
 *   family decomposes here because the two readings assign different victim
 *   sets, different enforcement intensities, and therefore different stable
 *   epsilon values to what colloquial history flattens into 'the Arian
 *   controversy.'
 *
 * KEY AGENTS:
 *   - homoiousian_synod_leadership: agenda-setter (organized/constrained) - drafts the formula, collects subscriptions, negotiates with the court; the seat the arrangement's gains accrue to
 *   - imperial_uniformity_administration: payer and enforcer (institutional/constrained) - wants one confession, receives perpetual revision
 *   - regional_episcopal_churches: primary beneficiary (organized/constrained) - teaches in inherited idioms under a tolerant boundary
 *   - anti_sabellian_eastern_theologians: beneficiary (moderate/identity_locked) - the reading is their exegetical home
 *   - pro_nicene_confessing_bishops: primary payer (powerful/identity_locked) - deposed and exiled rather than sign
 *   - ordinary_congregations: secondary beneficiary and diffuse payer (powerless/trapped)
 *   - arian_subordinationist_party: excluded (organized/constrained) - the boundary's negative reference point
 *   - ecclesiastical_historians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.56).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.58).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousios Doctrinal Boundary - Similar-Substance Reading of the Nicene Christological Kernel").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'ac1683d1-e052-4499-9fe3-9eda01441652').
narrative_ontology:cs_kernel_codification('ac1683d1-e052-4499-9fe3-9eda01441652', formalized).
narrative_ontology:cs_authority_grounding('ac1683d1-e052-4499-9fe3-9eda01441652', lineage).
narrative_ontology:cs_interpretation_layer_present('ac1683d1-e052-4499-9fe3-9eda01441652').
narrative_ontology:cs_reading_relation('ac1683d1-e052-4499-9fe3-9eda01441652', nicene_christological_kernel__homoousios_reading, forecloses).
narrative_ontology:cs_axiom('ac1683d1-e052-4499-9fe3-9eda01441652', foundational, ontological_distinction_of_father_and_son_binding).
narrative_ontology:cs_axiom_status(ontological_distinction_of_father_and_son_binding, holdable).
narrative_ontology:cs_axiom_grounding('ac1683d1-e052-4499-9fe3-9eda01441652', ontological_distinction_of_father_and_son_binding, theological).
narrative_ontology:cs_axiom('ac1683d1-e052-4499-9fe3-9eda01441652', secondary, monotheistic_clarity_via_paternal_primacy).
narrative_ontology:cs_axiom_status(monotheistic_clarity_via_paternal_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ac1683d1-e052-4499-9fe3-9eda01441652', monotheistic_clarity_via_paternal_primacy, theological).
narrative_ontology:cs_reference_frame('ac1683d1-e052-4499-9fe3-9eda01441652', apostolic_paternal_monarchy_norm).
narrative_ontology:cs_drift_state('ac1683d1-e052-4499-9fe3-9eda01441652', post_constantinopolitan_settlement, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ac1683d1-e052-4499-9fe3-9eda01441652', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_episcopal_churches).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, anti_sabellian_eastern_theologians).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, ordinary_congregations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, pro_nicene_confessing_bishops).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_uniformity_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, ordinary_congregations).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, paternal_monarchy_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, anti_sabellian_personal_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene regional synods, notably Ancyra in 358, to draft and defend a creedal formula describing the Son as like the Father in substance. Draft the anathemas fencing the formula on both sides, negotiate acceptable wording with imperial envoys, and collect subscription from other bishops. Their standing depends on the formula remaining the settlement point; abandoning it means surrendering the movement they lead, while defending it consumes their political capital with both the court and rival bishops.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousian_synod_leadership, agenda_setter,
    organized, biographical, constrained, continental).

% The emperor's religious officials need a single confession to administer - for civic ceremony, episcopal appointments, and peace between sees. They endorse whichever formula promises the broadest subscription and press it with exile and appointment leverage, but each imposed wording unravels as bishops regroup around rival readings, leaving uniformity permanently out of reach. They cannot exit the religious question: governing the empire requires managing its church.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_uniformity_administration, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, imperial_uniformity_administration, agenda_setter).

% Sees outside the imperial capitals keep discretion to teach the Father-Son relation in their own inherited idioms - derivation language from Proverbs, local baptismal creeds - so long as they reject Arian denial of the Son's divinity. The boundary protects them from forced subscription to a single metaphysical term while still fencing out subordinationism; their recurring cost is pressure to attend councils and sign whatever wording currently circulates.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_episcopal_churches, beneficiary,
    organized, generational, constrained, continental).

% Teachers formed in the Eastern exegetical tradition who read the Son's deity as genuinely derived from the Father's monarchy. The similar-substance reading is their intellectual home: it affirms Christ's true divinity while keeping the Father as sole source. Leaving the position would mean discarding the exegetical inheritance that constitutes their vocation; they defend it in treatises and synodal speeches even when it costs them imperial favor.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, anti_sabellian_eastern_theologians, beneficiary,
    moderate, generational, identity_locked, continental).

% Bishops - Athanasius foremost, with Western sees behind him - who hold that the Son is the Father's identical substance and treat anything weaker as reopening the door to Arianism. Under a similar-substance settlement their precise confession reads as suspect precision: they face deposition, cycles of exile, and communion barriers, yet their conviction is constitutive of their office and they will not sign a formula they regard as ambiguous. Their refusal forces every settlement cycle to spend resources policing them.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, pro_nicene_confessing_bishops, payer,
    powerful, generational, identity_locked, continental).

% Receive catechesis and liturgy shaped by whichever formula their bishop subscribes to; they gain a settled faith statement that affirms Christ's divinity without demanding they adjudicate metaphysical subtleties themselves. They bear disruption when bishops are deposed and communities split over wording, and they have no seat in the synods deciding any of it; leaving the church entirely is scarcely imaginable in their world.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, ordinary_congregations, beneficiary,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, ordinary_congregations, payer).

% Teachers who deny that the Son shares the Father's substance in any sense, holding him a superior creature. Both rival readings fence them out; they serve as the boundary's negative reference point, summoned to councils chiefly to recant. They would argue that any substance language overstates Christ's rank, but by the settlement period they appear in the record as objects of anathemas rather than participants in drafting.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, arian_subordinationist_party, excluded,
    organized, biographical, constrained, continental).

% Reconstruct the controversy from conciliar acts, surviving letters, and polemics; trace how the similar-substance reading rose as the moderate tent of the mid-century and dissolved once the distinction between substance and person let the identical-substance reading absorb its concerns. Take no side in the doctrine; map who paid and who gained across the successive settlement cycles.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, homoiousian_synod_leadership).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a shared doctrinal boundary around acceptable confession of Christ: wide enough that Eastern derivation-language traditions and Western precision traditions could both teach inside it, firm enough that Arian denial of the Son's true divinity stayed outside. Solves the collective problem of stating a common faith across hundreds of sees without forcing one metaphysical formula on all of them.
% TRANSFER_FUNCTION: Moves definitional authority to the synodal party that drafts the formula; moves subscription obligations onto every bishop, with deposition awaiting refusers; moves the cost of doctrinal precision onto the same-substance confessors, whose exact wording falls outside the tolerated band; and moves uniformity away from the imperial administration, which loses the single confession its governance sought.
% ABSENT_VOICES: Arian teachers are fenced outside the conversation by the boundary itself - they appear in the record mainly as anathema targets. Lay congregations have no formal seat in any drafting council. And the position that ultimately dissolved the dispute - distinguishing substance from person - had no advocate in the room until the Cappadocians articulated it decades later.
% DISAPPEARANCE_RATIONALE: Mid-interval, the similar-substance boundary was the widest tent the anti-Arian coalition possessed: overnight removal would have collapsed the church back into the identical-substance-versus-Arian standoff, forced the imperial administration to pick a narrower winner immediately, and stripped regional sees of the teaching latitude they exercised daily. After Constantinople in 381 the arrangement was already hollow, and its removal would have changed little.
% FOUNDING_PROBLEM: After Nicaea imposed identical-substance language, much of the Greek East read it as collapsing Father and Son into one person - the Sabellian fear - while Arian teaching denied the Son's true divinity outright. The arrangement was built to state Christ's genuine divinity while preserving the Father-Son distinction: one fence excluding both errors at once.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Athanasius's letters attack the similar-substance compromise as inadequate protection against Arianism - an opposing-seat attestation that the founding anxiety was real; imperial rescripts and Constantius's correspondence show the court treating doctrinal settlement as an urgent governing problem; Gregory of Nazianzus's later orations concede the era's terminological confusion from outside both camps. No party denies the founding problem existed; the persistent dispute is over which formula answered it, and whether it was ever fully answered.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scalars are anchored to the arrangement's enforcement-apex phase (t=32, c. 357 CE, the Ancyra/Sirmium period), when the similar-substance boundary was the operative settlement the court pressed and synods policed; the temporal series shows the full gestation-apex-decay arc around that anchor. Extractiveness 0.56: the boundary moved real costs onto identifiable seats - subscription demanded from every bishop, deposition facing the same-substance confessors whose exact wording fell outside the tolerated band, and the imperial uniformity objective indefinitely deferred - while stopping short of heavier extraction because the tolerated band was genuinely wide and most sees paid little beyond attendance and signature. Suppression 0.58, structural-dominant: deposition by peers, communion barriers, and imperial exile machinery did the coercive work; a minority of the effect was internalized self-censorship of wording among cautious bishops. Accessibility_collapse 0.42: alternatives did not collapse - preserving teaching latitude inside the fence was the design - and rival formulas kept circulating, which is why resistance stayed high (0.64): the same-substance party fought the settlement continuously rather than accommodating to it. Theater_ratio 0.30 at apex, climbing monotonically to 0.52 by t=56: in the final decade the boundary's coordinating work was increasingly performed rather than performed-upon, since the substance/person distinction had already let the rival reading absorb its function. Claim and metrics are independently authored: the tangled_rope claim states the structure I believe true (genuine anti-Arian coordination plus genuine asymmetric payment under active enforcement), and the metrics state what I judge descriptively so; the engine computes per-seat types from the structural data. All three tracked series share one eight-point grid. The suppression_requirement series is authored because the narrative genuinely tracks enforcement-capacity change - buildup from 0.22 to 0.58, then decay to 0.20 - not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical data. From the synod-leadership seat the arrangement is a mediating settlement it built and defends - coordination-first. From the pro-Nicene confessor seat the same boundary operates as enforced exclusion of a constitutive confession - extraction-first with identity-locked refusal. From the imperial administration seat it is a perpetually failing instrument - never delivering the single confession governance requires, yet irreplaceable because some settlement is always needed. Congregations experience distant wording disputes filtered through their bishops. The divergence is structural (role, power, exit), not observational error; the engine computes it and this story's claim does not adjudicate between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional episcopal churches and anti-Sabellian theologians sit near the beneficiary end: the boundary subsidizes their teaching latitude and exegetical inheritance at little direct cost. Ordinary congregations sit near symmetric (dual declaration): they receive a settled faith statement but absorb disruption costs diffusely and without a seat. Pro-Nicene confessing bishops sit near the full-target end: they pay in deposition, exile, and communion exclusion, and identity-locked exit pins them at the paying position. The imperial administration is declared a victim with a secondary agenda-setter role - it pays in forgone uniformity while wielding the enforcement the boundary rides on; the derivation from victim status alone would overshoot its target-directionality, but no override is authored because the dual position is carried structurally through the secondary role and per-seat computation reads it. The excluded Arian party sits outside the transfer surface: the fence aims at them, but they collect nothing and pay through exclusion from the conversation rather than through subscription.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents two symmetrical misreadings. Calling it pure coordination would erase the identifiable payers - the same-substance confessors deposed for precision and the imperial uniformity project indefinitely deferred - flattering the settlement with a harmony its enforcement record contradicts. Calling it pure extraction would erase the genuine tent function: the boundary solved a real collective problem (stating Christ's divinity without Sabellian collapse), preserved exits for most seats, and was the widest formula the anti-Arian coalition ever held. The mandate-outlived-function question resolves late in the interval: once the substance/person distinction let identical-substance language preserve everything the similar-substance reading protected, the arrangement's founding mandate died while its forms persisted - theater_ratio crossing 0.5 at t=56 marks the performance-only phase. Consistent with the R5 interview (status contested, dissolving), the arrangement ends superseded rather than defeated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates only the homoiousios_reading of the nicene_christological_kernel; how would the classification change if the sibling homoousios_reading were instantiated instead?',
    'Author the sibling story and compare: under the identical-substance reading the beneficiary/victim sets invert (the imperial uniformity project and Western sees gain; Eastern distinction-preserving bishops pay), enforcement intensifies, and base extractiveness rises.',
    'Cross-reading comparison is the corpus-level product; within this story the reading boundary keeps epsilon stable and prevents blurring the two arrangements into one averaged constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a two-reading kernel; sibling inversion of victim sets.').

omega_variable(
    ousia_hypostasis_verbal_dispute,
    'Was the homoiousios/homoousios opposition partly a verbal dispute - conflation of substance with person - dissolvable by the later Cappadocian distinction?',
    'Compare what each party''s anathemas actually targeted (Basil of Ancyra''s charge of Father-Son conflation versus Athanasius''s charge of Arian residue) against the substance/person distinction that ultimately satisfied both sides.',
    'If largely verbal, the foreclosure edge weakens and this arrangement looks closer to a transitional coordination device with incidental costs; if substantive, the rivalry was real and the asymmetric payment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ousia_hypostasis_verbal_dispute, conceptual, 'Whether the defining opposition was conceptual or terminological.').

omega_variable(
    enforcement_attribution,
    'How much of the interval''s suppression was the doctrinal boundary''s own enforcement machinery versus imperial courts using whichever formula was current as pretext?',
    'Separate synodal penalties (deposition by peers, subscription demands) from imperial penalties (exile, appointment leverage) in the surviving record, attributing each to its issuer.',
    'If imperial-driven, the constraint''s intrinsic suppression drops toward coordination-with-friction; if synodal machinery carried the coercive load, the extractive reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_attribution, empirical, 'Attribution of coercive force between church and court across the interval.').

omega_variable(
    plurality_valence_preference,
    'Is the exegetical plurality the arrangement preserved a benefit (legitimate teaching autonomy) or a cost (fragmentation threatening unity of confession)?',
    'Not resolvable from structure alone - it depends on the evaluator''s weighting of unity against liberty of teaching; resolve by making the value choice explicit.',
    'Under a unity-weighted evaluation the regional sees flip toward the paying side and effective extraction rises; under a liberty-weighted evaluation the current beneficiary declarations stand.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plurality_valence_preference, preference, 'Value-dependent valence of doctrinal pluralism.').

omega_variable(
    terminal_trajectory_supersession_vs_atrophy,
    'Did the arrangement end by supersession (its work absorbed into a better conceptual tool) or by atrophy (function decaying under disuse)?',
    'Compare the theater_ratio climb across the final decade against contemporaneous uptake of the substance/person distinction among the reading''s own heirs.',
    'Supersession supports reading the arrangement''s role as transitional support; atrophy would date decay earlier and weight the theater component of terminal classification more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_trajectory_supersession_vs_atrophy, empirical, 'Terminal lifecycle path of the arrangement after Constantinople 381.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_christological_kernel__homoiousios_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nice_tr_t8, nicene_christological_kernel__homoiousios_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(nice_tr_t16, nicene_christological_kernel__homoiousios_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(nice_tr_t24, nicene_christological_kernel__homoiousios_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(nice_tr_t32, nicene_christological_kernel__homoiousios_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(nice_tr_t40, nicene_christological_kernel__homoiousios_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(nice_tr_t48, nicene_christological_kernel__homoiousios_reading, theater_ratio, 48, 0.43).
narrative_ontology:measurement(nice_tr_t56, nicene_christological_kernel__homoiousios_reading, theater_ratio, 56, 0.52).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nice_be_t8, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(nice_be_t16, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(nice_be_t24, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(nice_be_t32, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(nice_be_t40, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(nice_be_t48, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement(nice_be_t56, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 56, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(nice_su_t8, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(nice_su_t16, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(nice_su_t24, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(nice_su_t32, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(nice_su_t40, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(nice_su_t48, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 48, 0.36).
narrative_ontology:measurement(nice_su_t56, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 56, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the Nicene Christological kernel decomposes into two epsilon-invariant readings. This story authors the similar-substance arrangement (boundary at likeness; pluralism-preserving; moderately extractive toward precision-confessing bishops and the imperial uniformity project). The sibling story authors the identical-substance arrangement (boundary at identity; uniformity-enforcing; heavily extractive toward Eastern distinction-preserving bishops). Interaction is bidirectional across the interval: the sibling's Nicaean precedent (325) structured the grievances under which this reading was drafted, and this reading's mid-century ascendancy pressured the sibling's holders into the terminological clarifications that eventually dissolved it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
