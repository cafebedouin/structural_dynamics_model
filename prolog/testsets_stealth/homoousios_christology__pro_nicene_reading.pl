% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Settlement: Enforced Creed and Anathema Machinery (325-451)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   The pro-Nicene reading of the homoousios kernel as an enforced
 *   arrangement: from Nicaea (325) to Chalcedon (451), the confession that
 *   the Son is of identical substance with the Father functioned as the
 *   membership boundary of the imperial church, administered through councils
 *   and episcopal anathema and given legal force by imperial edict. The
 *   arrangement has a genuine coordination function - it settled a dispute
 *   that had left the church unable to confess, baptize, or ordain
 *   coherently, and gave clergy, congregations, and the state one shared
 *   standard - and an asymmetric transfer structure: dissenting clergy lost
 *   office, buildings, and endowments to conforming appointees, and the
 *   machinery that did this was the arrangement's own enforcement arm. The
 *   claim and the metrics are authored independently: claimed_type is
 *   tangled_rope because both the coordination function and the enforced
 *   asymmetric transfer are structurally real; the metrics describe the
 *   arrangement's actual operation at interval end - high suppression,
 *   substantial extraction, low-moderate theater. The epsilon referent is the
 *   standing enforced settlement itself, assessed from the pro-Nicene seat:
 *   the reading judges the settlement warranted, and still records the
 *   transfer it performed.
 *
 * KEY AGENTS:
 *   - nicene_episcopate: agenda-setter and primary collector (institutional / identity_locked) - administers councils, creed, and anathema; receives deposed opponents' sees, buildings, and endowments
 *   - imperial_administration: beneficiary with the enforcement arm (institutional / arbitrage) - gives the creed legal force, collects provincial unification, can re-trade the settlement as Constantius II did
 *   - pro_nicene_theologians: agenda-setter who also pays (moderate / identity_locked) - authors the reading's theological content; bears exile when imperial favor turns (Athanasius's five exiles)
 *   - nicene_laity: coordination beneficiaries (organized / identity_locked) - receive one confession and one baptism; bear factional riot costs
 *   - arian_bishops: primary targets (organized / constrained) - condemned at Nicaea, ascendant mid-century, dispossessed after Theodosius
 *   - homoian_clergy: secondary targets (moderate / constrained) - ordinations invalidated and congregations reassigned when the Nicene settlement returns
 *   - dissenting_laity: diffuse targets (powerless / trapped) - lose buildings and sacramental standing with no assembly, press, or representation
 *   - pagan_elites: excluded (powerful / constrained) - never party to the deliberations; bear temple closures and office-as-confession-test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.7).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.84).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Settlement: Enforced Creed and Anathema Machinery (325-451)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'd6caf50d-e433-4c8a-ab8f-2c5d0f0ed129').
narrative_ontology:cs_kernel_codification('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', fixed_text).
narrative_ontology:cs_authority_grounding('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', extraction).
narrative_ontology:cs_interpretation_layer_present('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129').
narrative_ontology:cs_reading_relation('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', foundational, son_identical_divine_substance).
narrative_ontology:cs_axiom_status(son_identical_divine_substance, holdable).
narrative_ontology:cs_axiom_grounding('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', son_identical_divine_substance, theological).
narrative_ontology:cs_axiom('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', foundational, creed_restates_apostolic_teaching).
narrative_ontology:cs_axiom_status(creed_restates_apostolic_teaching, holdable).
narrative_ontology:cs_axiom_grounding('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', creed_restates_apostolic_teaching, theological).
narrative_ontology:cs_axiom('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', secondary, doctrinal_exclusion_warranted).
narrative_ontology:cs_axiom_status(doctrinal_exclusion_warranted, holdable).
narrative_ontology:cs_axiom_grounding('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', doctrinal_exclusion_warranted, conventional).
narrative_ontology:cs_reference_frame('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', apostolic_tradition_continuity).
narrative_ontology:cs_drift_state('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', chalcedonian_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d6caf50d-e433-4c8a-ab8f-2c5d0f0ed129', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopate).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_administration).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_laity).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, homoian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, pro_nicene_theologians).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, homoousios_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, nicene_creed_apostolic_authority).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, imperial_religious_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and staffs the councils, pronounces and transmits the creed, and administers the anathema that strips dissenting clergy of office, churches, and endowments - which then pass to conforming appointees. The office's authority to teach is constituted by the confession it enforces; abandoning the formula would dissolve the claim on which the office rests.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopate, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, nicene_episcopate, beneficiary).

% Issues the edicts that give the creed legal force - banishment of dissenting clergy, transfer of church buildings, exclusion from office - in exchange for a uniform cult that binds the provinces. Pays enforcement costs and at moments concedes moral authority to bishops (Ambrose compelling Theodosius's public penance). Can re-trade the settlement, as Constantius II did in the Homoian years, at the price of reopening the whole controversy.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_administration, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, imperial_administration, agenda_setter).

% Authors the reading's intellectual content - the defense of the identical-substance formula against subordinationist readings and the ousia/hypostasis grammar that stabilized it. Bears the arrangement's costs personally when imperial favor turns: Athanasius's five exiles are the pattern. Cannot exit without dissolving the identity their defense constituted.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, pro_nicene_theologians, agenda_setter,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, pro_nicene_theologians, payer).

% Receives a single shared confession, a unified liturgy, and the social standing of orthodoxy. Bears the arrangement's conflict costs - urban rioting between creedal factions, churches lost to rival parties - but collects the coordination good: one church, one baptism, one faith. Baptized into the formula, switching parties means sacramental and social rupture.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_laity, beneficiary,
    organized, biographical, identity_locked, continental).

% Teaches that the Son is a created, subordinate being. Condemned at Nicaea, ascendant under Constantius II, dispossessed after Theodosius: deprived of office, buildings, and legal standing. Those who remain inside the empire preach at the cost of exile; those who leave carry the confession beyond the frontier at the cost of see and homeland.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_bishops, payer,
    organized, biographical, constrained, continental).

% Clergy ordained under the compromise formulas that held imperial favor mid-century. When the Nicene settlement returns, their ordinations are invalidated, their congregations reassigned to conforming appointees, their careers ended. Less organized than the subordinationist episcopate, they absorb the reversal without a counter-hierarchy to soften it.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, homoian_clergy, payer,
    moderate, biographical, constrained, continental).

% Congregations that hold to the subordinationist teaching or simply resist the enforced formula. Lose their buildings to conforming appointees and face exclusion from the sacramental life that structures marriage, burial, and civic standing. Trapped in cities where the reassigned church is the only church, with no assembly, press, or representation through which to object.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_laity, payer,
    powerless, biographical, trapped, regional).

% Senatorial and civic aristocracies attached to the traditional cults. Never party to the creed's deliberations, they bear its civic consequences: temple closures, endowment confiscations, the conversion of public office into a confession test. Their objection - that the empire is trading one set of cults for an enforced metaphysics - has no seat in the councils.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, pagan_elites, excluded,
    powerful, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopate).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: a church spanning the empire could not confess, worship, baptize, or ordain coherently while presbyters and bishops taught incompatible accounts of the Son's relation to the Father. The creed supplies one shared formula for baptism, ordination, and communion, and one standard by which teaching offices are staffed.
% TRANSFER_FUNCTION: Moves ecclesiastical office, church buildings, endowments, and civic legitimacy from clergy and congregations that will not confess the identical-substance formula to those that will; moves enforcement capacity (banishment, confiscation, exclusion from office) from the imperial state into the service of the Nicene episcopate; moves ideological unification and a legitimation narrative to the imperial administration.
% ABSENT_VOICES: The subordinationist parties were present at Nicaea and were converted into excluded parties by the anathemas - after Theodosius their voice survives only as the condemned position read aloud before its condemnation. Pagan civic elites and Jewish communities were never in the room; they bore temple closures, endowment transfers, and office-as-confession-test without any seat. Dissenting laity had no assembly, press, or representation; their objection is reconstructed only from the charges recorded against them.
% DISAPPEARANCE_RATIONALE: If the enforced settlement vanished overnight, the empire's church would re-fragment into competing episcopal coalitions with rival confessions - the condition of the 340s and 350s: rival councils, rioting congregations, sees changing hands by force. The imperial administration would lose its unification instrument; the episcopate would lose the coercive backing that made its teaching office decisive; dissenting congregations would regain buildings and offices; the pagan civic order would face a church too divided to press its civic claims.
% FOUNDING_PROBLEM: The Arian dispute: teaching that the Son was a created, subordinate being spread from Alexandria through the empire's churches, producing incompatible liturgies and baptisms, street violence between congregational factions, and a church unable to state one faith. The emperor convened Nicaea to obtain a single formula that would restore unity to church and empire together.
% FOUNDING_PROBLEM_CORROBORATION: The Homoian councils (the Dedication Council of Antioch, 341; Rimini-Seleucia, 359) attest from outside the Nicene beneficiary set that the dispute was live and unresolved; the pagan historian Ammianus Marcellinus, writing from no confessional seat, records the strife's intensity; Jerome's testimony that 'the world groaned and marveled to find itself Arian' corroborates from within the reading's coalition but outside the enforcement machinery's beneficiary roll. No contemporary source outside the controversy denies the problem's liveness.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.70 at interval end: office, church buildings, endowments, and legal standing moved from dissenters to conformists through the arrangement's own machinery, and the transfer was not priced to any service the dissenters failed to render. Suppression is 0.84: after Theodosius the arrangement persisted by law - banishment of dissenting clergy, confiscation of their churches, exclusion from office - rather than by participant preference; alternatives collapsed inside the empire (accessibility_collapse 0.62) but survived beyond the imperial frontier and in pockets, so collapse is high but not total. Theater_ratio 0.32: the doctrinal function was real and continuously performed (creed recited at baptism and ordination, taught, defended in writing), but as the dispute settled, a growing share of enforcement activity became ceremonial repetition of settled conclusions and loyalty display. Resistance 0.66: six decades of rival councils, rioting congregational factions, and episcopal defiance met the arrangement. All three tracked series share one eight-point grid (325-451); the 355 dip in extraction and suppression records the Homoian ascendancy, when this reading's arrangement was itself out of power and its enforcement machinery dormant. gain_flow names the nicene_episcopate because deposed opponents' sees, buildings, and endowments demonstrably accrued to conforming appointees; fixing_cost is prohibitive because the seat that could re-trade the settlement - the imperial administration - found each re-trade consumed decades of controversy and was reversed.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement is pure exclusion: an Arian bishop under Theodosius experiences a formula he rejects taking his office, his building, and his congregation, enforced by an empire he cannot resist. From the agenda-setter seat the same structure is the defense of the apostolic faith against corruption - coordination with a regrettable necessity attached. The pro-Nicene theologians occupy both seats at once: they author the arrangement and bear its exiles. The imperial seat experiences a third structure: an instrument of unification whose enforcement costs are real and whose moral authority sometimes disciplines its own patron. The dissenting laity, powerless and trapped, had no coalition capacity - no assembly, no press, no representation - so the payer seats could not convert numbers into leverage. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the nicene_episcopate collects the arrangement's goods directly (sees, buildings, enforcement backing) and runs it - d at the beneficiary end. The imperial_administration collects provincial unification and legitimation but pays enforcement costs and periodically concedes authority to bishops; its dual position is carried by secondary_role agenda_setter rather than a directionality override, because an override keyed to the institutional power atom would misfire on the episcopate, which shares that atom but sits near the pure-beneficiary end. nicene_laity collect the coordination good (one confession, one baptism) diffusely while bearing factional riot costs. Targets: arian_bishops, homoian_clergy, and dissenting_laity sit at the target end; constrained and trapped exit amplifies their effective extraction - a deposed bishop cannot relocate his see inside the empire, and a dissenting congregation cannot relocate its city. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the directionalities the structure actually shows.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Reading the arrangement as pure extraction erases the real coordination good: the church of the 330s genuinely could not confess one faith, and the creed did solve that problem for the conforming majority. Reading it as pure coordination erases the anathema machinery's asymmetric transfer, which no coordination need required. Within the interval the founding problem stayed live - Constantinople 381 had to re-settle what Nicaea settled, and the Gothic mission carried the dispute beyond the frontier - so no mandate-outlives-function verdict is available yet; the founding_problem_boundary omega tracks the post-interval point where the enforcement machinery's targets outlived the founding dispute. The theater_ratio series (0.12 rising to 0.32) shows early drift toward ceremonial maintenance without reaching piton territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (pro_nicene_reading) of the homoousios_christology kernel; what structurally changes under the sibling readings (arian_reading, semi_arian_reading), and where exactly is the disagreement located?',
    'The sibling story files themselves: each reading instantiates a separate constraint with its own epsilon and beneficiary/victim structure. The disagreement is located in the meaning of the substance term - whether the Son''s substance is identical with, similar to, or other than the Father''s - which relocates the anathema''s targets.',
    'Under the arian_reading, the same enforcement machinery targets pro-Nicene bishops (as during Constantius II''s reign): the beneficiary and victim sets invert and per-seat classifications compute from inverted directionalities. This story''s epsilon and type are valid only for the pro-Nicene seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the substance term.').

omega_variable(
    substance_term_semantics,
    'What did homoousios assert - numerical identity of substance (Son and Father as one being) or generic identity (same kind of being)? The term carried both readings in 325, and the anathema''s reach depends on which.',
    'The Constantinopolitan settlement and the Cappadocian corpus (one ousia, three hypostases) stabilized the numerical-identity reading inside a triadic grammar; trace which semantic each enforcement action presupposed.',
    'A generic-substance semantic condemns less: positions affirming true deity while denying numerical identity would fall outside the anathema''s warrant, lowering the arrangement''s effective extraction and shrinking the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substance_term_semantics, conceptual, 'Semantic ambiguity of the kernel term and its effect on the exclusion mechanism''s reach.').

omega_variable(
    enforcement_vs_conviction,
    'How much of the arrangement''s post-381 persistence rested on imperial coercion versus genuine conviction among clergy and laity?',
    'Behavior beyond the enforcement frontier: Gothic kingdoms adopted the subordinationist confession when offered a choice; Armenian and Syriac community trajectories; persistence of dissent inside the empire under legal penalty.',
    'If conviction carries most of the persistence, the coordination function dominates and suppression is partly redundant; if coercion carries it, the arrangement sits nearer pure exclusion than the tangled-rope reading allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_conviction, empirical, 'Relative weight of coercion versus conviction in the arrangement''s persistence.').

omega_variable(
    imperial_church_capture_direction,
    'Who captured whom - did the episcopate capture imperial enforcement capacity, or did the imperial administration capture the church''s teaching office as an instrument of provincial unification?',
    'Trace the direction of discipline across the interval: episodes where bishops disciplined emperors (Ambrose compelling Theodosius''s penance after Thessalonica) against episodes where emperors deposed and exiled bishops at will (Constantius II, the early Theodosian edicts).',
    'If the episcopate captured the state, the gains accrue to the episcopal seat and the arrangement is clerical rent-collection; if the state captured the church, the episcopate is itself partly a target and the extraction is imperial. The two readings assign different directionality to the same institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_church_capture_direction, conceptual, 'Direction of capture between the episcopal and imperial beneficiary seats.').

omega_variable(
    founding_problem_boundary,
    'Was the founding problem - doctrinal fragmentation over the Son''s relation to the Father - still live at interval end, or had the enforcement machinery begun outliving the dispute it was built to settle?',
    'The post-451 enforcement record: whether anathema targets shifted from the original dispute''s parties to new dissenters, and whether the machinery''s maintenance costs persisted after the founding parties were gone.',
    'If the problem was dead or dying by 451, the arrangement''s later persistence is mandate-outlives-function and the story trends toward piton dynamics; if live, the tangled-rope classification holds through the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_boundary, empirical, 'Liveness boundary of the founding problem relative to the enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_pronicene_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(homoousios_pronicene_tr_t340, homoousios_christology__pro_nicene_reading, theater_ratio, 340, 0.15).
narrative_ontology:measurement(homoousios_pronicene_tr_t355, homoousios_christology__pro_nicene_reading, theater_ratio, 355, 0.18).
narrative_ontology:measurement(homoousios_pronicene_tr_t370, homoousios_christology__pro_nicene_reading, theater_ratio, 370, 0.2).
narrative_ontology:measurement(homoousios_pronicene_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.22).
narrative_ontology:measurement(homoousios_pronicene_tr_t400, homoousios_christology__pro_nicene_reading, theater_ratio, 400, 0.26).
narrative_ontology:measurement(homoousios_pronicene_tr_t425, homoousios_christology__pro_nicene_reading, theater_ratio, 425, 0.3).
narrative_ontology:measurement(homoousios_pronicene_tr_t451, homoousios_christology__pro_nicene_reading, theater_ratio, 451, 0.32).

% Extraction over time
narrative_ontology:measurement(homoousios_pronicene_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(homoousios_pronicene_be_t340, homoousios_christology__pro_nicene_reading, base_extractiveness, 340, 0.5).
narrative_ontology:measurement(homoousios_pronicene_be_t355, homoousios_christology__pro_nicene_reading, base_extractiveness, 355, 0.3).
narrative_ontology:measurement(homoousios_pronicene_be_t370, homoousios_christology__pro_nicene_reading, base_extractiveness, 370, 0.44).
narrative_ontology:measurement(homoousios_pronicene_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.6).
narrative_ontology:measurement(homoousios_pronicene_be_t400, homoousios_christology__pro_nicene_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(homoousios_pronicene_be_t425, homoousios_christology__pro_nicene_reading, base_extractiveness, 425, 0.7).
narrative_ontology:measurement(homoousios_pronicene_be_t451, homoousios_christology__pro_nicene_reading, base_extractiveness, 451, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_pronicene_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement(homoousios_pronicene_su_t340, homoousios_christology__pro_nicene_reading, suppression_requirement, 340, 0.48).
narrative_ontology:measurement(homoousios_pronicene_su_t355, homoousios_christology__pro_nicene_reading, suppression_requirement, 355, 0.25).
narrative_ontology:measurement(homoousios_pronicene_su_t370, homoousios_christology__pro_nicene_reading, suppression_requirement, 370, 0.4).
narrative_ontology:measurement(homoousios_pronicene_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.65).
narrative_ontology:measurement(homoousios_pronicene_su_t400, homoousios_christology__pro_nicene_reading, suppression_requirement, 400, 0.76).
narrative_ontology:measurement(homoousios_pronicene_su_t425, homoousios_christology__pro_nicene_reading, suppression_requirement, 425, 0.82).
narrative_ontology:measurement(homoousios_pronicene_su_t451, homoousios_christology__pro_nicene_reading, suppression_requirement, 451, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the homoousios controversy' covers three structurally distinct constraints - one per reading of the kernel. This file authors the pro-Nicene reading only: the enforced settlement whose epsilon, beneficiary/victim structure, and enforcement direction are specific to the conforming coalition's seat. The arian_reading and semi_arian_reading files author the same machinery from the subordinationist seats, with inverted victim sets and different epsilon; the three stories form a constraint family linked by affects_constraints. The mid-century Homoian ascendancy (reflected in the 355 dip of this story's series) is the period when the family's enforcement direction inverted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
