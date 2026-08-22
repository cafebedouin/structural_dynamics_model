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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousian Christology: Similar-Substance Reading of the Nicene Kernel
 *   domain: historical_theology/christology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story models the homoiousios ('of similar substance') reading of the
 *   fourth-century Christological kernel — the mid-fourth-century mediating
 *   position, associated with Basil of Ancyra and the 358 Council of Ancyra,
 *   that affirmed the Son's genuine and exalted likeness to the Father while
 *   preserving an ontological distinction the homoousian formula was feared
 *   to erase. This is one of at least two structurally distinct constraints
 *   riding the same underlying kernel (the nature of Christ's relation to the
 *   Father, adjudicated by councils and imperial power). The sibling reading
 *   — homoousios, full identity of essence — is authored as a separate
 *   constraint (homoousios_reading) with its own beneficiary/victim structure
 *   and its own eps. This story's eps is authored strictly for the
 *   homoiousian arrangement as it actually operated in the 340s-370s: a
 *   moderately extractive, moderately suppressive coordination device that
 *   let regional sees preserve autonomy and exegetical tradition at real cost
 *   to imperial unity and to the losing Nicene faction, not for any
 *   hypothetical fully-reconciled church the homoiousian bishops might have
 *   wished for.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.47).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.42).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousian Christology: Similar-Substance Reading of the Nicene Kernel").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "historical_theology/christology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'bc5687e1-3975-4b31-a187-1911ca0cd303').
narrative_ontology:cs_kernel_codification('bc5687e1-3975-4b31-a187-1911ca0cd303', distributed).
narrative_ontology:cs_authority_grounding('bc5687e1-3975-4b31-a187-1911ca0cd303', distributed).
narrative_ontology:cs_reading_relation('bc5687e1-3975-4b31-a187-1911ca0cd303', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('bc5687e1-3975-4b31-a187-1911ca0cd303', foundational, ontological_distinction_required_for_monotheistic_clarity).
narrative_ontology:cs_axiom_status(ontological_distinction_required_for_monotheistic_clarity, holdable).
narrative_ontology:cs_axiom_grounding('bc5687e1-3975-4b31-a187-1911ca0cd303', ontological_distinction_required_for_monotheistic_clarity, deontological).
narrative_ontology:cs_axiom('bc5687e1-3975-4b31-a187-1911ca0cd303', foundational, scriptural_subordination_language_is_doctrinally_load_bearing).
narrative_ontology:cs_axiom_status(scriptural_subordination_language_is_doctrinally_load_bearing, overridden).
narrative_ontology:cs_axiom_grounding('bc5687e1-3975-4b31-a187-1911ca0cd303', scriptural_subordination_language_is_doctrinally_load_bearing, conventional).
narrative_ontology:cs_reference_frame('bc5687e1-3975-4b31-a187-1911ca0cd303', origenist_gradational_ontology).
narrative_ontology:cs_drift_state('bc5687e1-3975-4b31-a187-1911ca0cd303', post_constantinople_381, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('bc5687e1-3975-4b31-a187-1911ca0cd303', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_traditionalist_clergy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, homoiousian_court_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_unity_project).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, nicene_creedal_partisans).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, lay_congregants_under_shifting_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, monotheistic_clarity_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, subordinationist_continuity_with_origenist_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provincial bishops, especially in Asia Minor and Syria, who favor homoiousios as a mediating formula preserving both scriptural subordination language and a rejection of Arian dissimilarity. They convene regional synods (e.g. Ancyra, 358) to promulgate the formula, ordain sympathetic clergy, and resist creedal dictation from Nicaea/Constantinople. Their exit from imperial doctrinal control is constrained by dependence on imperial patronage and the threat of exile, but they retain local liturgical and administrative autonomy that homoousian centralization would erase.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops, beneficiary).

% Clergy and theologians who read scripture's language of the Son being 'sent by' and 'subordinate to' the Father as doctrinally load-bearing. The homoiousios formula lets them preserve that reading without being branded Arian heretics. They gain interpretive room and career survival; their exit option is limited to switching camps entirely, which risks deposition.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_traditionalist_clergy, beneficiary,
    moderate, biographical, constrained, regional).

% Courtiers and imperially-connected bishops (notably under Constantius II) who promote homoiousios as an imperially-brokered compromise capable of holding the eastern and western churches together without full Nicene capitulation. They gain influence over imperial religious policy and can move between doctrinal factions as imperial favor shifts, giving them the most exit flexibility of any stakeholder in this story.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousian_court_faction, beneficiary,
    powerful, biographical, mobile, continental).

% The empire's ambition to use a single, universally binding creed as social and administrative glue is not itself a person, but its coherence is directly damaged: every regional synod adopting homoiousios produces a rival creedal center Constantinople cannot simply overrule, forcing repeated councils (Sirmium, Ancyra, Seleucia, Constantinople 360) instead of settlement.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_unity_project, payer,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoiousios_reading, imperial_unity_project).

% Bishops and theologians (Athanasius and allies) committed to homoousios as the only formula guarding full divine equality of the Son. They experience the homoiousios reading as doctrinal erosion that reopens the door to functional subordinationism and Arian sympathy, costing them imperial favor, sees and, at points, exile. Their exit options are constrained by the same imperial machinery the homoiousian faction currently controls.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, nicene_creedal_partisans, payer,
    organized, generational, constrained, continental).

% Ordinary believers whose bishops, baptismal formulas, and permitted liturgical language change with each provincial or imperial council. They bear the confusion and occasional excommunication risk of doctrinal whiplash with no voice in the councils deciding which substance-language is orthodox in their region this decade.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, lay_congregants_under_shifting_doctrine, payer,
    powerless, biographical, trapped, local).

% Assess the fourth-century homoian/homoiousian/homoousian contest retrospectively, reconstructing which formula served which faction's institutional and political interests versus which reflected genuine theological conviction.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Homoiousios coordinates a real theological problem: how to affirm the Son's genuine, exalted divinity while preserving scriptural language of distinction and subordination, and while avoiding both the Arian extreme (radical unlikeness) and full ontological identity that some regional traditions found philosophically and scripturally difficult. It lets diverse eastern churches maintain doctrinal continuity with their own exegetical and Origenist heritage rather than being forced into a single imported formula.
% TRANSFER_FUNCTION: Moves doctrinal authority and liturgical self-determination away from the centralized Nicene-Constantinopolitan settlement and toward regional episcopal synods; moves the cost of doctrinal instability onto lay congregants and onto the imperial project of using one creed as an instrument of political unity.
% ABSENT_VOICES: Lay congregants have no seat in any of the councils (Ancyra, Sirmium, Seleucia) adjudicating which substance-term is orthodox; monastic and desert communities holding independent theological views are rarely represented; Western Latin bishops, who largely favored homoousios and were structurally distant from these eastern debates, are talked about but not present as voting parties in most of the councils that produced homoiousian formulas.
% DISAPPEARANCE_RATIONALE: If the homoiousian reading vanished from the fourth-century contest, regional eastern episcopal autonomy would have far less doctrinal cover, Constantius II's mediating religious policy would collapse into an earlier, starker Nicene/Arian binary, and the councils of 359-360 (which produced the homoian and then briefly dominant compromise formulas) would not have occurred in the form they did. The eventual triumph of homoousios at Constantinople 381 would likely have arrived faster and with less negotiated accommodation of eastern subordinationist exegesis.
% FOUNDING_PROBLEM: How to state Christ's relationship to the Father in a way that refutes Arius (who denied the Son was truly divine) without collapsing scriptural subordination language and without adopting a term (homoousios) that some feared implied modalism or was philosophically unclear to eastern audiences steeped in Origenist gradational ontology.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians of the Arian controversy (outside any communion with institutional stake in the outcome) broadly attest that the philosophical/exegetical problem homoiousios addressed was real in the 340s-360s but was resolved institutionally rather than dissolved theologically: the Council of Constantinople (381) settled the term by imperial and conciliar fiat under Theodosius I, backed by coercive suppression of dissenting sees, not by a demonstration that homoiousian concerns were answered on their own terms. Surviving homoian and homoiousian communities (e.g. among Gothic Christians) persisted for generations after 381, corroborating that the 'problem' the formula solved outlived its official resolution.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.47 at 381, having peaked near 0.50 around the 359-360 councils) reflects the real cost imposed on imperial coherence and on Nicene partisans by the homoiousian bloc's capacity to generate rival, regionally-binding creeds — every synod that ratifies a distinct formula is a transfer of settlement-authority away from center and toward periphery, extracted at the cost of the empire's unity project and of lay confusion. Suppression is moderate (0.42) and lower than the eventual homoousian resolution's suppression, because the homoiousian position, for most of its life, was competing rather than dominant, and depended more on imperial court favor (under Constantius II) than on coercive uniform enforcement across the whole church. Accessibility collapse is low-to-moderate (0.4): the homoiousian formula never fully closed off appeal to homoousios or to the Arian/homoian alternatives; competing terms remained live and contested throughout the interval, which is precisely why resistance (0.68) is high — Nicene partisans like Athanasius mounted sustained, well-organized opposition the whole time. Theater ratio rises through the councils of the late 350s (peaking 0.34 at the Sirmium/Seleucia moment) as competing creedal formulas proliferated with diminishing returns to actual doctrinal clarity, then eases as the position lost imperial backing after Julian and Valentinian-era realignments.
 *
 * PERSPECTIVAL GAP:
 *   From the regional-bishop seat this looks like a rope: a genuine coordination solution letting the eastern church hold together theologically without capitulating to a formula many found philosophically opaque or spiritually flattening of scriptural subordination language. From the Nicene-partisan and imperial-unity seat it looks like a tangled rope shading toward snare: the coordination story (avoiding Arian error, respecting scripture) is real, but it also extracts settlement-authority from the center and imposes real costs on those trying to hold one universal church together. The engine computing different seat-level types from the same structural data models exactly this — the same formula is simultaneously the eastern church's best defense against both Arianism and Nicene overreach, and the imperial project's chief obstacle to doctrinal closure.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional bishops and traditionalist exegetes are the structural beneficiaries: the formula preserves their existing scriptural readings and their autonomy from a Nicene-Constantinopolitan center, so d sits low for them. The homoiousian court faction benefits doubly — doctrinal cover plus continued imperial influence — while retaining arbitrage-like mobility between doctrinal camps as imperial patronage shifts, which is why their exit option is authored as mobile rather than constrained despite their powerful position. Nicene partisans and the abstract imperial-unity project are targets: every gain in regional doctrinal autonomy is their loss in settled, universal creedal authority, so d sits high for them. Lay congregants are the most trapped and powerless payers, bearing doctrinal instability with zero voice in any council.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to state Christ's relation to the Father without falling into either Arian subordination-to-the-point-of-lesser-divinity or an ontological identity claim some eastern exegetes found textually and philosophically strained) was genuinely live through the 340s-360s. It did not so much get solved as get overridden: Constantinople 381, under Theodosius I's decisively pro-Nicene imperial power, imposed homoousios as the only legal formula and used administrative and coercive means (deposition, exile, denial of church buildings to non-conforming clergy) to enforce it. The persistence of homoian and homoiousian Christian communities well past 381 (notably among Gothic and other 'barbarian' Christian populations converted under Arian/homoian missionary influence) is the corroborating evidence that the underlying theological disagreement was suppressed institutionally rather than resolved on the terms the debate was actually conducted in — precisely the founding_problem_status: dead, disappearance_verdict: world_rearranges mismatch the R5 apparatus is built to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_theology_vs_political_convenience,
    'Was homoiousios primarily a sincere theological attempt to navigate between Arian subordinationism and a feared modalist reading of homoousios, or primarily a politically convenient mediating formula that let Constantius II avoid choosing a side and let regional bishops retain autonomy?',
    'Close comparative reading of Basil of Ancyra''s and George of Laodicea''s theological writings against the political correspondence and conciliar records of 357-360 to assess whether theological argument or political expedience drove formula selection at each council.',
    'If primarily sincere theology, the coordination function is stronger and the story sits closer to a rope with incidental extraction; if primarily political convenience, the coordination story is closer to cover for factional power-preservation, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_theology_vs_political_convenience, conceptual, 'Whether the homoiousian formula reflects sincere doctrinal reasoning or political convenience for regional and court factions.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the underlying kernel best framed as ''the ontological status of Christ relative to the Father'' (as this story assumes), or as ''who has the authority to define binding doctrine for the whole church'' — a framing under which homoiousios and homoousios are not two different answers to the same theological question but two different answers to a prior authority question that theology is downstream of?',
    'Compare the councils'' own stated rationale (theological argument from scripture and philosophy) against the pattern of imperial sponsorship switching sides (Constantius favoring homoian/homoiousian compromise, Theodosius imposing homoousios) to see which framing better predicts which formula wins at which moment.',
    'If the authority framing is correct, both readings'' ''extraction'' figures are substantially about power allocation rather than doctrinal content, and the true kernel network should include an authority-adjudication constraint neither reading currently names explicitly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is genuinely Christological or is better understood as an authority-allocation kernel that theology rides on top of.').

omega_variable(
    lay_reception_evidence_gap,
    'How did ordinary lay congregants actually experience and understand these successive shifts in required creedal language at the parish level — as meaningful theological change, as background noise imposed from above, or as something else entirely?',
    'Surviving liturgical, epigraphic, and homiletic evidence from parish-level sources (rather than conciliar and episcopal correspondence) in regions that shifted formula multiple times between 357 and 381.',
    'If lay reception shows minimal actual disruption, the ''payer'' burden on lay congregants is overstated and extraction should be revised downward; if it shows genuine confusion and excommunication risk, it corroborates the current authored victim status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_reception_evidence_gap, empirical, 'Whether lay congregants experienced the doctrinal shifts as a real cost or as elite-level noise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 341, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t341, nicene_christological_kernel__homoiousios_reading, theater_ratio, 341, 0.15).
narrative_ontology:measurement(nice_tr_t347, nicene_christological_kernel__homoiousios_reading, theater_ratio, 347, 0.18).
narrative_ontology:measurement(nice_tr_t353, nicene_christological_kernel__homoiousios_reading, theater_ratio, 353, 0.22).
narrative_ontology:measurement(nice_tr_t358, nicene_christological_kernel__homoiousios_reading, theater_ratio, 358, 0.28).
narrative_ontology:measurement(nice_tr_t361, nicene_christological_kernel__homoiousios_reading, theater_ratio, 361, 0.34).
narrative_ontology:measurement(nice_tr_t370, nicene_christological_kernel__homoiousios_reading, theater_ratio, 370, 0.31).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.3).

% Extraction over time
narrative_ontology:measurement(nice_be_t341, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 341, 0.28).
narrative_ontology:measurement(nice_be_t347, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 347, 0.33).
narrative_ontology:measurement(nice_be_t353, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 353, 0.38).
narrative_ontology:measurement(nice_be_t358, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 358, 0.44).
narrative_ontology:measurement(nice_be_t361, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 361, 0.5).
narrative_ontology:measurement(nice_be_t370, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 370, 0.44).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t341, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 341, 0.2).
narrative_ontology:measurement(nice_su_t347, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 347, 0.25).
narrative_ontology:measurement(nice_su_t353, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 353, 0.32).
narrative_ontology:measurement(nice_su_t358, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 358, 0.4).
narrative_ontology:measurement(nice_su_t361, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 361, 0.46).
narrative_ontology:measurement(nice_su_t370, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 370, 0.4).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, homoousios_reading).

% DUAL FORMULATION NOTE:
% This story and homoousios_reading are the two live readings of nicene_christological_kernel during the 340s-381 interval. They share a kernel (Christ's relation to the Father, adjudicated by councils under imperial sponsorship) but instantiate different beneficiary/victim structures and different eps: homoiousios (this story) benefits regional episcopal autonomy and exegetical continuity at the cost of imperial doctrinal unity (eps ~0.47, moderate, contested throughout the interval); homoousios benefits centralized conciliar authority and full-equality Trinitarian partisans at the cost of regional autonomy and subordinationist exegetical traditions, and — once imperially imposed at Constantinople 381 — carries higher suppression as the enforced, legally exclusive formula. Neither story averages or hedges across the other; each is authored as its own clean constraint per DP-001 eps-invariance, linked here for contamination/network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
