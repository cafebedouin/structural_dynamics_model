% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living-Constitution Interpretive Regime (US 1787 Kernel)
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   Under this reading, the 1787 Constitution operates as an aspirational
 *   framework whose meaning is updated by sitting institutions — chiefly the
 *   Supreme Court — as social understandings shift. The enumerated text
 *   anchors governmental structure; unenumerated principles such as privacy,
 *   dignity, and autonomy enter through aspirational clauses and
 *   evolving-standards reasoning. The regime solves a real problem: the
 *   formal amendment threshold is so high that adaptation by amendment almost
 *   never occurs, so an informal channel carries the entire adaptive load.
 *   The same channel concentrates meaning-fixing authority in courts and in
 *   the professional class that supplies them with accounts of social
 *   evolution; democratically enacted rules and rival meaning-fixing
 *   authorities bear the cost whenever the discerned norms cut against them.
 *   Capture episodes recur — the liberty-of-contract era is the canonical
 *   case — followed by corrections that themselves reaffirm the discernment
 *   forum. This file instantiates the living reading alone, with a single
 *   stable epsilon assessed over the standing arrangement it describes; the
 *   kernel contest is routed to the omega variables and kernel context, not
 *   averaged into these numbers.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda setter and primary collector ([institutional]/[constrained]) — selects, writes, and binds the evolved meanings
 *   - legal_academic_elite: secondary beneficiary ([powerful]/[mobile]) — supplies the content of 'evolved norms'; careers ride on the supply side
 *   - expanding_rights_claimants: beneficiary with contingent position ([powerless]/[trapped]) — their claims live or die by the discernment
 *   - state_democratic_majorities: primary payer ([organized]/[constrained]) — their enactments fall when evolved norms cut against them
 *   - popular_constitutional_movements: excluded payer ([organized]/[trapped]) — locked out of the forum where meaning is fixed
 *   - originalist_advocates: payer with unusual exit ([powerful]/[mobile]) — loses inside the regime but can capture the agenda-setter seat
 *   - comparative_law_scholars: analytical observer ([analytical]/[analytical]) — sees the full structure and comparable designs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.55).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.52).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living-Constitution Interpretive Regime (US 1787 Kernel)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'ca87969f-51cd-4f85-a318-a3fd27b96d0c').
narrative_ontology:cs_kernel_codification('ca87969f-51cd-4f85-a318-a3fd27b96d0c', fixed_text).
narrative_ontology:cs_authority_grounding('ca87969f-51cd-4f85-a318-a3fd27b96d0c', expertise).
narrative_ontology:cs_interpretation_layer_present('ca87969f-51cd-4f85-a318-a3fd27b96d0c').
narrative_ontology:cs_reading_relation('ca87969f-51cd-4f85-a318-a3fd27b96d0c', us_constitution_1787__originalist_reading, influences).
narrative_ontology:cs_reading_relation('ca87969f-51cd-4f85-a318-a3fd27b96d0c', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('ca87969f-51cd-4f85-a318-a3fd27b96d0c', foundational, constitutional_meaning_tracks_evolving_norms).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_evolving_norms, holdable).
narrative_ontology:cs_axiom_grounding('ca87969f-51cd-4f85-a318-a3fd27b96d0c', constitutional_meaning_tracks_evolving_norms, instrumental).
narrative_ontology:cs_axiom('ca87969f-51cd-4f85-a318-a3fd27b96d0c', secondary, unenumerated_rights_are_constitutionally_protected).
narrative_ontology:cs_axiom_status(unenumerated_rights_are_constitutionally_protected, holdable).
narrative_ontology:cs_axiom_grounding('ca87969f-51cd-4f85-a318-a3fd27b96d0c', unenumerated_rights_are_constitutionally_protected, deontological).
narrative_ontology:cs_reference_frame('ca87969f-51cd-4f85-a318-a3fd27b96d0c', enduring_aspirational_charter).
narrative_ontology:cs_drift_state('ca87969f-51cd-4f85-a318-a3fd27b96d0c', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ca87969f-51cd-4f85-a318-a3fd27b96d0c', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, legal_academic_elite).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, expanding_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_democratic_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, popular_constitutional_movements).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_advocates).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, unenumerated_rights_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine life-tenured judges decide which social changes count as constitutional evolution. They select which disputes reach them, write the operative meanings, and bind every other actor to those meanings through final, enforceable judgments. Each successful updating adds to their doctrinal authority; each divisive one draws legitimacy attacks, confirmation fights, and curbing proposals aimed at their institution. Exit is individual retirement; the seat itself passes to a successor facing the same incentives.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, supreme_court_justices, beneficiary).

% Law professors, elite practitioners, and think-tank lawyers produce the arguments about what society's values now require. Citation networks, clerkship pipelines, consultancy, and scholarly reputation all run through supplying persuasive accounts of evolving norms to the courts. They can and do change camps when the professional wind shifts; leaving constitutional discourse entirely is possible but costly to a career built on it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legal_academic_elite, beneficiary,
    powerful, biographical, mobile, national).

% Litigants whose claims — privacy, bodily autonomy, dignity, intimate association — have no firm anchor in the enumerated text. Their claims succeed only when a court reads their interest into the charter's aspirational sweep. Legislatures have often refused them relief, and the formal amendment route is effectively closed, so this forum is the only door they have. When the discernment turns against them, the same mechanism withdraws what it earlier conferred.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, expanding_rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% State electorates and legislatures whose enacted policies are struck down when courts read evolving national norms against them. They can amend their state constitutions, petition Congress, or pursue federal constitutional amendments that almost never succeed. Their policy choices survive only inside the boundaries of judicially articulated evolving principles, redrawn without their consent.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_democratic_majorities, payer,
    organized, generational, constrained, regional).

% Grassroots movements that hold that the people themselves may fix constitutional meaning through elections, mobilization, and direct action. Their interpretations reach the authoritative conversation only insofar as courts choose to incorporate them; the decisive argument happens in courtrooms and law reviews they do not staff. They would insist that meaning-discernment belong outside the professions, and they absorb the loss each time their readings are displaced by judicially announced ones.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, popular_constitutional_movements, excluded,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, popular_constitutional_movements, payer).

% Judges, scholars, and advocates committed to fixing constitutional meaning at ratification. Under this regime they lose the cases that turn on unenumerated or evolved principles, and their method survives chiefly as dissent and academic counter-programming. Their exit options are unusually good for a losing side: they can capture appointing pipelines and, periodically, the courts themselves — converting their seat into the agenda-setting one.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_advocates, payer,
    powerful, biographical, mobile, national).

% Analysts comparing this arrangement with other jurisdictions' adaptation machinery — structured amendment rules, living-tree doctrines, legislative override clauses, council-based constitutional renewal. They collect nothing from the arrangement and pay nothing into it; they record which designs keep adaptation accountable and which let it drift.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, comparative_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the adaptation problem of a supermajority-amendment charter governing a rapidly transforming society: Article V's threshold is so high that formal amendment almost never succeeds, so an informal channel — judicial updating of meaning in light of social evolution — keeps the 1787 framework operative across technological, demographic, and moral change.
% TRANSFER_FUNCTION: Moves meaning-fixing authority from the ratified text and its ratification-era public meaning to sitting judges and the professional class that supplies accounts of evolving norms; moves binding force from democratically enacted rules to judicially discerned principles, and moves the costs of overridden policy onto the enacting majorities.
% ABSENT_VOICES: Popular constitutional movements and ordinary citizens whose norms are discerned on their behalf are not in the room where meaning is fixed; state governments appear only as litigants; the ratification generation's understanding enters as evidence rather than as a party. Dissenting seats participate only at the sufferance of the forum's gatekeepers.
% DISAPPEARANCE_RATIONALE: If the evolutionary-interpretive regime vanished overnight and meaning snapped to fixed text plus amendments, large bodies of settled doctrine resting on unenumerated or evolved principles would be destabilized, millions of reliance interests built on those doctrines would be thrown open, rights claimants would lose their only functioning forum, and the Court's role would contract sharply toward a text-bound tribunal — the constitutional order would visibly reorganize.
% FOUNDING_PROBLEM: A charter drafted for an agrarian, slave-holding republic of four million people was expected to govern an industrial and post-industrial continental nation; its framers said it was built to endure for ages, and by the Progressive and New Deal eras a fixed-meaning approach was visibly obstructing democratic responses to industrial crisis, making an adaptation channel the price of the charter's survival.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding corroborate the endurance-and-adaptation expectation from outside the beneficiary set (Marshall's McCulloch opinion, framers' correspondence); political scientists document the near-zero amendment rate that makes some adaptation channel arithmetically necessary; and rival-method scholars — who reject judicial updating as the remedy — nonetheless concede the obsolescence problem is real. Corroboration for the problem's existence is broad; corroboration for this particular remedy is contested along exactly the lines the kernel contest records.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: substantial but bounded — the regime genuinely carries the adaptation load a broken amendment process cannot, yet recurrent capture episodes (the liberty-of-contract court most clearly) show real rents flowing to the discernment elite, and the endpoint value reflects a post-correction phase. Suppression is authored at 0.52 as a raw structural property, unscaled by power or scope: finality, contempt-backed enforcement against states, certiorari discretion, and the marginalization of rival fora constitute the coercive machinery, moderated by the fact that rival methods remain fully legal to practice and publish. Theater ratio 0.33: aspirational rhetoric sometimes decorates outcome-driven updating, but the core adjudicative function is real work performed continuously. Accessibility collapse is low (0.35) because this is precisely a contested kernel — rival interpretive methods remain available, practiced, and institutionally represented, so alternatives do not collapse upon understanding. Resistance is high (0.6): sustained counter-movements, confirmation wars, court-curbing proposals, and jurisdiction-stripping advocacy meet the regime continuously. The claim (tangled_rope) and the metrics were authored independently: the claim asserts a genuine coordination function joined to asymmetric extraction under active enforcement; the metrics describe observed operation without being tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the bench, the regime is stewardship of an enduring charter — each updating a fulfillment of the founders' design for permanence-through-adaptation. From the state-majority seat, the same structure is governance by unelected discerners whose norms arrive without consent and leave without appeal. From the rights-claimant seat, it is the only door in the building. Identity-lock operates on both elite sides: judicial identity fuses with the guardian-of-the-living-charter role, and academic careers are constituted by the discernment contest itself, so exit is unthinkable for the people who run the regime even when it is formally available. If that professional frame broke — if meaning-fixing authority migrated durably to popular or legislative fora — the academic supply side would collapse first and the Court's updating function would atrophy into performance.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court sits nearest the beneficiary pole: it collects doctrinal authority from every successful updating and controls the rules of collection. The academic elite shares that pole with high mobility damping its exposure further. Rights claimants are declared beneficiaries but occupy a contingent position — their subsidy depends on continued favorable discernment, which the measurement series shows reversing (the mechanism that conferred withdrew in 2022). State democratic majorities and popular movements sit nearest the target pole: constrained or trapped exit, costs imposed without consent. Originalist advocates are declared victims whose mobility and capture capacity pull their effective exposure down from what a trapped victim would bear — the derivation handles this through exit options, so no directionality override is authored; the residual nuance is recorded in the capture omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — charter obsolescence under an impassable amendment threshold — is live, so this is not a resolved-mandatrophy case; the danger runs the other way, toward mandate inflation, where 'adaptation' expands from carrying the load into routine policy revision, the liberty-of-contract pattern. Classifying the regime as tangled_rope preserves both halves of the structure: a rope label would hide the recurring elite capture of norm-discernment, while a snare label would erase the adaptation function without which the charter becomes either irrelevant or perpetually crisis-prone. On the mismatch consumer: founding_problem_status is live and the disappearance verdict is world_rearranges, so no dead-mandate/zombie flag is warranted; the piton signature is likewise distant, since the agenda setter profits richly enough to maintain the machinery vigorously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the us_constitution_1787 kernel. What would the originalist and positivist sibling readings change structurally, and where exactly is the disagreement located?',
    'Side-by-side compilation of the three readings over identical case histories, comparing victim sets, epsilon, and enforcement requirements; foreclosure analysis of whether any pair of readings can coexist within a single interpretive framework.',
    'Sibling readings relocate the victim set (ratification-generation expectations vs. democratically enacted rules vs. elite-discerned norms) and shift epsilon substantially; the classification in this file applies only to the living reading and must not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is the living reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    evolving_norms_discernment_capture,
    'Who determines which norms have ''evolved,'' and is that discernment captured by a professional elite rather than tracking the society whose norms they purport to articulate?',
    'Compare the institutional and demographic profile of the sources courts cite for evolving standards against the population governed by the resulting rules; test responsiveness of doctrinal change to documented public-opinion shifts versus professional-consensus shifts.',
    'If discernment is captured, effective extraction rises toward the snare boundary and the coordination half of the hybrid weakens; if genuinely dialogic, the arrangement sits closer to the rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_norms_discernment_capture, empirical, 'Elite capture of the ''evolving norms'' input channel — the flagged vulnerability of this reading.').

omega_variable(
    article_v_substitution_question,
    'Is judicial updating substituting for a broken amendment process (coordination) or preempting a workable one (extraction around democratic channels)?',
    'Counterfactual analysis of amendment campaigns abandoned because litigation was cheaper, versus campaigns never attempted; comparison with jurisdictions whose amendment thresholds are usable and whose courts therefore update less.',
    'Substitution supports the coordination half of the tangled_rope classification; preemption would recast the updating as rent-seeking that blocks a democratic repair path, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_v_substitution_question, empirical, 'Whether the informal adaptation channel complements or crowds out formal amendment.').

omega_variable(
    repudiation_cycle_reversibility,
    'Is the current repudiation pressure on this reading a terminal displacement of the living-interpretive regime, or another phase of the two-century oscillation visible in the measurement series?',
    'Track appointment-pipeline composition, retention rates of evolved precedents, and reliance-interest entrenchment over the coming decade; compare against the post-Lochner and post-Brown correction phases.',
    'Terminal displacement would date this constraint''s lifecycle decline; continued oscillation confirms the cyclical reinforcement pattern in which each swing re-legitimates the discernment forum itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(repudiation_cycle_reversibility, empirical, 'Whether the current anti-evolutionary phase is cyclical or terminal.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of rival meaning-fixing authorities structural (finality, contempt, certiorari control) or internalized (professional socialization that treats judicial supremacy as the natural order)?',
    'Post-institutional-change trajectory: if state courts and popular fora resume independent meaning-fixing when finality machinery loosens, suppression was structural; if they continue deferring in the absence of coercive backing, it was internalized.',
    'If internalized, true suppression exceeds the structural measure and persists after any institutional reform — the targets carry the deference with them after exit opens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized component of the regime''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1803, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usconst_living_tr_t1803, us_constitution_1787__living_reading, theater_ratio, 1803, 0.15).
narrative_ontology:measurement(usconst_living_tr_t1857, us_constitution_1787__living_reading, theater_ratio, 1857, 0.35).
narrative_ontology:measurement(usconst_living_tr_t1905, us_constitution_1787__living_reading, theater_ratio, 1905, 0.4).
narrative_ontology:measurement(usconst_living_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(usconst_living_tr_t1965, us_constitution_1787__living_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(usconst_living_tr_t1973, us_constitution_1787__living_reading, theater_ratio, 1973, 0.35).
narrative_ontology:measurement(usconst_living_tr_t2003, us_constitution_1787__living_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(usconst_living_tr_t2022, us_constitution_1787__living_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(usconst_living_tr_t2025, us_constitution_1787__living_reading, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(usconst_living_be_t1803, us_constitution_1787__living_reading, base_extractiveness, 1803, 0.3).
narrative_ontology:measurement(usconst_living_be_t1857, us_constitution_1787__living_reading, base_extractiveness, 1857, 0.62).
narrative_ontology:measurement(usconst_living_be_t1905, us_constitution_1787__living_reading, base_extractiveness, 1905, 0.68).
narrative_ontology:measurement(usconst_living_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(usconst_living_be_t1965, us_constitution_1787__living_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(usconst_living_be_t1973, us_constitution_1787__living_reading, base_extractiveness, 1973, 0.6).
narrative_ontology:measurement(usconst_living_be_t2003, us_constitution_1787__living_reading, base_extractiveness, 2003, 0.52).
narrative_ontology:measurement(usconst_living_be_t2022, us_constitution_1787__living_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(usconst_living_be_t2025, us_constitution_1787__living_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(usconst_living_su_t1803, us_constitution_1787__living_reading, suppression_requirement, 1803, 0.25).
narrative_ontology:measurement(usconst_living_su_t1857, us_constitution_1787__living_reading, suppression_requirement, 1857, 0.45).
narrative_ontology:measurement(usconst_living_su_t1905, us_constitution_1787__living_reading, suppression_requirement, 1905, 0.5).
narrative_ontology:measurement(usconst_living_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(usconst_living_su_t1965, us_constitution_1787__living_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(usconst_living_su_t1973, us_constitution_1787__living_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(usconst_living_su_t2003, us_constitution_1787__living_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(usconst_living_su_t2022, us_constitution_1787__living_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement(usconst_living_su_t2025, us_constitution_1787__living_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Constitution's authority' decomposes into three structurally distinct interpretive regimes sharing one kernel (us_constitution_1787). This file is the living reading; the originalist and positivist readings are separate stories with their own epsilon and victim sets. The living reading upstream-shaped both siblings historically — originalism organized as a counter-movement against living-reading dominance, and the positivist reading hardened its text-plus-amendments formula in response to unenumerated-rights doctrine — which is why this file declares an influences edge to the originalist sibling and a coexists_with edge to the positivist sibling. Decomposition follows the epsilon-invariance principle: measuring 'constitutional constraint' across all three readings at once would average incompatible victim structures into a meaningless scalar.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
