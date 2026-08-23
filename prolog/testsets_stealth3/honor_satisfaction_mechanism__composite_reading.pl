% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Gentlemanly Honor Satisfaction Mechanism (Code Duello Obligation Structure)
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   For roughly three centuries the gentlemanly duel operated as the licensed
 *   mechanism for settling grave personal offense among Europe's armed
 *   elites: a codified choreography of challenge, seconded negotiation, and
 *   bounded combat that promised satisfaction while limiting blood. It was
 *   defended as civilization itself and condemned as murder in lace. This
 *   story instantiates the COMPOSITE READING of that arrangement: its
 *   dissolution was not one event but the separate failure of independent
 *   supports - the state's consolidation of a violence monopoly that first
 *   threatened and eventually prosecuted the duel; a bourgeois normative
 *   economy that priced prudence over point d'honneur and starved the code of
 *   new recruits; life-insurance exclusions and pension forfeitures that
 *   converted honor into actuarial insolvency; and a recategorization that
 *   moved the duel from sacred duty to absurd relic. Claim and metrics are
 *   authored independently: the claimed type is tangled_rope - the operative
 *   core genuinely coordinated (feud prevention, bounded violence,
 *   class-boundary signaling) while extracting coerced participation and
 *   lives - while the end-state metrics describe a thinning, theatrical
 *   residue whose profile is piton-shaped. Assumptions stated: the interval
 *   maps one time unit to five years (t0 approx. 1700, the zenith of the
 *   codified code duello; tn approx. 2000, residual ceremonial forms only);
 *   sibling constraint identifiers are taken as the kernel-manifest tokens
 *   decline_reading and contraction_reading; the epsilon referent is the
 *   standing honor-satisfaction arrangement under contest, assessed by this
 *   reading's lights, never the rights-respecting or state-administered
 *   alternative this reading would endorse. Linked sibling stories belong to
 *   the same constraint family (see network.dual_formulation_note).
 *
 * KEY AGENTS:
 *   - officer_corps_honor_establishment: Primary beneficiary and co-administrator ([institutional]/[identity_locked]) - collects deference and promotion-rents while running the honor tribunals
 *   - landed_aristocracy: Class-boundary beneficiary ([powerful]/[constrained]) - satisfaction marks the caste line
 *   - dueling_professionals: Fee-collecting beneficiary ([organized]/[arbitrage]) - masters, gunsmiths, seconds, surgeons
 *   - reluctant_principals: Primary target ([moderate]/[trapped]) - coerced combatants bearing the arrangement's mortal costs
 *   - challenge_refusers: Penalty-bearing target ([moderate]/[trapped]) - pay the ostracism price for declining
 *   - bereaved_duel_widows: Collateral target ([powerless]/[trapped]) - inherit the losses with no remedy
 *   - civil_state_authorities: Counter-agenda-setter and jurisdictional loser ([institutional]/[constrained]) - defied for two centuries, enforcer only after the payoff matrix shifted
 *   - commercial_bourgeoisie: Excluded normative challenger ([organized]/[arbitrage]) - financed the reframing from outside the honor conversation
 *   - life_insurance_underwriters: Excluded actuarial repricer ([institutional]/[arbitrage]) - converted honor into uninsurable risk
 *   - ceremonial_mensur_fraternities: Terminal-residue administrator ([organized]/[identity_locked]) - keeps the descendent ritual alive as identity work
 *   - historical_sociology_analysts: Analytical observer ([analytical]/[analytical]) - reconstructs the structure from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.15).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.14).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Gentlemanly Honor Satisfaction Mechanism (Code Duello Obligation Structure)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical sociology/legal history/normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '716b1a1c-4e4f-4079-a48f-a35e5a023be8').
narrative_ontology:cs_kernel_codification('716b1a1c-4e4f-4079-a48f-a35e5a023be8', distributed).
narrative_ontology:cs_authority_grounding('716b1a1c-4e4f-4079-a48f-a35e5a023be8', practice).
narrative_ontology:cs_interpretation_layer_present('716b1a1c-4e4f-4079-a48f-a35e5a023be8').
narrative_ontology:cs_reading_relation('716b1a1c-4e4f-4079-a48f-a35e5a023be8', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('716b1a1c-4e4f-4079-a48f-a35e5a023be8', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('716b1a1c-4e4f-4079-a48f-a35e5a023be8', foundational, erosion_required_independent_parallel_pressures).
narrative_ontology:cs_axiom_status(erosion_required_independent_parallel_pressures, holdable).
narrative_ontology:cs_axiom_grounding('716b1a1c-4e4f-4079-a48f-a35e5a023be8', erosion_required_independent_parallel_pressures, empirically_contingent).
narrative_ontology:cs_axiom('716b1a1c-4e4f-4079-a48f-a35e5a023be8', secondary, recategorization_is_the_terminal_step).
narrative_ontology:cs_axiom_status(recategorization_is_the_terminal_step, holdable).
narrative_ontology:cs_axiom_grounding('716b1a1c-4e4f-4079-a48f-a35e5a023be8', recategorization_is_the_terminal_step, empirically_contingent).
narrative_ontology:cs_reference_frame('716b1a1c-4e4f-4079-a48f-a35e5a023be8', plural_pillar_honor_order).
narrative_ontology:cs_drift_state('716b1a1c-4e4f-4079-a48f-a35e5a023be8', post_multimechanism_erosion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('716b1a1c-4e4f-4079-a48f-a35e5a023be8', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, officer_corps_honor_establishment).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, landed_aristocracy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, dueling_professionals).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, ceremonial_mensur_fraternities).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, reluctant_principals).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, challenge_refusers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, bereaved_duel_widows).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, civil_state_authorities).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, code_duello_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, point_d_honneur_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior officers and regimental honor tribunals set the terms under which affairs of honor were judged: they convened boards, ruled on whether an apology sufficed, and decided which refusals ruined a career. Promotion, command trust, and standing in the mess flowed to men with clean courage-display records, and senior men rarely stood on the ground themselves while administering the expectation. Leaving the code would have meant disavowing the institution's own legitimacy narrative; retirement into civilian life was the only exit, and few took it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, officer_corps_honor_establishment, beneficiary,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, officer_corps_honor_establishment, agenda_setter).

% Great houses treated the right of satisfaction as a badge separating gentle blood from trade: artisans and merchants could neither give nor demand it. The code disciplined their sons' quarrels while marking the family as belonging to the governing caste. As parliamentary careers, finance, and marriage alliances opened routes to standing that required no sword, abandoning the code grew cheaper, and many families simply stopped mentioning it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, landed_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Fencing masters, gunsmiths specializing in pairs of smoothbore pistols, hired seconds, and dueling surgeons earned their living preparing and patching up affairs of honor. Their skills transferred readily to sporting clubs, theatrical swordplay, and civilian surgery once demand dried up, so they followed the market rather than defending the custom.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_professionals, beneficiary,
    organized, biographical, arbitrage, continental).

% Most men who stood on the dueling ground did not want to be there: they feared death or killing far more than they treasured vengeance, but a refused challenge meant dismissal from the regiment, barred doors in society, unmarriageable daughters, and print mockery. Emigration or a quiet transfer to a colonial posting were the only escapes, and both cost everything built at home.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, reluctant_principals, payer,
    moderate, biographical, trapped, regional).

% Men who declined to fight - on religious grounds, on principle, or from simple prudence - bore the penalty price of the system: ostracism from mess and club, lost promotions, caricature in the press. Some rebuilt reputations through war service or public usefulness; most endured quiet, permanent exclusion.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, challenge_refusers, payer,
    moderate, biographical, trapped, national).

% Wives, children, and dependent parents of killed or crippled principals inherited the loss with no remedy: the duel being illegal, no court would entertain a claim; insurers paid nothing under the dueling exclusion; and polite opinion held that the family had been paid in honor. Remarriage and charity were the practical exits.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bereaved_duel_widows, payer,
    powerless, biographical, trapped, local).

% Crowns and parliaments issued edict after edict against dueling from the early seventeenth century onward, and later generations built workable prosecution: courts-martial, loss of commission, prison terms. Every unpunished duel was a public demonstration that private violence could overrule royal justice, so the sovereign bore the arrangement's defiance directly; yet for two centuries juries and judges declined to convict peers, and enforcement waited on social change no statute alone could produce.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, civil_state_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, civil_state_authorities, payer).

% Merchants, manufacturers, and professionals stood outside the honor conversation: their money bought no satisfaction and their persons could not be challenged. They financed the press campaigns, evangelical societies, satirical novels, and parliamentary inquiries that reframed the duel as murder dressed in lace, and their own respectability economy - creditworthiness, insurance, prudent self-command - offered the rising generation a standing that needed no pistol.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, commercial_bourgeoisie, excluded,
    organized, biographical, arbitrage, continental).

% Life offices found gentlemen insured against duel-related death an adverse class: relatives of famous dueling families bought oversized policies, and a code that sent breadwinners to dawn meetings produced claims the premiums never priced. Offices wrote dueling exclusions into standard contracts in the nineteenth century, quietly converting a point of honor into an actuarial disqualification that reached households the state's edicts never touched.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, life_insurance_underwriters, excluded,
    institutional, generational, arbitrage, global).

% Student fencing corporations in Germany and Austria preserve the last living descendant of the code: heavily protected, medically staffed, strictly regulated facial-cutting bouts from which the lethal stakes have been deliberately engineered out. Membership networks and the visible scar bind the corporations together; dropping the ritual would dissolve what the corporation understands itself to be, so the practice continues by unanimous internal conviction even under university and legal disapproval.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, ceremonial_mensur_fraternities, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, ceremonial_mensur_fraternities, beneficiary).

% Historians and sociologists of violence - from the state-monopoly-of-violence thesis through court-society studies to the modern dueling histories built on trial records, regimental archives, insurance ledgers, and pamphlet wars - reconstruct the arrangement and supply the comparative framework in which its several gravediggers can be weighed against one another.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, historical_sociology_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, officer_corps_honor_establishment).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channeled potentially lethal private quarrels among armed men of nominally equal rank into a rule-bounded single-combat format with seconds, staged pauses for apology, and agreed weapons and distance - containing feud cycles, ambush, and factional slaughter - while simultaneously policing the boundary of who counted as a gentleman entitled to give and demand satisfaction.
% TRANSFER_FUNCTION: Moves life, limb, and standing: death-and-injury risk onto principals (disproportionately junior and reluctant men); deference, promotion, and precedence toward proven courage-displayers and the senior men who administer the code; fees toward fencing masters, arms makers, hired seconds, and surgeons; and, in the erosion phase, enforcement costs onto prosecutors and actuarial losses onto insurers and bereaved families.
% ABSENT_VOICES: The commercial middle classes (barred from giving or demanding satisfaction and therefore from the conversation that set the code's terms), women and widows who bore its losses without standing in it, refusers silenced by shame, and commoners excluded altogether. They spoke from outside: chapbooks and satirical novels, evangelical tracts, actuarial ledgers, and eventually parliamentary inquiry rooms.
% DISAPPEARANCE_RATIONALE: Contemporaries disputed exactly this. Honor partisans predicted that abolishing satisfaction would dissolve officer cohesion and leave gentlemen exposed to insult without redress; abolitionists answered that nothing of value would vanish except the killing. The record favors the abolitionists - where the code died, officer corps reorganized around courts-martial and examination, sociability migrated to clubs and parliaments, and no feud epidemic returned - but the disagreement was real, prolonged, and sincerely held, so the verdict is recorded as contested rather than rearranged.
% FOUNDING_PROBLEM: Late-medieval and Renaissance elite violence: armed gentlemen of equal standing, with no superior able to adjudge their quarrels promptly, facing a choice between swallowing intolerable offense and open feud. The code duello was built to convert that choice into a bounded ritual - one challenge, two principals, chosen weapons, seconds empowered to stop it - that preserved standing without unleashing vendetta.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state-formation scholarship and court-martial records show the founding problem (private adjudication of deadly quarrels among the armed) dissolving into public justice; military archives show honor courts replaced by disciplinary boards; and the surviving fencing corporations themselves concede the shift by redescribing their ritual as character-formation rather than satisfaction. No living party maintains the founding problem in its original form.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).
:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All three series run on one shared grid (t = 0,10,20,30,40,50,60; one unit = five years), so every metric is authored at every examined point. Base extractiveness falls 0.78 -> 0.15 as the four levers bite in overlapping sequence: criminalization with eventual teeth, bourgeois exit from the recruit pool, actuarial disqualification of duel-death, and finally recategorization. Suppression_requirement falls (0.66 -> 0.08) - an enforcement-decay trajectory, authored here because the story specifically tracks the thinning of the compulsion apparatus (ostracism machinery, honor tribunals, mess discipline) as exits opened; the structural suppression scalar ends at 0.14, retaining only intra-fraternity conformity pressure. Theater_ratio rises past 0.5 around t=45 (mid-twentieth century): the scar becomes a credential detached from satisfaction, proxy goals replace the real function, and the terminal residue is predominantly performed - the classic piton symptom, authored honestly even though the claim names the operative core. Accessibility_collapse is low (0.30) because the code never eliminated alternatives - apology protocols, seconded mediation, litigation, and studied contempt coexisted with it even at zenith - and at end state every alternative stands open. Resistance is high (0.68): three centuries of royal edicts, canon-law condemnation, press campaigns, evangelical agitation, and the long era in which courts declined to enforce the code's penalties. Suppression is authored as a raw structural property and is not scaled here; scaling of extractiveness by directionality and scope is the engine's arithmetic. Base properties report end-state values, matching the t=60 measurements.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structure. From the honor-establishment seat the arrangement is a sacred, self-enforcing order it administers and rarely bleeds in; from the reluctant-principal and refuser seats the same choreography is terror-backed extraction with the exits bricked up; from the state seat it is a standing usurpation of sovereign justice; from the excluded bourgeois and insurer seats it is an alien status economy whose demolition cost them nothing. Coalition failure among the payer seats is structural: the code individualized every affair (one challenge, one response), so refusers were isolated examples rather than a class, and widows were dispersed and legally remediless - the arrangement suppressed exactly the coordination that could have resisted it. Across the interval the same ritual form hosts opposite directionalities: what the eighteenth-century principal experienced as compulsion, the twenty-first-century fencer experiences as voluntary meaning-making.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (establishment, aristocracy, professionals, terminal fraternities) derive low directionality - the arrangement subsidizes them; declared victims (reluctant principals, refusers, widows, and the state) derive high directionality, amplified toward full-target by trapped exits for the human payers and dampened toward symmetry for the arbitrage-grade professionals who pivoted out. Identity_lock binds the establishment seniors and the end-state fraternity members, whose self-concept is constituted by the code and its descendant respectively. The state occupies an unusual configuration: it is declared among the arrangement's victims because every duel taxed its judicial monopoly, while its agenda-setter role belongs to the erosion phase - an agenda-setter that is simultaneously a target of the thing it eventually dismantled. No directionality_overrides are authored: the derivation from declared roles, power, and exit reproduces the intended ordering, and the override surface is keyed by power atom, which cannot distinguish the three institutional seats (establishment, state, insurers) without misassigning at least one.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the code duello as a pure snare erases the genuine coordination achievement - three centuries of feud suppression among armed elites, bounded combat with reconciliation exits - which is precisely why gentlemen defended it at catastrophic cost; reading it as a pure rope erases the coerced dead and the penalty-priced refusers. The tangled_rope claim holds both faces of the operative core. The falling suppression series and rising theater series then track the mandate outliving its function: the founding problem (private regulation of deadly quarrels among the armed) was dissolved by the very state-monopoly consolidation the arrangement spent two centuries defying, so the R5 interview records status dead while the disappearance verdict stays contested - the contest itself is historical datum, not a zombie flag. Finally, the terminal residue's profile (low extraction, high theater, no concentrated capturer of gains in the end state, identity-locked administrators who could change the ritual only by dissolving themselves) is what keeps the ceremonial Schmiss culture from being mistaken for a living satisfaction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (composite_reading) of the honor_satisfaction_mechanism kernel; what would the sibling readings change structurally?',
    'Compare the compiled sibling stories against the shared referent: decline_reading authors a fringe-survival end state (positive residual extraction, moderate accessibility collapse); contraction_reading authors categorical impossibility (accessibility collapse near ceiling, extraction near zero). Whichever terminal structure the comparative record supports selects the correct sibling epsilon profile.',
    'Adopting contraction_reading would push accessibility_collapse toward 0.85+ and end-state extraction toward zero for the shared referent; adopting decline_reading would keep end-state extraction positive wherever fringe practice survived. This file''s composite structure instead distributes causality across four independent mechanisms and locates the terminal step in recategorization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure of the shared honor-satisfaction kernel.').

omega_variable(
    single_lever_counterfactual,
    'Would any single mechanism - sovereign criminalization, the bourgeois normative economy, actuarial exclusion, or recategorization alone - have sufficed to dissolve the arrangement?',
    'Comparative jurisdictional analysis: regions where one mechanism acted without the others (early criminalization atop an intact honor economy; heavily commercialized regions under tolerant magistracies). Persistence of the practice under single-lever conditions falsifies single-lever sufficiency.',
    'If one lever sufficed, the composite reading collapses toward a simpler causal account and the arrangement''s tangled_rope persistence window shortens; if none sufficed, the multi-pillar reference frame is confirmed and the four-mechanism axiom stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_lever_counterfactual, empirical, 'Whether the erosion required genuinely independent parallel pressures.').

omega_variable(
    refusal_compulsion_internalization,
    'Was the compulsion binding reluctant principals primarily structural (regimental dependency, social access, marriage markets) or internalized (honor conscience experienced as self-binding)?',
    'Refuser and survivor testimony after sanctions lapsed: shame that evaporated with the sanction indicates structural compulsion; shame persisting after exit indicates internalized fusion.',
    'Internalized compulsion predicts the observed lag between legal tolerance and behavioral extinction and raises the identity-lock component of end-state persistence; purely structural compulsion predicts rapid decay once careers decoupled from the code.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refusal_compulsion_internalization, empirical, 'Structural versus internalized mechanism behind refusal penalties.').

omega_variable(
    actuarial_lever_weight,
    'How much of the nineteenth-century decline traces to insurance exclusions and pension forfeiture rather than to normative change?',
    'Dating exclusion-clause adoption in insurer archives against duel-fatality series across jurisdictions differing in life-insurance penetration.',
    'Re-weights the composite mechanism set; a negligible actuarial lever narrows the reading to state-plus-norms and weakens the breadth of the independent-pressures axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actuarial_lever_weight, empirical, 'Magnitude of the insurance mechanism within the composite account.').

omega_variable(
    recategorization_cause_or_effect,
    'Is category-shift an independent pressure in its own right, or a downstream effect of the three material mechanisms?',
    'Chronological tracing of category rhetoric (satire, medicalization of dueling mania, criminal-law drafting) against the dated onset of each material mechanism.',
    'If downstream, the composite reading reduces to a tri-mechanism core and the terminal-step axiom becomes derivative rather than independent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recategorization_cause_or_effect, empirical, 'Independence of the recategorization mechanism.').

omega_variable(
    terminal_residue_function_status,
    'Does the surviving ceremonial bout (regulated facial-cutting in student fencing corporations) still perform a live coordination function - fresh identity-cohesion work - or is it inertial maintenance of an atrophied one?',
    'Test whether corporate recruitment, alumni bonding, and member retention measurably depend on the ritual versus merely persist through it; compare corporations that abandoned the ritual.',
    'A live function would reclassify the terminal residue toward a renewed narrow coordination form; inertial maintenance confirms the piton-shaped terminal profile the measurement series projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_residue_function_status, empirical, 'Live function versus theatrical maintenance in the end-state residue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_mechanism__composite_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__composite_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__composite_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(hono_tr_t50, honor_satisfaction_mechanism__composite_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__composite_reading, theater_ratio, 60, 0.8).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(hono_be_t50, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 60, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(hono_su_t50, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 60, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel label 'how the gentlemanly duel ended'. The colloquial label conflates three structurally distinct claims, split here per the epsilon-invariance principle: decline_reading (a frequency-course description: the practice persisted at declining frequency to fringe status - moderate residual extraction, partial accessibility collapse), contraction_reading (a terminal-category claim: dueling became cognitively unthinkable - accessibility collapse near ceiling, extraction near zero), and this file, composite_reading (a causal-pluralism claim: dissolution required several independent material pressures plus recategorization - a distributed terminal profile). Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked here via network edges rather than averaged into one story. Upstream/downstream: composite_reading's demonstrated independence of the material levers exerts downstream pressure on contraction_reading's monism (documented in cs_structure.reading_relations), while decline_reading is an orthogonal course-description that coexists with both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
