% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Gate (Geneva III Article 4 Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of combatant status in
 *   the law of armed conflict: prisoner-of-war status and the combatant
 *   privilege attach only to members of formal state military organizations
 *   meeting the Article 4 criteria of Geneva Convention III, and non-state
 *   fighters are excluded categorically, whatever their organization,
 *   discipline, or conduct. The arrangement performs a real coordination
 *   function - a closed, verifiable class of lawful fighters keeps civilians
 *   outside the target set and gives captured soldiers a treatment floor -
 *   while transferring legal security asymmetrically: state forces hold
 *   immunity and guaranteed treatment; non-state fighters, once captured,
 *   enter the domestic criminal track, prosecutable for the act of fighting
 *   itself. The claim and the metrics are authored independently: the claimed
 *   type records the structure I believe true (genuine coordination plus
 *   asymmetric extraction under active enforcement), and the metrics record
 *   the arrangement's actual operation as this reading's own lights assess it
 *   over the standing arrangement under contest. Sibling readings of the same
 *   kernel text are separate constraint files linked through the network
 *   block.
 *
 * KEY AGENTS:
 *   - - high_contracting_states: Agenda-setter and collecting seat ([institutional]/[arbitrage]) - drafted, administers, and interprets the Article 4 criteria; declines revisions that would dilute the closed class
 *   - - state_armed_forces: Primary beneficiary ([institutional]/[mobile]) - holds combatant immunity and prisoner-of-war guarantees as a condition of membership, not conduct
 *   - - non_state_fighters: Primary target ([powerless]/[trapped]) - bears prosecution exposure and detention without prisoner-of-war guarantees; no conduct-based route into the protected class
 *   - - national_liberation_movements: Excluded claimant ([organized]/[constrained]) - won a rival codification over major-power objection; outside the conversation in conflicts against states that rejected it
 *   - - civilians_in_conflict_zones: Dual-positioned ([powerless]/[trapped]) - protected by the fighter/civilian line, exposed by its erosion where fighters gain nothing by identifying themselves
 *   - - icrc: Institutional observer ([institutional]/[analytical]) - visits, registers, documents, advocates; determines no one's status
 *   - - international_war_crimes_tribunals: Judicial observer ([institutional]/[analytical]) - applies functional criteria that quietly bypass the categorical rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.67).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.65).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Gate (Geneva III Article 4 Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'a209a520-580b-4de9-862d-7ebfad04a5b5').
narrative_ontology:cs_kernel_codification('a209a520-580b-4de9-862d-7ebfad04a5b5', fixed_text).
narrative_ontology:cs_authority_grounding('a209a520-580b-4de9-862d-7ebfad04a5b5', extraction).
narrative_ontology:cs_interpretation_layer_present('a209a520-580b-4de9-862d-7ebfad04a5b5').
narrative_ontology:cs_reading_relation('a209a520-580b-4de9-862d-7ebfad04a5b5', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('a209a520-580b-4de9-862d-7ebfad04a5b5', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('a209a520-580b-4de9-862d-7ebfad04a5b5', foundational, state_affiliation_prerequisite_for_combatant_privilege).
narrative_ontology:cs_axiom_status(state_affiliation_prerequisite_for_combatant_privilege, holdable).
narrative_ontology:cs_axiom_grounding('a209a520-580b-4de9-862d-7ebfad04a5b5', state_affiliation_prerequisite_for_combatant_privilege, conventional).
narrative_ontology:cs_axiom('a209a520-580b-4de9-862d-7ebfad04a5b5', secondary, closed_combatant_class_preserves_civilian_immunity).
narrative_ontology:cs_axiom_status(closed_combatant_class_preserves_civilian_immunity, holdable).
narrative_ontology:cs_axiom_grounding('a209a520-580b-4de9-862d-7ebfad04a5b5', closed_combatant_class_preserves_civilian_immunity, instrumental).
narrative_ontology:cs_reference_frame('a209a520-580b-4de9-862d-7ebfad04a5b5', westphalian_state_monopoly_on_lawful_force).
narrative_ontology:cs_drift_state('a209a520-580b-4de9-862d-7ebfad04a5b5', contemporary_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a209a520-580b-4de9-862d-7ebfad04a5b5', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_armed_forces).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, high_contracting_states).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, westphalian_state_monopoly_on_lawful_force).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, positivist_textual_supremacy_of_article_4).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the Third Geneva Convention and administer its Article 4 status criteria. Each detaining power decides, under its own procedures, which captured fighters receive prisoner-of-war status and which are remanded to domestic criminal process. Major military powers declined to ratify the 1977 protocol that would have extended status to certain non-state movements, preserving the original text they administer. As parties to conflicts against irregular forces, they retain the choice to prosecute captured fighters for the fact of fighting, and they bear the reciprocity risk that their own captured personnel someday face an adversary unconstrained by the same text.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, high_contracting_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, high_contracting_states, beneficiary).

% Soldiers of recognized state militaries carry the combatant privilege: lawful acts of war cannot be prosecuted as murder, and captured service members must receive prisoner-of-war treatment with named protections through repatriation. Their entitlement rests on membership in a state's armed forces rather than on conduct, discipline, or distinctive insignia. When captured by adversaries, they invoke the same convention their government administers, and their treatment becomes a test of the adversary's compliance rather than of their own category.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_armed_forces, beneficiary,
    institutional, generational, mobile, global).

% Members of organized armed groups - insurgents, guerrillas, militia fighters - fight under command structures and often with uniforms or open arms carriage, yet fall outside Article 4's categories because their group lacks state affiliation. Once captured, no determination of their discipline or conduct changes their category: they face prosecution under domestic law for acts that would be lawful for a uniformed soldier, and detention without prisoner-of-war guarantees. Nothing available to them at capture alters the classification; their routes out of the category are demobilization, escape, or their side's victory.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_fighters, payer,
    powerless, biographical, trapped, regional).

% Anti-colonial and national liberation movements campaigned through the 1974-77 diplomatic conferences for combatant status for organized forces fighting occupation and racist regimes, winning Additional Protocol I Article 1(4) over the objection of major military powers. Movements in conflicts against states that never accepted that protocol remain outside the conversation their campaign addressed; captured fighters from these movements are processed under the same domestic-law track as any other irregular, and their political representatives negotiate status, if at all, bilaterally with the states that hold the pen.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, national_liberation_movements, excluded,
    organized, generational, constrained, continental).

% Civilians rely on the fighter/civilian line for protection from deliberate attack. The line's value to them depends on fighters having reasons to identify themselves; where fighters gain nothing by wearing insignia - because status is closed to them regardless - distinguishing marks fade and the population the fighters move among absorbs the targeting risk. Civilians also host the prosecutions, detentions, and security operations the regime produces, and bear the escalation that follows when captured fighters are tried as common criminals.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, civilians_in_conflict_zones, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, civilians_in_conflict_zones, payer).

% Visits detention facilities in most armed conflicts, registers detainees, and publishes commentaries and periodic reports on how status categories operate in practice. It advocates that all detainees receive at least the Common Article 3 floor and presses detaining powers on treatment gaps, but holds no authority to determine anyone's status and depends on state consent for access.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc, observer,
    institutional, generational, analytical, global).

% Ad hoc tribunals and the International Criminal Court adjudicate which organized armed groups count as parties to a conflict and which individuals may lawfully be targeted or prosecuted. The Tadic jurisprudence articulated functional criteria - organization, command responsibility, conduct of hostilities - that courts now apply independently of whether a group has state affiliation, shaping the environment in which detaining states make status calls without itself conferring prisoner-of-war status on anyone.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_war_crimes_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, high_contracting_states).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the wartime identification problem: a closed, verifiable class of lawful fighters lets adversaries know whom they may attack and detain as fighters, keeps civilians outside the target set, and gives captured soldiers a treatment floor tied to their category rather than to their captor's discretion.
% TRANSFER_FUNCTION: Moves legal security asymmetrically: state service members receive combatant immunity and prisoner-of-war guarantees; non-state fighters, whatever their organization or conduct, are moved onto the domestic criminal track - prosecutable for the act of fighting, detainable without POW protections - while the administrative power to make that call remains with the detaining state.
% ABSENT_VOICES: Non-state armed groups had no delegation at the 1949 Diplomatic Conference and none in the Article 4 drafting room; captured fighters appear in the regime only as objects of determination, never as participants. National liberation movements forced entry into the 1974-77 conferences and won a rival codification over major-power objection, but fighters in conflicts against states that rejected that protocol remain outside the conversation. Today's organized armed groups negotiate status bilaterally, if at all, with the governments that administer the criteria applied to them.
% DISAPPEARANCE_RATIONALE: If the categorical exclusion vanished overnight, every detaining power's processing of captured irregulars would reorganize: status-determination boards would convene, pending prosecutions for the fact of fighting would collapse or convert, and detention regimes built around the no-status category - military commissions, security-internment tracks - would lose their legal basis. State forces' own protections would survive untouched; the rearrangement falls entirely on the non-state side of the line, which is itself a measure of where the arrangement's weight sits.
% FOUNDING_PROBLEM: After the Second World War's partisan and resistance warfare, states codifying the 1949 conventions needed to regularize who may lawfully fight: Article 4's categories were drawn to cover organized militias and resistance movements belonging to a party to the conflict while preserving the state monopoly on lawful belligerency and denying legitimacy to irregulars outside state command structures.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the ICRC's official commentaries and customary-IHL studies attest that the identification problem the article was built for persists while documenting the costs of categorical exclusion; the 1974-77 diplomatic conference record shows non-aligned and socialist states contesting the exclusion from outside the major-power bloc that benefits from it; Tadic-line jurisprudence attests that courts found the categorical rule unworkable for modern conflict and substituted functional criteria. No attestation comes from non-state armed groups themselves in the 1949 conference - they had no seat - which is itself signal about whose problem the founding text was written to solve.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.67, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.67 records the magnitude of the legal-security transfer: a captured fighter's exposure to domestic prosecution and non-POW detention is a large, concentrated loss falling on every member of the excluded class regardless of discipline or conduct. Suppression 0.65 is structural rather than rhetorical: the class is closed at capture, no conduct-based route into POW status exists for the excluded, and persistence depends on detaining powers actively maintaining the denial through status-review procedures and commission systems built around the no-status category. Theater 0.38 reflects the growing share of legal activity that defends the exclusion rather than performs the identification function - unlawful-combatant determinations, commission jurisprudence, and the memorandum literature elaborating justifications for a category whose core work the text already does. Accessibility_collapse 0.62: for a captured fighter the alternatives collapse almost completely, since nothing changes the category; at the system level, however, rival codifications remain live (Additional Protocol I, Common Article 3 jurisprudence), so collapse is partial rather than total. Resistance 0.58: the exclusion has met organized, sustained resistance - the entire 1974-77 diplomatic conference, decades of ICRC advocacy, judicial pushback - without displacing the text. The three series share one ten-point grid (1949-2026) so every metric is authored at every examined time point. Suppression_requirement is tracked because the enforcement history is genuinely dynamic, not static: a ratchet through 2001-2006 as detention architecture was deliberately constructed around the no-status category, followed by partial retreat after Hamdan and Boumediene. The 1977 step-change across all three series marks Additional Protocol I's adoption, which hardened major-power attachment to the unrevised text rather than softening it.
 *
 * PERSPECTIVAL GAP:
 *   From the high_contracting_states seat the arrangement computes as a workable legal framework those states built and staff - coordination with a fee attached. From the non_state_fighters seat the same structure computes as categorical exposure: every operative element (closed class, discretionary determination, domestic prosecution track) points at them, and their trapped exit amplifies effective extraction toward the full-target pole. Tribunals occupy a third position: their functional criteria treat the categorical rule as unworkable in practice while stopping short of granting status, so the judicial seat experiences the arrangement as a rule honored in the breach. The engine derives these divergent per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   state_armed_forces sits nearest the beneficiary pole: the arrangement subsidizes its members with immunity and guaranteed treatment at no administrative cost to them, and their mobility across conflicts preserves the subsidy. high_contracting_states collects the residual - prosecutorial discretion over captured irregulars and the ordering benefit of a closed fighter class - so its derived directionality is low but not zero, tempered by reciprocity risk to its own captured personnel. non_state_fighters sits near the full-target pole, amplified by trapped exit: no action available to them at capture alters their category. civilians_in_conflict_zones derives near-symmetric with a slight beneficiary tilt - protected by the line's existence, exposed by its erosion under no-status incentives. The observers (icrc, international_war_crimes_tribunals) take the analytical seat and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - regularizing identification of lawful fighters after the partisan warfare of the Second World War - remains half-live: interstate war still occurs, and for it the identification function works. What has atrophied is the exclusion's justification: the categorical denial was calibrated to a world of interstate war plus exceptional insurgency, while contemporary armed conflict is predominantly non-international, where the identification work is performed by functional criteria (Tadic-line organization and command tests, Additional Protocol II) that bypass the status gate entirely. The R5 mismatch read pairs founding_problem_status=contested with disappearance_verdict=world_rearranges: the arrangement is load-bearing enough that overnight removal would reorganize detention practice in every active conflict, yet the parties dispute whether the problem it was built for still needs this solution. That profile blocks both failure modes - it prevents mislabeling the whole regime as pure extraction, because the identification function is genuine and would be rebuilt in some form, while flagging the exclusion component as persisting past its justification rather than serving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the state-affiliation requirement intrinsic to the combatant_status_definition kernel, or is it one contestable reading of a fixed text that other readings reinterpret?',
    'Comparative structural analysis across the three sibling readings: each reading''s victim set, epsilon profile, and foreclosure relations are authored in separate files; convergence or divergence in computed classifications locates whether the disagreement lives in the text or in the readers.',
    'Adopting the national_liberation_reading removes non_state_fighters from the victim set for covered conflicts; adopting the functional_protection_reading dissolves the status gate''s practical bite entirely, converting this constraint''s extraction into a labeling exercise atop a status-independent floor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: this constraint is the state_centric_reading of the combatant_status_definition kernel; sibling readings would restructure the victim set.').

omega_variable(
    customary_status_drift,
    'Does the categorical exclusion of non-state fighters remain customary international law, or has state practice and jurisprudence converged on functional criteria that bypass the Article 4 gate?',
    'Systematic survey of state practice and opinio juris: detention practice in non-international armed conflicts, Tadic-line tribunal criteria, Additional Protocol II acceptance rates, and detaining-power status-review procedures.',
    'If functional criteria are now customary, the state-centric reading operates as minority practice sustained by major-power weight, which raises measured suppression (more active defense required per unit of persistence) and strengthens the case that the exclusion component is inertial rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_drift, empirical, 'Whether the reading''s operative rule still tracks living law or persists against it.').

omega_variable(
    compliance_incentive_feedback,
    'Does categorical denial of status remove non-state armed groups'' incentive to comply with conduct-of-hostilities rules, creating a nothing-to-lose feedback loop that degrades the very distinction the arrangement exists to protect?',
    'Compare distinction-compliance indicators across organized armed groups facing credible status pathways versus groups facing categorical denial, controlling for conflict intensity and group ideology.',
    'A strong feedback loop makes the constraint partially self-undermining: the extraction imposed on non-state fighters erodes the coordination benefit delivered to civilians, supporting functional readings even on state-centric premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_incentive_feedback, empirical, 'Whether the exclusion''s incentive effects corrode the arrangement''s own coordination function.').

omega_variable(
    civilian_protection_tradeoff,
    'Would extending combatant status to organized, disciplined non-state armed groups strengthen or weaken civilian immunity in practice?',
    'Empirical study of distinction-compliance in conflicts where status pathways exist (Additional Protocol I contexts, bilateral special agreements) versus conflicts where denial is categorical.',
    'Resolves the policy core of the sibling dispute: if civilian protection improves with status pathways, the closed-class axiom loses its instrumental grounding and the state-centric reading''s secondary justification fails; if protection degrades, the reading''s rationale holds against its critics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_protection_tradeoff, preference, 'The tradeoff underlying the closed-combatant-class axiom; resolution depends on both evidence and weighting of fighter incentives against civilian risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_state_centric_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement_basis(csd_state_centric_tr_t1949, observed).
narrative_ontology:measurement(csd_state_centric_tr_t1960, combatant_status_definition__state_centric_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(csd_state_centric_tr_t1960, observed).
narrative_ontology:measurement(csd_state_centric_tr_t1972, combatant_status_definition__state_centric_reading, theater_ratio, 1972, 0.25).
narrative_ontology:measurement_basis(csd_state_centric_tr_t1972, observed).
narrative_ontology:measurement(csd_state_centric_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.3).
narrative_ontology:measurement_basis(csd_state_centric_tr_t1977, observed).
narrative_ontology:measurement(csd_state_centric_tr_t1991, combatant_status_definition__state_centric_reading, theater_ratio, 1991, 0.27).
narrative_ontology:measurement_basis(csd_state_centric_tr_t1991, observed).
narrative_ontology:measurement(csd_state_centric_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement_basis(csd_state_centric_tr_t2001, observed).
narrative_ontology:measurement(csd_state_centric_tr_t2006, combatant_status_definition__state_centric_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement_basis(csd_state_centric_tr_t2006, observed).
narrative_ontology:measurement(csd_state_centric_tr_t2014, combatant_status_definition__state_centric_reading, theater_ratio, 2014, 0.44).
narrative_ontology:measurement_basis(csd_state_centric_tr_t2014, observed).
narrative_ontology:measurement(csd_state_centric_tr_t2020, combatant_status_definition__state_centric_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(csd_state_centric_tr_t2020, observed).
narrative_ontology:measurement(csd_state_centric_tr_t2026, combatant_status_definition__state_centric_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(csd_state_centric_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(csd_state_centric_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.55).
narrative_ontology:measurement_basis(csd_state_centric_be_t1949, observed).
narrative_ontology:measurement(csd_state_centric_be_t1960, combatant_status_definition__state_centric_reading, base_extractiveness, 1960, 0.57).
narrative_ontology:measurement_basis(csd_state_centric_be_t1960, observed).
narrative_ontology:measurement(csd_state_centric_be_t1972, combatant_status_definition__state_centric_reading, base_extractiveness, 1972, 0.6).
narrative_ontology:measurement_basis(csd_state_centric_be_t1972, observed).
narrative_ontology:measurement(csd_state_centric_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.63).
narrative_ontology:measurement_basis(csd_state_centric_be_t1977, observed).
narrative_ontology:measurement(csd_state_centric_be_t1991, combatant_status_definition__state_centric_reading, base_extractiveness, 1991, 0.59).
narrative_ontology:measurement_basis(csd_state_centric_be_t1991, observed).
narrative_ontology:measurement(csd_state_centric_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement_basis(csd_state_centric_be_t2001, observed).
narrative_ontology:measurement(csd_state_centric_be_t2006, combatant_status_definition__state_centric_reading, base_extractiveness, 2006, 0.74).
narrative_ontology:measurement_basis(csd_state_centric_be_t2006, observed).
narrative_ontology:measurement(csd_state_centric_be_t2014, combatant_status_definition__state_centric_reading, base_extractiveness, 2014, 0.71).
narrative_ontology:measurement_basis(csd_state_centric_be_t2014, observed).
narrative_ontology:measurement(csd_state_centric_be_t2020, combatant_status_definition__state_centric_reading, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement_basis(csd_state_centric_be_t2020, observed).
narrative_ontology:measurement(csd_state_centric_be_t2026, combatant_status_definition__state_centric_reading, base_extractiveness, 2026, 0.67).
narrative_ontology:measurement_basis(csd_state_centric_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(csd_state_centric_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.52).
narrative_ontology:measurement_basis(csd_state_centric_su_t1949, observed).
narrative_ontology:measurement(csd_state_centric_su_t1960, combatant_status_definition__state_centric_reading, suppression_requirement, 1960, 0.54).
narrative_ontology:measurement_basis(csd_state_centric_su_t1960, observed).
narrative_ontology:measurement(csd_state_centric_su_t1972, combatant_status_definition__state_centric_reading, suppression_requirement, 1972, 0.56).
narrative_ontology:measurement_basis(csd_state_centric_su_t1972, observed).
narrative_ontology:measurement(csd_state_centric_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement_basis(csd_state_centric_su_t1977, observed).
narrative_ontology:measurement(csd_state_centric_su_t1991, combatant_status_definition__state_centric_reading, suppression_requirement, 1991, 0.56).
narrative_ontology:measurement_basis(csd_state_centric_su_t1991, observed).
narrative_ontology:measurement(csd_state_centric_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement_basis(csd_state_centric_su_t2001, observed).
narrative_ontology:measurement(csd_state_centric_su_t2006, combatant_status_definition__state_centric_reading, suppression_requirement, 2006, 0.78).
narrative_ontology:measurement_basis(csd_state_centric_su_t2006, observed).
narrative_ontology:measurement(csd_state_centric_su_t2014, combatant_status_definition__state_centric_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement_basis(csd_state_centric_su_t2014, observed).
narrative_ontology:measurement(csd_state_centric_su_t2020, combatant_status_definition__state_centric_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement_basis(csd_state_centric_su_t2020, observed).
narrative_ontology:measurement(csd_state_centric_su_t2026, combatant_status_definition__state_centric_reading, suppression_requirement, 2026, 0.65).
narrative_ontology:measurement_basis(csd_state_centric_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'combatant status under IHL' covers three structurally distinct claims sharing one kernel text (Geneva III Article 4): who may hold combatant status (this state-centric reading versus the national_liberation_reading) and whether minimum protections depend on status at all (functional_protection_reading). Each is authored separately with its own stable epsilon per the epsilon-invariance principle: this reading's extraction concentrates on non-state fighters; the liberation reading's concerns non-recognition of the AP I extension by major powers; the functional reading's concerns the adequacy and reach of the status-independent floor. The upstream text (Article 4) is cited as authority by all three, so this reading links to both siblings; the functional reading exerts downstream pressure on this one through tribunal practice without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
