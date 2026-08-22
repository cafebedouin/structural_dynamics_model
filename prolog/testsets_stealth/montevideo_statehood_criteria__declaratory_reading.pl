% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria - Declaratory Reading (Objective Criteria Establish Statehood as Legal Fact)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The declaratory reading of the Montevideo kernel holds that statehood
 *   arises by operation of law once a polity satisfies four objective tests -
 *   permanent population, defined territory, effective government, and
 *   capacity to conduct foreign relations - with other governments'
 *   acknowledgment playing no constitutive role. This file authors THAT
 *   reading only, as a clean epsilon-invariant constraint; the constitutive
 *   and hybrid readings are separate stories with different victim sets and
 *   different epsilon values, linked via network.affects_constraints. The
 *   referent of epsilon is the declaratory doctrine as an operative
 *   arrangement - the rule in action, including its chronic enforcement gap -
 *   not the self-executing ideal its proponents endorse. Structurally the
 *   doctrine performs real coordinating work (an objective, non-discretionary
 *   membership test replaces bilateral leverage bargaining) while imposing
 *   asymmetric costs: parent states lose their territorial-integrity veto
 *   without consent, and a persistent class of criteria-meeting polities
 *   holds a declared status the practical order declines to deliver. The
 *   claim/metric relationship is deliberately unreconciled: claimed_type
 *   records the structure I believe true (tangled_rope - genuine coordination
 *   plus identifiable payers plus active maintenance against practice drift);
 *   the metrics record descriptive operation; the engine computes per-seat
 *   classifications independently.
 *
 * KEY AGENTS:
 *   - perpetually_unrecognized_entities: Primary target (powerless/identity_locked) - declared states in law, denied the substance; bear full running costs of statehood without its protections
 *   - parent_territorial_states: Primary target (powerful/constrained) - lose the territorial-integrity veto and the recognition-leverage tool without consent or compensation
 *   - successful_secession_entities: Primary beneficiary (moderate/mobile) - converted criteria-compliance into working statehood
 *   - established_minor_powers: Secondary beneficiary (organized/constrained) - shielded from discretionary exclusion by the objectivity of the test
 *   - established_great_powers: Practical administrator (institutional/arbitrage) - decide case by case whether declared status converts; retain the gate the rule formally opened
 *   - international_judicial_bodies: Doctrinal administrator (institutional/constrained) - apply and articulate the criteria; determination without delivery
 *   - legal_scholarship_community: Analytical maintainer (analytical/analytical) - carries the doctrine's articulation, documents the declaration-to-delivery gap
 *   - stateless_nations_without_control: Excluded (powerless/trapped) - fail the tests' rigidity; no seat in any revision forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.58).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.41).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria - Declaratory Reading (Objective Criteria Establish Statehood as Legal Fact)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '881ee9aa-3ec3-4cdc-ac4d-b05deb528b14').
narrative_ontology:cs_kernel_codification('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', formalized).
narrative_ontology:cs_authority_grounding('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', distributed).
narrative_ontology:cs_reading_relation('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', foundational, objective_criteria_constitute_statehood).
narrative_ontology:cs_axiom_status(objective_criteria_constitute_statehood, holdable).
narrative_ontology:cs_axiom_grounding('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', objective_criteria_constitute_statehood, conventional).
narrative_ontology:cs_axiom('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', secondary, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', recognition_is_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', objective_criteria_self_executing_statehood).
narrative_ontology:cs_drift_state('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', contemporary_recognition_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('881ee9aa-3ec3-4cdc-ac4d-b05deb528b14', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, successful_secession_entities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, established_minor_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_territorial_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, perpetually_unrecognized_entities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, state_equality_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, effectiveness_principle).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, montevideo_objective_criteria_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Polities that emerged through decolonization or state dissolution, met the four tests - permanent population, defined territory, functioning government, capacity to conduct foreign relations - and were ultimately dealt with as states. The objective test gave them a claim that did not depend on each powerful government's goodwill, and enough counterparts eventually engaged with them that the status became self-sustaining. They now operate as ordinary members: treaties, embassies, credit, votes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, successful_secession_entities, beneficiary,
    moderate, generational, mobile, national).

% Mid-sized states whose continued legal existence no longer hinges on any great power's favor. Because membership in the society of states turns on observable facts rather than a club's vote, they cannot credibly be threatened with deletion from the legal map; their exposure is limited to ordinary diplomatic friction. They carry standard membership burdens and receive the predictability of a fixed rule.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, established_minor_powers, beneficiary,
    organized, civilizational, constrained, global).

% States from which territories have broken away. Once the breakaway authority fields a population, a territory, a working government, and foreign-relations capacity, the rule extinguishes the parent's say: no consent is requested, no compensation is scheduled, and the old tool of withholding or conditioning acknowledgment disappears. What remains is protest, litigation over succession, and - at the extreme - the use of force, which the system then punishes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_territorial_states, payer,
    powerful, generational, constrained, national).

% Authorities that hold a population, a territory, and a functioning government, and conduct some foreign relations, yet after decades remain outside the club: no seat in the general assembly, thin access to finance, weak security guarantees, travel documents half-accepted. The rule tells them they already possess the legal status; the surrounding practice declines to deliver what the status is for. They keep paying the full running costs of statehood - defense, administration, diplomacy - while collecting few of its protections, and walking away would mean dissolving the project their institutions exist to pursue.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, perpetually_unrecognized_entities, payer,
    powerless, biographical, identity_locked, regional).

% Permanent Security Council members and bloc leaders. Their acknowledgment decisions decide, case by case, whether a polity's declared status turns into a working one - membership votes, financial access, and security ties all run through them. They cite the objective criteria when a new polity suits them and withhold acknowledgment when it does not, keeping the practical gate that the written rule formally opened. They could settle the rule's meaning by consistent practice or new agreement, and have declined to.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, established_great_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Courts and commissions that apply the four tests in succession, responsibility, and border disputes, and whose opinions keep the tests analytically alive. They cannot compel any government to deal with a polity they find qualifies; their product is reasoned determination, not delivery.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_judicial_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Academic lawyers who maintain the doctrine's articulation, document the gap between what the tests declare and how governments behave, and supply the criteria's content when novel cases arise. They carry the rule's intellectual weight but command no enforcement.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, legal_scholarship_community, observer,
    analytical, generational, analytical, global).

% Peoples with nationhood claims but no effective territorial government - dispersed, occupied, or too small to field the required apparatus. The tests' rigidity places them permanently outside eligibility, and they have no seat in any forum where the tests might be revised. Their objection - that the rule entrenches whoever already holds effective control and prices eligibility beyond their reach - is recorded nowhere official.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, stateless_nations_without_control, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, successful_secession_entities).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single, non-discretionary test for membership in the society of states, so that any government, court, or counterparty can determine who counts as a state without waiting for a political decision; replaces case-by-case bilateral acknowledgment bargaining - with its holdouts, conditions, and leverage games - with a rule that runs itself once the facts are found.
% TRANSFER_FUNCTION: Moves sovereign legal status - and, where the surrounding practice honors it, membership, treaty capacity, immunities, and finance - to polities satisfying the four tests; correspondingly moves away from parent states and established powers the discretionary power to grant, withhold, condition, or trade the entry of new polities.
% ABSENT_VOICES: Parent states facing secession were heard in the drafting-era conferences only through sponsors; the peoples now stuck in permanent limbo have no seat anywhere the criteria are articulated or revised; stateless nations unable to meet the tests are absent entirely. They would object that the rule entrenches effective control and prices eligibility beyond their reach; their objections survive only in academic literature and occasional protest statements.
% DISAPPEARANCE_RATIONALE: If the declaratory rule vanished overnight, statehood would revert to wholly political acknowledgment practice: every limbo case would be reopened bilaterally, courts would lose the baseline they use for succession, responsibility, and borders, and parent states would regain a consent right they currently lack - the legal order of the map would rearrange around whoever holds acknowledgment power.
% FOUNDING_PROBLEM: Interwar recognition had become a political weapon: governments withheld or granted acknowledgment to punish rivals and extract concessions, leaving new polities in limbo and counterparties unable to know with whom they could lawfully deal. The arrangement was built to replace that discretion with an objective test, so that a polity's existence would not depend on any great power's mood.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic-history scholarship on interwar recognition politics corroborates the founding problem from outside the beneficiary set; ICJ and arbitral practice attests the tests remain the operative baseline; and the limbo polities themselves, together with parent-state protests, attest the problem is not solved. No beneficiary-seat source is relied on.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 concentrates in two vectors: parent states' uncompensated loss of a consent right, and the limbo class's costs-without-protections - the latter amplified by the doctrine's own logic, which hands recognizers a ready excuse ('already a state; no acknowledgment owed') for withholding the engagement the entity needs. Suppression 0.41: the doctrine exerts real doctrinal pressure (courts treat the tests as the baseline against constitutive arguments) but does not close alternatives - constitutive and hybrid practice remain fully live, hence accessibility_collapse 0.25. Theater_ratio 0.52: a large share of current doctrinal activity is reaffirmation that changes nothing for the limbo class; the declaration-to-delivery conversion rate has fallen over the interval. Resistance 0.62: sustained parent-state protest, great-power constitutive practice, and scholarly contest. The measurement series run on ONE shared grid (seven points, T=0..90) so every metric is authored at every examined time point. The extractiveness series is non-monotonic: the dip at T~30 reflects the decolonization wave, when the doctrine's promise was delivered broadly and the limbo class temporarily shrank - an external driver, not intermittent reinforcement; extraction then climbs as limbo cases accumulate and legitimacy conditions attach at the margins. Theater climbs monotonically as conversion rates fall. Suppression_requirement falls to T~30 (the doctrine rode a favorable practice wave) then partially recovers as judicial and scholarly maintenance works against constitutive drift. Coordination type is identity_coordination: statehood is membership-boundary maintenance for the society of states, and the criteria are the evolving boundary rules. The FNL gaming alert applies - 'this is just objective law' is exactly the identity-framing that can mask extraction; the criterion_four_circularity omega carries that flag.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the judicial and scholarly seats the arrangement is a settled analytic baseline - low-extraction coordination they did not build but maintain. From the parent-state seat the same rule is uncompensated expropriation of a consent right, borne by actors with constrained exit. From the limbo-entity seat it is a promise that substitutes for delivery, borne by actors whose identity is fused with the statehood project and who therefore cannot walk away. From the great-power seat it is a selectively invocable instrument: formal leverage surrendered on paper, practical gatekeeping retained in fact. Same rule, four experiences; the engine derives this divergence from the authored power, exit, and role data - the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: successful_secession_entities are full recipients of the rule's product; established_minor_powers are shielded incumbents collecting predictability at standard cost. Victims derive high directionality: parent_territorial_states bear the transfer with constrained exit (sovereignty norms bind them; no opting out), and perpetually_unrecognized_entities sit nearest the full-target end - identity_locked exit amplifies their exposure because the doctrine's declaration is the very thing their institutions exist to pursue. established_great_powers sit near symmetric: target-side in losing formal conditioning leverage, beneficiary-side in retaining the practical gate and harvesting legitimacy from selective invocation - net d approximately 0.5. I note this rather than authoring a directionality_override because the override surface is keyed by power atom, and the international_judicial_bodies seat shares the institutional atom while sitting genuinely beneficiary-side; a single override would misapply to both.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Calling the doctrine a rope (pure coordination) would erase the victim structure - parents and the limbo class pay through the same structure that coordinates everyone else, and the rule requires active judicial and scholarly maintenance to hold against constitutive drift. Calling it a snare would erase the genuine and primary coordination function: the objective test really did replace leverage-game bargaining, and no single seat captures the arrangement's gains outright. On mandatrophy: the founding problem (politicized, weaponized acknowledgment) is contested-live, not dead - the doctrine has not outlived its function, so no resolved-mandatrophy flag is authored and no sunset applies. The piton signature is additionally blocked by the cost-asymmetry test: the seats that could change the rule (the great powers, through consistent practice or new agreement) actively profit from its current ambiguity, so the administrator's cost of fixing is prohibitive relative to what it bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_declaration,
    'This constraint is one reading of the montevideo_statehood_criteria kernel - the declaratory_reading. Are its classification inputs being read separately from the sibling readings rather than averaged across the kernel?',
    'Corpus hygiene: confirm constitutive_reading and hybrid_reading exist as independent stories with their own epsilon, beneficiary/victim structures, and claimed types; verify no cross-reading metric blending occurs at compile time.',
    'If readings were merged, epsilon would be unstable by construction (each reading fixes a different victim set) and every classification computed from this file would be contaminated by the siblings'' structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_declaration, conceptual, 'Committer-frame declaration: this file is the declaratory reading only; siblings are separate constraints.').

omega_variable(
    sibling_adoption_structural_delta,
    'What structurally changes if a sibling reading is adopted instead of this one?',
    'Comparative authoring: instantiate the constitutive and hybrid readings as separate stories and diff the derived victim sets, directionality profiles, and per-seat classifications against this file.',
    'Constitutive adoption removes perpetually_unrecognized_entities from the victim set (they become non-states, not denied states) and restores parent-state conditioning leverage; hybrid adoption adds a new victim class of criteria-meeting polities failing legitimacy conditions. Classification of this kernel is therefore reading-relative, not topic-relative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_adoption_structural_delta, conceptual, 'Expected structural delta across sibling readings of the statehood kernel.').

omega_variable(
    disagreement_location_sufficiency,
    'Where exactly do the readings disagree?',
    'Doctrinal analysis isolating the contested element: whether the four criteria are sufficient for statehood (this reading) or merely necessary (siblings), and whether acknowledgment is evidentiary or constitutive.',
    'Locating the dispute at criteria-sufficiency explains why the readings cannot be blended incrementally: any framework either accepts sufficiency or it does not; partial blends collapse into the hybrid reading, which is a distinct constraint, not a midpoint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_sufficiency, conceptual, 'The kernel contest is located at the sufficiency of the objective criteria and the ontological status of acknowledgment.').

omega_variable(
    criterion_four_circularity,
    'Is the fourth criterion - capacity to enter into relations with other states - separable from other states'' willingness to relate, or does it smuggle constitutivism back into the declaratory test?',
    'Comparative analysis of polities satisfying criteria one through three with varying degrees of foreign-relations capacity: if demonstrated capacity systematically tracks prior acknowledgment, the criterion is recognition-dependent and the declaratory test is internally compromised.',
    'If circular, the reading''s foundational axiom is weaker than claimed, effective extraction rises (the doctrine demands a showing it makes dependent on the very actors it disempowers), and the declaratory reading drifts toward the hybrid position without admitting it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_four_circularity, conceptual, 'Whether the declaratory test''s fourth criterion covertly reintroduces the recognition dependence it denies.').

omega_variable(
    enforcement_gap_harm_attribution,
    'Are the limbo class''s harms caused BY the declaratory doctrine (its declaration substitutes for delivery and supplies the ''no acknowledgment owed'' excuse) or by recognizers'' non-compliance with a doctrine that, if honored, would leave them whole?',
    'Counterfactual comparison: model limbo-class welfare under an explicit constitutive rule versus the declaratory rule with recognition behavior held constant; isolate cases where declaratory language is invoked as the stated reason for withholding engagement.',
    'If the doctrine itself is the causal vector, epsilon is understated and the arrangement trends snare-ward; if non-compliance is the sole cause, the doctrine is closer to a rope poorly honored, and reform should target practice rather than the rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_harm_attribution, conceptual, 'Attribution of the enforcement-gap harms: doctrine-as-cause versus practice-as-cause.').

omega_variable(
    effective_government_threshold_drift,
    'Has the ''effective government'' criterion silently tightened over the interval - for example through democratic-conditionality creep - converting the objective test into a moving target?',
    'Longitudinal coding of judicial and commission applications of the criterion across the interval, testing whether the evidentiary threshold for ''effective'' ratchets upward in later periods.',
    'If the threshold drifts, the doctrine''s objectivity claim erodes, criteria-meeting entities face retroactively raised bars, and the measured accessibility of statehood via the tests falls below what the static metrics assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_government_threshold_drift, empirical, 'Whether the effectiveness criterion''s application standard has drifted upward over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 75, 0.46).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 90, 0.52).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 45, 0.46).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 75, 0.56).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 90, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 45, 0.33).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 60, 0.37).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 90, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Montevideo criteria' conflates three structurally distinct claims about what establishes statehood. This file authors the declaratory reading (criteria sufficient; acknowledgment evidentiary; epsilon 0.58 with the limbo class and parent states as victims). The constitutive reading (acknowledgment constitutive) is a separate story with a different victim set - its limbo population consists of non-states rather than denied states - and restored parent-state leverage. The hybrid reading (criteria plus legitimacy conditions) adds a further victim class of criteria-meeting polities failing democratic or non-aggression conditions. The upstream story in citation practice is the declaratory reading: tribunals and scholarship cite the objective tests as the baseline, which the hybrid reading then amends and the constitutive practice quietly overrides. All three files link one another via network.affects_constraints; contamination propagates along these edges when any reading's purity degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
