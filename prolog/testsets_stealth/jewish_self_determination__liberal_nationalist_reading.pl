% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Liberal-Nationalist Reading of Jewish Self-Determination (Parity Principle)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story authors the liberal-nationalist reading of Jewish
 *   self-determination as a single clean constraint: the parity principle
 *   holding that the Jewish people constitute a nation entitled, equally with
 *   other nations, to self-determination. The principle operates as a shared
 *   standard in international politics - it legitimates the Jewish national
 *   claim, and by its own symmetry commits its holders to recognize the
 *   Palestinian national claim, making mutual recognition and territorial
 *   division the solution concept. KEY AGENTS (by structural relationship): -
 *   jewish_national_institutions: primary beneficiary and administrator
 *   (institutional/constrained) - receives recognition, immigration, and
 *   allocations; shapes how the standard is invoked. -
 *   jewish_diaspora_refugee_communities: intended principal beneficiary
 *   (moderate/mobile) - the standard secures their lawful door out of
 *   persecution. - palestinian_national_movement: reciprocal beneficiary
 *   bearing implementation costs (organized/constrained) - gains counterpart
 *   legitimacy, bears the burden of arrangements that depart from the
 *   standard. - great_power_recognition_system: agenda setter
 *   (institutional/arbitrage) - administers the currency of recognition. -
 *   liberal_zionist_public_intellectuals: interpreting beneficiary
 *   (moderate/identity_locked) - articulates and polices the standard's
 *   boundaries. - binationalist_dissenters: excluded voice
 *   (moderate/constrained) - rejects the nation-state template the standard
 *   presupposes. - nationalism_studies_scholars: analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.33).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Liberal-Nationalist Reading of Jewish Self-Determination (Parity Principle)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, '115e2740-4344-474b-977d-4cb8d90d2057').
narrative_ontology:cs_kernel_codification('115e2740-4344-474b-977d-4cb8d90d2057', formalized).
narrative_ontology:cs_authority_grounding('115e2740-4344-474b-977d-4cb8d90d2057', lineage).
narrative_ontology:cs_interpretation_layer_present('115e2740-4344-474b-977d-4cb8d90d2057').
narrative_ontology:cs_reading_relation('115e2740-4344-474b-977d-4cb8d90d2057', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('115e2740-4344-474b-977d-4cb8d90d2057', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('115e2740-4344-474b-977d-4cb8d90d2057', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('115e2740-4344-474b-977d-4cb8d90d2057', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('115e2740-4344-474b-977d-4cb8d90d2057', foundational, national_parity_entitles_jewish_self_determination).
narrative_ontology:cs_axiom_status(national_parity_entitles_jewish_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('115e2740-4344-474b-977d-4cb8d90d2057', national_parity_entitles_jewish_self_determination, deontological).
narrative_ontology:cs_axiom('115e2740-4344-474b-977d-4cb8d90d2057', foundational, competing_claims_resolved_by_mutual_recognition).
narrative_ontology:cs_axiom_status(competing_claims_resolved_by_mutual_recognition, holdable).
narrative_ontology:cs_axiom_grounding('115e2740-4344-474b-977d-4cb8d90d2057', competing_claims_resolved_by_mutual_recognition, conventional).
narrative_ontology:cs_reference_frame('115e2740-4344-474b-977d-4cb8d90d2057', equal_nations_parity_framework).
narrative_ontology:cs_drift_state('115e2740-4344-474b-977d-4cb8d90d2057', contemporary_occupation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('115e2740-4344-474b-977d-4cb8d90d2057', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_refugee_communities).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, liberal_zionist_public_intellectuals).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, wilsonian_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the national enterprise the parity standard underwrites: the immigration and settlement apparatus before 1948, state institutions after. They receive diplomatic recognition, immigration flows, and territorial allocations justified by the equal-nations standard, and they set much of the agenda for how the standard is invoked internationally. Exiting the standard would mean surrendering the legitimating vocabulary of the enterprise they run; re-grounding in religious or ancestral terms is possible but abandons the liberal-democratic legitimacy they cultivate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions, agenda_setter).

% Persecuted and formerly stateless communities across Europe, the Middle East, and North Africa for whom the equal-nations standard opened a lawful door: citizenship by return in a state that would take them when other states would not. They are the standard's intended beneficiaries, and their welfare rises and falls with its international standing. Their exit is literal mobility - migration - which the standard itself secures.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_refugee_communities, beneficiary,
    moderate, biographical, mobile, global).

% The counterpart national movement whose own statehood claim gains reciprocal legitimacy whenever the equal-nations standard is applied symmetrically; it accepted partition in 1988 and negotiates state-to-state. It also bears the costs of arrangements carried out under the standard's banner that depart from it - occupation, settlement growth, displaced communities - which it registers as breach of the standard rather than its fulfillment. Its exit runs through the same recognition architecture; abandoning the standard forfeits the reciprocal legitimacy it has won.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, payer).

% The concert of states and international organs that decide which national claims receive standing: the League Mandate, the 1947 partition vote, recognition and admission, Security Council resolutions. They administer the standard's currency - recognition - and can inflate or deflate it. Their exit is arbitrage: they apply the standard selectively, weighing alliances against principle.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, great_power_recognition_system, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Writers, jurists, and organizers who articulate the equal-nations case in liberal idiom and police its boundaries - condemning occupation as betrayal of the standard while defending the national claim itself. Their professional and moral identities are fused with the framework: if the parity premise failed publicly, their entire defense collapses, and the available fallbacks (covenant theology, security necessity) are ones they cannot avow. Exit is therefore costly in identity terms even where physically trivial.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_zionist_public_intellectuals, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, liberal_zionist_public_intellectuals, agenda_setter).

% Advocates of a single shared state, cultural autonomists, and non-nationalist Jews who reject the nation-state template the standard presupposes. They argue the relevant unit is the shared polity, not two national sovereignties, and that partition logic entrenches the conflict it manages. They speak in public but hold no seat in the standard's application: the framework has no category for their proposal except as a rejection of it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, binationalist_dissenters, excluded,
    moderate, generational, constrained, regional).

% Academic observers of nationalism and self-determination doctrine who trace how the standard arose, where it has been applied consistently or selectively, and what its operation has cost each party. They collect nothing and pay nothing; their output informs courts, diplomats, and movements.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, nationalism_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adjudicates competing national claims to overlapping territory by supplying a shared standard - equal nations, equal entitlements - under which both claims can be recognized and divided rather than fought to exclusivity; it converts an existential zero-sum into a negotiable boundary question.
% TRANSFER_FUNCTION: Moves recognition and standing (conferring equal presumptive validity on the Jewish national claim and, by symmetry, committing holders to recognize the Palestinian claim); moves people (lawful immigration and return); and, when operationalized, moves territory (partition allocations) - with the costs of implementation falling unevenly on the populations resident where allocation occurs.
% ABSENT_VOICES: Binationalists, diasporists, and non-nationalist religious universalists would object that the framework presupposes the very nation-state template in dispute, yet the standard's application has no seat for them - their proposal appears only as a rejection. Within the two-state logic, Palestinian citizens inside the Green Line surface solely as a minority-rights problem, never as a third national seat. Their absence is what lets unanimity around the parity-plus-partition package look like consensus rather than a framing choice.
% DISAPPEARANCE_RATIONALE: If the parity standard vanished overnight, the Jewish national claim would lose its dominant legitimating vocabulary - defenders would fall back on covenant theology or security necessity, grounds with narrower international purchase. Opponents would lose the symmetric lever they currently pull (if other peoples get states, so do Jews - and so do Palestinians). The two-state concept, mutual recognition instruments, and the recognition architecture built on equal-nations logic would lose their normative anchor, and the diplomatic order around the conflict would reorganize around whichever rival ground each party found available.
% FOUNDING_PROBLEM: Statelessness in the age of nationalism: a dispersed people with no state of its own, barred from immigration by most states (Evian 1938), murdered in the millions when the states they lived in turned on them, with no territorial refuge whose doors were open to them by right. The arrangement was built to secure what other peoples took for granted - a state that could not close its doors to them.
% FOUNDING_PROBLEM_CORROBORATION: The historical record attests the founding problem from outside the beneficiary set: the Evian Conference failure, the MS St. Louis, UNSCOP's majority report, and postwar refugee documentation were produced by non-Jewish and international actors. Present status is attested as contested: independent monitors of antisemitic violence document continuing persecution dynamics supporting a live reading, while Palestinian, postcolonial, and diasporist interlocutors attest that statelessness-without-refuge no longer describes most Jewish lives and that the standard now functions chiefly to shield implemented facts. No single outside seat attests a uniform status.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (interval end) on a rising series from 0.08: the parity principle began as nearly costless advocacy, and its operation accumulated asymmetric implementation - above all from 1967 onward - which even this reading's own lights register as divergence from the standard it invokes. The value sits in the low-to-moderate band the reading predicts, at the upper edge, because partition feasibility and mutual recognition have eroded. Suppression is authored at 0.33 as a raw structural property (unscaled by power or scope; only extractiveness is scaled downstream): the standard's active enforcement force, having been built up through the Mandate and the UN partition vote and peaked around 1967, has decayed as resolutions pass without application. Theater_ratio at 0.38 reflects two-state rhetoric maintained well past the point where policy substance matches it - process performance outpacing delivered settlements. Accessibility_collapse is low (0.30): rival framings of the same kernel remain live and articulated rather than collapsing under the parity standard. Resistance is substantial (0.58): postcolonial critique, diasporist argument, and religious-universalist objection meet the standard continuously. The claimed type is rope on independent structural grounds: a genuine collective-action problem (competing exclusive claims to one territory), a solution concept with modest coercive overhead relative to the alternatives, net benefit to both national movements in principle, and alternatives left standing. The metrics are authored separately and describe operational drift; the claim/metrics gap is data, not error. All three tracked series share one eight-point grid (T=0/1897 First Zionist Congress, T=20/1917 Balfour, T=25/1922 League Mandate, T=50/1947 partition vote, T=70/1967, T=96/1993 Oslo, T=110/2007, T=128/2025), so every metric is authored at every examined point. The suppression_requirement series is authored deliberately because enforcement-capacity change is the traced dynamic here (build-up then decay), not a static picture. The Oslo dip in extractiveness and theater is a real partial reversal driven by mutual recognition, not noise; the subsequent climb resumes the accumulation pattern.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the jewish_national_institutions seat, the standard is hard-won equal standing in a system that denied it - a coordination achievement. From the palestinian_national_movement seat, the same standard is honored in recognition rhetoric and breached in implementation, so its lived operation weighs heavier than its text. From the liberal_zionist_public_intellectuals seat, it is a moral framework under visible strain, defended by condemning deviations from it. From the great_power seat, it is a discretionary instrument applied selectively. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared and victims are deliberately not: within this reading's own lights, no group is a structural victim of the parity principle - costs that fall on Palestinians under occupation are classified by the reading itself as breaches of the standard, not products of it, and the corresponding victim declaration belongs to the sibling settler-colonial instantiation, not this one. Directionality follows the declarations: jewish_national_institutions sit near the beneficiary end (damped effective extraction, constrained exit keeping them short of full arbitrage); jewish_diaspora_refugee_communities sit nearest the beneficiary end (full subsidy via mobility the standard secures); palestinian_national_movement derive a mid-range directionality from their dual declaration - reciprocal benefit pulling down, implementation-cost burden pushing up; great_power_recognition_system sit mildly beneficiary-side with arbitrage-grade exit; liberal_zionist_public_intellectuals sit beneficiary-side but their identity_locked exit means the frame's failure would cost them heavily, which the derivation registers. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and adding overrides would duplicate structural data the derivation chain reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - statelessness with no lawful refuge - has been substantially addressed for most Jewish lives, which creates genuine mandate-outlived-function risk: the standard could persist as ritual legitimation after its original work is done. The classification apparatus prevents two opposite mislabels. Reading the arrangement as pure extraction (the sibling settler-colonial move) would erase the real coordination achievement - a shared standard that made mutual recognition and division negotiable at all. Reading it as untouched coordination would miss the measured drift: rising extractiveness and theater alongside decaying enforcement indicate the standard increasingly shields implemented facts it cannot justify. The R5 interview records the founding problem's status as contested rather than dead, so no zombie flag fires, but the contested status routes the story to cross-seat scrutiny rather than letting either the beneficiaries' or the critics' genealogy stand unexamined. The temporal series is the drift detector: if partition feasibility collapses entirely (see omega partition_feasibility_drift), the compensation half of the parity bargain fails and reclassification pressure follows from the data rather than from rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_ambiguity,
    'Does this story''s parity-principle constraint faithfully instantiate one reading of the jewish_self_determination kernel, or does the kernel''s contestability mean the parity premise itself smuggles in a characterization that sibling readings deny at the root?',
    'Compile the four sibling stories and compare epsilon values, beneficiary/victim structures, and computed types; sharp divergence confirms the decomposition, convergence suggests the kernel is one constraint with rhetorical variants.',
    'If the parity premise is rejected - Jewish peoplehood judged not nation-shaped, or nationhood judged not to warrant statehood - this constraint dissolves into the diasporist or settler-colonial instantiation, with different beneficiaries and an explicit victim set this story intentionally does not declare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_ambiguity, conceptual, 'Whether the parity reading cleanly instantiates one reading of a genuinely multi-reading kernel.').

omega_variable(
    nationhood_premise_contest,
    'Is Jewish peoplehood a nation in the sense that warrants territorial self-determination under the parity standard, or a religion and diasporic civilization whose collective claims are satisfiable without sovereignty?',
    'Comparative doctrinal analysis: how international law and practice have classified Jewish peoplehood (Mandate-era nationality provisions, UN debates, recognition instruments) against how comparable contested cases (Kurds, Roma) have been treated.',
    'If peoplehood is judged non-national, the parity entitlement lapses and the beneficiary structure of this constraint collapses; if national, the standing arrangement''s legitimacy rests on a premise its critics dispute rather than its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationhood_premise_contest, conceptual, 'The load-bearing nationhood premise beneath the parity claim.').

omega_variable(
    partition_feasibility_drift,
    'Is a partition-based mutual-recognition settlement still materially feasible, given settlement geography, demographic interpenetration, and political trajectories on both sides?',
    'Track negotiation parameters over time: border proposals, settlement population growth rates, recognition diplomacy, and the widening or closing gap between the two-state concept and implementable borders.',
    'If partition becomes infeasible, the parity standard''s operation can no longer compensate the counterpart claim; effective extraction then concentrates without offset, producing reclassification pressure away from the coordination reading and toward hybrid or extractive classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_drift, empirical, 'Whether the reading''s low-extraction condition (feasible partition, mutual recognition) still holds.').

omega_variable(
    reciprocity_symmetry_gap,
    'Do the standard''s holders actually extend the reciprocal recognition the parity premise demands, or is the standard invoked asymmetrically - defensively for one national claim and withheld in practice from the other?',
    'Systematic coding of elite and institutional discourse: the frequency and concreteness of endorsements of counterpart statehood by the standard''s principal beneficiaries, weighted against their conduct.',
    'Asymmetric invocation converts the standard from a shared adjudication device into a one-sided shield, raising effective extraction on the counterpart population, shifting seat classifications, and strengthening the identity_coordination gaming concern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_symmetry_gap, empirical, 'Whether the parity standard operates symmetrically as its text requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 128).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t50, observed).
narrative_ontology:measurement(jewi_tr_t70, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t70, observed).
narrative_ontology:measurement(jewi_tr_t96, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 96, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t96, observed).
narrative_ontology:measurement(jewi_tr_t110, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 110, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t110, observed).
narrative_ontology:measurement(jewi_tr_t128, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 128, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t128, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement_basis(jewi_be_t25, observed).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 50, 0.34).
narrative_ontology:measurement_basis(jewi_be_t50, observed).
narrative_ontology:measurement(jewi_be_t70, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 70, 0.44).
narrative_ontology:measurement_basis(jewi_be_t70, observed).
narrative_ontology:measurement(jewi_be_t96, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 96, 0.36).
narrative_ontology:measurement_basis(jewi_be_t96, observed).
narrative_ontology:measurement(jewi_be_t110, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 110, 0.46).
narrative_ontology:measurement_basis(jewi_be_t110, observed).
narrative_ontology:measurement(jewi_be_t128, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 128, 0.48).
narrative_ontology:measurement_basis(jewi_be_t128, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement_basis(jewi_su_t25, observed).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(jewi_su_t50, observed).
narrative_ontology:measurement(jewi_su_t70, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(jewi_su_t70, observed).
narrative_ontology:measurement(jewi_su_t96, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 96, 0.52).
narrative_ontology:measurement_basis(jewi_su_t96, observed).
narrative_ontology:measurement(jewi_su_t110, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 110, 0.4).
narrative_ontology:measurement_basis(jewi_su_t110, observed).
narrative_ontology:measurement(jewi_su_t128, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 128, 0.33).
narrative_ontology:measurement_basis(jewi_su_t128, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Jewish self-determination' decomposes into five structurally distinct claims that differ in warrant (parity, indigeneity, divine covenant), in characterization (national liberation versus settler colonialism), and in desired arrangement (territorial sovereignty versus diaspora pluralism). Each member gets its own epsilon, its own beneficiary/victim structure, and its own classification; this story is the parity reading, the most diplomatically entrenched member, whose success in securing recognition structurally reshapes the operating environment of the other four. Members are linked bidirectionally through affects_constraints; orphaning any member would hide the contamination paths along which one reading's credibility gains or losses propagate to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
