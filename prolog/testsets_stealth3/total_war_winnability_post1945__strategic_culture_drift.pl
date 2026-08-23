% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Strategic-Culture Drift: Total War's Discursive Exclusion from Elite Planning
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   After 1945 the major powers confronted a war form that had just consumed
 *   tens of millions of lives and now carried nuclear stakes. Elite strategic
 *   culture responded by progressively removing total war from the category
 *   of things serious professionals plan, teach, fund, and publish on: first
 *   through deliberate doctrinal choice, then through generational turnover
 *   that converted a chosen bracket into an unexamined absence. The material
 *   capacity for total war persists, since industrial plant, delivery
 *   systems, mobilization law, and historical knowledge all remain. What has
 *   atrophied is the discursive and institutional apparatus that would turn
 *   capacity into considered option: war-college curricula teach limited-war
 *   forms almost exclusively, strategy documents advertise comprehensiveness
 *   while planning runs on a narrow band of the possibility space, and
 *   mobilization-depth questions go unasked until crises expose them. The
 *   arrangement now persists largely by inertia: no body enforces the
 *   exclusion, yet no body bears enough of its cost to rebuild what was
 *   dismantled. Annual wargames and full-spectrum language maintain the
 *   appearance of complete strategic coverage while the deep planning layer
 *   stays dark. KEY AGENTS (by structural relationship): -
 *   limited_war_defense_intellectuals: primary beneficiary
 *   (organized/identity_locked) — inherits and reproduces the exclusion
 *   through canon, hiring, and curricula - arms_control_policy_community:
 *   secondary beneficiary (organized/constrained) — occupies the discursive
 *   ground a planning revival would contest - service_war_college_system:
 *   agenda setter (institutional/constrained) — administers the professional
 *   education where the exclusion reproduces; could reverse it at prohibitive
 *   cost - national_security_decision_makers: primary payer
 *   (powerful/constrained) — chooses from a pre-narrowed option menu, with a
 *   secondary incidental benefit from political convenience -
 *   joint_military_planners: payer (moderate/constrained) — executes planning
 *   cycles with no developed total-war branch - alliance_partner_states:
 *   payer (organized/constrained) — exposed to the patron's atrophied
 *   escalation management - revisionist_strategists: excluded voice
 *   (moderate/trapped) — mobilization advocates heard as eccentrics -
 *   historians_of_strategic_thought: analytical observer — sees the drift
 *   whole across archives
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.62).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Strategic-Culture Drift: Total War's Discursive Exclusion from Elite Planning").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '0c8eb95d-06b7-4878-8f90-9647535047e5').
narrative_ontology:cs_kernel_codification('0c8eb95d-06b7-4878-8f90-9647535047e5', distributed).
narrative_ontology:cs_authority_grounding('0c8eb95d-06b7-4878-8f90-9647535047e5', distributed).
narrative_ontology:cs_reading_relation('0c8eb95d-06b7-4878-8f90-9647535047e5', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('0c8eb95d-06b7-4878-8f90-9647535047e5', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('0c8eb95d-06b7-4878-8f90-9647535047e5', foundational, total_war_remains_materially_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_materially_reachable, holdable).
narrative_ontology:cs_axiom_grounding('0c8eb95d-06b7-4878-8f90-9647535047e5', total_war_remains_materially_reachable, empirically_contingent).
narrative_ontology:cs_axiom('0c8eb95d-06b7-4878-8f90-9647535047e5', foundational, discursive_exclusion_is_ideationally_driven).
narrative_ontology:cs_axiom_status(discursive_exclusion_is_ideationally_driven, holdable).
narrative_ontology:cs_axiom_grounding('0c8eb95d-06b7-4878-8f90-9647535047e5', discursive_exclusion_is_ideationally_driven, empirically_contingent).
narrative_ontology:cs_reference_frame('0c8eb95d-06b7-4878-8f90-9647535047e5', postwar_limited_war_consensus).
narrative_ontology:cs_drift_state('0c8eb95d-06b7-4878-8f90-9647535047e5', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c8eb95d-06b7-4878-8f90-9647535047e5', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, arms_control_policy_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, national_security_decision_makers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, joint_military_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, alliance_partner_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, national_security_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University faculty, think-tank fellows, and war-college professors whose careers, theories, and canons are built on limited-war forms: deterrence theory, arms control, counterinsurgency, precision warfare. They did not create the postwar bracketing of total war; they inherited it, and they reproduce it through hiring, peer review, curriculum design, and what counts as a serious research program. Their expertise stays relevant precisely because the competing framework never re-enters the competition; a serious return of total-war planning would bid for the same attention, posts, and funding their portfolio currently absorbs. Leaving the field would mean abandoning accumulated intellectual capital; staying inside it means never testing that capital against the excluded alternative.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    organized, biographical, identity_locked, global).

% Negotiators, verification specialists, and advocacy analysts whose vocation is managing nuclear danger through limitation agreements rather than war-fighting preparation. With total-war planning discourse absent, arms control remains the principal surviving instrument for addressing the danger they exist to address, and their conferences, treaties, and careers occupy ground a planning revival would contest. Exit would mean retraining into fields their skills only partly transfer to.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, arms_control_policy_community, beneficiary,
    organized, generational, constrained, global).

% The professional military education institutions and joint staff directorates that set curricula, run wargames, and publish doctrine. They administer the arrangement day to day: what is taught, what is gamed, what counts as a deliverable planning product. Reintroducing total-war branches would require faculty they do not have, industrial data they do not collect, and political cover no current leader supplies; the cost of rebuilding sits far above any cost they personally bear from the gap, so syllabi change slowly and wargames stay inside the familiar band.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, service_war_college_system, agenda_setter,
    institutional, generational, constrained, national).

% Presidents, ministers, secretaries, and national security council staff who choose among the options the planning system presents. At decision time they discover the missing rungs: mobilization lead times nobody has priced, industrial surges nobody has scheduled, escalation branches nobody has gamed. They also enjoy the arrangement's convenience, since no adviser forces them to defend unpopular total-war preparations before legislatures or publics. Commissioning a red-team study is easy; acting on one is not.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, national_security_decision_makers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, national_security_decision_makers, beneficiary).

% Mid-career officers on joint and service staffs who execute the planning cycles. The frameworks they inherit contain no developed total-war branch, so their products silently assume the conflict stays in familiar registers. Private curiosity about high-end scenarios meets promotion boards and clearance cultures that reward orthodoxy; the safest career move is to plan the wars the institution already knows how to plan.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, joint_military_planners, payer,
    moderate, biographical, constrained, national).

% Allies who rely on the patron's escalation management and extended deterrence. The patron's atrophied deep planning raises their exposure, because guarantees are only as credible as the planning behind them. They hedge with national programs and diversified procurement but cannot reconstruct the missing planning layer alone, and pressing the patron openly risks fracturing the alliance narrative.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, alliance_partner_states, payer,
    organized, generational, constrained, continental).

% Analysts, retired logisticians, and a few serving officers who argue for renewed high-end and total-war planning. Their memoranda circulate, their op-eds appear after each shock, and their proposals are filed as eccentricity once the news cycle turns. Occasional policy windows, such as a munitions shortfall or a mobilization scare, open briefly and then close with the crisis. They cannot leave the profession without losing the standing their argument requires.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, revisionist_strategists, excluded,
    moderate, biographical, trapped, national).

% Scholars who read the archives across generations and can see the drift whole: the deliberate postwar bracketing, its conversion into habit, and the forgetting that followed. They hold no planning authority and seek none; their contribution is the longitudinal record that distinguishes chosen restraint from unexamined absence.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, historians_of_strategic_thought, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After 1945, the exclusion gave great-power elites a shared assurance that planning, teaching, and public discussion would stay below the total-war threshold: it stabilized deterrence signaling, freed resources for limited-war contingencies, and gave domestic publics a bounded picture of what war would mean. Stated without evaluation, that was the problem the arrangement solved.
% TRANSFER_FUNCTION: Moves attention, funding lines, curricular space, and doctrinal authority away from total-war preparation and toward the limited-war frameworks (deterrence, arms control, counterinsurgency, precision strike) already held by the professional community: from the state's full option space to the incumbent epistemic portfolio.
% ABSENT_VOICES: Revisionist strategists, mobilization advocates, and industrial-base analysts hold the objection but sit at the profession's margins, heard as eccentrics; foreign strategic communities whose doctrines never adopted the exclusion are outside the conversation entirely. Both would argue the arrangement trades real preparedness for professional comfort.
% DISAPPEARANCE_RATIONALE: If the discursive exclusion vanished overnight, war-college curricula would grow total-war branches within a planning cycle or two, industrial-base commissions would acquire standing budgets, adversary intelligence services would register the doctrinal shift and reciprocate, and the limited-war frameworks' monopoly on attention and funding would break. The rearrangement is slow, because cultural arrangements dissolve slowly, but it is real.
% FOUNDING_PROBLEM: After 1945: how to keep great-power conflict below the total-war threshold that had just killed tens of millions and now carried nuclear stakes, by making limited war the only kind of war serious professionals planned, taught, and discussed.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by congressional industrial-base commissions and munitions-shortfall hearings, by serving logisticians' published assessments of mobilization lead times, and by comparative foreign-doctrine scholarship showing the exclusion is culturally contingent rather than necessary. The limited-war epistemic community itself, asked directly, tends to deny that the drift is a loss at all, which is why external corroboration is load-bearing here.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the cost is real but diffuse and contingent: foregone strategic flexibility, unpriced mobilization lead times, and atrophied industrial depth, rather than a direct rent transfer to a visible collector. Suppression is 0.42 as a raw structural property, and it is deliberately NOT scaled by power or scope in this authoring; note the divergence from the suppression_requirement series, which decays to 0.08 by 2025 — the enforcement machinery has dissolved, and nearly all remaining suppression is internalized professional norm (thinking about total war reads as morbid or unserious) plus thin structural gating in funding and promotion. Theater_ratio is 0.62: full-spectrum language, annual wargames, and residual civil-defense gestures perform coverage while the deep planning layer stays dark, and the share of performative activity has risen monotonically since 1945 apart from the late-Cold-War interruption. Accessibility_collapse is 0.62: the alternative (serious total-war planning frameworks) is historically documented and materially feasible but professionally near-unreachable, short of full collapse because archives, foreign doctrines, and revisionist networks keep partial access alive. Resistance is 0.30: recurring revisionist calls gain brief traction after shocks, then recede. The 1985 dip in extractiveness reflects the late-Cold-War nuclear war-fighting debate, which temporarily reopened discourse before the post-1991 closure accelerated both extraction and theater. Claim and metrics are independent: the piton claim rests on the cost-asymmetry test (the administering institutions could change the arrangement, but the cost of fixing exceeds what they bear, and no seat maintains it deliberately), while the metric values describe observed operation.
 *
 * PERSPECTIVAL GAP:
 *   From the war-college seat the empty curricular slot reads as prudence: why teach the unthinkable. From the decision-maker seat the same emptiness appears at crisis time as missing rungs on the escalation ladder and unpriced mobilization questions. From the historian's seat it reads as the third recurrence of a documented cycle of interwar forgetting. The payer seats and the beneficiary seats should therefore compute different types from identical structural data, and the agenda-setter seat should compute yet another: the engine derives this divergence from power, exit, and role declarations, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared beneficiaries sit near the subsidy end: the exclusion preserves the discursive monopoly their accumulated capital depends on, and identity_locked exit amplifies their attachment to the arrangement that protects that capital. The declared payers sit near the target end: decision-makers bear concentrated costs at decision time, planners bear them daily, and allies bear them through exposure they cannot individually remedy. One override is authored: national_security_decision_makers carry a secondary beneficiary position (political convenience, avoidance of toxic public debates), and a naive dual-role derivation would land them near symmetric; the override to d=0.6 encodes that their net position is target-leaning because the foregone flexibility concentrates on them at decision time while the convenience is diffuse and episodic. Enforcement decay is central to the directionality picture: the arrangement no longer needs enforcement, so persistence is inertial rather than coerced, which is what separates this story from an enforced-extraction reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, keeping great-power war below the total threshold, is contested-live as a danger, but the mechanism that once served it, conscious elite bracketing, has been displaced by declaratory policy, deterrence posture, and arms control. The discursive exclusion now persists as residue performing mainly theatrical coverage, which is the mandate-outlived-function condition, hence mandatrophy_resolved is declared true. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the flexibility it forecloses and the capacity it let decay; reading it as enforced extraction ignores the absence of enforcement and the diffuseness of accrual. The residue-with-cost-asymmetry structure, administered by institutions that could change it but bear little of its cost, is what the piton claim captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Which reading of the total_war_winnability_post1945 kernel does this constraint instantiate, and what would switching readings change?',
    'Compile the sibling stories (normative_reading_drop, structural_contraction_reading) and compare per-reading epsilon, beneficiary/victim sets, and computed types; the indexical classification resolves the kernel by holding readings separate rather than averaging them.',
    'All metrics and classifications authored here apply to the ideational-drift mechanism only. Adopting the structural_contraction reading would empty the victim set (nothing is foregone if nothing is reachable) and collapse this story toward a physical mountain; adopting the normative reading would relocate the binding force to legal-moral prohibition and change the enforcement profile entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame indexicality: this file is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    reachability_premise_disagreement,
    'Where exactly is the kernel contest located: on the reachable-space premise, on the causal locus of the exclusion, or on both?',
    'Structural audit separating material reachability (industrial capacity, delivery systems, mobilization law) from discursive accessibility (curricula, planning documents, fundable research programs); the readings partition on which side of that split carries the binding force.',
    'If material reachability fails, this reading''s foundational axiom is empirically refuted and the structural_contraction reading absorbs the phenomenon as a physical mountain; if reachability holds but legal-moral prohibition fully explains the exclusion, the normative reading absorbs it with a different enforcement signature and victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_premise_disagreement, conceptual, 'Locates the disagreement among sibling readings on the reachable-space premise and the exclusion''s causal locus.').

omega_variable(
    atrophy_reversibility,
    'Is the institutional forgetting reversible: could mobilization depth, curricula, and planning branches be rebuilt, or has industrial-base and expertise decay crossed a point of no return?',
    'Industrial-base audits, mobilization exercise timelines, and comparative cases where states reconstituted total-war capacity after long neglect.',
    'Reversible atrophy keeps remediation a live policy option and the arrangement a candidate for repair; irreversibility hardens the exclusion into a self-fulfilling fixture and pushes effective extraction upward as the foregone option becomes permanently priced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_reversibility, empirical, 'Whether the atrophied capacity can be reconstituted or the forgetting has become permanent.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (funding gates, promotion risk, clearance politics) versus internalized (professional identity that renders total-war thinking self-disqualifying)?',
    'Compare discourse freedom across positions facing different gate structures: retired strategists, foreign-trained analysts, and non-Western academies; if the silence persists where gates are absent, the internalized share dominates.',
    'If internalized dominance holds, removing gatekeepers would not restore discourse and remediation must target professional identity rather than access; the effective suppression then exceeds what the near-zero enforcement machinery suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized composition of the residual suppression.').

omega_variable(
    beneficiary_rent_or_inertia,
    'Do the limited-war defense intellectuals hold genuine rents that the exclusion protects, making the arrangement maintained-for-benefit, or do they merely inherit an inertia they did not build and do not defend?',
    'Trace funding, citation, and career flows under counterfactual reopening: would total-war planning programs compete away the community''s resources, and does any organized actor act to defend the exclusion rather than merely reproduce it?',
    'Genuine defended rents would push the classification toward enforced extraction with a capturing seat; inherited inertia with no defending actor confirms the residue reading, with accrual occurring without maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_rent_or_inertia, empirical, 'Distinguishes captured rents from passive inheritance in the beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.54).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2015, 0.58).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(tota_su_t1955, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1955, 0.35).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(tota_su_t1995, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(tota_su_t2015, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the obsolescence of total war after 1945' decomposes into three structurally distinct claims with different epsilon values: physical removal from the reachable space (structural_contraction_reading), legal-moral illegitimation (normative_reading_drop), and ideational drift out of elite discourse (this story). Each carries its own beneficiaries, victims, and classification; they are linked here as a constraint family. This reading sits downstream of the normative reading, whose legal prohibition supplies part of the cultural backdrop the drift operates in, and stands in direct logical opposition to the structural reading on the reachable-space premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
