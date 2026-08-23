% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: Outcomes-Based Compliance Standard for Autonomous Weapons (Performance-Equivalence Reading)
 *   domain: legal/military/technological governance
 *
 * SUMMARY:
 *   This story instantiates the outcomes_based_reading of the IHL
 *   distinction/proportionality kernel: the claim that obligations under
 *   Additional Protocol I are technology-neutral, attaching to outcomes
 *   rather than means, and are therefore satisfied when an autonomous system
 *   demonstrably performs distinction and proportionality at or above
 *   human-operator level. Under this reading, a passing score opens the
 *   permission structure: procurement, fielding, and delegated engagement
 *   become lawful acts. The arrangement has a genuine coordination function —
 *   it gives states a common, verifiable criterion where otherwise every
 *   deployment decision is an unstructured moral-contest — and an asymmetric
 *   extraction structure: civilian populations in conflict zones absorb the
 *   residual risk of metric failure with no exit and no audit access, while
 *   humanitarian-law custodians lose the deliberative terrain on which their
 *   interpretation binds. Per the epsilon-invariance principle, this file
 *   authors ONLY this reading, with one stable epsilon over one referent: the
 *   standing arrangement in which the performance threshold is the operative
 *   gate on autonomous-weapons lawfulness, assessed by the reading's own
 *   lights (what this reading itself concedes as cost: residual risk below
 *   threshold, accountability gaps, baseline discretion). The sibling
 *   readings are separate constraints, linked via
 *   network.affects_constraints. The claimed_type (tangled_rope) and the
 *   metrics are authored independently: the claim states what I believe is
 *   structurally true; the metrics state what I believe is descriptively true
 *   of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - - states_adopting_performance_standard: Agenda setter (institutional/arbitrage) — sets the standard via Article 36 reviews and doctrine; collects capability and legal cover
 *   - - defense_contractors: Primary beneficiary (powerful/arbitrage) — converts regulatory uncertainty into addressable market; supplies the benchmarks reviews rely on
 *   - - military_forces: Dual-positioned beneficiary/payer (organized/constrained) — gains reach and force protection; commanders retain formal accountability for outcomes they did not individually author
 *   - - weapons_review_and_test_bodies: Secondary beneficiary (institutional/constrained) — gains budget, staffing, and jurisdiction with each system evaluated; independence bounded by funding and access
 *   - - civilian_populations_in_conflict_zones: Primary target (powerless/trapped) — bears misclassification risk in conditions benchmarks did not cover; recourse is post hoc only
 *   - - humanitarian_law_custodians: Target (organized/identity_locked) — ICRC, mandate-holders, scholars; each migration of judgment into scored output narrows the space where their interpretation binds
 *   - - un_gge_delegations: Analytical observer (institutional/analytical) — shapes the terms of contest without fielding or certifying anything
 *   - - global_south_states_without_technical_capacity: Excluded voice (moderate/constrained) — cannot independently verify equivalence claims; objections register as preference, not evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.66).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Compliance Standard for Autonomous Weapons (Performance-Equivalence Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "legal/military/technological governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '00005ab2-86c2-40b5-95eb-65ebaa085d1d').
narrative_ontology:cs_kernel_codification('00005ab2-86c2-40b5-95eb-65ebaa085d1d', fixed_text).
narrative_ontology:cs_authority_grounding('00005ab2-86c2-40b5-95eb-65ebaa085d1d', expertise).
narrative_ontology:cs_interpretation_layer_present('00005ab2-86c2-40b5-95eb-65ebaa085d1d').
narrative_ontology:cs_reading_relation('00005ab2-86c2-40b5-95eb-65ebaa085d1d', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('00005ab2-86c2-40b5-95eb-65ebaa085d1d', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('00005ab2-86c2-40b5-95eb-65ebaa085d1d', foundational, ihl_obligations_are_means_neutral).
narrative_ontology:cs_axiom_status(ihl_obligations_are_means_neutral, holdable).
narrative_ontology:cs_axiom_grounding('00005ab2-86c2-40b5-95eb-65ebaa085d1d', ihl_obligations_are_means_neutral, conventional).
narrative_ontology:cs_axiom('00005ab2-86c2-40b5-95eb-65ebaa085d1d', foundational, performance_equivalence_satisfies_distinction_proportionality).
narrative_ontology:cs_axiom_status(performance_equivalence_satisfies_distinction_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('00005ab2-86c2-40b5-95eb-65ebaa085d1d', performance_equivalence_satisfies_distinction_proportionality, empirically_contingent).
narrative_ontology:cs_reference_frame('00005ab2-86c2-40b5-95eb-65ebaa085d1d', technology_neutral_performance_equivalence).
narrative_ontology:cs_drift_state('00005ab2-86c2-40b5-95eb-65ebaa085d1d', contemporary_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00005ab2-86c2-40b5-95eb-65ebaa085d1d', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_forces).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, weapons_review_and_test_bodies).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, states_adopting_performance_standard).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, military_forces).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, performance_equivalence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set national policy on autonomous weapons through Article 36 weapons reviews, military doctrine, and treaty positions. They decide what counts as adequate demonstration of compliance, fund the test infrastructure, and determine whether a given system may be fielded. They collect operational capability and legal cover from the standard they administer. Their alternatives — requiring human control in the loop, negotiating prohibitions, declining the technology — remain open, and peer competition raises the price of exercising them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_adopting_performance_standard, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, states_adopting_performance_standard, beneficiary).

% Design and sell autonomous targeting and engagement systems. Revenue depends on procurement programs that a passing performance score renders licensable; they supply the benchmarks, test data, and validation studies that reviews rely on. Their exit is commercial — capital and talent move toward whichever jurisdictions and product lines the prevailing standard admits.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate the systems once fielded. They gain reach, speed, and force protection from delegated engagement decisions, while individual commanders retain formal legal responsibility for outcomes their systems produce; after-action accountability falls on officers who did not take each shot. Unilateral exit means accepting a capability gap against peers who adopt the technology, so their posture is shaped as much by rivalry as by conviction.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_forces, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, military_forces, payer).

% Run the evaluations: design test scenarios, score distinction and proportionality performance against human baselines, and issue the findings that reviews act on. Their budget, staffing, and jurisdiction grow with each system submitted. Their findings are only as independent as their funding and access arrangements allow, and their professional standing is bound to the evaluation function they perform.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, weapons_review_and_test_bodies, beneficiary,
    institutional, biographical, constrained, national).

% Live where these systems operate. They cannot choose the classification thresholds applied overhead, cannot audit the test data behind a fielded system, and absorb the consequences when a system misclassifies in conditions the benchmarks did not cover. Their recourse runs through complaint mechanisms and advocacy channels that operate after the fact, from locations they often cannot leave.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, biographical, trapped, regional).

% ICRC delegates, UN mandate-holders, and IHL scholars who interpret distinction and proportionality for the international community. Each migration of proportionality judgment from deliberation to scored output narrows the space in which their interpretation binds. Their standing depends on remaining inside a role whose traditional content the standard progressively displaces, so stepping outside the role costs them the platform from which they speak.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    organized, generational, identity_locked, global).

% Meet under the Convention on Certain Conventional Weapons to debate governance of autonomous weapons. They compile positions, table proposals ranging from binding prohibition to non-binding guidelines, and produce consensus documents that constrain little but signal trajectories. Their seat is analytical: they shape the terms of contest without fielding, certifying, or bearing the risks at issue.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, un_gge_delegations, observer,
    institutional, generational, analytical, global).

% Lack the test infrastructure, contractor base, and review personnel to evaluate performance claims on their own account. They participate in diplomatic forums but cannot independently verify equivalence assertions, so their objections register as preference rather than evidence. They would demand verification regimes they can audit if resourced to do so.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, global_south_states_without_technical_capacity, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, verifiable criterion for a question states otherwise answer ad hoc: whether a given autonomous system may lawfully be fielded and delegated engagement authority. The performance threshold lets reviewers, procurers, and allies coordinate on a shared evidentiary gate instead of unstructured moral-political contest in every procurement decision.
% TRANSFER_FUNCTION: Moves lethal-engagement decision authority from human operators and deliberative legal processes to machine systems cleared by technical scores; moves the residual risk of misclassification onto civilian populations in operating areas; moves procurement revenue toward contractors and budget, staffing, and jurisdiction toward test and review bodies.
% ABSENT_VOICES: Civilian populations in conflict zones have no seat anywhere in the review chain that sets the thresholds governing fire directed at their neighborhoods; global-south states without technical capacity attend diplomatic forums but cannot verify the equivalence claims under debate; future victims of benchmark-blind failure modes cannot speak at all. The unanimity that performance-equivalence enjoys inside adopting institutions arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the outcomes-based standard vanished overnight, procurement pipelines built on score-gated licensure would stall pending a replacement criterion; fielded-system doctrines would revert to human-in-the-loop defaults; the diplomatic contest would reorganize entirely around the human-agency and categorical-prohibition positions; and test-and-evaluation bureaucracies would lose their mandate. Deployed systems, contractor balance sheets, and alliance interoperability commitments all currently hang on this gate.
% FOUNDING_PROBLEM: States developing autonomous weapons faced an unresolved applicability question: do existing IHL obligations of distinction and proportionality govern machine-made engagement decisions, and by what evidence can compliance be assessed? The outcomes-based reading was built to answer 'yes, provided measured performance matches or exceeds human operators,' converting an open legal-moral question into a testable gate that unblocked procurement.
% FOUNDING_PROBLEM_CORROBORATION: The assessment problem's liveness is corroborated from outside the benefiting parties: ICRC position papers and UN Secretary-General reports attest that the question of how to assess machine compliance remains unresolved and pressing, while disputing that performance equivalence is an adequate answer; General Assembly resolutions and CCW GGE records show states across blocs treating the assessment question as open. No party outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 (moderate-to-substantial): the reading's own lights concede residual risk below threshold, an accountability gap between formal commander responsibility and machine-authored engagements, and baseline-selection discretion — but the permission is conditional on demonstrated performance, which caps epsilon short of snare territory. Suppression is 0.58: the standard does not outlaw sibling readings (they persist as live diplomatic positions), but within adopting institutions it overrides moral objection as 'non-technical', and career and alliance pressure penalize internal dissent — suppression of alternatives inside the perimeter, coexistence outside it. Accessibility_collapse is 0.50: accepting outcome-measurement collapses the human-agency and prohibition framings considerably but not completely; both remain live in CCW forums and General Assembly resolutions. Resistance is 0.62: sustained custodial and civil-society opposition, with real diplomatic traction. Theater_ratio is 0.41 and rising: benchmark exercises are real measurement, but a growing share of 'demonstration' activity is constructed to pass — scenarios curated to the system's strengths, baselines selected favorably — classic Goodhart drift as the metric becomes the target. The temporal series run on one shared grid (t=0..30, one unit = one year, t=0 anchored at 2014 when CCW work on autonomous weapons began): base_extractiveness climbs as deployments outrun verification depth; suppression_requirement climbs because the story specifically traces enforcement-capacity build-out — test infrastructure, review boards, and certification practice maturing and hardening over the interval — which is why that series is authored rather than left to the static scalar; theater_ratio climbs with benchmark curation. Points through t=12 are observed; later points are authored projections and marked as such.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and test-body seats should compute something coordination-shaped: from inside, the standard is diligence — a rigorous, quantified gate that disciplines procurement and replaces vibes with evidence. The payer seats should compute enforced extraction: from a conflict-zone civilian's position, the same certification event is the document that moved the kill decision off a human conscience and onto a scorecard they cannot read; from the custodian's position, it is the instrument that converts a deliberative question into a technical one their office no longer answers. The military seat straddles: operational benefit up front, accountability exposure behind. The engine computes this per-seat divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for defense_contractors (arbitrage-grade exit pushes them toward the beneficiary pole) and weapons_review_and_test_bodies (constrained exit, but their funding and jurisdiction scale with the standard's adoption). military_forces carries a dual declaration — beneficiary with payer as secondary role — so the derivation should land them mid-range rather than at either pole: they collect capability and simultaneously bear the accountability tail. states_adopting_performance_standard sits near the beneficiary end as agenda-setter with secondary benefit, tempered by their exposure to diplomatic and reputational cost. civilian_populations_in_conflict_zones derives near-full-target directionality: victim status, powerless, trapped — they cannot decline the threshold imposed over them. humanitarian_law_custodians derives high directionality amplified by identity_locked exit: their professional and institutional identity is constituted by the interpretive role the standard displaces, so exit is not merely costly but self-dissolving. No directionality_overrides are authored: the role-plus-exit derivation captures every asymmetry this story contains, including the dual-positioned seats via secondary_role, and an override keyed only by power atom would misfire across same-power agents (test bodies and states share the institutional atom with different relationships the roles already encode).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to assess machine compliance with distinction and proportionality — is still live, so this is not a mandatrophy case and no sunset or resolved flag is authored. The tangled_rope classification guards both adjacent errors: against calling this a rope (the extraction is asymmetric and enforced — someone is coordinated and someone pays through the same certification structure), and against calling it a snare (the coordination function is genuine, the permission is conditional on demonstrated performance, and the sibling readings persist as live alternatives rather than suppressed exits). The trajectory to watch is piton-ward: if theater_ratio continues climbing while deployments outrun verification depth, the standard's measurement function atrophies into ritual clearance — benchmarks performed to certify rather than to know — leaving an inertial gate nobody benefits enough to repair and civilians cannot refuse. The rising theater series is authored precisely so that drift is detectable rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel ihl_distinction_proportionality (reading: outcomes_based_reading). What would the sibling readings — human_agency_reading and categorical_prohibition_reading — change structurally if instantiated instead?',
    'Compare against the sibling story files: human_agency_reading adds an irreducible-human-judgment requirement that removes the performance threshold as a sufficient condition; categorical_prohibition_reading deletes the permission structure entirely and reassigns nearly all seats to payer or excluded positions.',
    'The disagreement is located in whether the IHL obligation attaches to the outcome produced or to the judging subject and the act of delegation itself. Under a sibling reading, this file''s beneficiary set shrinks or empties, epsilon rises sharply, and the classification trends toward snare; under this reading, the coordination function is real and epsilon stays moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story instantiates one of three live readings of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    benchmark_to_combat_transfer_validity,
    'Does measured distinction/proportionality performance in test environments demonstrate equivalent performance under combat conditions — adversary adaptation, degraded communications, novel tactics, mixed urban crowds?',
    'Adversarial red-team evaluation regimes and, where available, after-action data from deployed systems compared against pre-deployment benchmark scores.',
    'If benchmark scores systematically fail to transfer, the reading''s core warrant collapses: deployments proceed on numbers that do not describe operating conditions, civilian risk rises without compensating assurance, and effective extraction climbs well above the authored 0.66.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_to_combat_transfer_validity, empirical, 'Whether laboratory and range performance equivalence carries to field conditions.').

omega_variable(
    human_baseline_selection_ambiguity,
    'Which human operators constitute the comparison baseline — the trained median, elite special operators, or fatigued conscripts under stress — and who decides?',
    'Comparative operator-performance studies across training tiers and stress conditions, plus disclosure of the baseline written into national weapons reviews.',
    'A weak baseline lowers the bar the system must clear and silently raises permitted autonomy; a strong baseline approaches the human_agency_reading''s practical demands. Baseline choice is a hidden dial on the entire permission structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_baseline_selection_ambiguity, conceptual, 'Baseline selection determines the de facto compliance threshold and is itself an unexamined locus of discretion.').

omega_variable(
    certification_independence_capture,
    'Are the bodies that design benchmarks and issue compliance findings sufficiently independent of the procuring states and contractors whose systems they evaluate?',
    'Audit of funding flows, access arrangements, and publication practices of national weapons-review and test organizations; comparison of government-scored versus independent third-party evaluations of the same systems.',
    'If captured, the enforcement machinery validates rather than gates, the theater component of the measured profile understates reality, and the constraint drifts from tangled_rope toward snare with contractors as concentrated capturers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_independence_capture, empirical, 'Independence of the verification layer from the parties whose systems it clears.').

omega_variable(
    interpretive_authority_displacement_valence,
    'Does routing proportionality judgment through scored outputs constitute a loss borne by humanitarian-law custodians, or a legitimate modernization of how a living legal tradition operates?',
    'Track whether custodial interpretation retains binding force over edge cases the metrics do not cover, and whether review bodies defer to or override custodial analysis when scores and legal advice diverge.',
    'If displacement is real, part of the measured extraction is authority transferred from a deliberative community to a measurement apparatus; if the custodial channel remains authoritative, the constraint is closer to a plain coordination standard with lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_displacement_valence, conceptual, 'Whether the standard displaces or merely supplements custodial interpretation.').

omega_variable(
    residual_risk_incidence_under_metric_failure,
    'When fielded systems err below their certified thresholds, who actually absorbs the error mass, and is any compensation or correction channel available to them?',
    'Casualty and incident attribution data from deployments, mapped against the classification thresholds in force at the time, plus tracking of complaint-channel responsiveness.',
    'Concentrated incidence on populations with no exit and no audit access is the load-bearing fact behind the victim declaration; diffuse or corrected incidence would soften the extraction asymmetry and pull the classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_incidence_under_metric_failure, empirical, 'Where the error mass lands when certified performance fails in the field.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_dp_obr_tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t0, observed).
narrative_ontology:measurement(ihl_dp_obr_tr_t6, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t6, observed).
narrative_ontology:measurement(ihl_dp_obr_tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t12, observed).
narrative_ontology:measurement(ihl_dp_obr_tr_t18, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t18, projected).
narrative_ontology:measurement(ihl_dp_obr_tr_t24, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t24, projected).
narrative_ontology:measurement(ihl_dp_obr_tr_t30, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(ihl_dp_obr_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(ihl_dp_obr_be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t0, observed).
narrative_ontology:measurement(ihl_dp_obr_be_t6, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t6, observed).
narrative_ontology:measurement(ihl_dp_obr_be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t12, observed).
narrative_ontology:measurement(ihl_dp_obr_be_t18, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t18, projected).
narrative_ontology:measurement(ihl_dp_obr_be_t24, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t24, projected).
narrative_ontology:measurement(ihl_dp_obr_be_t30, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(ihl_dp_obr_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl_dp_obr_su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t0, observed).
narrative_ontology:measurement(ihl_dp_obr_su_t6, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t6, observed).
narrative_ontology:measurement(ihl_dp_obr_su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t12, observed).
narrative_ontology:measurement(ihl_dp_obr_su_t18, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t18, projected).
narrative_ontology:measurement(ihl_dp_obr_su_t24, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t24, projected).
narrative_ontology:measurement(ihl_dp_obr_su_t30, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(ihl_dp_obr_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL compliance for autonomous weapons' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This story (outcomes_based_reading) is the downstream node: its permission structure is cited BY the other readings as the thing they oppose, and its adoption by major military powers changes the legitimacy conditions and resource availability under which the human_agency and categorical_prohibition readings operate — pressuring custodial institutions and prohibition coalitions without logically eliminating either (hence coexists_with edges, not forecloses). Each member carries its own epsilon, beneficiaries, victims, and claimed_type; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
