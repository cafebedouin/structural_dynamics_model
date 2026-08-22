% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Outcomes-Based Legality Threshold for Autonomous Engagement (IHL Distinction/Proportionality)
 *   domain: legal/military/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the IHL
 *   distinction/proportionality kernel: the outcomes-based reading, under
 *   which autonomous systems may lawfully select and engage targets whenever
 *   their distinction and proportionality performance demonstrably equals or
 *   exceeds that of human operators, because the law governs outcomes and not
 *   means. The constraint's operation: a certification gate converts an
 *   interpretive question into a measurable one; whoever controls the
 *   benchmark and the evidence controls the answer. The claim/metric gap is
 *   deliberate — the reading is CLAIMED as tangled_rope (real coordination
 *   function, real asymmetric extraction) and the metrics are authored as
 *   descriptively true of its actual operation; the engine computes per-seat
 *   classifications from the structural data and measures any divergence. KEY
 *   AGENTS (by structural relationship): - military_operational_commands:
 *   agenda setter (institutional/arbitrage) — owns thresholds, evidence, and
 *   doctrine - autonomous_systems_defense_contractors: primary beneficiary
 *   (powerful/mobile) — collects procurement flow, insulated from field risk
 *   - civilian_populations_in_conflict_zones: primary target
 *   (powerless/trapped) — absorbs residual failure risk -
 *   ihl_interpretive_custodians: secondary target (organized/constrained) —
 *   displaced interpretive authority, retained audit seat -
 *   combatant_operators: dual-positioned (moderate/constrained) —
 *   force-protection gains against responsibility-gap exposure -
 *   treaty_diplomats_ccw_gge: observer (institutional/analytical) -
 *   humanitarian_advocacy_coalitions: excluded (organized/trapped) —
 *   means-based voice inadmissible at the operative gate
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.6).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Legality Threshold for Autonomous Engagement (IHL Distinction/Proportionality)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "legal/military/technological").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '8703e8b3-faa7-4ec3-8728-6387bacf9d49').
narrative_ontology:cs_kernel_codification('8703e8b3-faa7-4ec3-8728-6387bacf9d49', formalized).
narrative_ontology:cs_authority_grounding('8703e8b3-faa7-4ec3-8728-6387bacf9d49', expertise).
narrative_ontology:cs_interpretation_layer_present('8703e8b3-faa7-4ec3-8728-6387bacf9d49').
narrative_ontology:cs_reading_relation('8703e8b3-faa7-4ec3-8728-6387bacf9d49', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('8703e8b3-faa7-4ec3-8728-6387bacf9d49', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('8703e8b3-faa7-4ec3-8728-6387bacf9d49', foundational, outcome_equivalence_suffices_for_ihl_compliance).
narrative_ontology:cs_axiom_status(outcome_equivalence_suffices_for_ihl_compliance, holdable).
narrative_ontology:cs_axiom_grounding('8703e8b3-faa7-4ec3-8728-6387bacf9d49', outcome_equivalence_suffices_for_ihl_compliance, empirically_contingent).
narrative_ontology:cs_axiom('8703e8b3-faa7-4ec3-8728-6387bacf9d49', foundational, law_governs_outcomes_not_means).
narrative_ontology:cs_axiom_status(law_governs_outcomes_not_means, holdable).
narrative_ontology:cs_axiom_grounding('8703e8b3-faa7-4ec3-8728-6387bacf9d49', law_governs_outcomes_not_means, conventional).
narrative_ontology:cs_reference_frame('8703e8b3-faa7-4ec3-8728-6387bacf9d49', technology_neutral_performance_threshold).
narrative_ontology:cs_drift_state('8703e8b3-faa7-4ec3-8728-6387bacf9d49', contemporary_deployment_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8703e8b3-faa7-4ec3-8728-6387bacf9d49', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, combatant_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, combatant_operators).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, performance_equivalence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run Article 36 weapons reviews, own the test-and-evaluation regimes that generate the performance evidence, and set national doctrine on when a certified autonomous system may select and engage targets without further human confirmation. They receive the capability gains of delegated engagement and control both the threshold and the evidence that clears it; they also carry the legal and political exposure when a certified system errs in ways the tests did not predict.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands, agenda_setter,
    institutional, generational, arbitrage, global).

% Design and build the autonomous engagement systems and much of the benchmark infrastructure used to certify them. Once the outcomes-based gate opens, procurement contracts flow to them; they are largely insulated from downstream liability for field failures and from the conflict zones where those failures land, and can pivot product lines or export markets if a given legal regime tightens.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Live where certified autonomous systems operate. They absorb the residual risk when distinction or proportionality performance degrades under conditions the certification suite did not cover — dense urban terrain, degraded communications, adversarial spoofing. They have no seat in threshold-setting, testing, or review processes, and no exit from the battlespace.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% ICRC delegations, humanitarian-law bodies, and military legal advisers whose traditional authority consists in judging whether a particular targeting decision honors distinction and proportionality. The outcomes-based standard relocates that determination into engineering benchmarks, retaining for them an auditing and commenting seat whose advice the certification gate can lawfully proceed without.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians, observer).

% Gain force protection: delegated engagement removes them from exposed observation and trigger roles and multiplies coverage per operator. They also carry the reverse side — responsibility-gap exposure when a system they supervised engages wrongly, career consequences attached to certification failures, and the moral injury of having delegated a judgment they were trained to own. Service obligations bound their ability to opt out of autonomous formations.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, combatant_operators, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, combatant_operators, payer).

% Chair and negotiate the CCW Group of Governmental Experts process where the adequacy of outcomes-based legality is formally debated. They broker compromise text, commission technical briefings, and take testimony from every other seat, but collect no direct gain from the standard and bear none of its field risk.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, treaty_diplomats_ccw_gge, observer,
    institutional, generational, analytical, global).

% Campaign for categorical or human-control limits on autonomous weapons. They are physically present in every diplomatic forum, but their distinctive claim — that machine-decided killing is objectionable in its means regardless of measured accuracy — is structurally inadmissible inside the outcomes-based frame, which treats means-based objections as category errors once the performance threshold is met. The operative gate is closed to them no matter how loud their participation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_advocacy_coalitions, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_defense_contractors).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an open-ended interpretive dispute — when may a machine select and engage a target consistently with distinction and proportionality — into a single testable threshold that states, developers, reviewers, and treaty bodies can all measure against, replacing case-by-case moral adjudication with a shared certification standard.
% TRANSFER_FUNCTION: Moves life-and-death decision authority from human operators and humanitarian-law interpreters to certified machine performance; moves procurement funds from state budgets to defense contractors; transfers the residual risk of metric failure under field conditions onto civilian populations in conflict zones.
% ABSENT_VOICES: Means-based objectors (humanitarian advocacy coalitions) are present in forums but structurally excluded from the operative gate — the framework cannot register their core objection. Civilian populations in conflict zones, who bear the failure risk, have no seat anywhere in the threshold-setting, testing, or review chain.
% DISAPPEARANCE_RATIONALE: If the outcomes-based threshold vanished overnight, procurement programs for certified autonomous engagement would lose their legality pathway and pause or restructure, Article 36 reviews would revert to open-ended interpretive judgment with no determinate answer, treaty negotiations would lose their central reference point, and every deployed system's authorization would fall back to whichever human-judgment requirement each state had previously rejected as unnecessary.
% FOUNDING_PROBLEM: IHL's distinction and proportionality rules were written presuming human judgment at the moment of force; machine-speed targeting broke that presumption, and without a determinate standard either all autonomous engagement is unlawful or legality is ad hoc per commander. The reading was built to supply the missing determinate answer: a performance threshold that permits beneficial autonomy while preserving a compliance guarantee.
% FOUNDING_PROBLEM_CORROBORATION: ICRC position papers, UN CCW GGE session reports, and academic international-humanitarian-law scholarship outside the benefiting parties all attest that the governing problem — legal settlement for machine-speed lethal decisions — remains unresolved and intensifying, even where those sources dispute this reading's solution to it.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.60: the gate's benefits concentrate (procurement, capability) while its costs displace onto parties with no seat — civilians bearing untested-condition risk, custodians losing the judgment function that constitutes their role. Suppression 0.48: the constraint coerces not by physical force but by rendering means-based objections inadmissible and by locking procurement programs and allied interoperability onto the certification pathway; alternatives persist politically (treaty proposals remain live) but not operationally once a state adopts the gate. Theater_ratio 0.34: benchmark suites measurably test something real, but a growing share of certification activity demonstrates performance in permissive conditions (controlled ranges, cooperative emitters, scripted scenarios) while warranting general field compliance — performative assurance layered on a functional core. Accessibility_collapse 0.55: within an adopting state's doctrine, alternatives (human-confirmation requirements, categorical refusal) collapse to irrelevance once metrics pass; across the international system, alternative framings remain visibly alive, keeping collapse well below natural-law levels. Resistance 0.62: sustained organized opposition — advocacy campaigns, ICRC positions, blocs of states pushing binding instruments — that the constraint must continuously outmaneuver in diplomatic process. All three temporal series run on one shared seven-point grid (2014–2026, two-year steps); the 2026 endpoints are marked projected. The monotonically rising base_extractiveness series models rent-layering: as certification infrastructure matured, the gap between what the gate verifies and what it warrants widened. The rising suppression_requirement series tracks genuine enforcement-capacity growth — formalized Article 36 review procedures, classification of test data, procurement lock-in — which is why it is authored despite the static-scalar default. T17 will read the extraction accumulation as an abductive hypothesis; it does not reclassify.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (military commands), the arrangement is a determinate, verifiable legality that enables responsible innovation and replaces unanswerable moral litigation with engineering. From the payer seats, the same structure is a self-graded examination: the parties who profit write the threshold, run the tests, classify the evidence, and transfer the failure modes to people who were never consulted. Custodians experience a third position — not extraction of money but extraction of function, the migration of a moral-legal judgment into a lab report they may comment on but cannot block. The engine computes these divergent per-seat types from power, exit, and directionality data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: military_operational_commands sit near the beneficiary end (they set the gate and collect capability, though they retain tail legal exposure); autonomous_systems_defense_contractors sit nearest the beneficiary pole (revenue with mobile exit and no field exposure); civilian_populations_in_conflict_zones sit nearest the full-target pole (trapped, powerless, absorbing failure risk); ihl_interpretive_custodians sit high-target (constrained exit, displaced function). One override is declared: combatant_operators hold power atom 'moderate', and their primary beneficiary role would derive a low d, ignoring the responsibility-gap exposure, career consequence, and moral injury that fall on them when delegated engagements fail — the override sets d to 0.45, reflecting a genuinely near-symmetric net position. Treaty diplomats derive near-symmetric (observer seat, analytical exit). Humanitarian advocacy coalitions, though excluded from the gate, remain structurally affected by it; their exclusion is the enforcement object, not an absence of stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: machine-speed targeting continues to outrun interpretive settlement, and no corroborating source outside the benefiting parties declares the problem dead. Mandatrophy is therefore NOT resolved, and the classification guards against mislabeling in both directions. Against the pure-extraction reading: the coordination function is genuine — a shared measurable benchmark solves a real collective-action problem (indeterminate legality chilling beneficial autonomy or licensing ad hoc per-commander discretion), so this is not a snare wearing a standards costume. Against the pure-coordination reading: the same gate that coordinates also displaces judgment-authority from custodians, concentrates receipts with contractors, and transfers untested-condition risk to trapped civilians — asymmetric extraction through the coordinating structure itself, which is what makes it a tangled rope rather than a rope. Rising theater_ratio is monitored as the leading indicator of piton drift: if benchmark suites become wholly ceremonial while procurement proceeds on momentum, the coordination half atrophies and the residue becomes maintained performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_outcomes_vs_human_agency,
    'This constraint is one reading of kernel ihl_distinction_proportionality (outcomes_based_reading). Its sibling human_agency_reading holds that distinction and proportionality require irreducible human moral judgment at the moment of lethal force. What changes structurally if the sibling reading is adopted instead?',
    'Not resolvable by data within this file — it is resolved by which reading the international legal order eventually institutionalizes in binding instruments and national doctrine. Tracked via treaty outcomes, state practice convergence, and the sibling stories'' own drift states.',
    'Under the human_agency sibling, the victim set expands to include every autonomously engaged target as such, the beneficiary set collapses (certification gates become unlawful), and epsilon is authored against a fundamentally different referent. The two constraints must never be merged or epsilon-averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_outcomes_vs_human_agency, conceptual, 'Committer structure: this story is the outcomes-based reading; the human-judgment sibling would relocate the entire victim/beneficiary structure.').

omega_variable(
    kernel_reading_outcomes_vs_categorical,
    'This constraint is one reading of kernel ihl_distinction_proportionality. Its sibling categorical_prohibition_reading holds that Martens Clause principles of humanity and public conscience prohibit autonomous weapons categorically, regardless of technical performance. Where exactly does the disagreement bite?',
    'Resolved by the eventual legal weight assigned to the Martens Clause — whether it operates as a substantive prohibition source or as interpretive guidance satisfiable by compliant performance. Tracked through treaty negotiation texts and customary-law argumentation.',
    'If the categorical sibling prevails, this constraint''s entire certification apparatus becomes legally inert — no threshold can authorize what the means itself forbids — and epsilon''s referent shifts from a conditional-permission regime to a prohibited-means regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_outcomes_vs_categorical, conceptual, 'Committer structure: disagreement located in whether the Martens Clause is a substantive limit or interpretive guidance.').

omega_variable(
    benchmark_generalization_adversarial_conditions,
    'Do certification benchmarks that demonstrate human-equivalent distinction/proportionality performance in controlled test conditions generalize to contested field conditions — dense urban terrain, degraded communications, adversarial spoofing, novel target presentations?',
    'Independent red-team evaluation regimes with published failure taxonomies; post-deployment audit of engagement records against certification-suite coverage; cross-state comparison of field incident rates for certified systems.',
    'If generalization fails systematically, the gap between what the gate verifies and what it warrants widens sharply: effective extraction on civilian populations rises well above the authored 0.60, the theater component dominates, and the constraint''s legitimacy trajectory bends toward captured-snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_generalization_adversarial_conditions, empirical, 'Whether demonstrated performance equals field performance — the empirical load-bearing wall of the reading.').

omega_variable(
    threshold_authorship_capture,
    'Who authors the pass threshold and the test conditions, and are they structurally independent of the parties whose products are being certified?',
    'Institutional mapping of benchmark-setting bodies: contractor participation in test-design, military ownership of acceptance criteria, presence or absence of independent custodian or third-party sign-off in the certification chain.',
    'If contractors and procuring commands co-author the gate they must pass, ''demonstrably exceeds human operators'' becomes a self-graded examination — the coordination function hollows while the extraction function persists, shifting the computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_authorship_capture, empirical, 'Whether the certification gate is independent or captured by its beneficiaries.').

omega_variable(
    responsibility_gap_allocation,
    'When a certified autonomous system engages contrary to distinction or proportionality, who bears legal and moral responsibility — the supervising commander, the authorizing state, the manufacturer, or no one?',
    'Emerging state practice in courts-martial, compensation claims, and domestic legislation allocating liability for autonomous-system engagements; doctrinal development in the law-of-armed-conflict literature.',
    'Allocation determines where the constraint''s extraction actually lands: if responsibility diffuses to no one, extraction concentrates on victims without recourse; if it falls on commanders, the combatant_operator seat''s directionality rises above the overridden 0.45 and the payer structure densifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_gap_allocation, conceptual, 'Unallocated liability is the silent fourth transfer the reading''s silence leaves open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2014, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2014, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2016, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2016, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2018, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2018, 0.19).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2018, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2020, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2020, 0.23).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2020, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2022, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2022, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2024, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2024, observed).
narrative_ontology:measurement(ihl_outcomes_reading_tr_t2026, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2026, 0.34).
narrative_ontology:measurement_basis(ihl_outcomes_reading_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ihl_outcomes_reading_be_t2014, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2014, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2016, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2016, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2018, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2018, 0.46).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2018, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2020, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2020, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2022, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2022, 0.54).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2022, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2024, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2024, 0.57).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2024, observed).
narrative_ontology:measurement(ihl_outcomes_reading_be_t2026, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(ihl_outcomes_reading_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl_outcomes_reading_su_t2014, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2014, 0.22).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2014, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2016, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2016, 0.26).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2016, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2018, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2018, 0.3).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2018, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2020, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2020, 0.34).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2020, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2022, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2022, 0.39).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2022, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2024, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2024, 0.44).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2024, observed).
narrative_ontology:measurement(ihl_outcomes_reading_su_t2026, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2026, 0.48).
narrative_ontology:measurement_basis(ihl_outcomes_reading_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL compliance for autonomous weapons' decomposes into three structurally distinct constraints — one per reading of the shared kernel ihl_distinction_proportionality. Each member has its own epsilon, its own beneficiary/victim structure, and its own claimed type; they are linked, not merged. The outcomes-based reading (this file) is downstream of an empirical wager (performance equivalence is demonstrable) and is cited BY procuring states as the permissive interpretation; the human_agency and categorical_prohibition siblings reject its core premise and would render its certification apparatus inert. Contamination propagates across the family: erosion of confidence in benchmark validity (this file's empirical omegas) strengthens the siblings' hand, while institutional entrenchment of certification regimes raises the siblings' cost of adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
