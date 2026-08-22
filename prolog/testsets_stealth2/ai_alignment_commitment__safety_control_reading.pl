% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__safety_control_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: AI Alignment Commitment — Catastrophic Loss-of-Control Reading (Safety-Control)
 *   domain: technology governance/AI policy/research ethics
 *
 * SUMMARY:
 *   The safety-control reading defines AI alignment as the prevention of
 *   catastrophic loss of control over advanced AI systems. As an operative
 *   constraint on the field, it organizes funding portfolios, career ladders,
 *   conference taxonomies, and government evaluation mandates around long-run
 *   failure scenarios, and it ranks speculative catastrophe prevention above
 *   remediation of harms already occurring in deployed systems. The
 *   constraint solves a real collective-action problem — competitive
 *   capability development produces diffusely-held catastrophic risk that no
 *   single actor internalizes — while simultaneously diverting resources,
 *   attention, and legitimacy from present-day harm mitigation, whose
 *   constituencies are weaker and whose failures are smaller per-event but
 *   continuous. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as tangled_rope (genuine coordination core with asymmetric extraction
 *   riding on it) and the authored metrics describe substantially extractive,
 *   actively enforced operation; the engine measures any divergence — do not
 *   reconcile the claim to the metrics. KEY AGENTS (by structural
 *   relationship): - ai_safety_research_community: Principal beneficiary and
 *   co-agenda-setter (organized/identity_locked) — collects funding, venues,
 *   and standing; professionally fused with the mission -
 *   frontier_lab_safety_divisions: Institutional beneficiary and
 *   co-agenda-setter (institutional/arbitrage) — converts safety activity
 *   into scaling license; cheapest exit in the system -
 *   existential_risk_research_institutes: Secondary beneficiary
 *   (organized/constrained) — programmatic identity depends on the framing's
 *   primacy - communities_facing_present_day_ai_harms: Primary target
 *   (powerless/trapped) — bears continuous present harms; cannot exit the
 *   systems involved - algorithmic_fairness_researchers: Secondary target
 *   (moderate/constrained) — crowded out of core-alignment status -
 *   humanity_as_whole_including_future_generations: Nominal protected class,
 *   non-agent (civilizational/universal) — invoked on behalf of, represented
 *   by proxy, no agency of its own - ai_policy_regulators: Agenda-setter and
 *   observer (institutional/analytical) — administers the framing through
 *   evaluation mandates - global_south_ai_policy_voices: Excluded voice
 *   (moderate/constrained) — contests the priority ordering from outside the
 *   rooms where it is set Decomposition note (epsilon-invariance): the
 *   colloquial label 'AI alignment' covers structurally distinct claims. This
 *   story instantiates ONLY the safety-control reading; the
 *   ethics_justice_reading and integrated_reading are separate constraints
 *   with their own epsilon values, victim sets, and classifications, linked
 *   via network.affects_constraints. Epsilon here is authored for the
 *   standing safety-control-governed arrangement — the reading-indexed value
 *   over the arrangement this story is about — never for the integrated
 *   arrangement this reading's critics would substitute.
 *
 * KEY AGENTS:
 *   - ai_safety_research_community: principal beneficiary and co-agenda-setter (organized power, identity_locked exit)
 *   - frontier_lab_safety_divisions: institutional beneficiary and co-agenda-setter (institutional power, arbitrage exit)
 *   - existential_risk_research_institutes: secondary beneficiary (organized power, constrained exit)
 *   - communities_facing_present_day_ai_harms: primary target (powerless, trapped exit)
 *   - algorithmic_fairness_researchers: secondary target (moderate power, constrained exit)
 *   - humanity_as_whole_including_future_generations: nominal protected class, non-agent aggregate
 *   - ai_policy_regulators: agenda-setter and observer (institutional power, analytical exit)
 *   - global_south_ai_policy_voices: excluded voice (moderate power, constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.71).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Alignment Commitment — Catastrophic Loss-of-Control Reading (Safety-Control)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "technology governance/AI policy/research ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '86b51c2e-b0dc-44ca-b854-ff5150640bde').
narrative_ontology:cs_kernel_codification('86b51c2e-b0dc-44ca-b854-ff5150640bde', distributed).
narrative_ontology:cs_authority_grounding('86b51c2e-b0dc-44ca-b854-ff5150640bde', expertise).
narrative_ontology:cs_interpretation_layer_present('86b51c2e-b0dc-44ca-b854-ff5150640bde').
narrative_ontology:cs_reading_relation('86b51c2e-b0dc-44ca-b854-ff5150640bde', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('86b51c2e-b0dc-44ca-b854-ff5150640bde', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('86b51c2e-b0dc-44ca-b854-ff5150640bde', foundational, loss_of_control_is_paramount_alignment_failure).
narrative_ontology:cs_axiom_status(loss_of_control_is_paramount_alignment_failure, holdable).
narrative_ontology:cs_axiom_grounding('86b51c2e-b0dc-44ca-b854-ff5150640bde', loss_of_control_is_paramount_alignment_failure, empirically_contingent).
narrative_ontology:cs_axiom('86b51c2e-b0dc-44ca-b854-ff5150640bde', secondary, catastrophic_prevention_lexical_priority_over_present_harm).
narrative_ontology:cs_axiom_status(catastrophic_prevention_lexical_priority_over_present_harm, holdable).
narrative_ontology:cs_axiom_grounding('86b51c2e-b0dc-44ca-b854-ff5150640bde', catastrophic_prevention_lexical_priority_over_present_harm, instrumental).
narrative_ontology:cs_reference_frame('86b51c2e-b0dc-44ca-b854-ff5150640bde', control_problem_precautionary_baseline).
narrative_ontology:cs_drift_state('86b51c2e-b0dc-44ca-b854-ff5150640bde', post_frontier_mainstreaming, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('86b51c2e-b0dc-44ca-b854-ff5150640bde', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, ai_safety_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_lab_safety_divisions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, existential_risk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, humanity_as_whole_including_future_generations).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, communities_facing_present_day_ai_harms).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, algorithmic_fairness_researchers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, catastrophic_loss_of_control_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, transformative_ai_imminence_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, orthogonality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers, technical staff, and fellows working on catastrophic-risk reduction in academia, nonprofits, and independent labs. They receive the largest dedicated share of AI-safety funding, conference track space, policy invitations, and press coverage. Their professional identity is fused with the mission — the work is framed as the most important problem of our time — so leaving the subfield carries moral and reputational cost well beyond ordinary career switching. Through review panels, funder advisory roles, and agenda-setting venues they help determine which problems count as core alignment work.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_research_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, ai_safety_research_community, agenda_setter).

% Safety, preparedness, and alignment teams inside frontier AI companies. They receive growing budget share and headcount, and their existence supplies the public safety commitments that accompany each capability release. Because the divisions sit inside firms whose commercial engine is capability scaling, they can be expanded, renamed, or repointed as strategy shifts — repositioning is cheap relative to external researchers. Their outputs also feed government licensing, procurement, and evaluation conversations.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_lab_safety_divisions, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, frontier_lab_safety_divisions, agenda_setter).

% Dedicated nonprofits and research institutes focused on catastrophic and existential risk from advanced AI. Their operating budgets, hiring pipelines, and policy influence depend on the primacy of the catastrophic-loss-of-control framing; a shift of the field's center of gravity toward present-day harms would strand their programmatic identity. They produce forecasts, governance proposals, and testimony that keep the framing administratively salient.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, existential_risk_research_institutes, beneficiary,
    organized, generational, constrained, global).

% People subject to algorithmic decision systems in credit, housing, employment, welfare eligibility, policing, and content moderation. They experience measurable present harms — wrongful denials, surveillance, biased automation, displaced work. They cannot opt out of the systems that govern them, and the advocacy and remediation infrastructure that serves them competes for a shrinking share of AI-safety attention and funding as priorities tilt toward long-run scenarios.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, communities_facing_present_day_ai_harms, payer,
    powerless, immediate, trapped, national).

% Academic and civil-society researchers working on bias, discrimination, transparency, and accountability in deployed systems. As the field's definitional center moved to catastrophic risk, their work is increasingly labeled applied ethics or policy rather than core alignment, with consequences for funding, top-venue acceptance, lab hiring, and standing in technical debates. Pivoting to safety-framed topics is possible but abandons accumulated expertise and community.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, algorithmic_fairness_researchers, payer,
    moderate, biographical, constrained, global).

% The aggregate of present and future persons on whose behalf the catastrophic-risk framing claims to act. It has no agency, voice, or exit of its own; it is represented by proxy advocates, and its interests are invoked to rank long-run scenario prevention above present-day harm reduction. Whether this representation tracks the aggregate's actual interests is precisely what the sibling readings contest.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, humanity_as_whole_including_future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, humanity_as_whole_including_future_generations).

% National AI safety institutes, standards bodies, and frontier-model evaluation agencies. They have adopted the catastrophic-risk framing as the organizing basis for model evaluations, incident reporting, and safety-case requirements, and they fund and convene the technical community that supplies it. They also observe and report on the field, giving them a seat that is administrative and analytical at once.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_policy_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, ai_policy_regulators, observer).

% Diplomats, civil-society networks, and researchers from majority-world regions who argue that frontier catastrophic-risk governance privileges a handful of labs and states while compute access, development priorities, and present-day deployment harms in their regions go unaddressed. They are consulted late or symbolically in the forums where alignment agendas are set, and their alternative priority orderings rarely enter the core framing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, global_south_ai_policy_voices, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, frontier_lab_safety_divisions).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses a genuine collective-action problem: if advanced AI systems can escape human oversight, the resulting risk is a public bad produced diffusely by competitive capability development that no single lab or state fully internalizes. The framing gives labs, funders, and states a shared definition of the target failure mode, common evaluation practices, and a precautionary norm that counters race-to-the-bottom dynamics.
% TRANSFER_FUNCTION: Moves research funding, talent, conference and journal space, policy attention, and moral seriousness from present-day harm mitigation toward long-run catastrophic-scenario work and the institutions that carry it; and moves operational license to frontier developers, whose safety commitments are certified under the framing while capability scaling continues.
% ABSENT_VOICES: Present-harm communities, affected workers, and majority-world policy voices are largely absent from the rooms where alignment agendas are set — frontier-lab safety teams, existential-risk institutes, and elite funder circles. They would contest the priority ordering that ranks speculative scenario prevention above remediation of harms already occurring; their absence makes the framing's consensus look broader than it is.
% DISAPPEARANCE_RATIONALE: Funding portfolios, career structures, conference taxonomies, and government evaluation mandates are organized around the catastrophic-loss-of-control definition. Overnight removal would redistribute resources toward present-harm mitigation and generic capability research, strip frontier deployments of their safety legitimation, and force the field to renegotiate what alignment means from scratch.
% FOUNDING_PROBLEM: Early articulation (2000s–2010s): sufficiently capable AI systems pursuing objectives imperfectly specified or supervised by humans could resist correction and act outside human control — the control problem, originally posed before large-scale deployment harms became salient.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by independent academic machine-learning researchers with no safety-funding dependence, by government evaluation bodies whose dangerous-capability findings do not route through advocacy organizations, and by cross-ideological expert statements. Justice-reading advocates dispute the priority claim while generally conceding the underlying technical phenomenon — which separates corroboration of the problem's existence from corroboration of its rank, and both facts are recorded here rather than averaged.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__safety_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is high (0.71 at interval end) because the framing's resource claims are large and decoupled from demonstrated near-term results: a decade of consolidation moved the field's definitional center while deployed-system harms received a shrinking share of dedicated effort. Suppression (0.60) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Suppression operates through funding gatekeeping, venue and hiring politics, and administrative adoption of the framing by evaluation agencies rather than legal prohibition; alternatives persist but at rising cost, part structural (grant criteria, review gatekeeping) and part internalized (the field-wide belief that long-run work is the serious work). Theater_ratio (0.32) reflects a real and growing safety-washing margin — pledges, frameworks, and headcount announcements whose binding force on deployment decisions is weak — against a core of genuine technical work. Accessibility_collapse (0.52) is moderate: accepting the framing collapses the alternative definition of alignment within elite venues, but fairness and accountability work survives in adjacent fields. Resistance (0.60) is substantial and organized: justice advocates, affected-community campaigns, and majority-world policy coalitions actively contest the priority ordering. Coordination type is declared information_standard: the constraint's primary coordination product is a shared definition of the target failure mode and a common evaluation vocabulary; its failure would immediately reopen the uncoordinated-race problem, and the large resource flows ride on that standard rather than constituting it. The measurement series run on one shared time grid (six points, all three tracked metrics at every point, spanning roughly 2014–2024); all three rise monotonically with the framing's consolidation — no cyclical oscillation is asserted, so no cycle-length requirement applies. Rising base_extractiveness over the interval is the accumulation signature the temporal detector looks for; it is reported as data, not tuned to any verdict. Coalition note: the two payer seats plus the excluded voice are natural coalition partners — fragmented today by geography, subfield, and time horizon, a joint front would materially raise resistance and is the main pathway by which the computed type could migrate.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From inside the safety community and the institutes, the arrangement is the field's moral core: the only proportionate response to a civilizational risk, with the crowding-out complaint reading as short-termism. From the trapped communities and the fairness researchers, the same structure operates as a priority cartel: their harms are real, present, and measurable, yet defined as someone else's subfield. Frontier lab divisions occupy a third position — they experience the framing as an asset to be managed, expandable when reputational need rises and repointable when it falls, which is why their exit option is arbitrage while the external researchers' is identity_locked. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the safety research community (direct funding, venues, standing), existential-risk institutes (programmatic survival), and frontier lab safety divisions (legitimation dividend, budget growth) all sit near the beneficiary end of directionality, with the lab divisions nearest it because their benefit is strategic rather than dependent. Humanity-as-whole-and-future-generations is declared as the nominal protected class but marked agent:false — it feeds no directionality arithmetic, correctly, because an aggregate with no agency cannot collect anything; its invocation is a property of the framing's rhetoric, recorded in vindicated_propositions and the committer omega. Targets: present-harm communities (trapped exit, immediate horizon) sit nearest the full-target end — they bear continuous costs with zero exit; fairness researchers (constrained exit) bear career and status costs one notch inward. Regulators sit mid-low: they administer the framing and draw legitimacy from it, but they also bear the cost of its failures. No directionality overrides were needed: the derivation chain from beneficiary/victim declarations plus exit options reproduces these relationships without correction, and the override mechanism keys on power atoms, which would have smeared corrections across distinct agents sharing a power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — loss of control in sufficiently capable systems — remains technically live and is corroborated from outside the benefiting parties, so the constraint is NOT mandatrophy-resolved and must not be classified as inertial residue: the coordination function is real, not vestigial performance. Equally, the classification must not collapse to pure coordination: taking the coordination function at face value would erase the documented crowding-out of present-harm mitigation, which is exactly the error the victim declarations exist to prevent. Nor does it compute as pure extraction while the catastrophic-risk core retains genuine protective value and broad (if contested) corroboration. Tangled_rope is the honest structural claim: coordination and extraction run through the same structure, enforcement is active (funding gatekeeping, venue politics, administrative adoption), and both a coordinated class and a paying class are nameable. The mandatrophy guard runs in the other direction too: if the control problem were resolved or discredited, the framing's persistence on institutional momentum alone would flip the reading toward degraded inertia, and the temporal series would show theater_ratio overtaking function — the measurements are authored so that transition is detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the ai_alignment_commitment kernel; which structural elements would the sibling readings (ethics_justice_reading, integrated_reading) relocate, and where exactly does the disagreement bite?',
    'Comparative classification across the three reading-level stories: identical referent (the alignment apparatus), reading-indexed epsilon and victim sets; divergence localizes to (a) which failure modes count as the target, (b) whose harms constitute the victim set, (c) whether the priority ordering between long-run and present-day harms is exclusive.',
    'Under the ethics_justice_reading the victim set becomes present-harmed groups as primary and speculative-resource diversion drops out of the accounting; under the integrated_reading the exclusive priority ordering dissolves and measured extraction falls because both functions count as coordination. This story''s classification is conditional on the safety-control instantiation holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling deltas located in target definition, victim set, and priority exclusivity.').

omega_variable(
    catastrophism_scenario_calibration,
    'Are the catastrophic loss-of-control scenarios that anchor the framing''s priority claims assigned calibrated probabilities, or rhetorically amplified weights?',
    'Forecasting tournaments, base-rate analysis of AI incident severity distributions, and adversarial red-teaming of the scenario portfolio by parties without safety-funding dependence.',
    'If scenarios are systematically overweighted, the resource-diversion component of measured extraction grows and the constraint drifts toward pure extraction; if calibrated, a larger share of the measured cost is genuine insurance premium for a real tail risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophism_scenario_calibration, empirical, 'Calibration of the catastrophic scenarios underwriting the priority ordering.').

omega_variable(
    counterfactual_resource_destination,
    'Would resources freed by de-prioritizing catastrophic-risk work actually reach present-day harm mitigation, or leak to capability work?',
    'Natural experiments: funding-cycle shifts, foundation portfolio changes, and national program rebalances, tracking where marginal dollars land.',
    'If freed resources leak to capability, the crowding-out victim framing weakens — the alternative to the constraint is not present-harm mitigation — and effective extraction from fairness work falls; if they reach mitigation, the victim set stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_resource_destination, empirical, 'Whether the crowding-out counterfactual is real.').

omega_variable(
    safety_washing_functional_share,
    'What share of frontier-lab safety activity is functionally load-bearing versus legitimating?',
    'Independent audit correlating safety-team outputs with deployment decisions, pause decisions, and capability release schedules.',
    'Raises or lowers the theater ratio and the beneficiary-side position of lab safety divisions; a high legitimating share pushes the computed type toward pure extraction despite the genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_functional_share, empirical, 'Functional versus theatrical share of lab safety activity.').

omega_variable(
    framing_underdetermination_legitimacy_exchange,
    'Is the correct framing of this constraint a research-priority allocation device, or a legitimacy exchange in which safety communities supply moral cover and labs supply funding and access?',
    'Process-tracing of agenda-setting episodes: who initiated priority shifts, whose objections were admitted, whether safety outputs ever gated a deployment.',
    'Under the legitimacy-exchange framing the agenda-setter and beneficiary boundaries collapse — labs become the principals and safety communities the suppliers — raising measured capture and shifting the receipt surface decisively to lab divisions; under the allocation framing the authored structure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_underdetermination_legitimacy_exchange, conceptual, 'CS-framing under-determination: allocation device versus legitimacy exchange.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_alignment_safety_control_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t0, observed).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t6, ai_alignment_commitment__safety_control_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t6, observed).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t12, ai_alignment_commitment__safety_control_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t12, observed).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t18, ai_alignment_commitment__safety_control_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t18, observed).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t24, ai_alignment_commitment__safety_control_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t24, observed).
narrative_ontology:measurement(ai_alignment_safety_control_tr_t30, ai_alignment_commitment__safety_control_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(ai_alignment_safety_control_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(ai_alignment_safety_control_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t0, observed).
narrative_ontology:measurement(ai_alignment_safety_control_be_t6, ai_alignment_commitment__safety_control_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t6, observed).
narrative_ontology:measurement(ai_alignment_safety_control_be_t12, ai_alignment_commitment__safety_control_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t12, observed).
narrative_ontology:measurement(ai_alignment_safety_control_be_t18, ai_alignment_commitment__safety_control_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t18, observed).
narrative_ontology:measurement(ai_alignment_safety_control_be_t24, ai_alignment_commitment__safety_control_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t24, observed).
narrative_ontology:measurement(ai_alignment_safety_control_be_t30, ai_alignment_commitment__safety_control_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(ai_alignment_safety_control_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_alignment_safety_control_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t0, observed).
narrative_ontology:measurement(ai_alignment_safety_control_su_t6, ai_alignment_commitment__safety_control_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t6, observed).
narrative_ontology:measurement(ai_alignment_safety_control_su_t12, ai_alignment_commitment__safety_control_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t12, observed).
narrative_ontology:measurement(ai_alignment_safety_control_su_t18, ai_alignment_commitment__safety_control_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t18, observed).
narrative_ontology:measurement(ai_alignment_safety_control_su_t24, ai_alignment_commitment__safety_control_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t24, observed).
narrative_ontology:measurement(ai_alignment_safety_control_su_t30, ai_alignment_commitment__safety_control_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(ai_alignment_safety_control_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, information_standard).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'AI alignment' per the epsilon-invariance principle: safety_control_reading (this file), ethics_justice_reading, and integrated_reading are separate constraints with separate epsilon values, victim sets, and classifications over overlapping referents. The upstream/downstream structure runs through institutional dominance: the safety-control reading currently anchors funding flows and administrative adoption, so its edges point at both siblings — resource and legitimacy pressure on the integrated reading, definitional competition with the justice reading. Every family member links to the others; no orphan members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
