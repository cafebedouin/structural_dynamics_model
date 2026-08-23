% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential-Risk-First AI Governance Priority (Loss-of-Control Reading)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   This story instantiates the existential_risk_reading of the
 *   ai_risk_governance_priority kernel: the rule that AI risk governance must
 *   rank prevention of catastrophic loss-of-control and
 *   permanent-potential-curtailment scenarios above all other AI governance
 *   concerns. Under the rule, research funding, regulatory design, and
 *   philanthropic capital flow to alignment research, adversarial evaluation
 *   of frontier models, and governance frameworks built around AGI scenarios,
 *   while demonstrated present harms (biased deployment, surveillance, labor
 *   displacement) are explicitly subordinated. KEY AGENTS (by structural
 *   relationship): frontier_ai_labs — agenda-setter and principal recipient
 *   (institutional/arbitrage) — drafts the governance frame and collects its
 *   concentration effects; xrisk_research_institutes — core beneficiary
 *   (organized/identity_locked) — supplies the frame's intellectual
 *   personnel; agi_governance_consultancies — secondary beneficiary
 *   (organized/mobile); present_harm_affected_populations — principal payer
 *   (powerless/trapped); fairness_and_bias_researchers and
 *   small_ai_builders_and_open_source_devs — payers (moderate/constrained);
 *   civil_society_near_term_advocates — excluded and payer
 *   (organized/constrained); national_ai_regulators — institutional co-setter
 *   with partial benefit; future_humanity — non-agent claimant seat anchoring
 *   the moral structure; interdisciplinary_ai_ethicists — analytical
 *   observer. Per Rule 1, the sibling readings are separate constraint files
 *   (linked via network.affects_constraints); this file hedges nothing across
 *   readings and carries one fixed epsilon for its own constraint. The claim
 *   and the metrics are independent authored facts: claimed_type states the
 *   structural belief (a genuine precautionary coordination function fused
 *   with asymmetric transfer under active enforcement); the metrics describe
 *   observed operation without being tuned to any predicted engine verdict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.66).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.58).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential-Risk-First AI Governance Priority (Loss-of-Control Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '3bf14c42-241c-4a8b-bb67-b617f070b39a').
narrative_ontology:cs_kernel_codification('3bf14c42-241c-4a8b-bb67-b617f070b39a', distributed).
narrative_ontology:cs_authority_grounding('3bf14c42-241c-4a8b-bb67-b617f070b39a', expertise).
narrative_ontology:cs_interpretation_layer_present('3bf14c42-241c-4a8b-bb67-b617f070b39a').
narrative_ontology:cs_reading_relation('3bf14c42-241c-4a8b-bb67-b617f070b39a', ai_risk_governance_priority__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('3bf14c42-241c-4a8b-bb67-b617f070b39a', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('3bf14c42-241c-4a8b-bb67-b617f070b39a', foundational, loss_of_control_prevention_lexically_prior).
narrative_ontology:cs_axiom_status(loss_of_control_prevention_lexically_prior, holdable).
narrative_ontology:cs_axiom_grounding('3bf14c42-241c-4a8b-bb67-b617f070b39a', loss_of_control_prevention_lexically_prior, empirically_contingent).
narrative_ontology:cs_axiom('3bf14c42-241c-4a8b-bb67-b617f070b39a', foundational, potential_curtailment_outweighs_present_harm_aggregate).
narrative_ontology:cs_axiom_status(potential_curtailment_outweighs_present_harm_aggregate, holdable).
narrative_ontology:cs_axiom_grounding('3bf14c42-241c-4a8b-bb67-b617f070b39a', potential_curtailment_outweighs_present_harm_aggregate, deontological).
narrative_ontology:cs_reference_frame('3bf14c42-241c-4a8b-bb67-b617f070b39a', xrisk_precaution_lexical_supremacy).
narrative_ontology:cs_drift_state('3bf14c42-241c-4a8b-bb67-b617f070b39a', post_frontier_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3bf14c42-241c-4a8b-bb67-b617f070b39a', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, agi_governance_consultancies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, fairness_and_bias_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, small_ai_builders_and_open_source_devs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, national_ai_regulators).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, civil_society_near_term_advocates).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, catastrophic_loss_of_control_premise).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, longtermist_moral_weighting).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, differential_technological_development_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the largest frontier models and set the terms of AI risk debate: publish safety frameworks, staff policy teams, host governance summits, and propose licensing and evaluation regimes calibrated to frontier capability thresholds. Capital, talent, and regulatory attention concentrate around their announced milestones. Compliance architectures they can afford to build function as entry barriers to smaller competitors, and the safety-leader designation secures continued license to scale. Exiting the frame would mean conceding the governance narrative to rival labs or to regulators.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Run research programs, fellowships, and forecasting projects devoted to alignment and catastrophic-risk scenarios; receive the largest dedicated philanthropic and grant streams in AI safety; supply personnel to lab safety teams and government advisory bodies. Their professional standing, publication venues, and career pipelines are constituted by the catastrophic-risk frame. Leaving the frame means forfeiting the community, credibility, and funding that define their work.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes, beneficiary,
    organized, generational, identity_locked, global).

% Build scenario exercises, preparedness frameworks, evaluation protocols, and institutional design documents for governments and firms preparing for advanced-AI contingencies. Revenue follows framework complexity and is paid whether or not the scenarios materialize. Exit is easy: the skill set ports directly to generic enterprise risk consulting.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, agi_governance_consultancies, beneficiary,
    organized, biographical, mobile, global).

% Experience the deployed-system harms the priority ordering ranks below speculative scenarios: discriminatory credit, hiring, and housing algorithms; opaque content moderation; surveillance tooling; automated displacement of livelihoods. Mitigation of these harms competes for the same governance bandwidth and funding, and consistently loses. They cannot opt out of systems embedded in essential services; their recourse runs through complaint channels designed by the system operators.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% Produce audit methodologies, bias benchmarks, and deployment-harm analyses. Grant calls, headline conference tracks, and lab headcount have shifted toward alignment and frontier-evaluation work, and their subfield contracts relative to the catastrophic-risk portfolio. Retraining toward safety topics abandons accumulated expertise and the populations their work served; staying means competing for a shrinking share of attention, venues, and funding.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, fairness_and_bias_researchers, payer,
    moderate, biographical, constrained, global).

% Ship models and applications below frontier scale. Compute-threshold licensing, mandatory evaluations, and incident-reporting regimes impose fixed costs they cannot amortize, while the hazard categories those regimes target are defined by frontier-scale phenomena. Open-weight release faces restriction proposals authored in the catastrophic-risk frame. Exit means leaving the sector or relocating development abroad; neither preserves their current access to users and markets.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, small_ai_builders_and_open_source_devs, payer,
    moderate, immediate, constrained, global).

% Organize around algorithmic accountability, worker protections, and community data rights. They testify in public consultations but are absent from the closed-door frontier fora and invitation-only summits where governance agendas are drafted. Their issues are acknowledged rhetorically and funded marginally, and organizational survival depends on grant streams that the priority ordering redirects.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, civil_society_near_term_advocates, excluded,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, civil_society_near_term_advocates, payer).

% Codify whichever priority frame reaches them into legislation and standards: safety institutes, evaluation mandates, incident reporting. They gain mandate, budget, and headcount from catastrophic-risk framing, and bear political exposure both if framed scenarios fail to materialize and if present harms escalate unaddressed. Once frameworks are legislated, abandonment is blocked by statute, treaty commitments, and inter-agency dependencies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, national_ai_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, national_ai_regulators, beneficiary).

% Non-agent seat: the class whose potential annihilation or permanent curtailment anchors the priority claim. Whether they are protected or merely invoked depends on unresolved probability questions about loss-of-control outcomes; they collect nothing observable from the arrangement and bear nothing measurable of its costs. Seated for completeness of the moral structure; excluded from derivation arithmetic.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% Analyze the governance field from outside its funding streams: trace how priority-setting distributes attention, compare stated rationales against budget lines and conference programs, and publish comparative assessments across the competing framings. Hold no stake in the allocation they study.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, interdisciplinary_ai_ethicists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates anticipatory action against a low-frequency, high-severity hazard that markets underprice: pooled funding for alignment research, shared capability-threshold information, third-party evaluation infrastructure, and a common vocabulary that lets governments regulate a technology whose failure mode is hypothesized rather than observed.
% TRANSFER_FUNCTION: Moves research funding, philanthropic capital, talent, regulatory bandwidth, and moral authority toward x-risk institutes, frontier-lab safety programs, and AGI-scenario consultancies; correspondingly moves priority, and with it mitigation resources, away from demonstrated present harms and the populations bearing them.
% ABSENT_VOICES: Populations currently harmed by deployed systems hold no seat in the frontier fora where priorities are drafted; near-term-harms researchers are present in public debate but absent from closed-door governance design; global-majority communities affected by data extraction and moderation decisions are represented only through intermediaries, if at all.
% DISAPPEARANCE_RATIONALE: If the priority rule vanished overnight, dedicated alignment funding streams would compress into general AI-safety and ethics portfolios, frontier labs would lose the safety-leadership frame (and would immediately begin rebuilding some successor framing), near-term harm mitigation would gain relative share of governance bandwidth, and governments would lean harder on existing product-safety, anti-discrimination, and competition instruments.
% FOUNDING_PROBLEM: Early-2020s recognition that capability scaling might produce goal-directed systems beyond reliable human oversight before adequate control methods exist — the loss-of-control problem statement crystallized by successive capability demonstrations between roughly 2019 and 2023.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: several national AI safety institutes and government advisory bodies treat loss-of-control as a live planning scenario; a subset of tenured machine-learning academics publishes on catastrophic-risk plausibility without drawing on alignment funding; former lab safety personnel have testified to internal concern. Disputed from outside by AI-fairness, labor-economics, and civil-liberties scholars who locate the live problem in deployed systems. Corroboration is real but not unanimous — the attesting parties share no funding dependence on the alignment portfolio, yet the dispute over the problem's priority remains open.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) reflects large-scale redirection of research funding, regulatory bandwidth, and philanthropic capital toward speculative-capability scenarios, with the opportunity cost concentrated on populations experiencing present harms; the transfer is decoupled from demonstrated-harm reduction. Suppression (0.58) is authored as a raw structural property and is deliberately not scaled by power or scope: enforcement is primarily epistemic and institutional (grant gatekeeping, venue norms, seriousness signaling, hiring filters) rather than statutory coercion. Theater ratio (0.46) is elevated but not dominant: scenario documents, preparedness frameworks, and evaluation rituals increasingly circulate as intra-elite signal, while adversarial testing and interpretability work retain a functional core. Accessibility collapse (0.42): the near-term-first alternative remains fully articulable and institutionally alive, so alternatives are pressured but far from collapsed. Resistance (0.60): a sustained counter-movement of fairness researchers, labor organizers, and civil-society advocates contests the frame continuously. All three tracked metrics run on one shared time grid (t=0..12, step 2) so every metric is authored at every examined point. The trajectories are ratchets rather than cycles: each frontier capability demonstration permanently raises the precaution baseline (rectified escalation), so no oscillation phase is claimed; a cyclical scare-funding reading is carried instead in the institutional_identity_persistence and loss_of_control_probability omegas. The suppression_requirement series is authored because the story specifically traces enforcement intensification: the epistemic gatekeeping apparatus matured and hardened as the stakes framing escalated.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the frontier-lab seat, the arrangement presents as a mature industry managing its own externalities: the same rule that consumes public bandwidth also secures the lab's license to scale and raises rivals' compliance costs. From the x-risk institute seat it presents as underfunded existential stewardship — what payers experience as diversion is, from inside, the minimum viable precaution. Present-harm populations and fairness researchers experience governance capacity being consumed by scenarios in which they appear only as abstractions, while their concrete, documented injuries wait in queue. Regulators experience mandate expansion rather than cost. Identity lock differentiates the institutes from the consultancies despite nominally identical beneficiary positions: institutes cannot exit without dissolving the professional selves the frame built, whereas consultancies port their skills out trivially. The engine computes per-seat classifications from the structural data; nothing in this commentary adjudicates which seat's experience is authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive derivation. frontier_ai_labs (agenda_setter plus beneficiary, arbitrage-grade exit) derives near the beneficiary pole; a story-level override lifts the institutional power atom to d=0.22 because labs bear self-imposed compliance costs and political exposure and regulators carry statutory lock-in — their net positions sit modestly off the pure-beneficiary end in ways the beneficiary-plus-exit derivation alone understates. xrisk_research_institutes (beneficiary, identity_locked) sit nearest the subsidy end. agi_governance_consultancies (mobile, arbitrage-adjacent) sit closest to full beneficiary. present_harm_affected_populations (trapped, powerless payers) sit near full target; fairness_and_bias_researchers and small_ai_builders_and_open_source_devs (constrained payers) sit high-target; civil_society_near_term_advocates (excluded plus payer) sit upper-middle — they bear deprioritization costs yet also harvest platform and funding from the controversy itself. national_ai_regulators derive low-moderate. future_humanity is a non-agent claimant seat: its directionality is indeterminate pending resolution of the loss-of-control probability, so it feeds no arithmetic — the dependency is routed through the omegas. Receipt surface: the constraint's gains demonstrably accrue to frontier_ai_labs (compliance-moat effects, safety-legitimized scaling, capital and talent concentration), so gain_flow names that seat rather than asserting diffuseness; fixing is prohibitive because the frame's defenders control the definitional levers, the epistemic infrastructure is sunk in careers and venues, and no single actor internalizes enough of the rebalancing benefit to pay the removal cost.
 *
 * MANDATROPHY ANALYSIS:
 *   A pure-snare reading (the near-term sibling's natural verdict) would erase the constraint's real coordination face: capability-threshold information sharing, third-party evaluation infrastructure, and disclosure norms address a collective-action problem that markets demonstrably underprice, whatever the probability dispute resolves to. A pure-rope reading (the insider's natural verdict) would erase the asymmetric face: the same architecture transfers careers, capital, and moral authority toward the frame-defining institutions while deprioritized populations wait. Tangled rope keeps both faces load-bearing and relocates the live question to enforcement: what sustains the asymmetry (funding gatekeeping, definitional control, identity lock) versus what sustains the coordination (hazard monitoring, eval infrastructure). On genealogy: founding_problem_status is contested, not dead — the founding problem retains corroborators outside the benefiting parties, so no zombie flag fires prematurely; if the probability omega resolves low while the institutes persist on identity reproduction, mandatrophy_resolved becomes the correct subsequent declaration and the theater trajectory should steepen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ascendancy,
    'Which reading of the ai_risk_governance_priority kernel ends up governing actual institutional behavior — this existential-risk reading, the near-term-harms reading, or the bridge reading?',
    'Track legislative text, funding line items, and summit agendas against each reading''s predicted signature: speculative-capability spend share versus deployed-harm spend share, and whether governance instruments assume a forced ranking or refuse one.',
    'If the bridge reading ascends, this constraint''s forced-choice structure dissolves into shared-cost unified frameworks and its extraction falls; if the near-term-harms reading ascends, this constraint demotes to a minority position and its beneficiary seats lose the gatekeeping function that sustains them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ascendancy, conceptual, 'Committer structure: this story is one reading of a contested kernel; which sibling governs practice changes the constraint''s victim register and epsilon distribution.').

omega_variable(
    loss_of_control_probability,
    'What is the actual probability that unrestrained capability scaling produces loss-of-control or permanent-curtailment outcomes, as estimable by parties outside the alignment-funded ecosystem?',
    'Independent forecasting tournaments with resolved scoring, capability-trajectory extrapolation audits, and pre-registered warning indicators negotiated across disagreeing research schools.',
    'A high resolved probability strengthens the coordination face and drives effective extraction down toward the price of genuine insurance; a low resolved probability leaves the resource transfer standing on identity and market structure alone, pushing the payer-seat classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_of_control_probability, empirical, 'The load-bearing empirical dispute underneath the priority claim.').

omega_variable(
    safety_framework_market_barrier,
    'Do the governance architectures proposed under this priority — compute thresholds, licensing, mandatory evaluations — function primarily as hazard containment or as market-entry barriers concentrating the industry?',
    'Compare compliance cost curves for frontier incumbents against new entrants; test whether threshold placements track published hazard evidence or incumbent capability levels; observe which firms draft the regimes.',
    'Barrier-dominant findings raise the frontier labs'' capture share and harden the arrangement toward extraction at the agenda-setter seat; containment-dominant findings validate a portion of the transfer as genuine insurance cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_framework_market_barrier, empirical, 'Whether the safety-led governance design is containment or moat.').

omega_variable(
    future_present_moral_weighting,
    'Can the moral weighting that ranks potential curtailment of humanity''s future above aggregate present harm survive scrutiny as a governance principle, or does it function as authorization for deferring present obligations indefinitely?',
    'Structured comparison with precedent cases where future-generation claims were weighed against present harm; deliberative exercises that include the populations currently bearing deferred costs.',
    'If the weighting fails scrutiny, the priority rule loses its normative warrant and the measured transfer stands exposed as unmotained redistribution; if it survives, part of the measured extraction reflects the legitimate price of long-horizon stewardship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_present_moral_weighting, conceptual, 'Conceptual dispute over the moral weights the priority rule presumes.').

omega_variable(
    institutional_identity_persistence,
    'Does the x-risk research sector''s growth track updated hazard assessment, or does it reproduce itself through professional identity, fellowship pipelines, and venue loyalty irrespective of the evidence?',
    'Compare sector headcount and funding elasticity against revisions in published probability estimates; track whether downward updates shrink programs or leave them intact.',
    'Identity-driven persistence marks the coordination face as performative maintenance and pushes the theater trajectory upward; evidence-elastic persistence supports the genuine-coordination component of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_persistence, empirical, 'Mandatrophy probe: whether persistence tracks the hazard or the profession.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the crowding-out of present-harms research enforced externally (grant gatekeeping, editorial and venue filtering) or internalized (researchers pre-conforming to seriousness norms without external force)?',
    'Fund deployed-harms AI research outside the gated channels and measure whether the subfield recovers; survey researchers'' stated preferences against their actual submission and grant-seeking behavior.',
    'Internalized dominance means suppression persists after the gates are removed, so effective suppression exceeds the structural measure; purely external gatekeeping means removing the gates restores the balance quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Mechanism split underlying the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xr_first_priority_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(xr_first_priority_tr_t2, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement(xr_first_priority_tr_t4, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(xr_first_priority_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(xr_first_priority_tr_t8, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(xr_first_priority_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(xr_first_priority_tr_t12, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 12, 0.46).

% Extraction over time
narrative_ontology:measurement(xr_first_priority_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(xr_first_priority_be_t2, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2, 0.34).
narrative_ontology:measurement(xr_first_priority_be_t4, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(xr_first_priority_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(xr_first_priority_be_t8, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(xr_first_priority_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(xr_first_priority_be_t12, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 12, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(xr_first_priority_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement(xr_first_priority_su_t2, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2, 0.29).
narrative_ontology:measurement(xr_first_priority_su_t4, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(xr_first_priority_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(xr_first_priority_su_t8, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(xr_first_priority_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(xr_first_priority_su_t12, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'AI risk governance priorities' covers three structurally distinct claims — this existential-risk-first rule, the near-term-harms-first rule, and the bridge/unified-framework rule. Each carries its own epsilon, its own victim register, and its own resource-flow signature; forcing one story to span all three would average unstable observables. Capability-scaling demonstrations upstream feed this reading's urgency claims; this reading's resource dominance downstream pressures the near-term reading's operating environment. Family members link mutually through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__existential_risk_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
