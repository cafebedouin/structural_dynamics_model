% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: AI Safety Control Alignment Commitment (Catastrophic Risk Reading)
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   The AI alignment commitment is a contested kernel. This constraint
 *   represents ONE reading: alignment as preventing catastrophic loss of
 *   control over superintelligent AI systems. The reading frames existential
 *   risk as the primary moral and resource-allocation imperative. It
 *   prioritizes speculative future harms (extinction, loss of human agency in
 *   a superintelligent-dominated world) over present-day observable harms
 *   (algorithmic bias, surveillance, labor displacement). Under this reading,
 *   the victim set is 'humanity including future generations,' and
 *   beneficiaries are the institutions that define and research
 *   catastrophic-risk mitigation. This reading EXTRACTS resources from
 *   justice-focused research and from communities experiencing current harms
 *   by reframing their concerns as lower priority than extinction prevention.
 *   The claim/metric gap is intentional: the reading is CLAIMED as
 *   tangled_rope (genuine coordination for safety plus asymmetric extraction
 *   from justice work) while the metrics describe substantial active
 *   extraction and suppression. Sibling readings (ethics_justice_reading,
 *   integrated_reading) contest what alignment means; this story instantiates
 *   one specific reading with one specific ε.
 *
 * KEY AGENTS:
 *   - Safety research institutions (Anthropic, DeepMind, OpenAI safety teams, academia): set agendas, define alignment, receive funding concentrated on catastrophic-risk research.
 *   - Control-focused governance bodies: adopt safety-control framing to justify capability restrictions and safety certification requirements.
 *   - Capability-restraint advocates: benefit from catastrophic-risk framing as justification for policy agenda of slowed capability development.
 *   - Present-day harm mitigation resources: research funding, personnel, institutional attention directed toward fairness/bias/accountability work — these are the payers under this reading.
 *   - Affected communities from current AI systems (discriminated against in hiring/lending, wrongly arrested via facial recognition, displaced by automation): powerless, trapped, excluded from alignment-definition conversations; their concrete suffering is bracketed as lower priority.
 *   - Justice-focused research communities: AI fairness, algorithm auditing, AI ethics researchers whose work is deprioritized as subordinate to catastrophic-risk research.
 *   - Global South AI communities and governance voices: excluded from defining alignment; experience AI harms (data extraction, surveillance, labor displacement, economic dependency) differently; cannot shape the safety-control framing imposed globally.
 *   - Future generations (non-agent beneficiary): invoked by the reading as the primary moral constituency but cannot speak for themselves; the reading claims authority to represent their interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Safety Control Alignment Commitment (Catastrophic Risk Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '20de72d8-61d7-4b79-840d-540deb4faab3').
narrative_ontology:cs_kernel_codification('20de72d8-61d7-4b79-840d-540deb4faab3', distributed).
narrative_ontology:cs_authority_grounding('20de72d8-61d7-4b79-840d-540deb4faab3', expertise).
narrative_ontology:cs_interpretation_layer_present('20de72d8-61d7-4b79-840d-540deb4faab3').
narrative_ontology:cs_reading_relation('20de72d8-61d7-4b79-840d-540deb4faab3', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('20de72d8-61d7-4b79-840d-540deb4faab3', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('20de72d8-61d7-4b79-840d-540deb4faab3', foundational, catastrophic_control_loss_primary_harm).
narrative_ontology:cs_axiom_status(catastrophic_control_loss_primary_harm, holdable).
narrative_ontology:cs_axiom_grounding('20de72d8-61d7-4b79-840d-540deb4faab3', catastrophic_control_loss_primary_harm, deontological).
narrative_ontology:cs_axiom('20de72d8-61d7-4b79-840d-540deb4faab3', secondary, superintelligence_timeline_imminent).
narrative_ontology:cs_axiom_status(superintelligence_timeline_imminent, holdable).
narrative_ontology:cs_axiom_grounding('20de72d8-61d7-4b79-840d-540deb4faab3', superintelligence_timeline_imminent, empirically_contingent).
narrative_ontology:cs_reference_frame('20de72d8-61d7-4b79-840d-540deb4faab3', technical_control_framework).
narrative_ontology:cs_drift_state('20de72d8-61d7-4b79-840d-540deb4faab3', contemporary_ai_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20de72d8-61d7-4b79-840d-540deb4faab3', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, safety_research_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, control_focused_governance_bodies).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, capability_restraint_advocates).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_day_harm_mitigation_resources).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, affected_communities_from_current_ai_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, justice_focused_research_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leading AI safety labs (Anthropic, DeepMind, OpenAI safety teams, academic institutions) set research agendas prioritizing catastrophic risk scenarios: loss of control over superintelligent systems, misalignment with human values, instrumental convergence leading to extinction-level outcomes. They receive significant funding for this research direction, shape which problems are considered tractable, and define what alignment means operationally. Their framing treats present-day harms as lower-priority relative to speculative future catastrophic scenarios.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, safety_research_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Regulatory bodies and policy organizations adopt the catastrophic-control-loss framing to set safety standards, evaluation criteria, and deployment restrictions. This framing provides justification for slowing capability development and requiring certification of control mechanisms, which aligns with precaution and technical governance. The beneficiary position derives from the framing's legitimacy as 'serious science' rather than direct resource capture.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, control_focused_governance_bodies, beneficiary,
    institutional, generational, constrained, global).

% Organizations and individuals advocating for slowed AI development, compute restrictions, and international governance frameworks benefit from the catastrophic-risk framing because it provides an existential imperative for their policy agenda. The framing makes restraint arguments politically viable and morally urgent. They do not run the research but shape which research directions receive institutional support.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, capability_restraint_advocates, beneficiary,
    organized, generational, mobile, global).

% Funding, research time, and institutional attention dedicated to AI fairness, bias mitigation, transparency, and accountability in current-generation systems. These resources compete directly with catastrophic-risk research for grants, personnel, and researcher attention. The safety-control reading subordinates present-day harms as 'tractable but lower priority than existential risk.' Resources flow away from fairness research toward safety research, creating a structural extraction.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_day_harm_mitigation_resources, payer,
    powerful, biographical, constrained, global).

% Communities experiencing concrete, present-day harms from algorithmic bias, surveillance, labor displacement, and automated decision-making (criminal justice, lending, hiring, content moderation). They are not parties to the alignment-definition conversation and have no seat at safety research agendas or governance bodies. The catastrophic-risk framing brackets their actual suffering as 'lower-order compared to extinction scenarios,' rendering their interests absent from prioritization.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, affected_communities_from_current_ai_systems, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, affected_communities_from_current_ai_systems, excluded).

% AI fairness, algorithm auditing, AI ethics, and social impact research communities. They work on problems that affect living people: discriminatory loan decisions, facial recognition false positives, content moderation errors. Under the safety-control reading's dominance, their research is reframed as 'alignment subproblems' subordinate to control problems, or as separate 'ethics work' not core to alignment. Funding and institutional prestige flow toward catastrophic-risk research; their work must justify itself against the existential-risk standard.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, justice_focused_research_communities, payer,
    moderate, biographical, constrained, global).

% A non-agent beneficiary category: the safety-control reading invokes humanity's future as the primary victim set ('we must align AI to prevent extinction'). Future people cannot advocate for themselves; the reading speaks on their behalf. This abstraction legitimizes present resource diversion while concentrating decision-making authority in the institutions claiming to represent future interests.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generations).

% Frontier labs (Anthropic, OpenAI, DeepMind, Meta AI) developing cutting-edge capability. They are nominally subject to safety constraints but also capture significant portions of the safety research community (employment, collaboration, compute access). They are excluded from legitimate governance voice under the reading because any capability-developer input is treated as self-interested. This creates a structural dynamic where the constraint is enforced by those whose incentives align with capability restraint, not by independent safety verification.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_capability_labs, excluded,
    powerful, biographical, constrained, global).

% AI researchers, deployment communities, and governance voices from countries outside the Anglophone Western academic-industrial core. Safety standards and alignment definitions set in US/EU institutions are imposed globally. These communities experience concrete AI harms (colonial data extraction, surveillance infrastructure, labor displacement) and have different threat models (political oppression via AI, economic dependency). They are excluded from shaping what 'alignment' means; the catastrophic-risk framing ignores their material interests.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, global_south_ai_communities, excluded,
    powerless, biographical, trapped, global).

% External analytical perspective on the constraint's structure: which problems are named, which are bracketed, which communities hold legitimacy, which are rendered absent. The observer seat tracks the allocation of moral urgency and institutional resources across the competing framing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, analytical_observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, safety_research_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Focuses AI research and governance on identifying and preventing catastrophic failure modes (loss of control, misalignment of superintelligent systems, instrumental convergence toward human-incompatible goals). Solves a genuine coordination problem: frontier labs have incentives to maximize capability and speed; this commitment creates a shared framework for slowing deployment and requiring control verification before scaling.
% TRANSFER_FUNCTION: Moves research funding, institutional authority, and governance attention away from present-day fairness and justice problems toward speculative catastrophic-risk mitigation. Concentrates legitimacy in safety-focused institutions; subordinates fairness research as secondary or 'ethics work.' Extracts resources from communities experiencing current harms by rendering those harms as lower priority than extinction scenarios.
% ABSENT_VOICES: Communities experiencing concrete present-day harms (discriminatory hiring, surveillance, wrongful arrest via biased systems) are absent — they have no seat in safety research agendas or governance definition. Global South researchers and communities are excluded from defining what alignment means. Justice-focused researchers experience their work as deprioritized but lack power to reframe the research agenda. Future generations cannot speak — the reading invokes them as the primary beneficiary but they cannot contest the framing.
% DISAPPEARANCE_RATIONALE: If this constraint (the framing that alignment = catastrophic-control-loss prevention) disappeared, AI research funding and governance attention would rebalance toward present-day harms and justice problems. Capability-focused labs would face fewer institutional restrictions. Global and local AI governance would reassert priority for concrete, observable harms. The moral and material urgency structure of AI development would reorganize.
% FOUNDING_PROBLEM: Advanced AI systems could become superintelligent and lose alignment with human values, leading to outcomes that destroy human civilization or cause extinction. The problem is framed as a technical control problem analogous to engineering safety in other high-consequence domains.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers and some AI labs attest the founding problem is live and critical. Capability labs publicly endorse safety work but privately prioritize capability scaling. Justice researchers, affected communities, and Global South voices dispute that the framing captures the actual harms or the most urgent problems. AI systems are causing observable, documented harms today (discriminatory decisions, labor displacement, surveillance); whether speculative extinction scenarios should receive priority over concrete current harms is the contested core. Empirical analysis shows AI harms are already asymmetrically distributed toward powerless groups, and the safety-control framing does not address this distribution.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) and rising (from 0.48 at t=0) because the safety-control reading directs scarce research and governance resources away from present-day justice problems toward speculative future scenarios. The extraction is asymmetric: safety institutions benefit from legitimacy and funding concentration; justice communities and affected populations pay. Suppression is high (0.71) because the framing suppresses alternative definitions of alignment—justice-focused and integrated readings are treated as less serious science or as secondary 'ethics work,' not core alignment. The suppression mechanism is internalized: justice researchers accept the framing as scientifically legitimate (catastrophic risk IS a real problem) even as their own work is deprioritized. Theater rises over the interval (0.25→0.42) as safety research becomes increasingly theatrical—progress on 'alignment' is measured in published papers and model evals, but the underlying framing (that control loss is the primary harm that matters) remains uncontested and thus invisible to audit. Accessibility collapse is moderate (0.62): alternatives (justice-first framings, integrated approaches) are not completely suppressed but are made costly—if you want funding and institutional legitimacy, you accept the catastrophic-risk framing as primary. Resistance is moderate (0.58): justice researchers and affected communities resist, but without institutional power; their resistance persists but does not dislodge the dominant framing.
 *
 * PERSPECTIVAL GAP:
 *   From the safety-research-institution seat, this is genuine coordination (we slowed deployment to handle a real problem) with some legitimate resource-allocation trade-offs (extinction matters most). From the affected-communities seat, this is extraction dressed as science (my concrete suffering is told it's less important than your speculative scenario). From the justice-researcher seat, it's both: real coordination on a real problem, but one whose definition excludes my own expertise and marginalizes my research. The engine computes per-seat types from the structural data: the agenda-setter seat should compute as a beneficiary seat with low directionality; the payer seats (justice resources, affected communities, Global South) should compute with high directionality (high extraction); the observer seat should compute as analytical. The divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety institutions and governance bodies benefit without bearing costs—they set agendas, receive resources, define what counts as legitimate research. Their directionality is low (d ≈ 0.15–0.25): beneficiaries subsidized by the constraint. Justice researchers and present-day harm mitigation resources are the targets: they pay in deprioritized funding and marginal institutional status. Their directionality is high (d ≈ 0.75–0.85): they extract the burden of the safety-control reading's dominance. Affected communities are trapped payers with zero seat at the table; their d is near 1.0. The reading's invocation of future generations is a high-abstraction beneficiary claim that concentrates authority in present institutions interpreting for absent parties—this is a characteristic pattern of extractive framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (superintelligent systems losing alignment → extinction) is CONTESTED. Safety institutions attest it is live and critical. Justice researchers and affected communities attest that present-day AI harms are live and critical. Global South voices attest that colonialism via AI is live and critical. The safety-control reading does not resolve between these contests; it asserts that its founding problem is the highest priority, thus rendering other problems secondary. This is mandatrophy candidate material: the founding problem exists, but its framing as PRIMARY depends on an uncontested axiom (that extinction risk outweighs present harm) that the framing itself does not defend. The measurement series show theater rising (performative safety research) while extractiveness plateaus (the resource diversion settles into a stable pattern). A piton signature would be all performance with no real coordination; a tangled_rope signature is what we observe: real coordination (slowing dangerous capability) plus real extraction (prioritization that harms justice work). The constraint persists because both the coordination and the extraction are defended simultaneously—you cannot argue against the safety work without seeming to endorse unaligned superintelligence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_loss_speculative_vs_empirical,
    'Is the loss-of-control scenario sufficiently empirically grounded to justify prioritizing it over present-day harms that are already observable and quantifiable?',
    'Empirical prediction: timeline forecasts for superintelligence emergence; track rate of realized misalignment failures in deployed systems relative to predicted catastrophic scenarios. Compare prediction accuracy of control-risk models against actual AI harm prevalence in production systems.',
    'If present-day misalignments (bias, manipulation, automation harms) are far more frequent and harmful than control-loss scenarios, the reading''s resource prioritization becomes harder to defend. If speculative scenarios never materialize while concrete harms accumulate, the reading recalibrates or fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_loss_speculative_vs_empirical, empirical, 'Whether catastrophic control-loss scenarios are empirically probable enough to justify the resource extraction from justice work.').

omega_variable(
    representation_authority_future_generations,
    'Who legitimately speaks for future generations'' interests in the present, and through what accountability mechanism?',
    'Procedural test: do governance structures include affected present-day communities (who experience actual harm) and Global South voices (who have different threat models) in defining what ''alignment for future generations'' means? Do these groups have veto power or only advisory status?',
    'If future-generation representation concentrates entirely in Western safety institutions without input from present-day victims or Global South, the reading is vulnerable to captured authority — using future people as justification for present resource allocation without their consent or diverse representation. A more inclusive deliberation could revise what alignment means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representation_authority_future_generations, conceptual, 'Whether the framing''s invocation of future generations is democratically grounded or an authority concentration mechanism.').

omega_variable(
    suppression_mechanism_internalized_structural,
    'Is the measured suppression (0.71) driven by structural barriers (funding allocation, hiring gates) or by internalized acceptance of the safety-control framing as scientifically legitimate?',
    'Counterfactual: if justice-research funding were increased to match safety-research funding and hiring barriers were removed, would justice researchers shift focus or would the framing hierarchy remain? Post-suppression trajectory: do justice researchers who exit the field report structural pressure or internalized loss of legitimacy?',
    'If suppression is primarily structural, removing barriers could rebalance research directions. If suppression is primarily internalized, the framing must be explicitly contested and alternative legitimacy frameworks established. The distribution of effort affects remedial strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_structural, empirical, 'Whether suppression of justice-focused research is structural or internalized.').

omega_variable(
    kernel_reading_contest_underspecification,
    'The safety_control_reading and its siblings (ethics_justice_reading, integrated_reading) inhabit a single contested kernel. Which reading''s framing of ''alignment'' becomes hegemonic depends on which actors control research agendas and governance definition. Is this a contest amenable to empirical resolution, or is it a permanent value-pluralism that will cycle across readings as power shifts?',
    'Historical trajectory: track which reading receives funding, prestige, and governance authority over the next decade. Identify which material conditions (catastrophic incident, policy intervention, research breakthrough, political shift) would cause the hegemonic reading to flip from safety_control to ethics_justice or integrated. Test whether empirical progress on any reading delegitimizes others.',
    'If the contest is empirically resolvable (one reading''s model is falsified), it will cycle according to evidence. If the contest is fundamentally a value-pluralism (competing visions of which harms matter most), the constraint will persist as tangled_rope indefinitely—coordinating on safety while extracting from justice—because no reading can fully win without suppressing the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_underspecification, conceptual, 'Whether the kernel contest is empirically resolvable or a permanent value-pluralism requiring pluralistic governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__safety_control_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__safety_control_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__safety_control_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__safety_control_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__safety_control_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__safety_control_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__safety_control_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__safety_control_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__safety_control_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__safety_control_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__safety_control_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__safety_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_commitment__safety_control_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__safety_control_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel decomposes into three constraint stories corresponding to three contested readings. Each reading instantiates a different constraint with different ε values, different victim/beneficiary sets, and different classifications. The safety_control_reading (this story) prioritizes catastrophic-risk mitigation and extracts resources from justice work; the ethics_justice_reading prioritizes present-day harm prevention; the integrated_reading claims both must be addressed simultaneously. These are not three viewpoints on one constraint—they are three structurally distinct constraints with different extractiveness profiles and different stakeholder geometries. The network links show which readings' operational success creates pressure on which others (safety_control influences both siblings by setting funding and agenda conditions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__safety_control_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
