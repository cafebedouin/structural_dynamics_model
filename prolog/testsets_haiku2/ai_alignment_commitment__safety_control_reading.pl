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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: AI Safety Control Commitment (Catastrophic Risk Framing)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The safety-control reading of AI alignment instantiates a commitment to
 *   preventing catastrophic loss of control over advanced AI systems. This
 *   reading prioritizes speculative future failure modes (value misalignment,
 *   deceptive systems, uncontrolled AI exceeding human oversight capacity)
 *   over present measurable harms (algorithmic bias, unfair discrimination,
 *   opacity in consequential domains). The reading directs research agenda,
 *   governance authority, and funding flows toward technical control problems
 *   and catastrophic-risk mitigation. The kernel 'AI alignment commitment'
 *   admits multiple readings with different victim sets, different extraction
 *   mechanisms, and different claims about what counts as the central
 *   problem. This story instantiates ONE reading — the safety-control framing
 *   — and treats it as a constraint with its own extractiveness, suppression,
 *   and structural relationships. The CLAIM that this is genuine coordination
 *   (tangled_rope) and the METRICS showing high theater and suppression
 *   requirement are authored independently; divergence is the measurement the
 *   corpus takes.
 *
 * KEY AGENTS:
 *   - AI safety research institutions (MIRI, FHI, OpenAI safety teams): agenda-setters and beneficiaries; frame the control problem as central and secure research funding via the catastrophic-risk narrative
 *   - Safety governance agencies (UK AI Safety Institute, EU AI Act, US NIST): institutional agenda-setters; derive authority from the safety-control framing and enforce it through policy and regulation
 *   - Existential-risk community (longtermists, effective altruists, catastrophe researchers): beneficiaries; benefit from resource flows and legitimacy directed toward existential-risk mitigation
 *   - Present AI-harm victims (people harmed by algorithmic bias in hiring, lending, policing, healthcare): payers and trapped; experience concrete harms that are subordinated to speculative future scenarios
 *   - Marginalized communities and developing-world jurisdictions: payers via resource constraint; capacity and resources are directed toward safety-control governance rather than present-harm mitigation
 *   - Alternative alignment researchers (justice-focused, fairness-focused, integrated approaches): excluded; face institutional gatekeeping and narrative marginalization when catastrophic-control scenarios dominate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Safety Control Commitment (Catastrophic Risk Framing)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '25177277-435b-4cf2-8faf-be291b4d0a09').
narrative_ontology:cs_kernel_codification('25177277-435b-4cf2-8faf-be291b4d0a09', distributed).
narrative_ontology:cs_authority_grounding('25177277-435b-4cf2-8faf-be291b4d0a09', extraction).
narrative_ontology:cs_interpretation_layer_present('25177277-435b-4cf2-8faf-be291b4d0a09').
narrative_ontology:cs_reading_relation('25177277-435b-4cf2-8faf-be291b4d0a09', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('25177277-435b-4cf2-8faf-be291b4d0a09', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('25177277-435b-4cf2-8faf-be291b4d0a09', foundational, control_loss_catastrophe_is_primary_threat).
narrative_ontology:cs_axiom_status(control_loss_catastrophe_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('25177277-435b-4cf2-8faf-be291b4d0a09', control_loss_catastrophe_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('25177277-435b-4cf2-8faf-be291b4d0a09', secondary, technical_control_solutions_are_prerequisite).
narrative_ontology:cs_axiom_status(technical_control_solutions_are_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('25177277-435b-4cf2-8faf-be291b4d0a09', technical_control_solutions_are_prerequisite, instrumental).
narrative_ontology:cs_reference_frame('25177277-435b-4cf2-8faf-be291b4d0a09', technical_alignment_as_civilizational_priority).
narrative_ontology:cs_drift_state('25177277-435b-4cf2-8faf-be291b4d0a09', contemporary_post_2023_ai_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25177277-435b-4cf2-8faf-be291b4d0a09', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, safety_governance_agencies).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, existential_risk_community).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, near_term_ai_harm_victims).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, resource_constrained_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda on what counts as alignment, defines the control problem, secures research funding by emphasizing catastrophic-risk narratives, and conducts the technical work on control-focused approaches. Institutions like MIRI, FHI, OpenAI safety teams benefit from the framing that positions their research as critical to human survival. Exit is minimal because safety research is their institutional identity; mobility is conceptual rather than exit — they could reorient toward other AI topics but have not chosen to.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutions, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutions, beneficiary).

% National and international governance bodies (UK AI Safety Institute, EU AI Act safety provisions, US NIST AI RMF) that frame regulation and oversight around catastrophic-control scenarios. They derive enforcement mandate, policy authority, and budget from the existential-risk framing. Exit is constrained by political commitments; they cannot easily pivot without appearing to have misallocated prior resources or abandoned the issue they claimed urgency over.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, safety_governance_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Researchers, forecasters, and funders invested in existential-risk narratives (longtermist effective altruists, catastrophe-risk specialists, AI safety forecasters) who benefit from the legitimacy and resource flow the safety-control reading directs toward their framing. They maintain the narrative through publications, research funding, and public discourse; exit is mobile but not exercised because the reading's success is their research identity.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, existential_risk_community, beneficiary,
    organized, civilizational, mobile, global).

% Individuals harmed RIGHT NOW by AI bias, discrimination, and opacity in hiring, lending, criminal justice, medical diagnosis, content moderation, and surveillance. Their harms are concrete and measurable. They are subordinated in resource allocation and priority because the safety-control reading frames present harms as less critical than speculative future catastrophes. They cannot exit the systems causing them harm; they are trapped in the systems and powerless to influence alignment research agendas.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, near_term_ai_harm_victims, payer,
    powerless, immediate, trapped, global).

% Communities experiencing discriminatory AI deployments in hiring, criminal justice, benefits administration, and automated decision-making that affects their lives. The safety-control reading directs AI governance attention and resources toward future control scenarios while present injustice persists and deepens. They bear extraction in the form of continued subjection to biased systems while remediation resources flow toward speculative catastrophe prevention.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_marginalized_communities, payer,
    powerless, biographical, trapped, regional).

% Lower-income countries and regions whose AI governance capacity is limited, whose immediate development needs (healthcare optimization, educational resource allocation, agricultural yield improvement) could benefit from AI deployment, but who are pressured or obligated to adopt safety-control frameworks developed in high-income countries. They pay through capacity constraints, delayed access to beneficial AI, and resource redirection toward safety governance rather than development. Exit is constrained by economic dependence and capacity limitations.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, resource_constrained_jurisdictions, payer,
    powerless, generational, constrained, global).

% Researchers pursuing justice-focused, fairness-focused, participatory, or integrated approaches to AI alignment are structurally excluded from agenda-setting and funding authority when catastrophic-control scenarios monopolize resources. They face institutional gatekeeping (peer review rejection, conference exclusion), funding denial, and narrative marginalization. Exit is constrained by career dependence on institutional legitimacy and funding.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, alternative_alignment_researchers, excluded,
    moderate, generational, constrained, global).

% Non-agent referent invoked as the beneficiary of present-day safety interventions. Future generations cannot participate in debates about which risks to prioritize, how resources should be allocated between present and speculative future harms, or which readings of the alignment problem are valid. They are deployed rhetorically to justify present resource allocation but have no agency in the decision.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generations).

% External researchers and analysts examining the structural relationships, resource flows, and victim/beneficiary classifications of different alignment readings and their constraints. The observer seat is not engaged in advancing any reading but in measuring the constraint's operation across all seats.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, ai_safety_research_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research, governance, and resource allocation toward a unified problem definition: preventing catastrophic loss of human control over AI systems through technical alignment research and safety-focused governance. Solves the collective-action problem of ensuring different research teams, institutions, and regulators work on a coherent threat model rather than at cross-purposes.
% TRANSFER_FUNCTION: Transfers research funding, governance authority, policy-setting power, and public attention FROM present-day AI-harm mitigation (bias correction, fairness engineering, transparency) TO speculative catastrophic-scenario research. The transfer moves resources from immediate, measurable harms to speculative, high-impact scenarios that sit 10-50+ years in the future. It also moves authority over what counts as 'alignment' from affected communities and justice researchers to safety-focused technical institutions.
% ABSENT_VOICES: Communities experiencing present AI harms (algorithmic bias victims, people in policing/benefits surveillance, marginalized groups), justice-focused AI researchers, developing-world governance bodies, and future-generation advocates (who cannot speak but are invoked as beneficiaries) are structurally absent from the agenda-setting table dominated by safety institutions. They would argue for simultaneous attention to present injustice and that resource scarcity creates trade-offs between present and speculative future harm prevention.
% DISAPPEARANCE_RATIONALE: If the safety-control reading vanished, governance institutions would reallocate resources toward present-harm mitigation (bias audits, fairness requirements, transparency mandates), research funding would shift toward integrated and justice-focused approaches, and the framing of AI alignment would expand to include present-day discrimination and harm. The catastrophic-risk narrative currently functions as a prioritization mechanism; its disappearance would reorder which harms count as urgent.
% FOUNDING_PROBLEM: The founding problem is genuine: large-scale AI systems deployed in consequential domains (hiring, lending, criminal justice, content moderation) are already causing measurable harm through bias, opacity, and misalignment with user and stakeholder values. The safety-control reading reformulates this as a control-loss problem and projects it forward into speculative scenarios where AI capabilities exceed human ability to supervise. The founding problem predates the safety-control reading by decades (algorithmic bias was documented in the 1980s); the control-catastrophe framing is a recent interpretive layer.
% FOUNDING_PROBLEM_CORROBORATION: Safety-focused institutions, longtermist funders, and AI governance bodies attesting the control problem is the critical priority. Justice-focused researchers, affected communities, and present-harm documentation (studies of discriminatory AI in hiring, lending, criminal justice) attest that the founding problem is already manifesting as present injustice and that resource scarcity forces prioritization choices. Academic literature (O'Neill on weapons of math destruction, Buolamwini on facial recognition bias, Selbst on fairness and abolition) documents present harms and contests the priority assignment. No corroboration from the present-harm victims themselves — they are excluded from the corroborating set by structural position.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.51 to 0.68 over 25 years because the safety-control framing consolidates institutional authority, research funding flows, and policy agenda around a single victim set (humanity-as-whole in speculative futures) while systematically subordinating alternative framings and present-harm mitigation. Theater ratio rises from 0.32 to 0.54 because the constraint's operation requires increasing performative emphasis on catastrophic scenarios (AI safety summits, existential-risk reports, worst-case modeling) to maintain institutional credibility even as empirical evidence accumulates for present harms. Suppression requirement stays high (0.58-0.71) because the constraint's persistence depends on actively marginalizing alternative research directions, silencing present-harm victims from agenda-setting conversations, and maintaining the narrative that speculative future scenarios are more urgent than measured present injustice. The shared time grid captures extractiveness rising as the reading consolidates power, theater rising as the consolidation requires performative maintenance, and suppression requirement stabilizing at a high plateau because the exclusion of alternatives is structural, not declining. Accessibility collapse (0.62) is moderate because alternative alignment framings remain conceptually available — they are not eliminated, only marginalized — and resistance (0.58) is substantial because justice researchers and present-harm communities actively contest the priority assignment.
 *
 * PERSPECTIVAL GAP:
 *   From the safety-institution seat, the arrangement is genuine coordination: they have solved (or are solving) a real collective-action problem by unifying research teams, governance bodies, and funders around a coherent threat model. The threat (catastrophic AI misalignment) is real, and coordination efforts are genuinely hard. From the present-harm-victim seat, the same structure operates as institutional extraction: real, measured harms are subordinated to speculative scenarios; resources that could address present injustice are redirected; and affected communities are excluded from decision-making about which problems to prioritize. Both seats are describing true structural properties; the constraint enables one seat to set priorities that disadvantage the other. The engine computes this per-seat divergence from the structural data (beneficiaries vs. payers, exit options, power levels); the divergence is where the classification reveals the constraint's true functioning.
 *
 * DIRECTIONALITY LOGIC:
 *   The safety-control reading exhibits strong asymmetric extraction: beneficiary seats (safety institutions, governance agencies, existential-risk community) derive authority, funding, and agenda-setting power; payer seats (present-harm victims, marginalized communities, justice researchers) lose resources and are excluded from governance conversations. The beneficiary seats maintain relatively high exit mobility (they could reorient if the framing collapsed) but do not exercise it because the reading's success is their institutional identity. The payer seats are trapped: present-harm victims have no exit from the systems causing them harm; developing-world jurisdictions are constrained by resource and capacity limits; justice researchers are constrained by institutional gatekeeping. Directionality for beneficiaries is near 0.0 (they benefit from the constraint's operation); for payers and excluded voices it approaches 1.0 (they bear the extraction). The analytical observer sits at d=0.5 (symmetric position).
 *
 * MANDATROPHY ANALYSIS:
 *   The safety-control reading exhibits partial mandatrophy: the founding problem — AI systems causing present harms through bias and unfairness — is live and measurable. The constraint purports to address this by coordinating research and governance toward a unified threat model. However, the reading's specific interpretation (control loss in speculative futures is the CENTRAL problem) has partially decoupled from the founding problem it claims to solve. Present injustice persists and worsens in real time; resources directed toward catastrophic-risk mitigation do not address present harms. The constraint persists partly because it solves the genuine coordination problem (unified research direction) but increasingly through performative emphasis on worst-case scenarios rather than present-harm mitigation. This is the signature of tangled_rope entering piton phase: the coordination function (real) is increasingly inseparable from the extraction mechanism (resource subordination), and theater rises as the extraction requires narrative maintenance. The constraint has not yet become pure piton because the coordination function (unified research agenda) remains essential to safety-focused institutions; but the direction is toward mandatrophy where the constraint's founding justification has been substantially decoupled from its actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_vs_justice_temporal_relationship,
    'Are the control problem and the justice problem sequential (control must be solved first, then justice) or simultaneous (both require parallel attention with current resource constraints creating trade-offs)?',
    'Empirical monitoring: do societies that prioritize present-harm mitigation and justice-focused alignment research simultaneously develop safer, more aligned AI systems than those that deprioritize present harms? Comparative governance analysis across jurisdictions.',
    'If the problems are sequential-with-control-first, the priority assignment is structurally justified and the extraction is necessary cost. If they are simultaneous, the subordination of present harms is misaligned with the actual problem structure and the constraint exhibits unnecessary extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(control_vs_justice_temporal_relationship, empirical, 'Whether catastrophic-risk mitigation and present-harm prevention must be sequential or can be simultaneous.').

omega_variable(
    reading_substitution_and_cover_story,
    'Is the safety-control reading a genuine alternative interpretation of the alignment kernel, or a cover story for resource concentration and research-agenda capture by a specific institutional coalition?',
    'Historical and institutional analysis: examine whether alternative alignment readings (justice-focused, integrated) are evaluated on their merits or systematically excluded through funding gatekeeping, publication rejection, and agenda-setting exclusion. Post-exit analysis: if the safety-control framing collapses, do institutions voluntarily expand to other readings, or do they resist?',
    'If genuine alternative interpretation: the constraint is a legitimate coordination mechanism with asymmetric but defensible extraction. If cover story: the constraint is primarily extractive and the coordination function is secondary theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_substitution_and_cover_story, empirical, 'Whether the safety-control reading is a substantive interpretation or an institutional cover story.').

omega_variable(
    catastrophic_risk_vs_speculative_narrative,
    'What is the empirical probability that uncontrolled AI systems will exceed human control in dangerous ways, and how much of the safety-control reading''s urgency depends on that probability vs. on narrative framing and institutional commitment independent of probability?',
    'Calibration study: compare expert probability estimates across time; examine whether safety institutions update probability estimates in response to new evidence or maintain high-catastrophe-probability narratives regardless of empirical updates.',
    'If catastrophic-risk probability is high and stable: the safety-control reading''s urgency is empirically justified. If probability is low or unstable but the reading''s urgency persists: the reading is narrative-driven and the constraint''s persistence is partly independent of the threat model it claims to address.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_risk_vs_speculative_narrative, empirical, 'Whether catastrophic AI risk probability justifies the safety-control reading''s urgency or whether urgency persists independently of probability.').

omega_variable(
    alternative_reading_coexistence_feasibility,
    'Is it structurally possible for the safety-control reading to coexist with justice-focused and integrated readings in genuine dialogue, or does the control reading''s claim to logical priority (control problems must be solved first) foreclose coexistence?',
    'Institutional experiment: attempt to establish multi-reading governance structures (representation from safety, justice, and integrated approaches on alignment boards) and monitor whether genuine dialogue emerges or whether the control reading maintains veto authority.',
    'If coexistence is feasible: the constraint permits alternative readings to persist, and the suppression (0.71) reflects deliberate exclusion, not structural necessity. If control reading logically forecloses alternatives: coexistence fails and the classification should reflect foreclosure rather than suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_feasibility, conceptual, 'Whether the safety-control reading''s logical structure permits coexistence with justice-focused readings or forecloses it.').

omega_variable(
    suppression_internalization_in_research_community,
    'Is the measured suppression (0.71) externally enforced (institutional gatekeeping, funding denial) or internalized (researchers self-exclude from justice-focused work because they have adopted the control framing as their own intellectual identity)?',
    'Post-suppression-removal analysis: if institutional barriers and funding constraints were removed, would marginalized researchers and alternative approaches expand rapidly, or have they internalized the control-priority framing such that suppression persists?',
    'If internalized: the constraint''s effective suppression is higher than structural measures suggest because the targets carry suppression with them. If external: removal of barriers would enable rapid alternative-approach expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_research_community, empirical, 'Whether suppression is structural or internalized in the research community.').

omega_variable(
    readings_of_ai_alignment_kernel_contention,
    'This constraint instantiates ONE reading of the contested kernel AI_ALIGNMENT_COMMITMENT. The kernel admits at least three live readings: safety_control_reading (this one), ethics_justice_reading (focusing on present-day bias and discrimination), and integrated_reading (treating control and justice as non-exclusive). Are these readings genuinely coexisting live positions held by different institutional factions, or does the safety-control reading logically foreclose the others by asserting control is prior?',
    'Examine foundational claims (axioms) each reading makes about the relationship between control and justice; determine whether they directly contradict (foreclosure possible) or merely prioritize differently (coexistence likely). Test against historical record: have all three readings maintained active research communities and institutional backing, or has the control reading consolidated monopoly authority?',
    'If coexistence is real: the constraint belongs to a contested family with multiple valid readings, and OQ-83 readings-of-a-kernel analysis applies. If control reading forecloses the others: coexistence dissolves and the other readings may not survive as live positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(readings_of_ai_alignment_kernel_contention, conceptual, 'Kernel reading contest structure: coexistence vs. foreclosure of alternative AI-alignment readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__safety_control_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__safety_control_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__safety_control_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__safety_control_reading, theater_ratio, 25, 0.54).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__safety_control_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__safety_control_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__safety_control_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__safety_control_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__safety_control_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__safety_control_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__safety_control_reading, suppression_requirement, 15, 0.7).
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
% The kernel AI_ALIGNMENT_COMMITMENT decomposes into three structurally distinct constraint stories, each instantiating a different reading of what alignment means. The safety-control-reading (this story) prioritizes speculative catastrophic-risk scenarios and positions control as logically prior; the ethics-justice reading prioritizes present-day harms and institutional bias; the integrated reading treats control and justice as simultaneous, non-exclusive problems. Each reading has its own victim set, ε value, and extraction mechanism. These are not observations of the same constraint from different angles — they are different constraints generated by different readings of the same kernel. The ε-invariance principle requires separate constraint stories for each reading because the referent for measurement differs: what counts as 'alignment,' who counts as victim, and what counts as extraction are all reading-dependent. All three stories are linked via network.affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
