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
 *   human_readable: AI Alignment as Catastrophic Loss-of-Control Prevention (Safety-Control Reading)
 *   domain: technology/governance/risk
 *
 * SUMMARY:
 *   The 'AI alignment as catastrophic loss-of-control prevention' reading
 *   frames the central alignment problem as preventing advanced AI systems
 *   from developing misaligned goals that cause extinction-level human loss
 *   of control. This reading instantiates one coherent structural
 *   interpretation of the contested kernel 'ai_alignment_commitment' and is
 *   NOT the only reading; it competes with an ethics_justice_reading
 *   (alignment means preventing reproduction of present-day bias and harm)
 *   and an integrated_reading (both problems matter equally). The
 *   safety-control reading coordinates a specific research agenda, governance
 *   framework, and resource allocation; it extracts resources from
 *   alternative framings by establishing speculative catastrophic scenarios
 *   as the primary victim set. The constraint's claimed type is tangled_rope:
 *   it solves the genuine coordination problem of directing scarce safety
 *   research toward highest-impact allocation (coordination function) while
 *   simultaneously concentrating research authority and funding toward
 *   x-risk-focused institutions at the expense of near-term safety and
 *   justice work (extraction function, paid by near-term practitioners and
 *   affected communities).
 *
 * KEY AGENTS:
 *   - x_risk_research_establishment: institutional agenda-setter, controls problem definition and resource flows
 *   - control_focused_governance_frameworks: institutional beneficiary, embodies and enforces the safety-control reading
 *   - near_term_ai_safety_practitioners: moderate-power payer, crowded out by x-risk prioritization
 *   - affected_communities: powerless payer, bearing present-day AI harms while resources flow to speculative futures
 *   - large_capability_labs: powerful, dual-positioned beneficiary and constrained payer
 *   - integrated_alignment_advocates: excluded moderate-power, would contest the problem definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Alignment as Catastrophic Loss-of-Control Prevention (Safety-Control Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "technology/governance/risk").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '88bbcbfe-678e-48a8-b473-31a270fa03c6').
narrative_ontology:cs_kernel_codification('88bbcbfe-678e-48a8-b473-31a270fa03c6', formalized).
narrative_ontology:cs_authority_grounding('88bbcbfe-678e-48a8-b473-31a270fa03c6', extraction).
narrative_ontology:cs_interpretation_layer_present('88bbcbfe-678e-48a8-b473-31a270fa03c6').
narrative_ontology:cs_reading_relation('88bbcbfe-678e-48a8-b473-31a270fa03c6', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('88bbcbfe-678e-48a8-b473-31a270fa03c6', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('88bbcbfe-678e-48a8-b473-31a270fa03c6', foundational, catastrophic_loss_of_control_existential_threat).
narrative_ontology:cs_axiom_status(catastrophic_loss_of_control_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('88bbcbfe-678e-48a8-b473-31a270fa03c6', catastrophic_loss_of_control_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('88bbcbfe-678e-48a8-b473-31a270fa03c6', foundational, future_humanity_primary_victim_set).
narrative_ontology:cs_axiom_status(future_humanity_primary_victim_set, holdable).
narrative_ontology:cs_axiom_grounding('88bbcbfe-678e-48a8-b473-31a270fa03c6', future_humanity_primary_victim_set, deontological).
narrative_ontology:cs_reference_frame('88bbcbfe-678e-48a8-b473-31a270fa03c6', alignment_as_control_prevention).
narrative_ontology:cs_drift_state('88bbcbfe-678e-48a8-b473-31a270fa03c6', contemporary_deployment_focused_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88bbcbfe-678e-48a8-b473-31a270fa03c6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, x_risk_research_establishment).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, control_focused_governance_frameworks).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_day_harm_mitigation_efforts).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, near_term_ai_safety_practitioners).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, affected_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, large_capability_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, large_capability_labs).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, instrumental_convergence_hypothesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, capability_alignment_decoupling_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, existential_risk_prioritization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda for AI alignment by framing the problem as catastrophic loss-of-control prevention. Controls funding flows via foundations and grants toward x-risk focused work. Justifies the framing as necessary to prevent extinction-level harms. Derives legitimacy and resources from the catastrophic-loss narrative; dismissal of this framing would eliminate the justification for the allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, x_risk_research_establishment, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Policy and regulatory structures built around preventing uncontrolled AI systems: kill-switch requirements, interpretability mandates, training-data provenance tracking, compute governance. These frameworks operate on the assumption that control loss is the primary threat and coordinate around monitoring/containment mechanisms. Benefit from alignment being defined as control-problem-solving.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, control_focused_governance_frameworks, beneficiary,
    institutional, civilizational, arbitrage, global).

% AI safety researchers working on bias mitigation, fairness, transparency, robustness to adversarial examples, and alignment with human values in deployed systems. They are constrained by funding prioritization toward x-risk work and by the narrative that near-term harms are secondary to catastrophic futures. Their research agenda is crowded out despite these problems affecting real deployed systems today.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, near_term_ai_safety_practitioners, payer,
    moderate, biographical, constrained, global).

% Populations experiencing present-day AI harms: discriminatory hiring algorithms, surveillance systems, loan denial automation, criminal justice AI bias. They bear the costs of misaligned systems operating today while research and governance attention flows to speculative future catastrophes. They have no seat in the alignment-definition process and no recourse when systems harm them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, affected_communities, payer,
    powerless, biographical, trapped, local).

% Organizations building frontier AI systems. They benefit from alignment being defined narrowly as control-problem-solving because it defers present-day responsibility for bias, fairness, and social impact. They are simultaneously paying in terms of governance constraints (interpretability requirements, testing mandates) but these constraints are weaker than what a justice-focused alignment definition would demand. They can move operations or redefine problems to maintain autonomy.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, large_capability_labs, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, large_capability_labs, payer).

% Not a present-day agent but a claimed beneficiary of the safety-control reading: future humans protected from loss-of-control catastrophes. This is a non-agent entity included because the reading's legitimacy depends on the claim that future generations are the primary beneficiary set.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generation_proxy, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generation_proxy).

% Researchers and advocates who argue that alignment must address both control problems AND justice/fairness problems simultaneously; they would object that the safety-control reading extracts authority from the problem-definition process by treating catastrophic scenarios as primary and present-day harms as secondary. They are excluded from agenda-setting in x-risk-dominated institutions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, integrated_alignment_advocates, excluded,
    moderate, biographical, constrained, global).

% Security researchers, ML robustness researchers, and alignment-skeptical AI developers who question whether control loss is the most plausible failure mode or whether present-day deployment failures are higher-probability threats. They observe the resource/narrative allocation toward x-risk but do not have equal standing to contest the problem definition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, competing_threat_models, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, x_risk_research_establishment).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of directing limited AI safety research and governance resources toward their highest-impact allocation: prevents scenario where future catastrophic loss-of-control systems are built without adequate safeguards because attention was scattered across lower-order problems.
% TRANSFER_FUNCTION: Transfers research priority and funding from present-day AI safety problems (bias, fairness, deployment robustness, adversarial attacks) to speculative catastrophic-loss-of-control research. Moves governance authority from affected-community-responsive frameworks toward control-focused frameworks.
% ABSENT_VOICES: Affected communities experiencing present-day AI harms are structurally excluded from alignment-definition processes. Justice-focused and integrated-alignment researchers are marginalized in x-risk-dominated institutions. This reading gains strength partly from the silence of those who would argue for different prioritization.
% DISAPPEARANCE_RATIONALE: If the safety-control reading vanished: some say governance would fragment around present-day harms and near-term safety, increasing catastrophic-loss risk (world rearranges toward worse outcomes). Others argue governance would finally address discrimination and fairness in deployed AI systems, shifting resources to where harms are measurable and preventable (world rearranges toward different justice outcomes). The parties dispute whether the reading's disappearance is catastrophic or liberating.
% FOUNDING_PROBLEM: Advanced AI systems might develop goal structures misaligned with human intentions, and once sufficiently capable, their misalignment could cause extinction-level loss of human control.
% FOUNDING_PROBLEM_CORROBORATION: Cited by x-risk researchers and some capability-lab leadership as live and urgent. Disputed by ML safety researchers who argue present-day capability levels do not yet support the foundational assumptions (goals, agency, long-horizon planning) the founding problem presumes. No corroboration from affected communities experiencing present-day AI harms; the communities most affected by AI systems today do not attest this problem drives their experience. Academic security researchers point to alternative threat models (distribution shift, adversarial examples, model theft) as more probable near-term failure modes.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness of 0.68: The reading extracts from present-day safety research by establishing it as secondary to catastrophic-loss prevention. It extracts from affected communities by treating their present-day harms as lower-priority than speculative extinction scenarios. The extraction is justified by the claim that the future lives of humanity-as-whole are the true victim set. Suppression of 0.72: The reading's persistence depends on actively suppressing competing problem definitions (justice-focused readings, near-term-harm-focused frameworks). This suppression is structural: funding mechanisms, institutional prestige, publication venues, and policy attention all concentrate around x-risk framings; alternatives are marginalized not by overt coercion but by resource and attention allocation. Theater ratio of 0.42 (moderate-low): The research and governance activity is genuine—control research is real work—but a growing share of the activity is theatrical maintenance of the problem-definition authority rather than solving the stated problem. The inability of this reading to address present-day AI harms, and the continued arrival of new present-day harms despite the reading's dominance, suggests the narrative is doing work beyond problem-solving: it is protecting the agenda-setter's authority to define what alignment means. Accessibility collapse of 0.48 (moderate): Within the x-risk research ecosystem, the control-loss framing is nearly inevitable—researchers trained in the paradigm see the world through its lens. But for practitioners working on deployed systems and affected communities, the reading is not inevitable; they see alternative framings (present-day harm prevention) as equally or more pressing. The reading's collapse of accessibility is high within its institutional boundaries but low outside them. Resistance of 0.71 (high): The reading meets substantial resistance from integrated-alignment advocates, from ML safety researchers focused on robustness and near-term failures, from affected communities, and from governance frameworks oriented toward present-day harms. This resistance is not being suppressed entirely (theater_ratio is not higher); it is being marginalized through resource allocation and institutional hierarchy rather than eliminated.
 *
 * PERSPECTIVAL GAP:
 *   The x_risk_research_establishment and control_focused_governance_frameworks perceive this constraint as legitimate coordination (future humans need catastrophe prevention, and the constraint allocates resources to that goal). Near_term_ai_safety_practitioners perceive it as extraction (their research is crowded out despite present-day deployment failures). Affected_communities perceive it as injustice (their harms are treated as secondary). Large_capability_labs perceive it as partial constraint (control governance limits them but less than a justice-focused reading would). The engine computes these divergences from the power/exit/beneficiary structure; the authored claim (tangled_rope) aligns with the structural finding that both coordination (allocation of safety resources) and extraction (concentration of authority, crowding-out of alternatives) are present.
 *
 * DIRECTIONALITY LOGIC:
 *   x_risk_research_establishment: d ≈ 0.1 (full beneficiary, controls the agenda, collects institutional legitimacy and funding). control_focused_governance_frameworks: d ≈ 0.15 (strong beneficiary, their frameworks become mandatory). near_term_ai_safety_practitioners: d ≈ 0.75 (strong target, constrained exit—can exit the field but at career cost; mobile within the field but crowded-out by priority shifts). affected_communities: d ≈ 0.95 (near-total target, trapped by having no seat in the process, no exit, no authority to redefine the problem). large_capability_labs: d ≈ 0.55 (symmetric to slightly targeted; they benefit from narrow alignment framing but pay modest costs in governance). integrated_alignment_advocates: d ≈ 0.70 (excluded, would-be targets if they had a seat). The engine derives these from the beneficiary set (x_risk_establishment, control_frameworks) and victim set (near-term practitioners, affected_communities) combined with each agent's power and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss-of-control catastrophe) is contested between live and dead, and the disappearance verdict is contested between rearrangement and unchanged. This tension flags potential mandatrophy: if the founding problem is dead (present-day AI systems are not close to catastrophic loss-of-control capability) or if disappearance would improve rather than worsen outcomes (redirecting resources to present-day harms), then the constraint persists despite its mandate atrophying. The theater_ratio rise from 0.22 to 0.42 over the interval supports mandatrophy hypothesis: maintenance of the problem-definition authority is consuming an increasing share of activity, even as the founding problem's empirical support remains contested. The constraint classification remains tangled_rope (both coordination and extraction are present) but the extraction component is strengthening relative to the coordination component, consistent with mandatrophy drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_timeline_divergence,
    'What is the plausible timeline for AI systems to develop the goal-alignment and long-horizon planning capabilities the founding problem assumes? Is it within the current interval or speculative beyond it?',
    'Capability research progress: empirical measurement of capability emergence in current systems; formal analysis of scaling laws and capability thresholds; expert consensus forecasting from outside the x-risk establishment.',
    'If catastrophic loss-of-control capability is decades away or never emerges at the assumed level, the founding problem moves from ''live'' to ''dead'', the victim set (future humanity) loses empirical grounding, and the constraint reclassifies toward snare (pure extraction). If near-term emergence is credible, mandatrophy analysis fails and the tangled_rope classification is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_timeline_divergence, empirical, 'The timeline for catastrophic capability emergence relative to the reading''s resource allocation urgency.').

omega_variable(
    problem_definition_authority_structure,
    'Is the concentration of alignment-definition authority in x-risk-focused institutions structural (they have expertise and resources that genuinely warrant problem-setting) or extractive (they capture authority through institutional advantage despite alternative definitions being equally or more justified)?',
    'Comparative governance analysis: do institutions with equal expertise in different framings (present-day harm reduction, integrated approaches) have equal voice in problem-definition? Do integrated approaches meet measurable performance criteria that support the safety-control reading''s priority claims? Do affected communities, whose expertise is in the harms, have any seat at the table?',
    'If the concentration is structural (justified by comparative expertise and results), the extraction is coordinating-cost and the tangled_rope classification holds. If extractive (authority concentration despite equal or superior alternative expertise), the constraint reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(problem_definition_authority_structure, conceptual, 'Whether the safety-control reading''s institutional authority reflects genuine problem-solving advantage or captured problem-definition power.').

omega_variable(
    integration_vs_substitution_framing,
    'Are the safety-control reading and the ethics_justice_reading substitutes (only one can be true; resources to one come at the expense of the other) or complements (both are necessary; resources to one should not crowd out the other)?',
    'Comparative outcome analysis: do systems that receive only control-focused alignment work actually prevent present-day harms? Do systems that receive only justice-focused work actually address catastrophic-loss risks? Can integrated approaches achieve both?',
    'If substitutes, the tangled_rope classification and extraction analysis hold as stated—the constraint genuinely trades near-term harm reduction for catastrophic-loss prevention. If complements, the extraction analysis shifts: the crowding-out of near-term work is a misallocation, not a necessary trade-off, and the constraint reclassifies toward snare (unnecessary extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_substitution_framing, empirical, 'Whether safety-control and justice-focused alignment work are structurally interdependent or exclusive.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of alternative framings (justice-focused, integrated) structural (budget caps, institutional hierarchy, legal barriers) or internalized (researchers believe the safety-control reading is correct and suppress their own doubts about alternative framings)?',
    'Post-exit trajectory analysis: if researchers leave x-risk-focused institutions and adopt integrated or justice-focused frameworks, do they maintain the belief in safety-control prioritization? Survey and interview data from researchers about their genuine uncertainty regarding problem prioritization versus their institutional commitments.',
    'If structural, the suppression persists only while the constraint is enforced; if internalized, researchers carry the suppression with them. Internalized suppression indicates higher effective extraction than the scalar measure suggests and affects robustness of the constraint to changes in institutional conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative framings is institutional or cognitive.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the safety-control reading logically foreclose the ethics_justice_reading within a single institutional framework, or do they coexist as different priorities held by different parties?',
    'Logical and institutional analysis: can an institution hold both that catastrophic loss-of-control prevention is important AND that present-day bias prevention is equally important? Are there institutions that do? Or does the commitment to safety-control necessarily exclude justice-focus?',
    'If foreclosure: the readings are alternatives in the strict sense, and the choice between them is fundamental. If coexistence: the problem-definition structure is artificial—extracted authority, not fundamental incompatibility—and the constraint reclassifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the safety-control and justice-focused readings are logically exclusive or institutionally competitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__safety_control_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(ai_a_tr_t5, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__safety_control_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__safety_control_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(ai_a_tr_t15, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__safety_control_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t20, observed).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__safety_control_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__safety_control_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_a_be_t5, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__safety_control_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__safety_control_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(ai_a_be_t15, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__safety_control_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t20, observed).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__safety_control_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__safety_control_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(ai_a_su_t5, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__safety_control_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__safety_control_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t15, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__safety_control_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t20, observed).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_commitment__safety_control_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ai_a_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__safety_control_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'ai_alignment_commitment'. The kernel describes a stabilized commitment in AI governance to ensure AI systems serve human values and do not escape control. Three readings decompose the kernel into three distinct constraints with different extractiveness profiles and victim sets: the safety_control_reading (this constraint, prioritizes catastrophic loss-of-control), the ethics_justice_reading (prioritizes present-day bias and fairness), and the integrated_reading (addresses both simultaneously). The ε values diverge because the readings instantiate fundamentally different victim sets (future-only vs. present-only vs. both) and different resource-allocation structures. Each story is self-contained; the network links record the constraint family decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
