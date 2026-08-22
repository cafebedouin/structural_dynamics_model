% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential AI Risk Prioritization Frame
 *   domain: technology/governance/risk assessment
 *
 * SUMMARY:
 *   The existential AI risk reading frames AGI misalignment as an
 *   extinction-level threat requiring urgent resource concentration on
 *   alignment research, capability controls, and international coordination.
 *   This reading instantiates a specific allocation mechanism: longtermist
 *   institutions set research priorities and funding flows; near-term
 *   algorithmic justice concerns are recontextualized as secondary or
 *   distracting; the victim set extends to future generations (a non-agent
 *   abstraction represented by current beneficiary institutions). The
 *   constraint is CLAIMED as tangled_rope—genuine coordination on AGI safety
 *   plus asymmetric extraction from near-term justice constituencies. The
 *   authored metrics describe rising extractiveness (asymmetry between who is
 *   coordinated and who pays) and rising theater (enforcement activity
 *   increasingly directed at suppressing near-term framing rather than pure
 *   safety research). This is one reading of the contested kernel
 *   ai_risk_prioritization; the sibling reading
 *   ai_risk_prioritization__near_term_harms_reading instantiates the
 *   competing frame.
 *
 * KEY AGENTS:
 *   - longtermist_x_risk_institutions (agenda-setter, organized power): defines research priorities, controls funding flows
 *   - alignment_research_funders (beneficiary, powerful): direct recipients of billions in directed funding
 *   - capability_control_advocates (beneficiary/agenda-setter, powerful institutional): shape policy and research direction toward AGI governance
 *   - near_term_justice_constituencies (payer, powerless/immediate horizon): subjects of current algorithmic harms, absorb resource opportunity cost
 *   - displaced_workers (payer, moderate power): experience job loss; policy attention redirected to long-term risks
 *   - near_term_ai_harms_researchers (excluded, moderate power): research agenda subordinated to existential framing
 *   - future_generations (abstract beneficiary, non-agent): represented but unable to participate in allocation decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.71).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential AI Risk Prioritization Frame").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology/governance/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'abc6d102-8ad1-41fd-bdd6-6647d9ed20dc').
narrative_ontology:cs_kernel_codification('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', distributed).
narrative_ontology:cs_authority_grounding('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', extraction).
narrative_ontology:cs_interpretation_layer_present('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc').
narrative_ontology:cs_reading_relation('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', foundational, agi_misalignment_extinction_primary_risk).
narrative_ontology:cs_axiom_status(agi_misalignment_extinction_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', agi_misalignment_extinction_primary_risk, empirically_contingent).
narrative_ontology:cs_axiom('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', foundational, alignment_research_primary_response_lever).
narrative_ontology:cs_axiom_status(alignment_research_primary_response_lever, holdable).
narrative_ontology:cs_axiom_grounding('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', alignment_research_primary_response_lever, instrumental).
narrative_ontology:cs_reference_frame('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', frontier_ai_capability_unaligned_catastrophic_risk).
narrative_ontology:cs_drift_state('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', contemporary_2024_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abc6d102-8ad1-41fd-bdd6-6647d9ed20dc', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_x_risk_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, capability_control_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, alignment_research_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_justice_constituencies).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_bias_subjects).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research and funding priorities within AI safety and EA communities, framing existential misalignment as the canonical failure mode. Controls or influences major funding flows (billions annually from longtermist foundations). Defines what counts as legitimate AI risk research and enforces this framing through research agendas, hiring, conference selection, and grant allocation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_x_risk_institutions, agenda_setter,
    organized, civilizational, arbitrage, global).

% Direct financial beneficiaries of the existential framing: billions in funding flow to alignment and capabilities research addressing AGI-level risks. Their portfolio value and institutional legitimacy depend on the existential framing remaining the authoritative one. They face no enforcement barrier to directing resources elsewhere but do so rarely because the existential frame is their primary comparative advantage in EA and tech governance circles.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, alignment_research_funders, beneficiary,
    powerful, generational, mobile, global).

% Benefit from resource allocation toward AGI capability control, safety infrastructure, and international coordination frameworks. Include research organizations, policy advocates, and some national security establishments that frame AGI as the central governance challenge. Their institutional authority depends on the existential framing being credible and urgent.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_control_advocates, beneficiary,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, capability_control_advocates, agenda_setter).

% Subjects of algorithmic discrimination, bias in deployed systems, labor displacement from current AI applications. Pay in the form of deferred resources and attention: funding that could address current harms is redirected to long-term research; policy attention to immediate discriminatory systems is subordinated to existential governance. Their exit options are extremely constrained—they cannot opt out of deployed systems and lack resources to redirect funding flows.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_justice_constituencies, payer,
    powerless, immediate, trapped, local).

% Harmed by bias in hiring algorithms, credit scoring, policing systems, and medical diagnostics operating today. Experience measurable discrimination. The existential framing recontextualizes their harm as a 'near-term distraction' from the primary risk. They lack institutional power to redirect resources or reframe the risk hierarchy and are geographically dispersed, limiting coalition formation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_bias_subjects, payer,
    powerless, biographical, trapped, regional).

% Losing employment to automation and algorithmic job displacement now. The existential framing treats their displacement as a minor feature of the long-term landscape rather than an urgent justice concern. Policy resources and research attention directed to AGI alignment are unavailable for retraining, transition support, or labor protections addressing current displacement. They have some coalition power through labor organizations but limited access to funding or governance architecture.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, displaced_workers, payer,
    moderate, biographical, constrained, regional).

% The canonical beneficiary in the existential framing. Cannot participate in current decision-making or contest resource allocation. Represented (often without deliberative input) by longtermist institutions that claim to steward their interests. Non-agent entity: included for narrative completeness as the abstract moral subject the constraint nominally protects.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_generations).

% Researchers focused on algorithmic fairness, current AI bias, labor displacement, and near-term governance. Would contest the existential framing as misallocating resources and obscuring urgent harms. Excluded from agenda-setting in longtermist funding streams and EA community governance; their research is often characterized as short-termist or instrumentally motivated rather than addressing the 'real' risks.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_researchers, excluded,
    moderate, biographical, constrained, global).

% Develop frontier AI capabilities in industry and academic labs. Position themselves as observers of the existential risk debate, sometimes claiming neutrality while benefiting from the urgency framing (justifies aggressive capability advancement as necessary precursor to alignment research). Their interests are partially aligned with the existential framing (legitimates their work) but partially in tension (capability controls might restrict their research directions).
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_researchers, observer,
    institutional, generational, mobile, global).

% Would be substantially harmed by AGI-level existential risks but have minimal voice in defining the risk frame or allocating response resources. Also subject to near-term algorithmic harms (exported bias in credit, hiring, content moderation systems) but cannot redirect funding or governance attention. Excluded from both the longtermist institutions setting the existential frame and from the near-term justice constituencies that might contest it.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, developing_nation_stakeholders, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, longtermist_x_risk_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scientific and policy attention on AGI safety as a civilization-level problem; aligns research incentives toward long-horizon technical work rather than episodic responses to specific harms; concentrates expertise and resources on the constraint of misaligned superintelligence—a genuine coordination problem no single actor would solve independently.
% TRANSFER_FUNCTION: Redirects research funding, policy attention, and institutional legitimacy from near-term algorithmic justice and labor protection toward AGI alignment research and capability controls. Billions flow from longtermist philanthropies to x-risk research institutions; policy capacity is allocated to international AGI governance frameworks rather than algorithmic auditing and bias remediation; career paths and professional prestige flow to researchers working on existential timescales rather than near-term fairness.
% ABSENT_VOICES: Subjects of current algorithmic discrimination, displaced workers in specific sectors, near-term AI harms researchers, developing nation stakeholders affected by exported algorithmic bias, and labor advocates focused on employment protection. These parties would contest the prioritization framework but are largely excluded from the institutions and funding networks that set the existential agenda. Their absence is structural, not accidental: the existential framing positions their concerns as instrumentally useful at best (for testing alignment properties) and distracting at worst.
% DISAPPEARANCE_RATIONALE: If the existential prioritization frame disappeared and resources reallocated to near-term harms, the world would substantially rearrange: funding flows to deployed-AI auditing and fairness work would increase; labor protections and retraining would expand; policy capacity would address current discrimination rather than AGI governance. The existential framing's beneficiaries (x-risk institutions, capability control advocates) argue the world would rearrange catastrophically—existential risks would go unaddressed, increasing extinction probability. The near-term justice constituencies argue the world would rearrange toward justice for present harms without materially decreasing AGI preparation. The contest is not whether disappearance matters (it clearly does) but whether the rearrangement is beneficial or catastrophic.
% FOUNDING_PROBLEM: Unaligned artificial general intelligence poses an extinction-level threat to humanity. As AI systems advance toward general reasoning, the difficulty of ensuring their goals align with human values increases. Current progress toward AGI, combined with inadequate alignment research capacity, creates an existential emergency requiring urgent resource concentration on alignment, capability control, and international coordination frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Attested by leading researchers in AGI safety, longtermist philosophers, and some AI capabilities researchers who accept the alignment frame. Contested by near-term AI harms researchers who argue the founding problem is misdiagnosed (AGI misalignment is speculative; near-term harms are measurable), by labor advocates who argue the founding problem is wrongly sequenced (near-term displacement is an urgent problem now), and by developing nation representatives who argue the frame privileges northern institutional interests over immediate global harms. External corroboration for the existential urgency comes from a subset of academic AI researchers and EA-aligned experts; external contestation comes from labor economists, civil rights organizations, and Global South governance advocates.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval as the existential framing consolidates institutional authority and funding flows concentrate. At t=0, the existential frame competes with near-term concerns; by t=15-25, it dominates longtermist institutions and EA governance; by t=35-40, it plateaus as the established orthodoxy, with remaining resistance absorbed into niche near-term research. Theater rises from 0.18 to 0.42 as enforcement increasingly focuses on maintaining the existential priority against contestation (suppressing near-term framing as short-termist, redefining fairness work as peripheral to the main problem) rather than on pure safety research. Suppression rises from 0.52 to 0.71 as the institutional apparatus actively excludes near-term voices from agenda-setting and funding. The constraint is tangled_rope, not pure snare: genuine coordination problem (AGI safety) plus real extraction (near-term justice constituencies bear the cost of redirected attention). Beneficiaries are organized and powerful (institutions, funders); victims are dispersed (near-term justice, displaced workers) or excluded (near-term researchers) or abstract (future generations), making coalition resistance difficult. The measurement series reveals a ratchet of suppression: once the existential frame dominates institutions, the active enforcement required to maintain it against alternative framings increases, suggesting the constraint's stability depends on continuous suppression rather than on universal acceptance of the underlying coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   From the longtermist x-risk institutional seat, this is genuine coordination—the existential risk is real and urgent, and reallocating resources is the correct response to an unprecedented threat. From the near-term justice seat, this is extraction disguised as coordination—the allocation mechanism concentrates attention on speculative long-term risks while measurable harms are deferred, and the mechanism is sustained by institutional power and funding control, not by epistemic consensus. From the future generations seat (represented by longtermist institutions), this is stewardship of their interests; from the developing nation excluded seat, this is appropriation of future interests by northern institutional power. The engine computes directionality from the structural data: longtermist institutions are near-beneficiary (d ≈ 0.1-0.2); near-term constituencies are near-target (d ≈ 0.85-0.95); the gap emerges from power, exit options, and victim/beneficiary declarations, not from claimed framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Longtermist x-risk institutions (powerful, organized, mobile exit options) benefit from the existential framing—they control its instantiation and resource flows. Their directionality is low (beneficiary end of spectrum, d ≈ 0.15). Near-term justice constituencies (powerless, trapped exit options, immediate horizon) pay through deferred attention and resources. Their directionality is high (target end of spectrum, d ≈ 0.90). Capability control advocates sit slightly toward beneficiary (d ≈ 0.3-0.4): they benefit from the framing's policy authority but also bear some reputational risk if the existential prediction proves incorrect or if near-term harms go severely unaddressed. Near-term harms researchers are excluded, not victims—exclusion is structural, not extraction, though it is enforced (d ≈ 0.65). Future generations are the nominal beneficiaries but non-agents; their interests are mediated entirely through longtermist institutions, which creates a structural conflict of interest—the beneficiary (future generations) cannot contest their representation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits early mandatrophy dynamics. The founding problem (unaligned AGI extinction risk) was live when the existential frame emerged (2010s, responding to increasing AI capabilities). By 2026, the founding problem remains contested rather than solved (near-term researchers and Global South stakeholders dispute whether misalignment is the primary risk or a secondary concern relative to capability deployment and algorithmic harms). However, the constraint is increasingly maintained by institutional inertia and enforcement rather than by continued consensus that the problem is urgent and unsolved. The rising theater_ratio (from 0.18 to 0.42) signals that a growing share of enforcement activity is devoted to suppressing near-term framing and maintaining the existential priority against contestation, not to advancing safety research directly. The suppression_requirement rising from 0.52 to 0.71 indicates that active force is required to keep the framing dominant. The plateau of extractiveness at 0.68 after t=25 suggests the constraint has reached a stable institutional state—further consolidation requires diminishing returns. A mandatrophy verdict is not yet warranted (the founding problem remains genuinely contested), but the trajectory shows a constraint transitioning from coordination plus extraction toward pure enforcement of a minority frame. The engine will flag this as a stage-2 mandatrophy candidate if the divergence between founding_problem_status (contested) and the continued extraction widens further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agi_timescale_uncertainty,
    'How many years until transformative AGI capable of misalignment-induced extinction risk? Is it 5 years, 50 years, or 500 years?',
    'Observable progress toward AGI; capability benchmarks; researcher consensus (though consensus itself is contested); concrete AGI arrival.',
    'If AGI is 500+ years away, the existential framing radically misallocates resources and justifies near-term harms. If AGI is 5-10 years away, the existential prioritization is justified. If timescale remains fundamentally uncertain (best estimate 2025-2100 with wide credence spread), both framings retain validity claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agi_timescale_uncertainty, empirical, 'Fundamental uncertainty about AGI arrival timeline drives all downstream resource allocation disputes.').

omega_variable(
    alignment_problem_solvability,
    'Is AGI alignment a solvable technical problem amenable to research focus (existential reading assumes yes), or is it an unsolvable structural problem rooted in the incentive structures of AGI development (near-term reading suspects no)?',
    'Failure or success of alignment research producing systems demonstrably safer than capability-first approaches; empirical test of alignment techniques on next-generation frontier systems.',
    'If alignment is solvable, the existential frame''s resource concentration is justified. If alignment is unsolvable or capability control is the only viable approach, the existential frame is catastrophically wrong and near-term harms research becomes the primary concern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_problem_solvability, empirical, 'Whether alignment research produces measurable safety improvements or merely the appearance thereof.').

omega_variable(
    extinction_vs_misery_moral_weight,
    'If aggregate future suffering is lower under near-term justice policies + delayed AGI + eventual misaligned-AGI-induced extinction than under existential prioritization + near-term injustice + reduced extinction risk, which frame is correct?',
    'Moral philosophy (no empirical resolution); stakeholder deliberation; comparison of suffering metrics across framings. This is irreducibly normative.',
    'This omega cannot be resolved empirically—it depends on terminal values and moral uncertainty. Frames emphasizing extinction prevention weight future existence axiomatically; frames emphasizing present justice weight current suffering axiomatically. Resolution requires deliberation, not discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinction_vs_misery_moral_weight, preference, 'Irreducible value disagreement between extinction prevention and present justice.').

omega_variable(
    institutional_capture_vs_epistemic_leadership,
    'Is the dominance of the existential frame evidence of genuine epistemic leadership by x-risk researchers (who have correctly identified the primary risk), or evidence of institutional capture by longtermist institutions (who have concentrated resources and defined the risk hierarchy for institutional benefit)?',
    'Audit of resource allocation decisions: do they track published safety research consensus or institutional preference? Analysis of which researchers succeed in funding (do near-term researchers get funded at proportional rates?). Measurement of alternative-frame research output given equal resources.',
    'If epistemic leadership: the existential frame should remain dominant indefinitely. If institutional capture: removing institutional bottlenecks should produce equivalent safety research output with different risk hierarchy. This distinction affects the legitimacy of the extraction component of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_epistemic_leadership, empirical, 'Whether frame dominance reflects truth or power.').

omega_variable(
    representation_of_future_interests,
    'Can longtermist institutions legitimately represent the interests of future generations who cannot participate in current resource allocation? Is representation through institutional stewardship adequate, or is it a structural conflict of interest?',
    'Governance innovation: mechanisms for future-stake representation in current allocation decisions; accountability structures for longtermist trustees; deliberative processes including near-term stakeholders in existential framing decisions.',
    'If representation is adequate, future generations are genuine beneficiaries and the frame is valid coordination. If it is inadequate, the constraint exhibits structural victims (future generations) represented by their alleged beneficiaries, which is a snare signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_of_future_interests, conceptual, 'Whether future interests can be legitimately represented by current institutions.').

omega_variable(
    near_term_frame_sibling_reading_contest,
    'Is the near_term_harms_reading logically incompatible with this existential reading, mutually enabling, or structurally separable (both could be true simultaneously)?',
    'Logical analysis: do the axioms of each reading contradict? Can both AGI misalignment and near-term algorithmic harm be primary risks? Empirical test: do resources directed to near-term justice reduce existential safety research effectiveness, or are they complementary?',
    'If incompatible (forecloses): only one can be defended. If compatible (coexists): the allocation contest is empirical and empirically resolvable. If separable: the constraint is pure extraction (exists to subordinate one valid concern to another) rather than genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_term_frame_sibling_reading_contest, conceptual, 'Structural relationship between existential and near-term risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__existential_risk_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_prioritization__existential_risk_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_prioritization__existential_risk_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(ai_r_tr_t35, ai_risk_prioritization__existential_risk_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(ai_r_tr_t40, ai_risk_prioritization__existential_risk_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(ai_r_be_t35, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(ai_r_be_t40, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(ai_r_su_t35, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(ai_r_su_t40, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, algorithmic_bias_regulatory_constraint).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, labor_displacement_policy_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading (existential_risk_reading) of the contested kernel ai_risk_prioritization. The sibling reading ai_risk_prioritization__near_term_harms_reading instantiates the competing frame with different victim/beneficiary structure, timescale, and resource allocation. Both stories share the kernel (AI systems carry risks) but diverge fundamentally on risk hierarchy and enforcement mechanism. Network links track downstream effects: this reading (existential prioritization) influences and partially suppresses the near_term_harms and labor_displacement constraints by redirecting resources and policy attention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
