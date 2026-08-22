% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Credibility Paradox: Deterrence Through Incredible Threat
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   Nuclear-armed great powers maintain deterrence through credible threats
 *   of use, yet use guarantees mutual annihilation, rendering the threat
 *   logically incredible. This reading instantiates the CREDIBILITY PARADOX
 *   framing: deterrence is not structurally stable; great powers seek usable
 *   nuclear options (counterforce, escalation dominance, limited-war
 *   doctrine) to resolve the paradox; 'unthinkability' is rhetorical
 *   performance, not structural law; war remains reachable via escalation
 *   ladders. The constraint is a TANGLED ROPE: genuine coordination function
 *   (prevent great-power war) bound together with asymmetric extraction
 *   (subordination of non-nuclear states, institutional resources to military
 *   communities). The measurement series models the 80-year post-WWII
 *   interval, showing extractiveness rising from the founding period (0.38)
 *   to stabilization (0.62), with theater_ratio (performative activity in
 *   doctrine and signaling) rising initially then plateauing as the
 *   constraint matured.
 *
 * KEY AGENTS:
 *   - nuclear_armed_great_powers: institutional power, civilizational horizon, trapped exit — set and enforce the deterrence arrangement; extract regional dominance and political concessions; must perform credibility despite paradox
 *   - non_nuclear_states: powerless, biographical horizon, trapped exit — live under strategic subordination; cannot develop arsenals; cannot exit the security dilemma
 *   - smaller_allied_states: moderate power, biographical horizon, constrained exit — receive extended deterrence but live under existential threat; face strategic subordination as price of protection
 *   - military_strategic_communities: organized power, generational horizon, constrained exit — derive careers and institutional purpose from managing the paradox through doctrine and capability development
 *   - arms_control_advocates: organized power, generational horizon, constrained exit — excluded from primary decision-making; argue the paradox proves deterrence failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.71).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Credibility Paradox: Deterrence Through Incredible Threat").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '049f69e4-b45e-48f8-98e0-03f829d071c2').
narrative_ontology:cs_kernel_codification('049f69e4-b45e-48f8-98e0-03f829d071c2', implicit).
narrative_ontology:cs_authority_grounding('049f69e4-b45e-48f8-98e0-03f829d071c2', extraction).
narrative_ontology:cs_reading_relation('049f69e4-b45e-48f8-98e0-03f829d071c2', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('049f69e4-b45e-48f8-98e0-03f829d071c2', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('049f69e4-b45e-48f8-98e0-03f829d071c2', foundational, deterrence_credibility_paradox_unsolvable).
narrative_ontology:cs_axiom_status(deterrence_credibility_paradox_unsolvable, holdable).
narrative_ontology:cs_axiom_grounding('049f69e4-b45e-48f8-98e0-03f829d071c2', deterrence_credibility_paradox_unsolvable, empirically_contingent).
narrative_ontology:cs_axiom('049f69e4-b45e-48f8-98e0-03f829d071c2', foundational, escalation_control_possible_via_strategy).
narrative_ontology:cs_axiom_status(escalation_control_possible_via_strategy, holdable).
narrative_ontology:cs_axiom_grounding('049f69e4-b45e-48f8-98e0-03f829d071c2', escalation_control_possible_via_strategy, instrumental).
narrative_ontology:cs_axiom('049f69e4-b45e-48f8-98e0-03f829d071c2', secondary, unthinkability_is_rhetorical_not_structural).
narrative_ontology:cs_axiom_status(unthinkability_is_rhetorical_not_structural, holdable).
narrative_ontology:cs_axiom_grounding('049f69e4-b45e-48f8-98e0-03f829d071c2', unthinkability_is_rhetorical_not_structural, deontological).
narrative_ontology:cs_reference_frame('049f69e4-b45e-48f8-98e0-03f829d071c2', cold_war_mutual_deterrence_regime).
narrative_ontology:cs_drift_state('049f69e4-b45e-48f8-98e0-03f829d071c2', contemporary_post_cold_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('049f69e4-b45e-48f8-98e0-03f829d071c2', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_armed_great_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, smaller_allied_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, smaller_allied_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategic_communities).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_dominance_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, flexible_response_strategy).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, counterforce_capability_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess overwhelming destructive capacity and maintain credible threat of use to deter existential challenges. They build counterforce capabilities, develop limited-war doctrine, and conduct nuclear signaling to sustain belief in their willingness to use weapons. They extract political concessions, alliance dominance, and regional influence through possession of the threat. The paradox they inhabit: their deterrence function depends on others believing they WILL use nuclear weapons despite the mutual-destruction guarantee; maintaining that belief requires constant performance of willingness (doctrine refinement, capability development, strategic signaling) even though actual use would be catastrophic for themselves.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_armed_great_powers, agenda_setter,
    institutional, civilizational, trapped, global).

% Live under existential threat from nuclear-armed powers or under extended deterrence umbrellas that make them potential targets of retaliation. They cannot develop their own arsenals (non-proliferation regime), cannot neutralize the threat through conventional military means (asymmetric power gap), and cannot exit the security dilemma through diplomatic accommodation (the threat persists regardless of compliance). They bear the cost of strategic uncertainty, political subordination to nuclear-armed protectors, and the constant low-level anxiety of living in a system where transcontinental exchange could occur at any escalation moment.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    powerless, biographical, trapped, global).

% Receive extended nuclear deterrence (protection from adversaries with nuclear weapons) but pay for it through strategic subordination, hosting of nuclear forces, and acceptance that a nuclear exchange could occur on their territory. They depend on the great power's willingness to use nuclear weapons to defend them, but that very willingness—if tested—could be catastrophic for the allied state itself. Their exit is identity-locked: breaking the alliance means redefining their entire international role and security strategy, not merely changing partners.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, smaller_allied_states, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, smaller_allied_states, beneficiary).

% Nuclear strategy, weapons development, doctrine refinement, and strategic signaling constitute massive institutional and professional careers. Military academies, defense contractors, strategic think tanks, and government agencies derive funding, prestige, and operational purpose from managing the credibility problem. They profit from the performance (doctrine, capability development, signaling exercises) that sustains the paradox, not from its resolution. Their identity as strategic professionals is fused with the constraint's continuation; advocating for disarmament or deterrence-abandonment would require exiting the profession itself.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, military_strategic_communities, beneficiary,
    organized, generational, identity_locked, global).

% Argue for abolition, drastic reduction, or structural elimination of the credibility paradox through disarmament. They are excluded from the primary decision space (great powers control deterrence policy) and their frameworks are actively contested by the benefiting institutional structures. They would argue that the credibility paradox is proof that deterrence is a failed doctrine and that the constraint perpetuates danger rather than preventing it. Their exclusion is structural: they have no seat at the table where deterrence policy is set, and the great powers have active institutional incentive to keep them excluded.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the logical structure of the deterrence paradox and debate whether it is a real structural constraint (the paradox proves deterrence is unstable and war remains reachable) or a rhetorical problem (credibility can be managed through signaling, escalation dominance, and counterforce capability). They occupy the analytical seat outside the enforcement structure; their theories feed back into military doctrine and policy but do not directly command outcomes. This reading embodies the strategic-theorist perspective that the paradox is genuine and destabilizing.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_armed_great_powers).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nuclear deterrence solves the collective-action problem of preventing great-power war by raising the cost of aggression to mutual annihilation. The coordination problem it addresses: without a credible deterrent, larger powers would use military force to resolve disputes; with a credible deterrent, both sides prefer diplomatic resolution because any military conflict risks escalation into mutual destruction.
% TRANSFER_FUNCTION: Transfers political concessions, regional dominance, and alliance subordination from non-nuclear and smaller allied states to nuclear-armed great powers. Also transfers institutional resources (funding, careers, prestige) from the broader political economy to military-strategic communities that manage the credibility performance. Non-nuclear states pay through strategic dependence and exposure; military institutions collect through expanded budgets and institutional importance.
% ABSENT_VOICES: Arms-control advocates and disarmament communities are structurally excluded from decisions to maintain or build nuclear arsenals. They would argue the paradox is proof of deterrence failure and advocate abolition; their voices appear in NGO campaigns and academic conferences but do not shape the constraint's enforcement. Secondary excluded voice: non-state actors and future generations who cannot participate in the strategic signaling that sustains the threat but will suffer the consequences if it fails.
% DISAPPEARANCE_RATIONALE: If the credibility paradox constraint vanished—if deterrence were somehow stabilized (via structural inevitability) or eliminated (via disarmament)—great-power conflict dynamics would fundamentally reorganize. Nuclear-armed states would lose the primary tool of political coercion; non-nuclear states would escape strategic subordination; military-strategic institutions would lose their primary institutional rationale. The international system would face genuine security dilemmas without the (paradoxical) stabilizing fiction of mutual annihilation. If the paradox is indeed unstable (as this reading claims), its disappearance might occur through escalation and war, not through peaceful reform.
% FOUNDING_PROBLEM: Cold War strategic competition: the Soviet Union and United States possessed roughly equivalent nuclear arsenals; each needed to deter the other from initiating war or blackmail; neither possessed meaningful conventional superiority; mutual destruction became the baseline assumption—hence the paradox of deterrence through an incredible threat. The founding problem was: how can we prevent war when both sides possess civilization-destroying weapons?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's existence is confirmed by historical record (Soviet-American nuclear parity, proxy-war avoidance, doctrine evolution, declassified strategic assessments). The contested element is whether this founding problem STILL exists post-Cold War or has been superseded. Military strategists argue the problem remains live and justify counterforce capabilities and deterrence modernization accordingly; arms-control advocates argue the founding problem is obsolete (Russia and China are not peer existential threats in the way the USSR was) and the constraint persists through institutional inertia and institutional interest (piton dynamics). Outside corroboration comes from declassified strategic documents, Cold War history, game-theoretic assessment of great-power military capabilities, and empirical analysis of whether great-power conflict dynamics have changed—NOT from the current great powers themselves, whose incentive is to justify continued deterrence performance.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the constraint's dual nature: genuine coordination benefit (deterrence prevents war) is bound with clear extraction (political subordination, institutional capture). Suppression at 0.71 is high because maintaining credibility requires active performance—if suppression were removed, the paradox would become visible and the constraint would likely collapse into a rational-dropout reading (war avoidance through cost-benefit rather than threat credibility). Theater at 0.58 is elevated because nearly 60% of enforcement effort goes into PERFORMING credibility (doctrine refinement, capability development, strategic signaling, declaratory policy) rather than functioning deterrence itself. This reading claims the constraint is a TANGLED ROPE because both coordination (prevent great-power war) and extraction (subordinate non-nuclear states, capture military resources) are structural and necessary to the arrangement. The paradox is not a defect the engine corrects; it is the core of the constraint itself—deterrence works BECAUSE the threat is incredible and yet must be treated as credible for political effect. Accessibility collapse at 0.47 is moderate: alternatives (disarmament, arms-control agreements, conventional deterrence) remain theoretically available but carry high political and institutional cost. Resistance at 0.73 is substantial because arms-control communities actively resist the constraint and challenge its legitimacy; great-power governments must continuously defend deterrence doctrine against this resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute radically different types from the same constraint. The great-power agenda-setter seat experiences genuine coordination (deterrence prevents war) with manageable extraction (political subordination is a necessary feature, not a bug). The non-nuclear target seat experiences pure extraction (subordination without meaningful protection, since the threat remains existential and uncontrollable). The smaller-allied seat experiences a hybrid: genuine protection from the great power BUT existential exposure to the deterred party's retaliation. These are not measurement disagreements; they are structural role differences the engine correctly resolves into divergent classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The derived directionality chain works as follows: (1) Great powers are declared beneficiaries (control the constraint, extract concessions, set terms). (2) Non-nuclear states are declared victims (no beneficiary role, trapped exit, powerless). (3) The derivation maps beneficiary → d near 0.0, victim + powerless + trapped → d near 1.0. (4) Smaller allies have constrained (not trapped) exit and moderate power, plus a declared secondary beneficiary role (they receive protection), so their d lands in the 0.6-0.75 range (partly-trapped targets with some protection). (5) Military-strategic communities are beneficiaries (institutional resources) with constrained exit (professional identity), so d lands in the 0.2-0.4 range (beneficiaries with some identity-lock). The automatic derivation captures the structural asymmetry without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE: great-power conflict remains a real risk and deterrence provides genuine stability (no great-power war since 1945 despite multiple near-misses). The extraction problem is also LIVE: non-nuclear states remain subordinated and the constraint persists partly through their inability to exit. The constraint is NOT mandatrophy-resolved because the founding problem (prevent great-power war) has not outlived its function—the international system still depends on deterrence for stability. However, this reading embeds an internal tension: if deterrence is truly stable through the paradox (the threat is incredible but functions anyway), then the founding problem persists indefinitely and the constraint is permanent. If deterrence is actually unstable (as this reading claims via the credibility paradox), then war remains reachable and the founding problem is still live but the constraint is failing at its primary function. Mandatrophy would arise if the founding problem disappeared (great-power conflict became genuinely impossible) but the constraint persisted. The current state is NOT mandatrophy; it is a stable but extractive tangled_rope with contested claims about its actual stability (the reading claims it is unstable; the structural-contraction sibling claims it is stable; the rational-dropout sibling claims stability is achievable through cost-benefit rather than credibility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_paradox_logical_depth,
    'Is the credibility paradox a genuine logical impossibility (deterrence fundamentally cannot work) or a practical management problem (credibility is difficult but achievable through signaling and doctrine)?',
    'Comparative historical analysis of escalation moments (Cuban Missile Crisis, Berlin Crisis, Korean War) where the credibility of the threat was tested and either held or collapsed. Empirical examination of whether actual military signaling, doctrine, and capability deployment succeeded in conveying credibility despite the paradox.',
    'If the paradox is a genuine logical impossibility, deterrence is unstable and war remains reachable (supporting this reading''s credibility_paradox framing). If credibility is practically achievable, the paradox is a rhetorical concern and deterrence is stable (supporting structural_contraction reading). This determines whether the constraint''s core function—preventing great-power war—actually succeeds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_paradox_logical_depth, empirical, 'Whether the credibility paradox is logically or practically insurmountable.').

omega_variable(
    extraction_vs_coordination_decoupling,
    'Is the political extraction (subordination of non-nuclear states, military institutional capture) structurally necessary to sustain deterrence, or is it a separable rent-capture phenomenon?',
    'Counterfactual analysis: could deterrence be maintained in a world where non-nuclear states were politically independent (no extended nuclear umbrella) and military institutions were structured differently (e.g., civilian-led, rotational leadership)? Comparison with arms-control scenarios where deterrence is minimized and extraction mechanisms are removed.',
    'If extraction is structurally necessary (the great power must dominate non-nuclear states to maintain the credibility performance), then this constraint is an unavoidable tangled_rope. If extraction is separable, then deterrence could be maintained without subordination, and the constraint could be decomposed into a pure-coordination rope (deterrence) plus a separate snare (subordination). This determines whether the constraint''s extraction is inherent or parasitic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decoupling, conceptual, 'Whether deterrence''s coordination function requires the extraction mechanism or not.').

omega_variable(
    theater_ratio_escalation_mechanism,
    'As theater_ratio rises (doctrine, signaling, and performance become increasingly decoupled from functional deterrence), does the rising performance actually enhance credibility or does it begin to undermine it by becoming visibly performative?',
    'Discourse analysis of strategic statements and doctrine over the interval: trace whether increased theorizing and signaling activity correlates with foreign-policy outcomes that suggest enhanced credibility (diplomatic victories, deterrent successes) or degraded credibility (increased strategic competition, reduced deterrent effects). Examine whether adversaries treat escalating performance as credible performance or as theater.',
    'If rising theater enhances credibility, then the constraint''s performative aspects are functional and theater_ratio is a feature not a bug. If rising theater degrades credibility, then the constraint is approaching a phase transition where the performance becomes visible as performance and the credibility collapse (supporting both the credibility_paradox reading and the rational_dropout reading''s skepticism). The theater measurement series shows plateau at t=50 onward, suggesting either stabilization of performance at sustainable levels or saturation of credibility gains from theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_escalation_mechanism, empirical, 'Whether increased strategic performance (doctrine, signaling) continues to enhance credibility or begins to undermine it.').

omega_variable(
    sibling_reading_empirical_differentiation,
    'What empirical facts would distinguish this reading (credibility paradox / instability / war reachable via escalation) from the structural_contraction reading (stability via logical inevitability) and the rational_dropout reading (stability via cost-benefit)?',
    'Future contingency: a crisis where a nuclear-armed power initiates conventional military action against a peer with confidence in escalation control (believing a counterforce strike or limited nuclear use is feasible) versus a crisis where no such action is taken despite conventional military advantage (treating escalation as impossible). The credibility_paradox reading predicts attempted escalation control and crisis risk; structural_contraction predicts escalation is impossible and hence war does not occur; rational_dropout predicts cost-benefit prohibits war but does not forbid its initiation if costs are miscalculated.',
    'Observation of escalation attempts or near-misses due to miscalculated credibility would support this reading. Observation of pure rationality-based peace (powers recognize costs and back down) supports rational_dropout. Observation of no escalation whatsoever supports structural_contraction. The differentiation requires future events or detailed historical case analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_differentiation, empirical, 'Empirical differentiation between the three sibling readings of the nuclear impossibility kernel.').

omega_variable(
    suppression_of_disarmament_alternative,
    'To what extent does the constraint''s enforcement require active suppression of disarmament advocacy and arms-control alternatives, versus passive institutional inertia?',
    'Institutional analysis of arms-control negotiations, funding patterns for disarmament research versus deterrence research, media treatment of abolitionist versus deterrence arguments, and political barriers to treaty ratification. Examine whether disarmament alternatives are excluded by passive structural factors or by active institutional opposition from benefiting parties.',
    'If suppression is largely active (arms-control advocates are systematically marginalized, funded alternatives are excluded), the constraint''s enforcement depends on suppressing the knowledge of viable alternatives—a sign of a snare or heavily-extractive tangled_rope. If suppression is passive (alternatives exist but are politically unpopular or seem institutionally unlikely), the constraint is more rope-like (coordination sustained by preference, not coercion). The suppression value at 0.71 suggests substantial suppression; determining whether it is active or passive affects the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_disarmament_alternative, empirical, 'Whether suppression of disarmament alternatives is active institutional policy or passive structural inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(nucl_tr_t0, observed).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(nucl_tr_t10, observed).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(nucl_tr_t20, observed).
narrative_ontology:measurement(nucl_tr_t30, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement_basis(nucl_tr_t30, observed).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(nucl_tr_t40, observed).
narrative_ontology:measurement(nucl_tr_t50, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 50, 0.57).
narrative_ontology:measurement_basis(nucl_tr_t50, observed).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(nucl_tr_t60, observed).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement_basis(nucl_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(nucl_be_t0, observed).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(nucl_be_t10, observed).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(nucl_be_t20, observed).
narrative_ontology:measurement(nucl_be_t30, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(nucl_be_t30, observed).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(nucl_be_t40, observed).
narrative_ontology:measurement(nucl_be_t50, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(nucl_be_t50, observed).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(nucl_be_t60, observed).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(nucl_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(nucl_su_t0, observed).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(nucl_su_t10, observed).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(nucl_su_t20, observed).
narrative_ontology:measurement(nucl_su_t30, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(nucl_su_t30, observed).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(nucl_su_t40, observed).
narrative_ontology:measurement(nucl_su_t50, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(nucl_su_t50, observed).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(nucl_su_t60, observed).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement_basis(nucl_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.18).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% The nuclear impossibility kernel decomposes into three structurally distinct readings with different ε values and beneficiary/victim structures. This story (credibility_paradox_reading) asserts deterrence is unstable and war is reachable via escalation-ladder dynamics; it has higher extractiveness (0.62) and higher resistance (0.73) than the structural_contraction reading (which claims stability is assured by physics). The three stories are linked via network.affects_constraints because each reading's credibility depends partly on refuting the others' core premises. The rational_dropout reading influences both alternatives by claiming stability is achievable through rational cost-benefit without relying on either paradoxical credibility or physical impossibility; this rendering of the constraint family preserves the ε-invariance principle by giving each reading its own independent extraction measure (credibility paradox claims extraction rises with destabilizing theater; rational dropout claims extraction is stable; structural contraction claims extraction is near-zero).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
