% ============================================================================
% CONSTRAINT STORY: learned_helplessness_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_learned_helplessness_trap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: learned_helplessness_trap
 *   human_readable: Learned Helplessness Trap: Internalized Powerlessness as Self-Sustaining Extraction
 *   domain: psychology/behavioral_economics/social_dynamics
 *
 * SUMMARY:
 *   Learned helplessness is a constraint where an agent's internalized belief
 *   that their actions cannot affect outcomes becomes self-enforcing and
 *   extractive. The original learned helplessness experiments demonstrated
 *   that animals (and humans) subjected to uncontrollable aversive stimuli
 *   develop a cognitive and behavioral pattern: they cease attempting to
 *   escape or control future aversive events, even when escape becomes
 *   possible. The constraint operates through the internalization of
 *   powerlessness. Unlike constraints maintained by external enforcement
 *   (physical barriers, legal prohibition, economic coercion), learned
 *   helplessness sustains itself through the victim's identity-fused belief
 *   that resistance is futile. This makes it a snare from the victim's
 *   perspective — high extraction, high suppression, reliant on continued
 *   suppression rather than active enforcement. The beneficiary system
 *   (abuser, oppressive regime, structural inequality) experiences it as
 *   coordination: the victim enforces the constraint on themselves. The
 *   critical feature is that the binding mechanism is cognitive
 *   (identity-locked) rather than purely material (trapped or constrained).
 *   This creates a specific diagnostic signature: the agent could exit (they
 *   have objective capability) but cannot imagine exiting because their
 *   identity has been constituted through the helpless role. The
 *   extractiveness trajectory shows accumulation over time as the identity
 *   lock deepens — early interventions face lower identity inertia, but as
 *   the pattern persists, it becomes more ingrained in the agent's
 *   self-concept, narrative identity, and behavioral repertoire. The theater
 *   ratio increases alongside extractiveness because maintaining the
 *   psychological suppression requires ongoing narrative reinforcement: the
 *   agent must repeatedly encounter or internalize stories about the
 *   immutability of their situation, the futility of effort, and the
 *   naturalness of their powerlessness.
 *
 * KEY AGENTS:
 *   - The Trapped Agent: Primary victim (powerless/identity_locked) — agent whose identity has fused with learned helplessness; experiences the constraint as unchangeable despite objective exit paths
 *   - The Extraction Beneficiary: Primary beneficiary (institutional/arbitrage) — abuser, oppressive system, or structural inequality that benefits from the agent's helplessness; experiences the constraint as coordination (victim-enforced suppression)
 *   - The Aware Observer: Secondary agent (moderate/constrained) — therapist, peer, organizer, or community member who recognizes the extraction but faces barriers to intervention; experiences mixed coordination and extraction
 *   - The Intervention Program: Organized actor (organized/mobile) — cognitive behavioral therapy, community empowerment, trauma-informed intervention that treats helplessness as remediable; sees the constraint as temporary with sunset clause
 *   - The Institutional Perpetuator: System maintainer (institutional/arbitrage) — bureaucracy, administrative apparatus, or institutional inertia that maintains helplessness narratives despite loss of clear functional purpose; sees constraint as degraded piton
 *   - The Analytical Observer: Universalizing perspective (analytical/analytical) — risks naturalizing the constraint as inherent to human psychology; false summit that masks the social and institutional origins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(learned_helplessness_trap, 0.68).
domain_priors:suppression_score(learned_helplessness_trap, 0.75).
domain_priors:theater_ratio(learned_helplessness_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(learned_helplessness_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(learned_helplessness_trap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(learned_helplessness_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(learned_helplessness_trap, snare).
narrative_ontology:human_readable(learned_helplessness_trap, "Learned Helplessness Trap: Internalized Powerlessness as Self-Sustaining Extraction").
narrative_ontology:topic_domain(learned_helplessness_trap, "psychology/behavioral_economics/social_dynamics").

domain_priors:requires_active_enforcement(learned_helplessness_trap).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(learned_helplessness_trap, extraction_beneficiary).
narrative_ontology:constraint_victim(learned_helplessness_trap, helpless_agent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED AGENT (SNARE) — Agent perceives no viable escape path. Through repeated exposure to uncontrollable aversive events, the agent has internalized a frame where effort produces no outcome — resistance is futile. The binding mechanism is cognitive: the agent's identity has fused with the role of powerlessness. Exit would require not merely surmounting external barriers but abandoning the identity-constituted belief that surmounting is impossible. This is identity_locked, not merely trapped or constrained. The agent structures their behavior, expectations, and self-concept around learned helplessness.
constraint_indexing:constraint_classification(learned_helplessness_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AWARE OBSERVER (TANGLED ROPE) — An agent or group with some power and constrained exit options who recognizes the learned helplessness mechanism but cannot fully escape it. For example, a community organizer, therapist, or peer who sees the extraction and wants to intervene but faces institutional, economic, or relational barriers to doing so effectively. They experience both coordination function (helping the trapped agent recognize agency) and extraction (the system continues to reward their complicity in maintaining the status quo).
constraint_indexing:constraint_classification(learned_helplessness_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE BENEFICIARY SYSTEM (ROPE) — From the perspective of the extraction beneficiary (institutional systems, abusers, oppressive regimes, or structural inequality), learned helplessness is a coordination mechanism: it solves the problem of how to maintain control without continuous active enforcement. The beneficiary experiences the constraint as pure coordination — the trapped agent does the enforcing for them. Exit is arbitrage: the beneficiary can switch contexts, populations, or mechanisms and still extract value. No enforcement cost is visible from this perspective because the victim has internalized the mechanism.
constraint_indexing:constraint_classification(learned_helplessness_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ORGANIZED INTERVENTION (SCAFFOLD) — Organized intervention programs (cognitive behavioral therapy, community empowerment initiatives, trauma-informed care, social movements) address learned helplessness as a temporary, solvable problem with a sunset clause. These interventions treat the identity lock as remediable through exposure to controllable outcomes, identity reframing, and collective efficacy building. The scaffold perspective sees the learned helplessness trap as something being actively dismantled through organized effort. High suppression is tolerated only if it declines over the time horizon as new identity frames and agency experience accumulate.
constraint_indexing:constraint_classification(learned_helplessness_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE INSTITUTIONAL INERTIA (PITON) — From the perspective of an institution that has 'become' the learned helplessness mechanism (a bureaucracy, prison system, welfare administration, colonial administrative apparatus), the constraint appears degraded and self-perpetuating through theater. The institution maintains helplessness narratives and classification systems that prevent agents from accessing pathways to agency, but the institution itself has lost sight of any functional purpose beyond maintenance. The beneficiary is not maximizing extraction — they are maintaining a ritual. Theater ratio is high because the institution devotes significant resources to convincing itself (and the trapped agent) that the trap is necessary or natural.
constraint_indexing:constraint_classification(learned_helplessness_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NATURAL LAW VIEW (FALSE SUMMIT) — A naturalized perspective that treats learned helplessness as an immutable consequence of human psychology or 'the way things are.' This view frames the trap as emerging from universal features of learning and motivation (e.g., 'people naturally give up when effort fails repeatedly'). However, the base properties reveal this as a false summit: the constraint exhibits high suppression, high theater, and clear beneficiaries — all inconsistent with a mountain. The naturalization itself is part of the extraction mechanism. The engine detects this as false natural law.
constraint_indexing:constraint_classification(learned_helplessness_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE PARTIALLY AWARE VICTIM (SNARE) — A moderate-power agent who has internalized learned helplessness in specific domains (professional identity, educational capacity, health behavior) but retains some power in other domains. They are not powerless globally but have fused their identity with helplessness in contexts where they were repeatedly subjected to uncontrollable aversive events. This perspective shows how identity_locked exit differs from trapped or constrained: the agent could sometimes leave but cannot recognize this from within the identity frame constituted through helplessness.
constraint_indexing:constraint_classification(learned_helplessness_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(learned_helplessness_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(learned_helplessness_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(learned_helplessness_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(learned_helplessness_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(learned_helplessness_trap, TR),
    TR >= 0.70.

:- end_tests(learned_helplessness_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. The trap extracts agency, autonomy, and adaptive capacity from the victim. The measurement trajectory shows growth from 0.35 (initial exposure) to 0.68 (established pattern) as the identity lock deepens. Early in the pattern, the agent may still retain some sense of potential efficacy; over time, the identity fusion intensifies and objective exit paths become psychologically inaccessible. Suppression (0.75): High. Suppression operates through multiple channels: (1) structural barriers to agency in the original aversive context, (2) internalized narratives about incapability, (3) identity fusion with the helpless role, (4) reduced information flow about alternative possibilities. The suppression is mixed structural and internalized — initially structural (real uncontrollability in aversive context), then becomes increasingly internalized as the cognitive pattern generalizes beyond the original context. Theater ratio (0.58): Moderate-high and increasing. Theater sustains the identity lock through narratives ('this is who I am,' 'effort never works,' 'people like me don't succeed'). These narratives are both descriptive (capturing some real patterns from the agent's experience) and performative (the agent enacts helplessness, which produces outcomes consistent with helplessness expectations, which reinforces the identity). The theater increases over time because maintaining the identity lock requires ongoing narrative work — the agent must continuously reinterpret experiences that might contradict the helplessness frame.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications depending on the observer's position. The trapped agent's snare classification is their lived reality: they experience constant extraction, see no exit path, and have internalized helplessness as identity. The beneficiary's rope classification is equally authentic from their position: they genuinely coordinate with the victim's internalized constraint, invest minimal enforcement, and reap benefits from the arrangement. The scaffold perspective is not an alternative classification of the same static thing — it is an intervention perspective that identifies the constraint as temporary and solvable, which becomes self-fulfilling if the intervention succeeds. The piton perspective reveals how institutional systems maintain extraction long after the beneficiaries have stopped actively extracting. The mountain perspective is the false summit: it takes the prevalence of learned helplessness (a real psychological pattern) and naturalizes the specific structural conditions (aversive context, restricted information, identity fusion, institutional reinforcement) that produce and maintain it. The perspectival gap is greatest between the victim and the beneficiary: they experience opposite extraction flows from the same constraint, and their disagreement about whether exit is possible is not resolvable by appealing to objective exit paths (the victim cannot imagine using them) or by pointing to the victim's theoretical agency (the beneficiary denies any constraint exists). The gap is resolved only by recognizing that the binding mechanism is identity-locked: the agent's cognitive frame makes exit impossible even if structural escape is available.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for the trapped agent reflects the combination of powerlessness, identity_locked exit, and victim status. The agent has no structural escape from the original aversive context (or the escape is blocked by material barriers), and their identity has fused with the helpless role, making psychological exit impossible even if structural exit becomes available. This produces d ≈ 0.95 (near-total target of extraction) and f(d) ≈ 1.42, resulting in high experienced extractiveness chi. The beneficiary's directionality reflects institutional power and arbitrage exit: they can switch contexts, populations, or extraction mechanisms while maintaining their position. This produces d ≈ 0.05 (full beneficiary), f(d) ≈ -0.12, and negative chi (they experience net benefit, no extraction cost). The aware observer, with moderate power and constrained exit, experiences d ≈ 0.55-0.65 depending on their structural relationship: if they are embedded in the system maintaining helplessness (e.g., a therapist in a system that profits from long-term treatment), d is higher; if they are external advocates (e.g., peer from a different community), d is lower. The organized intervention, with organized power and mobile exit options, experiences d ≈ 0.40 (victim-leaning but with agency), producing moderate extraction from their perspective as they work against the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The learned helplessness trap resolves mandatrophy through perspectival multiplicity: all six types are legitimate structural readings depending on the observer's position. This is not a case of mislabeling or hidden coordination — it is a case where the constraint genuinely exhibits different structural properties from different positions. The mandatrophy resolution requires understanding that (a) the victim's snare is their objective structural position, (b) the beneficiary's rope is their objective structural position, (c) the piton perspective reveals institutional inertia in perpetuating the constraint, (d) the scaffold perspective identifies the conditions for escape, and (e) the mountain perspective is a false summit that naturalizes the contingent institutional arrangements. The false summit detection is key: the constraint appears natural ('learned helplessness is how brains work') until examined from the beneficiary or intervention perspectives, which reveal that the constraint's persistence depends on specific institutional arrangements (isolation from counter-evidence, identity-reinforcing narratives, restricted access to models of successful agency). Once these are changed, the 'natural' constraint dissolves. The mandatrophy analysis also clarifies why simple interventions (telling the agent 'you can escape') fail: the identity lock is not an information gap but a constitutive feature of how the agent understands themselves. Effective intervention requires either (1) creating experiences of successful agency that gradually reshape identity (scaffold perspective), (2) systemic change that removes the institutional reinforcement of helplessness narratives (tangled rope to rope transition), or (3) radical identity reframing through community or therapeutic support (identity_locked exit becoming mobile or arbitrage).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_structural_suppression,
    'Is the suppression measured in learned helplessness trap primarily internalized (agent-generated) or structural (externally maintained)?',
    'Longitudinal observation of suppression persistence post-escape: if suppression persists after removal from the original aversive context, indicates internalization; if suppression drops when external barriers removed, indicates structural suppression that was externally maintained.',
    'If internalized: measured suppression underestimates true binding strength — agent carries suppression even after escape, making re-entrapment risk high. If structural: escape removes suppression immediately. Classification remains Snare either way, but treatment and recovery trajectories differ significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_structural_suppression, empirical, 'Whether suppression is internalized or structurally maintained').

omega_variable(
    identity_lock_vs_constrained_exit,
    'Are the agent''s barriers to exit primarily identity-based (would require abandoning a self-concept) or material/economic (would require paying a high cost)?',
    'Identity intervention experiments: expose agent to counter-identity evidence and successful models of agency in similar contexts. If agent''s resistance drops when identity frame shifts, indicates identity_locked. If agent still cannot exit despite identity shift due to material barriers, indicates constrained or trapped.',
    'If identity_locked: cognitive/therapeutic interventions targeting identity reframing show promise. If material/constrained: interventions must address resource barriers. Misclassification leads to ineffective help (treating material poverty as identity problem, or treating trauma-based identity lock as mere resource scarcity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether exit barriers are identity-based or material').

omega_variable(
    primary_beneficiary_identification,
    'Who or what entity is the primary beneficiary of the learned helplessness state? Is it an individual abuser, an institutional system, structural inequality, or some combination?',
    'Causal analysis of benefit flow: trace who captures resources, status, or control when the trapped agent remains helpless. If removal of the trapped agent improves beneficiary''s position, indicates active extraction. If beneficiary''s position is unchanged, indicates institutional inertia (piton) rather than active snare.',
    'If individual abuser: intervention focuses on removal of beneficiary or escape support for victim. If institutional: intervention requires systemic reform or collective action to change the rules. If structural inequality: intervention requires addressing the background distribution of power/resources, not just the individual trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_beneficiary_identification, empirical, 'Identity and nature of primary beneficiary').

omega_variable(
    theater_mechanism_specification,
    'What specific narrative or performance maintains the learned helplessness trap alongside actual suppression? What theater sustains the agent''s identity-locked state?',
    'Narrative analysis: identify the stories the trapped agent has internalized (about their own capability, about the immutability of their situation, about ''how things are''). Distinguish genuine constraints from narratives about constraints. Measure the proportion of observed suppression that is narrative-dependent.',
    'If theater is high: narrative interventions and counter-storytelling (peer examples, identity reframing, exposure to successful models) can reduce suppression without material change. If theater is low: narrative alone is insufficient; structural barriers must be materially removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_mechanism_specification, empirical, 'The narratives and performances sustaining the trap').

omega_variable(
    cycle_recovery_trajectory,
    'After escaping learned helplessness, does the agent show recovery trajectory toward restored agency, or does the identity lock persist despite structural change?',
    'Longitudinal measurement of agentic behavior post-escape: does the agent gradually re-engage with control attempts? Do new experiences of successful action reshape identity? Timeline for confidence in agency to return.',
    'If recovery is slow or incomplete: indicates strong identity lock requiring therapeutic intervention beyond structural escape. If recovery is rapid: indicates suppression was primarily structural, now removed. Affects prognosis and required intervention depth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cycle_recovery_trajectory, empirical, 'Recovery trajectory post-escape from learned helplessness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(learned_helplessness_trap, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lh_tr_t0, learned_helplessness_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lh_tr_t2, learned_helplessness_trap, theater_ratio, 2, 0.45).
narrative_ontology:measurement(lh_tr_t4, learned_helplessness_trap, theater_ratio, 4, 0.55).
narrative_ontology:measurement(lh_tr_t6, learned_helplessness_trap, theater_ratio, 6, 0.58).
narrative_ontology:measurement(lh_tr_t8, learned_helplessness_trap, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(lh_be_t0, learned_helplessness_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lh_be_t2, learned_helplessness_trap, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(lh_be_t4, learned_helplessness_trap, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(lh_be_t6, learned_helplessness_trap, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(lh_be_t8, learned_helplessness_trap, base_extractiveness, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(learned_helplessness_trap, attachment_coordination).
narrative_ontology:boltzmann_floor_override(learned_helplessness_trap, 0.12).
narrative_ontology:affects_constraint(learned_helplessness_trap, structural_oppression).
narrative_ontology:affects_constraint(learned_helplessness_trap, transgenerational_trauma).
narrative_ontology:affects_constraint(learned_helplessness_trap, institutional_learned_helplessness).

% DUAL FORMULATION NOTE:
% Learned helplessness at individual level (this story: ε=0.68, identity-fused) is structurally distinct from learned helplessness at institutional or systemic level. Individual learned helplessness typically emerges from specific aversive histories and identity fusion. Institutional learned helplessness emerges when organizations or communities collectively internalize constraints, often inherited from historical oppression (colonialism, slavery, structural racism). Write separate stories for individual-psychological and structural-institutional versions and link via affects_constraints to show how individual traps can scale to systemic inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(learned_helplessness_trap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
