% ============================================================================
% CONSTRAINT STORY: friction_as_intervention_medium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_friction_as_intervention_medium, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: friction_as_intervention_medium
 *   human_readable: Friction as Intervention Medium in Digital Habit Modification
 *   domain: technology_governance/behavioral_psychology/social_infrastructure
 *
 * SUMMARY:
 *   Friction-based interventions in digital habit modification introduce
 *   physical, temporal, or cognitive barriers between user and device to
 *   interrupt automatic habit loops. The structural delta is that friction
 *   works through architectural constraint rather than willpower: a Yondr
 *   pouch physically prevents phone access; an NFC tap requirement (Brick)
 *   forces deliberate action; moving a charging station to a common area
 *   increases activation energy for nighttime scrolling. Empirical evidence
 *   shows genuine coordination benefits: Yondr deployments in schools
 *   correlate with 15% grade improvements and 44% reductions in behavioral
 *   referrals; Brick users report sustained usage reduction without the
 *   willpower depletion of app-based timers. The constraint is downstream of
 *   attention_as_capturable_resource (mountain) — friction interventions
 *   exist because attention capture is a structural feature of digital
 *   platforms. Friction does not change the underlying attention economics;
 *   it changes the user's interface to those economics. The constraint
 *   exhibits low extraction (0.18) because friction primarily serves user
 *   goals (self-regulation, academic performance, family harmony) rather than
 *   extracting value from users. The modest extraction reflects coordination
 *   overhead (cost of friction tools, inconvenience of reduced spontaneous
 *   access) and potential displacement effects (users routing around friction
 *   by switching platforms). Theater ratio is low (0.15) because friction
 *   interventions have measurable behavioral outcomes — they are not
 *   performative. The slight increase over the interval reflects early
 *   adopters (high motivation, good tool-goal fit) being joined by later
 *   adopters (lower motivation, poorer fit), increasing the proportion of
 *   friction deployments that are theater rather than function.
 *
 * KEY AGENTS:
 *   - Self-Regulating User: Primary beneficiary (moderate/mobile) — voluntarily adopts friction to solve intertemporal coordination problem between present and future self
 *   - Educational Institution: Primary beneficiary (institutional/arbitrage) — uses friction to restore classroom attention commons; Yondr data shows genuine coordination benefit
 *   - Adolescent Under School Policy: Mixed position (moderate/constrained) — experiences friction as temporary scaffold; constrained exit during school but benefits from improved outcomes
 *   - Parent Implementing Household Friction: Beneficiary (moderate/mobile) — uses friction to mediate parent-child device negotiation; reduces household conflict
 *   - Digital Wellbeing Advocacy Coalition: Beneficiary (organized/mobile) — promotes friction as coordination alternative to willpower or regulation
 *   - Analytical Observer: Sees friction effectiveness as mountain (universal property of habit loop architecture)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(friction_as_intervention_medium, 0.18).
domain_priors:suppression_score(friction_as_intervention_medium, 0.22).
domain_priors:theater_ratio(friction_as_intervention_medium, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(friction_as_intervention_medium, extractiveness, 0.18).
narrative_ontology:constraint_metric(friction_as_intervention_medium, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(friction_as_intervention_medium, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(friction_as_intervention_medium, rope).
narrative_ontology:human_readable(friction_as_intervention_medium, "Friction as Intervention Medium in Digital Habit Modification").
narrative_ontology:topic_domain(friction_as_intervention_medium, "technology_governance/behavioral_psychology/social_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(friction_as_intervention_medium, users_seeking_self_regulation).
narrative_ontology:constraint_beneficiary(friction_as_intervention_medium, educational_institutions).
narrative_ontology:constraint_beneficiary(friction_as_intervention_medium, parents_and_guardians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SELF-REGULATING USER (ROPE) — User voluntarily adopts friction tools (Brick, Yondr pouch, app timers) to interrupt automatic habit loops. Experiences the constraint as pure coordination: the tool solves the collective action problem between present self (wants to check phone) and future self (wants sustained attention). Low extraction — the friction serves the user's own goals. Mobile exit options — can remove friction at any time.
constraint_indexing:constraint_classification(friction_as_intervention_medium, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (ROPE) — School adopts Yondr pouches or device-free policies to restore classroom attention commons. Experiences the constraint as coordination mechanism: solves the multi-agent problem where individual students defecting (checking phones) degrades learning environment for all. Yondr data shows 15% grade improvement and 44% behavioral referral decrease — genuine coordination benefit. Arbitrage exit — institution can discontinue policy if ineffective.
constraint_indexing:constraint_classification(friction_as_intervention_medium, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADOLESCENT UNDER SCHOOL POLICY (SCAFFOLD) — Student subject to mandatory Yondr pouch policy experiences friction as temporary external support. Constrained exit during school hours but mobile outside. Sees the constraint as scaffold: external structure compensating for underdeveloped prefrontal self-regulation, with implicit sunset as executive function matures. Low extraction because the friction genuinely improves outcomes (grades, peer interaction) that the student values, even if compliance is involuntary in the moment.
constraint_indexing:constraint_classification(friction_as_intervention_medium, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: PARENT IMPLEMENTING HOUSEHOLD FRICTION (ROPE) — Parent uses charging station in common area, NFC tap requirements, or physical device separation to manage child's screen time. Experiences as coordination: the friction tool mediates the parent-child negotiation over device access, converting a willpower contest into a structural constraint both parties can reference. Low extraction — the friction reduces household conflict while preserving parental authority. Mobile exit — can adjust friction level as needed.
constraint_indexing:constraint_classification(friction_as_intervention_medium, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical perspective, friction's effectiveness derives from an immutable property of habit formation: the cue-routine-reward loop requires low activation energy to execute automatically. Introducing physical or temporal distance between cue and routine breaks automaticity by forcing deliberate action. This is not a policy choice but a structural feature of how procedural memory works. The constraint is a mountain because it reflects the underlying neuroscience of habit architecture — any intervention that increases activation energy will disrupt automatic execution, regardless of cultural context or institutional arrangement.
constraint_indexing:constraint_classification(friction_as_intervention_medium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DIGITAL WELLBEING ADVOCACY COALITION (ROPE) — Organized groups (Center for Humane Technology, Common Sense Media, Wait Until 8th) promote friction-based interventions as alternative to willpower-dependent abstinence or platform regulation. Experience the constraint as coordination mechanism: friction tools enable collective action (school-wide policies, community norms) that individual willpower cannot sustain. Low extraction — the coalition's goals align with user wellbeing. Mobile exit — advocacy groups can shift strategy if friction proves ineffective or if platform design changes reduce need for external friction.
constraint_indexing:constraint_classification(friction_as_intervention_medium, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(friction_as_intervention_medium_tests).
:- end_tests(friction_as_intervention_medium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Friction interventions primarily serve user goals (self-regulation, academic performance, reduced family conflict) rather than extracting value from users. The modest extraction reflects genuine coordination overhead: friction tools have monetary cost (Yondr pouches, Brick devices), impose inconvenience (reduced spontaneous access, need to plan device usage), and may create displacement effects (users switching to less-restricted platforms rather than reducing total usage). The extraction is not zero because friction is not frictionless — the intervention imposes real costs even when it works as intended. Suppression (0.22): Low. Exit options are generally mobile for voluntary adopters and constrained (but not trapped) for those subject to institutional mandates. Students can exit Yondr policies by changing schools or graduating; children can exit parental friction by aging out or negotiating. The suppression is non-zero because institutional friction (school policies, parental controls) does constrain choice in the moment, and because effective friction must be somewhat difficult to circumvent (otherwise it would not interrupt habit loops). Theater ratio (0.15): Low. Friction interventions have measurable behavioral outcomes (Yondr grade improvements, Brick usage reduction) and work through a clear structural mechanism (increasing activation energy for automatic behaviors). The theater component reflects friction deployments that are performative rather than functional: app-based screen time limits that users immediately override, charging stations placed in accessible locations that do not actually increase friction, or institutional policies adopted for signaling purposes without enforcement. The slight increase over the interval (0.10 → 0.15) reflects diffusion from early adopters (high motivation, good tool-goal fit) to later adopters (lower motivation, poorer fit), increasing the proportion of deployments that are theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits minimal perspectival gap because all agents experience it as coordination (rope) or temporary support (scaffold), with the analytical observer seeing the underlying mechanism as mountain. There is no snare or tangled_rope perspective because friction interventions do not extract asymmetrically — they impose costs (inconvenience, reduced spontaneous access) but those costs are borne by the same agents who receive the benefits (improved self-regulation, better academic outcomes, reduced family conflict). The gap between the user perspectives (rope/scaffold) and the analytical perspective (mountain) reflects the distinction between the intervention (policy choice, rope) and the mechanism (habit loop architecture, mountain). Users experience friction as a coordination tool they can adopt or reject; the analytical observer sees that friction works because of an immutable property of how habits form. Both perspectives are correct — the intervention is contingent (rope) even though its effectiveness derives from necessity (mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   All primary agents are beneficiaries of the friction constraint — it solves coordination problems they face (intertemporal self-coordination, classroom attention commons, parent-child device negotiation). The constraint has no clear victim group because friction is generally voluntary or applied by agents with legitimate authority (schools, parents) to agents who benefit from the intervention (students' grades improve, family conflict decreases). The modest extraction (0.18) reflects coordination overhead and potential displacement effects, not asymmetric extraction from a victim group. The analytical observer sees friction effectiveness as a mountain — a universal property of habit loop architecture — because the mechanism (increasing activation energy disrupts automaticity) is invariant across cultural and institutional contexts. This is not naturalization of a contingent arrangement; it is recognition of an actual structural feature of procedural memory. The constraint is downstream of attention_as_capturable_resource (mountain) but is itself a rope (coordination mechanism) rather than a mountain, because the decision to deploy friction is a policy choice even though friction's effectiveness derives from immutable neuroscience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination mechanisms (rope) can be built on top of immutable structural features (mountain). Friction interventions are ropes — policy choices that solve coordination problems — but their effectiveness derives from a mountain (the neuroscience of habit formation). The mandatrophy risk is naturalizing the intervention: claiming that friction is the only way to address digital habit formation because it is grounded in neuroscience. This would be a false summit — the underlying mechanism is a mountain, but the intervention is one of many possible ropes. Alternative coordination mechanisms (platform design changes, social norm shifts, economic incentives) could address the same problem through different structural pathways. The constraint is a rope because agents can choose whether to deploy friction, even though friction's effectiveness (when deployed) is determined by immutable properties of procedural memory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_vs_reduction,
    'Does friction reduce total device usage or merely displace it to other platforms/times?',
    'Longitudinal tracking of total screen time across all devices and platforms before/after friction intervention; control for substitution effects',
    'If displacement dominates: friction is coordination theater (users route around friction without reducing total usage). If genuine reduction: friction is effective coordination tool.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_vs_reduction, empirical, 'Whether friction reduces usage or displaces it').

omega_variable(
    friction_habituation_timeline,
    'How quickly do users habituate to friction interventions, rendering them ineffective?',
    'Time-series analysis of intervention effectiveness; identification of decay curves for different friction types (physical barriers vs temporal delays vs cognitive load)',
    'If habituation is rapid (<3 months): friction is temporary scaffold requiring escalation. If habituation is slow (>12 months): friction is durable coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_habituation_timeline, empirical, 'Timeline for friction habituation and effectiveness decay').

omega_variable(
    voluntary_vs_imposed_effectiveness,
    'Does friction effectiveness differ between voluntary adoption (self-imposed) and institutional mandate (school policy, parental control)?',
    'Comparison of outcome metrics (usage reduction, academic performance, wellbeing indicators) between voluntary friction adopters and those subject to institutional mandates; control for selection effects',
    'If voluntary adoption is more effective: friction is coordination tool requiring buy-in. If imposed friction is equally effective: friction works through structural mechanism independent of user motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_imposed_effectiveness, empirical, 'Whether friction effectiveness depends on voluntary adoption').

omega_variable(
    socioeconomic_access_asymmetry,
    'Do friction interventions require resources (devices, pouches, institutional support) that create access asymmetries?',
    'Analysis of friction tool adoption rates and effectiveness across socioeconomic strata; identification of cost barriers to implementation',
    'If significant access barriers exist: friction interventions may increase digital wellbeing inequality, with low-SES users unable to access coordination tools available to high-SES users. If barriers are minimal: friction is broadly accessible coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(socioeconomic_access_asymmetry, empirical, 'Whether friction tools create socioeconomic access barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(friction_as_intervention_medium, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(friction_tr_t0, friction_as_intervention_medium, theater_ratio, 0, 0.1).
narrative_ontology:measurement(friction_tr_t3, friction_as_intervention_medium, theater_ratio, 3, 0.12).
narrative_ontology:measurement(friction_tr_t6, friction_as_intervention_medium, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(friction_be_t0, friction_as_intervention_medium, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(friction_be_t3, friction_as_intervention_medium, base_extractiveness, 3, 0.17).
narrative_ontology:measurement(friction_be_t6, friction_as_intervention_medium, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(friction_as_intervention_medium, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of attention_as_capturable_resource (mountain). The upstream constraint establishes that attention capture is a structural feature of digital platforms; this constraint addresses how users can coordinate to protect their attention against capture. The two constraints have different extractiveness values because they represent different structural layers: attention_as_capturable_resource has near-zero extraction (it is a natural law of information economics), while friction_as_intervention_medium has modest extraction (it is a coordination mechanism with overhead costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
