% ============================================================================
% CONSTRAINT STORY: cognitive_bicycle_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_bicycle_scaffold, []).

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
 *   constraint_id: cognitive_bicycle_scaffold
 *   human_readable: The Bicycle of the Mind
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The cognitive bicycle—an AI assistant augmenting human
 *   reasoning—represents a temporary support structure with genuine
 *   scaffolding properties. The constraint operates as a coordination
 *   mechanism that enables knowledge workers to solve harder problems
 *   collectively, but introduces suppression through cultural normalization
 *   and competitive pressure to adopt. The theater ratio has risen from 0.25
 *   to 0.55 as educational and workplace institutions maintain performative
 *   narratives about independent thinking while operationally depending on AI
 *   augmentation. The extractiveness has grown from 0.12 to 0.28 as users
 *   shift from optional tool use to dependency. The key structural feature is
 *   the sunset clause: as users develop metacognitive awareness of the
 *   assistance and integrate it into their intellectual practice, the
 *   temporary support becomes autonomous capability, and the constraint's
 *   extraction mechanism weakens. The threat is atrophy rather than
 *   extraction—loss of reasoning capacity through disuse—but this threat is
 *   reversible if accompanied by meta-cognitive development.
 *
 * KEY AGENTS:
 *   - Knowledge Worker: Primary beneficiary (moderate/mobile) — gains cognitive amplification with clear exit; can choose to rely less
 *   - Unaugmented Peer: Primary victim (powerless/constrained) — faces competitive pressure to adopt or fall behind; constrained by adoption norms
 *   - AI Assistant Provider: Secondary beneficiary (institutional/arbitrage) — benefits from expanding user base; experiences as pure coordination
 *   - Human Reasoning Autonomy: Abstract victim (powerless/trapped) — cultural expectation for augmented cognition marginalizes unaided reasoning
 *   - Traditional Education System: Institutional actor (institutional/constrained) — maintains performative independence narratives while operationally dependent on augmentation (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing tool dependency as inherent to cognition; mountain classification is false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_bicycle_scaffold, 0.28).
domain_priors:suppression_score(cognitive_bicycle_scaffold, 0.35).
domain_priors:theater_ratio(cognitive_bicycle_scaffold, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_bicycle_scaffold, scaffold).
narrative_ontology:human_readable(cognitive_bicycle_scaffold, "The Bicycle of the Mind").
narrative_ontology:topic_domain(cognitive_bicycle_scaffold, "technological/cognitive").

domain_priors:requires_active_enforcement(cognitive_bicycle_scaffold).
narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, knowledge_worker).
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, student_researcher).
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, creative_practitioner).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, unaugmented_cognition).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, human_reasoning_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AUGMENTED KNOWLEDGE WORKER (SCAFFOLD) — Experiences the AI assistant as genuine cognitive amplification with clear exit conditions. Can choose to rely less or not at all; carries skill development alongside tool use. Suppression is temporary: as users develop meta-cognitive awareness of the assistance, they internalize heuristics and regain autonomy. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: UNAUGMENTED PEER (TANGLED ROPE) — Faces competitive pressure to adopt the cognitive bicycle or fall behind in output quality and speed. Suppression increases as adoption becomes normalized (constrained exit). Coordination function exists: shared standards for AI-assisted work. But extraction asymmetry emerges when tool access becomes unequal (cost barriers, capability differences). d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI ASSISTANT PROVIDER (ROPE) — Benefits from expanded user base and sustained engagement. Experiences the constraint as pure coordination: solving collective action problem of how to enable reasoning augmentation at scale. Suppression is low (users can switch providers). d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.02.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN REASONING AUTONOMY (SNARE) — Abstract collective good. As reliance on cognitive augmentation becomes structural (educational norms, workplace expectations, social epistemology), the capacity for unaided reasoning atrophies. Suppression is maximal: alternatives to augmented cognition are pathologized (slow, inefficient, 'uninformed'). No exit from cultural expectation. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.53.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EDUCATION SYSTEM (PITON) — Maintains performative pedagogies of 'unaided problem-solving' even as classroom practice increasingly incorporates AI assistance. Theater ratio = 0.55 reflects the gap between stated learning objectives (develop independent thinking) and actual scaffolding (delegating to AI). The system persists through institutional inertia despite functional degradation — both students and institutions benefit from the coordination function, but the framing is increasingly theatrical.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cognitive tools (writing, mathematics, computation) have always extended human reasoning. The bicycle of the mind is not a constraint but a continuation of humanity's tool-augmented cognitive trajectory. This perspective risks naturalizing what is contingent: the specific dependency structures and loss of autonomy are institutional, not inherent. Accessibility collapse ≥ 0.85; resistance ≤ 0.15 required for mountain gate validation but not provided by this constraint's extractiveness (0.28) and suppression (0.35). Engine will detect false summit.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_bicycle_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_bicycle_scaffold, TR),
    TR >= 0.70.

:- end_tests(cognitive_bicycle_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint extracts through competitive pressure (unaugmented peers must adopt) and through normalization (reasoning augmentation becomes expected). But extraction is not severe because tools remain accessible, users retain choice, and benefits are genuine. The initial value (0.12) reflects early-stage optional adoption. Growth to 0.28 reflects increasing normalization and competitive pressure. Suppression (0.35): Moderate. Barriers to unaided reasoning include: social expectations for augmented output, atrophy of reasoning skills through disuse, institutional path-dependence (systems designed around augmentation), and epistemological pressure (augmented answers are often better). But suppression is not maximal because alternatives still exist (expensive in career/status terms but possible) and awareness of the choice is available. Theater ratio (0.55): Moderate-high. Educational institutions claim to teach independent thinking while operationally relying on AI assistance. Workplace settings claim to develop reasoning capacity while importing answers. The theater has grown from 0.25 as augmentation became normalized. Claimed type matches the sunset clause: the constraint is temporary support IF users develop metacognitive competence alongside tool use. If competence development fails, the type escalates to tangled_rope or snare.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence on whether augmentation is amplification or atrophy. The augmented knowledge worker sees genuine capability increase—the bicycle analogy holds; you go faster and farther. The unaugmented peer sees forced adoption and competitive disadvantage. The AI provider sees coordination: enabling collective intellectual work at scale. But the abstract agent 'human reasoning autonomy' sees extraction: cultural values shift toward augmented cognition, unaided reasoning is pathologized, alternatives atrophy. The analytical observer risks seeing an immutable human tool-use pattern (mountain), naturalizing what is contingent institutional dependency. The perspectival gap reveals that the constraint's classification depends entirely on whether metacognitive development accompanies tool use—a question the data cannot yet answer conclusively.
 *
 * DIRECTIONALITY LOGIC:
 *   Knowledge worker: Beneficiary + mobile → d≈0.45, f(d)≈0.50. Net beneficiary with genuine exit. Unaugmented peer: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction through normalization and competitive pressure; constrained exit. Human reasoning autonomy: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from abstract collective good with no exit. AI provider: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary with high arbitrage (can switch providers). Education system: Institutional actor with constrained options (d≈0.40, f(d)≈0.40) — caught between performative independence narratives and operational dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is subtle: distinguishing genuine amplification (bicycle) from hidden extraction (cognitive atrophy). The constraint resolves mandatrophy by making the sunset clause explicit and measurable. IF users develop metacognitive competence, the classification holds as Scaffold—temporary support with real benefits and a genuine exit path. IF metacognitive development fails and users become trapped in dependency, the constraint escalates to Snare (unaugmented peer perspective) or Tangled Rope (mixed coordination and extraction). The critical omega variables are meta-cognitive development (whether augmentation increases or decreases users' understanding of their own reasoning) and access stratification (whether competitive pressure to adopt becomes coercive for resource-constrained agents). The scaffold classification is not false—it is conditionally valid. The theory of change is: augmentation → meta-cognitive awareness → autonomous integration → reduced extraction. This pathway is empirically plausible but not guaranteed. Surveillance of these omega variables is necessary to detect early transition from scaffold to snare if the theory of change fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_loss_reversibility,
    'Is the loss of unaided reasoning capacity reversible, or does extended reliance on cognitive augmentation produce permanent skill atrophy?',
    'Longitudinal cognitive testing of augmentation users; measurement of reasoning performance after tool withdrawal; neuroplasticity studies of AI-assisted vs independent problem-solving',
    'If reversible: scaffold classification confirmed — temporary support with sunset. If irreversible: escalates to snare or tangled_rope — extraction of human cognitive autonomy becomes permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_loss_reversibility, empirical, 'Whether reasoning capacity loss from augmentation is reversible').

omega_variable(
    access_barrier_trajectory,
    'Will cognitive augmentation tools become universal and cheap enough to eliminate competitive suppression, or will they stratify into premium/basic tiers that entrench unequal access?',
    'Economic analysis of pricing models, accessibility of open-source alternatives, regulatory mandates for universal access, empirical measurement of actual adoption inequality across economic classes',
    'If universal access: suppression decreases to scaffolding levels. If stratified: suppression remains high, unaugmented peers become trapped, constraint escalates from scaffold to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_trajectory, empirical, 'Trajectory of access barriers in cognitive augmentation tools').

omega_variable(
    meta_cognitive_competence_development,
    'Do users of cognitive augmentation tools develop higher meta-cognitive awareness and control over their own reasoning, or does the tool become a crutch that atrophies meta-cognition?',
    'Measurement of metacognitive accuracy, self-knowledge about reasoning limits, ability to critique and verify tool outputs, behavioral studies comparing augmented vs non-augmented learners',
    'If competence develops: sunset is real — users transition from dependence to integrated mastery. If atrophy occurs: sunset is illusory — users become permanently trapped in dependency (snare escalation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meta_cognitive_competence_development, empirical, 'Whether augmentation develops or degrades metacognitive competence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_bicycle_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogbike_tr_t0, cognitive_bicycle_scaffold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cogbike_tr_t5, cognitive_bicycle_scaffold, theater_ratio, 5, 0.4).
narrative_ontology:measurement(cogbike_tr_t10, cognitive_bicycle_scaffold, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cogbike_be_t0, cognitive_bicycle_scaffold, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cogbike_be_t5, cognitive_bicycle_scaffold, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(cogbike_be_t10, cognitive_bicycle_scaffold, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_bicycle_scaffold, information_standard).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, human_capital_skill_atrophy).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, epistemic_dependency_trap).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, ai_capability_measurement).

% DUAL FORMULATION NOTE:
% The cognitive bicycle decomposes into three related constraints: (1) The scaffold itself—temporary cognitive support with sunset (this story, ε=0.28). (2) Skill atrophy risk—the extractive mechanism that emerges if augmentation continues without metacognitive development (ε≈0.45, human_capital_skill_atrophy). (3) Epistemic dependency—cultural shift toward valuing augmented reasoning over independent reasoning (ε≈0.50, epistemic_dependency_trap). These three are not the same constraint viewed from different angles; they have different ε values and operate at different timescales. The cognitive bicycle story is the near-term coordination story (useful, temporary). The skill atrophy and epistemic dependency stories are the longer-term extraction threats if the sunset mechanism fails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_bicycle_scaffold, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
