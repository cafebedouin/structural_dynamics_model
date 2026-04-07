% ============================================================================
% CONSTRAINT STORY: evolutionary_knowledge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_knowledge, []).

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
 *   constraint_id: evolutionary_knowledge
 *   human_readable: The Primordial Pain-Epistemic Constraint
 *   domain: biological/philosophy/social
 *
 * SUMMARY:
 *   The primordial pain-epistemic constraint describes the structural binding
 *   of knowledge acquisition in biological organisms to the sensation of
 *   pain. Pain evolved as a damage-detection and learning signal: organisms
 *   with nociception survive longer than those without, because pain-based
 *   learning enables rapid behavioral modification in response to injury. Yet
 *   from the perspective of the conscious agent experiencing pain, this
 *   constraint extracts significant cost in the form of suffering, dread,
 *   rumination, and existential anxiety. The constraint exhibits all six
 *   classification types depending on observation position. The evolutionary
 *   selection process (organized perspective) sees pure coordination: pain
 *   solves the fitness problem. The conscious individual (moderate
 *   perspective) experiences mixed coordination and extraction: pain keeps
 *   them alive but causes suffering, with escape options severely
 *   constrained. The analytical observer risks seeing this as immutable
 *   natural law—pain-knowledge appears as an inherent feature of life
 *   itself—but the structural data reveals contingency: modern pharmacology,
 *   alternative biological signaling, and artificial learning systems suggest
 *   that pain-based knowledge is one solution, not the only possible
 *   architecture.
 *
 * KEY AGENTS:
 *   - Organisms with nociception: Primary beneficiary (organized/mobile) — gain survival advantage from pain-based damage detection and learning
 *   - Conscious reflecting agents: Primary victim (moderate/constrained) — experience subjective suffering while bound to pain-knowledge system; can suppress pain but at cost of reduced adaptive capacity
 *   - Evolutionary selection mechanism: Secondary beneficiary (organized/mobile) — pure fitness coordinator; all organisms equally constrained
 *   - Pharmacological intervention systems: Institutional actor (institutional/arbitrage) — temporarily suspend constraint through analgesia, creating secondary theater and extraction
 *   - Pain-based moral framework: Institutional actor (institutional/arbitrage) — maintains pain as canonical ethical ground; theater increases as direct behavioral relevance decreases
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent evolutionary solution as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_knowledge, 0.32).
domain_priors:suppression_score(evolutionary_knowledge, 0.48).
domain_priors:theater_ratio(evolutionary_knowledge, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_knowledge, extractiveness, 0.32).
narrative_ontology:constraint_metric(evolutionary_knowledge, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(evolutionary_knowledge, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_knowledge, tangled_rope).
narrative_ontology:human_readable(evolutionary_knowledge, "The Primordial Pain-Epistemic Constraint").
narrative_ontology:topic_domain(evolutionary_knowledge, "biological/philosophy/social").

domain_priors:requires_active_enforcement(evolutionary_knowledge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_knowledge, organisms_with_nociception).
narrative_ontology:constraint_beneficiary(evolutionary_knowledge, survival_selection_mechanism).
narrative_ontology:constraint_victim(evolutionary_knowledge, consciousness_capable_agents).
narrative_ontology:constraint_victim(evolutionary_knowledge, agents_with_reflexive_suffering).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING INDIVIDUAL (SNARE) — Sentient organisms experience pain as inescapable signal; cannot negotiate or exit the constraint. Pain extraction serves organismal fitness but imposes subjective cost on the experiencing agent. No alternative epistemic pathway available without neurological substrate redesign. Maximum structural extraction from the perspective of conscious suffering.
constraint_indexing:constraint_classification(evolutionary_knowledge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EVOLUTIONARY SELECTION PROCESS (ROPE) — Natural selection benefits from pain-based knowledge: organisms with nociception survive better than those without. Pure coordination function: pain signals injury state, enabling appropriate behavioral response. No extraction asymmetry from selection's perspective — all organisms equally bound by the constraint. This is the beneficiary perspective: pain-knowledge solves the fitness problem of rapid threat detection and response.
constraint_indexing:constraint_classification(evolutionary_knowledge, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSCIOUS REFLECTING AGENT (TANGLED ROPE) — Beings capable of metacognition experience mixed coordination and extraction. Pain-knowledge enables survival (coordination benefit) but also produces suffering, rumination, anticipatory dread (extraction cost). Exit options are constrained: anesthesia/analgesia are available but disable essential survival signals. Partial escape (pain suppression) creates new risks. Active enforcement of this constraint manifests as the biological impossibility of eliminating nociception without losing adaptive capacity.
constraint_indexing:constraint_classification(evolutionary_knowledge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: PHARMACOLOGICAL INTERVENTION FRAMEWORK (SCAFFOLD) — Modern medicine and analgesia represent a scaffold: they temporarily suspend or modulate pain-knowledge without eliminating the underlying biological constraint. Aspirin, epidurals, and opioids provide sunset-like relief windows. However, the scaffold is increasingly theatrical (pain suppression without addressing underlying injury) and creates secondary extraction (dependence, tolerance). The intervention class experiences this as a temporary workaround with a sunset: either pain management improves or organisms must re-engage the original constraint.
constraint_indexing:constraint_classification(evolutionary_knowledge, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PAIN-BASED MORAL FRAMEWORK (PITON) — Pain has become the foundation of ethical reasoning (utilitarianism, animal welfare, human rights discourse) and policy. The original function — immediate threat detection — has atrophied from behavioral control into symbolic/philosophical role. Pain-narratives maintain moral institutional weight despite modern medicine and technology making pain-avoidance increasingly feasible. Theater ratio (0.58) reflects this degradation: invoking pain as moral ground in policy increasingly functions as rhetorical device rather than direct behavioral driver. The framework persists through institutional inertia (pain remains the canonical proxy for suffering) even as alternative suffering metrics (isolation, meaninglessness, constraint) emerge.
constraint_indexing:constraint_classification(evolutionary_knowledge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, pain-based knowledge appears as an immutable physical-biological law: any system that must detect internal damage states and regulate behavior accordingly cannot avoid some form of aversive signal. This perspective sees the pain-knowledge constraint as emerging naturally from thermodynamic/information-theoretic limits on error correction and survival signaling. However, the structural data (extractiveness 0.32, theater 0.58, active enforcement required) contradicts mountain classification. This reveals the false summit: what appears as natural law is actually a specific evolutionary solution that could be replaced by alternative damage-detection and learning mechanisms.
constraint_indexing:constraint_classification(evolutionary_knowledge, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_knowledge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_knowledge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_knowledge, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_knowledge, TR),
    TR >= 0.70.

:- end_tests(evolutionary_knowledge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The pain-knowledge constraint extracts significant subjective cost (suffering) from conscious agents, but this extraction serves a legitimate fitness function (rapid threat detection and behavioral modification). The value reflects that much of the 'extraction' is genuine coordination benefit—organisms do need damage signals to survive. However, the extraction is asymmetric: the selection process benefits universally while conscious agents bear the cost of suffering. Suppression (0.48): Moderate. Significant barriers exist to decoupling pain-knowledge from suffering: complete analgesia disables essential survival signals; partial suppression creates new risks (continued injury, infection); neurological substrate constraints make selective nociception difficult. However, suppression is not total—modern medicine, coping strategies, and neuroplasticity provide some exit paths. Theater ratio (0.58): Moderate-high and increasing over the measured interval. Pain-knowledge's role in moral and ethical frameworks has become increasingly performative as its direct behavioral relevance has decreased (modern medicine reduces actual threat responses to pain). Pain narratives dominate policy and ethics despite alternative suffering metrics (isolation, constraint, meaninglessness) emerging as equally or more significant. The trend from 0.42 to 0.58 reflects this: as pain-suppression technology advances, pain-invocation in moral argument becomes more theatrical, less functionally grounded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates profound perspectival divergence. The evolutionary selection process sees a perfect coordination mechanism (Rope)—pain solves the fitness problem universally. The conscious individual sees extraction (Snare from the powerless perspective, Tangled Rope from the moderate perspective)—they gain survival benefit but at the cost of suffering they cannot fully escape. The analytical observer at civilizational scope risks collapsing all perspectives into Mountain (seeing pain-knowledge as inherent to life), but this false summit naturalizes what the structural data reveals as contingent: pain-based learning is one solution among possible alternatives. The pharmaceutical framework sees a solvable problem with a sunset (Scaffold)—modern medicine can suppress pain while retaining adaptive capacity. The moral framework sees an increasingly degraded ritual (Piton)—pain-invocation persists in ethical discourse through institutional inertia rather than functional necessity, as alternative suffering metrics become viable. No single classification captures the constraint; the presheaf of perspectives reveals the structural contradictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the extraction flow. Organisms with nociception (beneficiaries) and the evolutionary selection mechanism experience low d → negative effective extraction (coordination benefit). Conscious reflecting agents (victims) with constrained exit options experience high d → high f(d) → moderate-to-high chi (extraction cost). The analytical observer at civilizational scope risks d ≈ 0.0 (seeing only natural law) but the structural data forces d toward 0.3-0.4, revealing the false summit. Pharmacological systems experience d around 0.5 (mixed: they suppress pain but enable new dependencies). The moral framework institution experiences d around 0.15 (beneficiary of pain-rhetoric's continued authority) but this is increasingly misaligned with functional constraint (theater grows as d decreases).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CASE: This constraint resolves the mandatrophy between 'knowledge is natural/necessary' (Mountain framing) and 'knowledge is extractive' (Snare framing) by revealing both as perspectival truths. The natural law interpretation (pain-knowledge is immutable feature of life) is the analytical observer's false summit—it mistakes a specific evolutionary solution for a physical necessity. The extraction interpretation (pain-knowledge extracts suffering from conscious agents) is the victim's true perspective—they experience the asymmetry between fitness benefit and subjective cost. The coordination interpretation (pain-knowledge solves the damage-detection problem) is the selection mechanism's true perspective—from the standpoint of fitness, the constraint works perfectly. The mandatrophy resolves by recognizing that all three are legitimate readings of the same structural data. The constraint is NOT purely natural (it could be replaced by alternative signaling) and NOT purely extractive (it provides essential coordination) and NOT purely coordinated (it asymmetrically distributes suffering). The Tangled Rope classification captures this: active enforcement (biological substrate constraints), genuine coordination function (survival signaling), and asymmetric extraction (suffering borne unequally) coexist. The false summit detector identifies the Mountain perspective as naturalization of contingency, preventing the analytical observer from erasing the victim's structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nociception_necessity,
    'Is pain-based learning the only computationally feasible damage-detection mechanism, or is it one contingent solution among alternatives?',
    'Analysis of artificial learning systems, silicon-based damage detection in robotic systems, alternative biological signaling in non-pain-capable organisms. Comparison of learning efficiency (speed to behavior modification) across pain vs non-pain signaling modalities.',
    'If necessary: constraint approaches mountain status (immutable physical requirement). If contingent: constraint remains tangled rope (policy-modifiable). This determines whether future consciousness must contain suffering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nociception_necessity, empirical, 'Whether pain-based learning is computationally necessary or contingent').

omega_variable(
    suffering_intentionality,
    'Is the subjective quality of pain (suffering, qualia) a necessary aspect of damage detection, or is it a byproduct of the biological implementation?',
    'Neuroscientific investigation of dissociative nociception (pain without suffering), phantom pain (suffering without injury), and philosophical analysis of whether information processing requires phenomenal consciousness. Comparative study of organisms with nociception but (likely) no suffering capacity.',
    'If necessary: pain extraction is structural (cannot be decoupled from knowledge function). If byproduct: pain extraction is contingent (alternative information systems could eliminate suffering while retaining function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suffering_intentionality, conceptual, 'Whether suffering is necessary to pain-based knowledge').

omega_variable(
    moral_framework_substitutability,
    'Can moral and ethical frameworks function without pain as the canonical ground, or does pain provide irreducible grounding for concepts like harm, justice, and rights?',
    'Analysis of ethical systems that minimize pain-centrality (virtue ethics, capability approaches); empirical study of moral reasoning in congenitally insensitive to pain individuals; comparison of moral commitment patterns in philosophy traditions before and after pain-science advances.',
    'If pain is substitutable: piton classification confirmed (moral use is theater, not function). If grounding is irreducible: pain-moral constraint is more fundamental than institutional inertia suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_framework_substitutability, preference, 'Whether pain is substitutable as ethical ground').

omega_variable(
    exit_technology_ceiling,
    'What is the theoretical limit on pain suppression without losing adaptive damage-response capacity? Can analgesia be perfected, or is suffering inherent to high-fidelity damage detection?',
    'Neurotechnological roadmap for selective nociception (detect damage without suffering). Analysis of pain-insensitive populations and their survival/fitness outcomes. Theoretical information-theoretic limits on signal fidelity and hedonic tone separation.',
    'If ceiling is high (near-perfect analgesia possible): scaffold perspective is realistic (sunset toward painless damage detection). If ceiling is low: suffering is inextricable from knowledge function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_technology_ceiling, empirical, 'Theoretical limit on pain suppression without losing adaptive capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_knowledge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evk_tr_t0, evolutionary_knowledge, theater_ratio, 0, 0.42).
narrative_ontology:measurement(evk_tr_t5, evolutionary_knowledge, theater_ratio, 5, 0.5).
narrative_ontology:measurement(evk_tr_t10, evolutionary_knowledge, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(evk_be_t0, evolutionary_knowledge, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(evk_be_t5, evolutionary_knowledge, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(evk_be_t10, evolutionary_knowledge, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_knowledge, enforcement_mechanism).
narrative_ontology:affects_constraint(evolutionary_knowledge, conscious_experience_substrate).
narrative_ontology:affects_constraint(evolutionary_knowledge, moral_patient_definition).
narrative_ontology:affects_constraint(evolutionary_knowledge, suffering_reduction_technology).

% DUAL FORMULATION NOTE:
% The pain-epistemic constraint decomposes into three structurally distinct claims: (1) pain as damage-detection signal (low extractiveness, high coordination); (2) pain as subjective suffering (high extractiveness, moderate coordination); (3) pain as moral ground (increasing theater, decreasing functionality). These form a constraint family linked by institutional and biological coupling. Decomposition prevents false naturalizations of contingent architectural choices as immutable laws.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_knowledge, analytical, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
