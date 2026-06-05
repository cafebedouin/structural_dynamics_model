% ============================================================================
% CONSTRAINT STORY: evolutionary_knowledge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: evolutionary_knowledge
 *   human_readable: The Primordial Pain-Epistemic Constraint
 *   domain: biological/philosophy/social
 *
 * SUMMARY:
 *   The primordial pain-epistemic constraint describes a fundamental
 *   structural relationship between conscious suffering and knowledge
 *   acquisition rooted in evolutionary biology. The constraint states that
 *   knowledge, as an evolved biological adaptation, is necessarily grounded
 *   in the sensation of pain — organisms learn what is harmful through direct
 *   nociceptive experience. This creates a structural asymmetry: pain-bearing
 *   organisms acquire knowledge through suffering, while abstract knowledge
 *   systems (mathematics, logic, language) enable decoupling from
 *   pain-grounding. The constraint exhibits all six DR types from different
 *   observational positions. From the victim's perspective (the conscious
 *   sufferer), it appears as a snare — inescapable coupling of learning to
 *   suffering. From the selection process's perspective, it appears as pure
 *   coordination (rope) — elegant solution to survival-knowledge alignment.
 *   From the civilizational knowledge system's perspective, it appears as a
 *   temporary scaffold with visible sunset — cultural accumulation of
 *   abstract knowledge reduces reliance on direct pain-grounding. The
 *   pain-medical industrial complex maintains performative reliance on
 *   pain-based learning despite alternatives. And from the most general
 *   analytical position, it risks appearing as a mountain (natural law of
 *   cognition) — but the empirical metrics reveal it as a contingent
 *   institutional constraint, not immutable law. The constraint's
 *   extractiveness (0.52) reflects that pain-bearing consciousness is the
 *   prerequisite for knowledge in the biological domain, while the
 *   theater_ratio (0.58) captures the degree to which contemporary knowledge
 *   systems unnecessarily maintain pain-grounding through institutional
 *   inertia rather than genuine necessity.
 *
 * KEY AGENTS:
 *   - Pain Sensation Systems: Primary beneficiary (institutional/arbitrage) — evolutionary logic channels survival-critical information through nociception; gains efficiency through direct coupling of harm signals to learning
 *   - Conscious Sufferers: Primary victim (powerless/trapped) — bear the phenomenal cost of all biological learning; no exit from pain-knowledge coupling
 *   - Knowledge Seekers: Secondary victim (moderate/constrained) — benefit from pain-grounded knowledge (proven evolutionary mechanisms) but constrained by inability to abstract beyond pain-aligned domains
 *   - Evolutionary Selection Process: Structural beneficiary (institutional/arbitrage) — pure coordination mechanism enabling organisms to learn lethal risks without trial-and-error death
 *   - Civilizational Knowledge Systems: Organized agents (organized/mobile) — science, mathematics, logic, symbolic notation enable knowledge abstraction independent of individual pain experience; building sunset pathway
 *   - Pain-Medical Industrial Complex: Institutional actor (institutional/constrained) — perpetuates pain-based learning in medical education, research protocols, and clinical training through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent biological arrangement as universal law of knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_knowledge, 0.52).
domain_priors:suppression_score(evolutionary_knowledge, 0.68).
domain_priors:theater_ratio(evolutionary_knowledge, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_knowledge, extractiveness, 0.52).
narrative_ontology:constraint_metric(evolutionary_knowledge, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(evolutionary_knowledge, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_knowledge, tangled_rope).
narrative_ontology:human_readable(evolutionary_knowledge, "The Primordial Pain-Epistemic Constraint").
narrative_ontology:topic_domain(evolutionary_knowledge, "biological/philosophy/social").

domain_priors:requires_active_enforcement(evolutionary_knowledge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_knowledge, pain_sensation_systems).
narrative_ontology:constraint_beneficiary(evolutionary_knowledge, survival_aligned_organisms).
narrative_ontology:constraint_victim(evolutionary_knowledge, abstract_knowledge_pursuit).
narrative_ontology:constraint_victim(evolutionary_knowledge, consciousness_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONSCIOUS SUFFERER (SNARE) — Cannot escape the pain-knowledge coupling; all learning routes through suffering. Trapped in the constraint that epistemic access requires nociceptive grounding. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(evolutionary_knowledge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE KNOWLEDGE SEEKER (TANGLED ROPE) — Benefits from pain-grounded knowledge (evolved survival mechanisms), but also constrained by inability to abstract beyond pain-aligned domains. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(evolutionary_knowledge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EVOLUTIONARY SELECTION PROCESS (ROPE) — Pure coordination mechanism. Pain-epistemic coupling solves the survival-knowledge alignment problem without requiring conscious intent or external enforcement. Enables organisms to learn what kills them. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary through natural selection.
constraint_indexing:constraint_classification(evolutionary_knowledge, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: CIVILIZATIONAL KNOWLEDGE SYSTEM (SCAFFOLD) — Organized agents (science, logic, mathematics, language) are building abstractions and notational systems that decouple knowledge from direct pain-grounding. Sunset clause: as culture accumulates abstract knowledge (symbolic logic, mathematics, peer review), reliance on primary pain experience declines. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low effective extraction; clear exit pathway visible.
constraint_indexing:constraint_classification(evolutionary_knowledge, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PAIN-MEDICAL INDUSTRIAL COMPLEX (PITON) — Institutional maintenance of pain-based learning paradigms in medical education, research ethics, and clinical training despite availability of alternatives (simulation, computational modeling). theater_ratio=0.58 borderline for piton gate; persists through inertia rather than structural necessity. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.26.
constraint_indexing:constraint_classification(evolutionary_knowledge, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the fullest analytical perspective, the pain-knowledge constraint may be seen as immutable: any biological organism with finite resources and nonzero risk must couple learning to aversive experience to survive. This is a natural law of embodied cognition. However, empirical base properties (ε=0.52, suppression=0.68, theater=0.58) contradict this reading — the constraint is contingent institutional practice, not immutable law.
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
    constraint_indexing:constraint_classification(evolutionary_knowledge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evolutionary_knowledge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_knowledge, TR),
    TR >= 0.70.

:- end_tests(evolutionary_knowledge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes significant extraction: all knowledge acquisition in biological organisms requires pain experience as a grounding mechanism. Organisms cannot access certain knowledge domains (survival risks in particular) without bearing nociceptive costs. However, extractiveness is not maximal (0.70+) because abstract knowledge systems (mathematics, symbolic logic) enable partial decoupling — cultural transmission reduces individual suffering burden. Suppression (0.68): High. Significant barriers prevent exit from pain-knowledge coupling: the biological substrate makes direct pain experience necessary for survival-relevant learning; alternative information channels (simulation, abstract training) are underdeveloped in most domains; institutional practices (medical education, trauma training) normalize pain-based learning. Suppression is not maximal (0.90+) because alternatives exist and are proliferating. Theater ratio (0.58): Moderate. Contemporary knowledge systems (especially medical and military training) maintain pain-grounding partly through genuine necessity and partly through institutional tradition. The rise of simulation, AI training, and abstract methods shows that some pain-based learning is performative theater maintained despite viable alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why indexical classification is essential. The evolutionary selection process sees pure coordination (Rope) — pain-knowledge coupling solves the critical survival problem through elegant mechanism. The conscious sufferer sees pure extraction (Snare) — they bear the full phenomenal cost with no option to exit. The knowledge seeker sees mixed coordination and extraction (Tangled Rope) — they benefit from proven mechanisms but are constrained by inability to access knowledge domains outside pain-aligned contexts. The civilizational knowledge system sees a temporary problem with a clear sunset (Scaffold) — symbolic systems, mathematical notation, and peer review are decoupling knowledge from individual pain-grounding. The pain-medical institutional system sees its own degraded ritual (Piton) — pain-based training persists through tradition despite proven simulation alternatives. The most general analytical perspective risks seeing natural law (Mountain) — 'knowledge requires embodied experience, embodied experience requires pain' — but the empirical data reveals this as false summit: theater_ratio shows significant performative content; suppression shows alternatives are viable; the scaffold perspective shows organized agents are actively replacing pain-grounding.
 *
 * DIRECTIONALITY LOGIC:
 *   Pain Sensation Systems: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; coupling works in their favor. Conscious Sufferers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit, full phenomenal cost. Knowledge Seekers: Victim + constrained → d≈0.62, f(d)≈0.85. Significant extraction through constraint on knowledge domains; moderate mobility through alternative systems. Civilizational Knowledge System: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; organized agents see clear alternatives and sunset pathway. Pain-Medical Industrial Complex: Institutional + constrained → d≈0.45, f(d)≈0.50. Moderate extraction through path dependency; constrained by availability of superior alternatives (simulation). Evolutionary Selection Process: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Beneficiary; mechanism works by design. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Risk of false summit: naturalizing contingent arrangement as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the pain-knowledge coupling is neither purely coordinative nor purely extractive — it is a hybrid that changes character across observational domains. In the biological domain (early evolution), the constraint is pure coordination (Rope): pain-knowledge coupling solves the survival-learning alignment problem optimally given constraints. In the domain of conscious experience (individual lifetime), the constraint is extraction (Snare): the conscious organism bears nociceptive costs while evolutionary processes capture the benefit. In the domain of cultural knowledge systems (civilizational timescale), the constraint is becoming a scaffold with visible sunset: abstract systems (mathematics, logic, symbolic notation, AI training) are replacing pain-grounding as the primary knowledge mechanism. The medical-institutional domain shows piton degradation: pain-based training is maintained through tradition despite superior alternatives (simulation). The key to mandatrophy resolution is recognizing that 'pain-knowledge coupling' is NOT a single constraint viewed from multiple angles — it is a constraint family with different ε values at different scales. The biological necessity (mountain-like at the evolutionary timescale) is distinct from the institutional practice (tangled_rope at the civilizational timescale). The framework prevents false summits by enforcing that if a constraint looks like a mountain from one perspective but tangled_rope from another with very different data, those are different constraints requiring separate stories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pain_necessity_threshold,
    'Is pain-grounding epistemically necessary for knowledge acquisition, or merely historically sufficient?',
    'Comparative cognitive analysis: assess whether abstract knowledge domains (mathematics, logic, symbolic systems) require nociceptive grounding or whether cultural scaffolding enables genuine knowledge abstraction independent of pain experience.',
    'If pain is necessary: constraint is mountain-like (natural law of cognition). If merely historical: constraint is tangled_rope with sunset (cultural artifact being actively replaced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pain_necessity_threshold, empirical, 'Whether pain-grounding is epistemically necessary or historically contingent').

omega_variable(
    abstract_knowledge_existence,
    'Can knowledge exist without being grounded in any organism''s phenomenal experience?',
    'Philosophical analysis of mathematical Platonism, logical realism, and the ontological status of abstract objects; investigation of whether mathematical truths depend on instantiation in conscious systems or exist independently.',
    'If abstract knowledge is ontologically independent: the constraint is extraction (pain-embodied agents monopolize the knowledge-production process). If knowledge necessarily requires phenomenal grounding: the constraint may be unavoidable (mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abstract_knowledge_existence, conceptual, 'Ontological status of abstract knowledge independent of phenomenal experience').

omega_variable(
    simulated_pain_substitutability,
    'Can artificial pain signals (simulation, computational analogs, AI training loss functions) functionally replace biological nociception for learning purposes?',
    'Empirical study of learning rates, retention, and behavioral adaptation across: biological pain experience, simulated pain signals, abstract loss functions. Compare organisms trained via nociception vs. those trained via alternative error signals.',
    'If substitutable: scaffold perspective is correct — sunset is real, alternatives are viable. If irreplaceable: snare perspective validated — conscious suffering is non-negotiable cost of knowledge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulated_pain_substitutability, empirical, 'Whether artificial pain signals can functionally replace biological nociception').

omega_variable(
    consciousness_pain_coupling,
    'Does epistemic access to knowledge require consciousness, or merely information processing? Is the pain-consciousness-knowledge triangle necessarily coupled?',
    'Neuroscientific and philosophical investigation: can unconscious information processing (e.g., proprioceptive learning, implicit memory) constitute genuine knowledge? Does pain require consciousness to function epistemically?',
    'If consciousness is decoupled: knowledge can be acquisition through non-painful information integration (systems can learn without suffering). If necessarily coupled: the constraint is more fundamental than previously assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consciousness_pain_coupling, empirical, 'Whether consciousness is necessary to the pain-knowledge coupling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_knowledge, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evol_know_tr_t0, evolutionary_knowledge, theater_ratio, 0, 0.42).
narrative_ontology:measurement(evol_know_tr_t3, evolutionary_knowledge, theater_ratio, 3, 0.5).
narrative_ontology:measurement(evol_know_tr_t6, evolutionary_knowledge, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(evol_know_be_t0, evolutionary_knowledge, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(evol_know_be_t3, evolutionary_knowledge, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(evol_know_be_t6, evolutionary_knowledge, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_knowledge, resource_allocation).
narrative_ontology:boltzmann_floor_override(evolutionary_knowledge, 0.35).
narrative_ontology:affects_constraint(evolutionary_knowledge, embodied_cognition_asymmetry).
narrative_ontology:affects_constraint(evolutionary_knowledge, consciousness_measurement_problem).
narrative_ontology:affects_constraint(evolutionary_knowledge, medical_education_structure).

% DUAL FORMULATION NOTE:
% The primordial pain-epistemic constraint decomposes into at least three structurally distinct claims with different ε values: (1) Biological necessity: pain-grounding is required for survival-relevant learning in organisms with finite resources (ε≈0.08, Mountain). (2) Phenomenal coupling: conscious suffering is the necessary mechanism by which organisms internalize harm signals as knowledge (ε≈0.52, Tangled Rope). (3) Institutional practice: contemporary knowledge systems (medical education, trauma training) unnecessarily maintain pain-grounding through tradition despite alternatives (ε≈0.65, Snare/Piton hybrid). Each has different resolution mechanisms and temporal horizons. The present story focuses on the phenomenal-institutional coupling (ε=0.52); related constraints handle the pure biological necessity and the institutional practices separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_knowledge, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
