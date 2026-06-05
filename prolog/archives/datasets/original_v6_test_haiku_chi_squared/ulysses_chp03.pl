% ============================================================================
% CONSTRAINT STORY: ulysses_chp03
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp03, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp03
 *   human_readable: The Ineluctable Modality (Sandymount Strand)
 *   domain: philosophical/social/technological
 *
 * SUMMARY:
 *   Stephen Dedalus walks Sandymount Strand on the morning of June 16, 1904,
 *   thinking about the 'ineluctable modality of the visible' — the binding of
 *   consciousness to sensory data, the impossibility of escaping the
 *   phenomenological present. This constraint operates across multiple
 *   registers: as a philosophical problem (how can thought transcend
 *   sensation?), as a social/educational constraint (Stephen's material
 *   circumstances bind him to immediate bodily survival), and as a
 *   literary/technological constraint (how can narrative represent
 *   consciousness without being enslaved to linear sequential description?).
 *   The constraint exhibits all six DR types from different perspectives.
 *   From Stephen's internal perspective, it is a snare: consciousness is
 *   trapped within sensation, unable to achieve pure abstraction. From the
 *   perspective of the modernist literary apparatus, it is a scaffold: new
 *   narrative techniques promise eventual liberation. From the institutional
 *   perspective of Victorian narrative convention, it is a piton: the rule to
 *   describe 'what is visible' persists through inertia. From the
 *   phenomenological tradition, it appears to be a mountain: an invariant
 *   structure of consciousness itself. The constraint's theater_ratio (0.68)
 *   reflects that much of literary realism is performative grounding — the
 *   detail serves aesthetic and narrative purposes more than epistemic ones.
 *
 * KEY AGENTS:
 *   - Stephen's Consciousness: Primary victim (powerless/trapped) — bound by sensory modality; bears the cost of inability to transcend the phenomenological present
 *   - Sensory Apparatus: Primary beneficiary (powerful/arbitrage) — functions naturally through the modality; experiences zero extraction
 *   - The Philosophical Apprentice: Secondary victim (moderate/constrained) — seeks intellectual development but constrained by embodied reality; also benefits from sensory grounding against solipsism
 *   - The Modernist Literary Movement: Organized agent (organized/mobile) — building narrative techniques to transcend the constraint; sees sunset pathway through innovation
 *   - Victorian Literary Convention: Institutional actor (institutional/arbitrage) — maintains the rule to describe the visible; sees own process as degraded (piton perspective)
 *   - The Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent embodiment as transcendental necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp03, 0.32).
domain_priors:suppression_score(ulysses_chp03, 0.48).
domain_priors:theater_ratio(ulysses_chp03, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp03, extractiveness, 0.32).
narrative_ontology:constraint_metric(ulysses_chp03, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ulysses_chp03, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp03, tangled_rope).
narrative_ontology:human_readable(ulysses_chp03, "The Ineluctable Modality (Sandymount Strand)").
narrative_ontology:topic_domain(ulysses_chp03, "philosophical/social/technological").

domain_priors:requires_active_enforcement(ulysses_chp03).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp03, immediate_sensory_experience).
narrative_ontology:constraint_beneficiary(ulysses_chp03, bodily_presence).
narrative_ontology:constraint_victim(ulysses_chp03, cognitive_freedom).
narrative_ontology:constraint_victim(ulysses_chp03, imaginative_transcendence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN'S CONSCIOUSNESS (SNARE) — Bound by the visible modality; cannot escape the tyranny of sensory input. The body walks Sandymount Strand, and consciousness is trapped within the phenomenological present. Cannot transcend the immediate (the 'what is'), cannot access pure thought without mediation through sensation. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.41.
constraint_indexing:constraint_classification(ulysses_chp03, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SENSORY APPARATUS (ROPE) — Eyes, ears, proprioception coordinate Stephen's navigation. The visible modality is not extractive from the senses' perspective; it is their native function. They benefit from being stimulated and deployed. d≈0.15, f(d)≈-0.01, σ=0.8 → χ≈-0.01. Effectively zero extraction; pure coordination.
constraint_indexing:constraint_classification(ulysses_chp03, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: PHILOSOPHICAL APPRENTICE (TANGLED ROPE) — Stephen seeks to develop intellectual and imaginative powers. The sensory constraint provides real coordination (grounding thought in embodied reality, preventing solipsism) but also imposes asymmetric extraction: he must spend cognitive energy processing sensation when he would prefer to dwell in pure philosophy or memory. Constrained exit because he cannot escape embodiment without ceasing to exist. d≈0.68, f(d)≈1.05, σ=0.8 → χ≈0.35.
constraint_indexing:constraint_classification(ulysses_chp03, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: MODERNIST LITERARY MOVEMENT (SCAFFOLD) — Joyce and his contemporaries are building techniques (stream of consciousness, interior monologue, phenomenological notation) that will eventually transcend the tyranny of linear narrative and spatial description. These techniques temporarily accept the constraint (anchoring consciousness in embodied presence) while creating a pathway to eventual liberation through stylistic innovation. The constraint has a sunset: as modernist form matures, the binding power of ineluctable modality weakens through narrative technique. d≈0.35, f(d)≈0.28, σ=0.9 → χ≈0.20. Mobile exit because the literary apparatus can migrate to new forms.
constraint_indexing:constraint_classification(ulysses_chp03, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: VICTORIAN LITERARY CONVENTION (PITON) — The constraint to describe 'what is visible' (realist narrative, spatial grounding, objective external world) was once functionally necessary for the novel form. By 1904, it has become largely performative: conventions persist through institutional inertia while the underlying narrative function has atrophied. Modernist writers must still nominally respect grounding in sensory detail, but the form no longer demands it. theater_ratio=0.68 ≥ 0.70 approaches piton gate. The constraint is maintained as ritual, not necessity.
constraint_indexing:constraint_classification(ulysses_chp03, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHENOMENOLOGICAL VIEW (MOUNTAIN) — From a transcendental perspective, the ineluctable modality of the visible is indeed a natural law of consciousness itself. Husserl's phenomenology treats the binding of consciousness to sensory data as an invariant structure: there is no consciousness without intentionality directed toward an object, and all objects appear through sensory modalities. The modality is not extractive; it is the fundamental structure of Being. However, the structural data (ε=0.32, suppression=0.48, theater=0.68) reveals this as a false summit: the constraint is contingent on embodied existence and linguistic representation, not transcendental.
constraint_indexing:constraint_classification(ulysses_chp03, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp03_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp03, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp03, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp03, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp03_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The constraint does extract in the sense that Stephen's cognitive energy must be devoted to processing sensation when he would prefer contemplation. But the extraction is not severe because sensation also provides genuine coordination benefit (sensory grounding prevents fantasy/delusion) and is intrinsic to consciousness, not imposed externally. The value reflects that roughly one-third of Stephen's mental labor is consumed by the modality constraint itself, while two-thirds potentially remains available for transcendent thought. Suppression (0.48): Moderate. Significant barriers to transcendence: the body requires constant attention (hunger, cold, fatigue), linguistic representation traps thought in sequential articulation, memory is reconstructed through sensory imagination. But suppression is incomplete — abstraction, memory, and fantasy do occur; they are merely mediated, not prevented. Theater ratio (0.68): High-moderate. Victorian realism's insistence on detailed sensory description is increasingly performative by 1904 — it serves literary tradition and aesthetic closure more than epistemological necessity. The theater reflects the gap between the rule to describe sensation and the actual information content of such description.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why all six types emerge from the same structural data. Stephen's consciousness sees a snare — sensation tyrannizes thought. The sensory apparatus sees coordination — this is what it does. The philosophical apprentice sees tangled rope — sensation both grounds and constrains. The modernist movement sees a scaffold with sunset — narrative innovation promises transcendence. Victorian convention sees its own performative ritual (piton) — the rule to describe the visible persists through institutional inertia. The analytical/phenomenological perspective risks seeing a mountain — treating contingent embodiment as transcendental necessity. The perspectival gap is maximal here: from Stephen's view, the constraint is nearly absolute; from the sensory apparatus's view, it is zero; from the modernist view, it is temporary; from the analytical view, it is eternal. No single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen's consciousness: Victim + trapped → d≈0.92, f(d)≈1.38. Near-total extraction. Sensory apparatus: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary; nearly zero effective extraction. Philosophical apprentice: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; constrained exit keeps d from reaching 0.85. Modernist movement: Organized + mobile → d≈0.35, f(d)≈0.28. Low effective extraction; mobile exit provides strategic flexibility. Victorian convention: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification derives from theater gate (0.68 ≥ 0.70 is borderline), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the engine's false summit detector will flag this given the moderate ε and suppression values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The mandatrophy would arise if a single perspective had to claim the constraint is simultaneously coordinating (rope) and extracting (snare). This constraint avoids false mandatrophy by disaggregating: sensory coordination is real (rope perspective legitimate); cognitive extraction is real (snare perspective legitimate); these are experienced by structurally different agents. The tangled_rope classification for the philosophical apprentice correctly models the mixed experience. The false mountain (analytical/phenomenological) is caught by base property inspection: if the constraint were transcendentally necessary, suppression would be near-zero and ε would be ≤0.25. The actual values (ε=0.32, suppression=0.48) reveal contingency. The scaffold perspective (literary/technical) shows that even if the phenomenological claim were true at the level of consciousness-in-itself, the social/technological constraint can still be structured as temporary through narrative innovation — sunset is orthogonal to transcendental status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phenomenological_necessity,
    'Is the binding of consciousness to sensory modality a transcendental necessity (true for all conscious being) or a contingent feature of human embodiment and language?',
    'Comparative analysis of phenomenological vs. analytical philosophy traditions; thought experiments about non-embodied or artificial consciousness; linguistic analysis of how description itself imposes modality constraints',
    'If transcendental: mountain classification is justified. If contingent: constraint is social/linguistic (tangled_rope from other perspectives becomes dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phenomenological_necessity, conceptual, 'Whether the modality constraint is transcendental or contingent').

omega_variable(
    imaginative_escape_possibility,
    'Can consciousness genuinely transcend sensory mediation through memory, fantasy, or abstract thought, or is the ''escape'' always filtered through sensory imagination (phantom sensations, visualized abstractions)?',
    'Phenomenological introspection; analysis of how abstract thought is actually coded in consciousness; examination of whether ''pure thought'' (e.g., mathematics, logic) relies on implicit sensory scaffolding',
    'If pure transcendence possible: suppression is overstated, constraint is weaker (snare → rope). If all thought requires sensory mediation: suppression is fundamental, constraint is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imaginative_escape_possibility, empirical, 'Whether consciousness can transcend sensory mediation').

omega_variable(
    literary_technique_escape_efficacy,
    'Do modernist narrative techniques (stream of consciousness, interior monologue, fragmentation) actually liberate meaning from the constraint of linear spatial/temporal description, or do they merely notate the constraint differently without escaping it?',
    'Close reading of Joyce''s actual Proteus chapter: does the interior monologue transcend modality or inscribe it more minutely? Comparison with post-modernist and digital narrative forms that claim further liberation',
    'If techniques genuinely escape: scaffold sunset is real, constraint has structural termination point. If they merely redescribe: constraint persists regardless of form, scaffold perspective is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_technique_escape_efficacy, empirical, 'Whether narrative techniques escape the modality constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp03, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proto_tr_t0, ulysses_chp03, theater_ratio, 0, 0.52).
narrative_ontology:measurement(proto_tr_t4, ulysses_chp03, theater_ratio, 4, 0.6).
narrative_ontology:measurement(proto_tr_t8, ulysses_chp03, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(proto_be_t0, ulysses_chp03, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(proto_be_t4, ulysses_chp03, base_extractiveness, 4, 0.25).
narrative_ontology:measurement(proto_be_t8, ulysses_chp03, base_extractiveness, 8, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp03, information_standard).
narrative_ontology:affects_constraint(ulysses_chp03, bildungsroman_developmental_closure).
narrative_ontology:affects_constraint(ulysses_chp03, embodied_consciousness_binding).

% DUAL FORMULATION NOTE:
% The Proteus chapter constraint decomposes into three related but structurally distinct claims: (1) phenomenological claim — consciousness is always intentionally directed at sensory objects (near-mountain); (2) literary/narrative claim — description requires spatial/temporal sequence (tangled_rope, high theater); (3) existential/social claim — embodied human reality binds attention to immediate survival (snare, high suppression). Each has different ε, different sunset conditions, and different perspectives. This story primarily addresses claim 2 and 3; the phenomenological claim is treated as a false summit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
