% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parable_fish_turtle, []).

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
 *   constraint_id: parable_fish_turtle
 *   human_readable: The Ontological Lake (Fish and Turtle Parable)
 *   domain: philosophical/social/epistemology
 *
 * SUMMARY:
 *   The Ontological Lake parable illustrates a structural constraint on
 *   understanding: agents can only fully comprehend phenomena within their
 *   lived ontological frame. A fish born and raised in water cannot conceive
 *   of air, land, or rain except as incoherent violations of their
 *   fundamental categories. A turtle, inhabiting both lake and land, can
 *   witness both worlds but cannot bridge the gap — translating
 *   land-existence into water-categories destroys the coherence of the
 *   translation. The parable models how frameworks of understanding function
 *   not as neutral instruments but as constraints: they enable coordination
 *   within the frame while rendering alternate frames literally
 *   incomprehensible to those who have never experienced them. The constraint
 *   operates across multiple levels: neurologically (sensory apparatus limits
 *   perception), linguistically (language embeds ontological categories),
 *   institutionally (frameworks become established disciplines, with
 *   gatekeepers, incentives, and power structures). The parable's power lies
 *   in its ambiguity: is the fish's inability to understand the land a
 *   natural limit (Mountain), an institutional barrier (Snare/Tangled Rope),
 *   or a temporary stage that ontological pluralism can transcend (Scaffold)?
 *   The actual answer — which the indexical classification system can resolve
 *   — depends on the observer's structural position and empirical
 *   commitments.
 *
 * KEY AGENTS:
 *   - The Fish: Primary victim (powerless/trapped) — cannot conceive of ontologies outside water-existence; trapped by their own cognitive structure and societal reinforcement
 *   - The Turtle: Secondary victim (powerless/trapped but with liminal knowledge) — aware of both lake and land but unable to bridge the gap without ontological suicide; extraction occurs through enforced silence
 *   - Dominant Ontology Framework Owners: Primary beneficiary (institutional/arbitrage) — maintain unified understanding that enables coordination; benefit from framework monopoly and the impossibility of external critique
 *   - Alternate Ontology Communities: Secondary beneficiary and victim (moderate/constrained) — benefit from existence of counter-examples to framework universality; constrained by inability to demonstrate validity within dominant frame's evidentiary standards
 *   - Ontological Pluralism Movement: Organized actors (organized/constrained) — working to build bridges between frameworks; see the constraint as temporary and solvable through comparative epistemology
 *   - Academic Discipline: Institutional actor (institutional/arbitrage) — maintains performance of rigorous framework-internal investigation while protecting the frame itself from scrutiny
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional constraint as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parable_fish_turtle, 0.52).
domain_priors:suppression_score(parable_fish_turtle, 0.68).
domain_priors:theater_ratio(parable_fish_turtle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, 0.52).
narrative_ontology:constraint_metric(parable_fish_turtle, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(parable_fish_turtle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parable_fish_turtle, tangled_rope).
narrative_ontology:human_readable(parable_fish_turtle, "The Ontological Lake (Fish and Turtle Parable)").
narrative_ontology:topic_domain(parable_fish_turtle, "philosophical/social/epistemology").

domain_priors:requires_active_enforcement(parable_fish_turtle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parable_fish_turtle, dominant_ontology_community).
narrative_ontology:constraint_beneficiary(parable_fish_turtle, framework_owners).
narrative_ontology:constraint_victim(parable_fish_turtle, alternate_ontology_communities).
narrative_ontology:constraint_victim(parable_fish_turtle, experientially_excluded_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FISH (SNARE) — Trapped within the lake's ontological frame. The fish cannot conceive of air, land, or existence beyond water. Their understanding is constrained to aquatic categories. Exit is impossible without ontological death. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE TURTLE (SNARE) — Trapped by their own partially-translated nature. Turtles experience both lake and land but cannot fully communicate the land's existence to fish without destroying the fish's ontological coherence. The constraint extracts silence and isolation from the turtle's liminal position. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALTERNATE ONTOLOGY COMMUNITY (TANGLED ROPE) — Constrained by inability to demonstrate their claims within the lake's evidentiary standards. But also benefit from the lake's existence as proof that alternative ontologies are possible — the lake's stability provides evidence that multiple worlds can coexist. d≈0.72, f(d)≈1.10, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(parable_fish_turtle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMINANT ONTOLOGY FRAMEWORK OWNERS (ROPE) — Institutional beneficiaries who maintain the lake's ontological consistency. They experience the constraint as pure coordination: the unified framework enables fish society to function, predict, and organize. The framework solves collective action problems (who gets water, mating territories, predator alerts). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.06. Net beneficiary; coordination function is genuine.
constraint_indexing:constraint_classification(parable_fish_turtle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ONTOLOGICAL PLURALISM MOVEMENT (SCAFFOLD) — Organized agents (comparative anthropologists, phenomenologists, indigenous knowledge movements) see the lake's bounded ontology as a temporary constraint with a sunset. They work to build bridges: multiple-perspective epistemology, indigenous-knowledge integration, non-Western philosophical frameworks. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26. Low effective extraction because the movement has agency and sees the constraint as changeable.
constraint_indexing:constraint_classification(parable_fish_turtle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC DISCIPLINE (PITON) — The formalized study of the lake has become substantially theatrical. Ichthyology (fish study) performs rigorous investigation within the aquatic frame while performatively dismissing (or pathologizing) any suggestion that non-aquatic ontologies might be valid. Journals publish peer-reviewed fish behavior, but the frame itself — that the lake is all of reality — is protected from scrutiny. theater_ratio=0.58 reflects this performative gate-keeping. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.05. The discipline benefits from the constraint but sees its own procedures as degraded.
constraint_indexing:constraint_classification(parable_fish_turtle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN — FALSE SUMMIT) — From a civilizational/universal perspective, the parable risks naturalizing a contingent epistemological trap as an inherent feature of knowledge itself: 'beings can only understand what their structure permits.' But the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts the mountain classification. The engine will compute this as a false summit, revealing that the 'inherent cognitive limit' framing — which appears in much epistemology — actually naturalizes a social constraint into necessity. The constraint is institutional (born from framework ownership, not from physics or logic).
constraint_indexing:constraint_classification(parable_fish_turtle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parable_fish_turtle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parable_fish_turtle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parable_fish_turtle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parable_fish_turtle, TR),
    TR >= 0.70.

:- end_tests(parable_fish_turtle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts understanding from those trapped within a single ontological frame. But the extraction is not pure coercion — the dominant framework genuinely enables coordination and prediction within its domain. The 0.52 reflects that extraction is real (lost possibilities, suppressed alternatives) but mixed with genuine coordination benefit. Suppression (0.68): High. Substantial barriers prevent exit or even conception of alternatives: institutional gatekeeping (disciplines police boundaries), linguistic embedding (language naturalizes categories), neurocognitive reinforcement (children are raised within single frame), career incentives (challenging the frame threatens professional standing). Suppression is not total — the Pluralism Movement and individual turtles can partially escape. Theater ratio (0.58): Moderate. Academic disciplines perform rigorous investigation but the frame itself is protected from scrutiny through meta-level gate-keeping: empiricist epistemology appears neutral but enshrines the dominant ontology's categories as 'what exists.' The theater is in the claim that frame-choice is not a choice but a discovery of reality itself. Claimed type: Tangled Rope. The constraint requires active enforcement (institutional gatekeeping, socialization, disciplinary boundaries). It has genuine beneficiaries (framework owners who solve coordination problems) and genuine victims (alternate ontology communities, liminal figures like the turtle). It extracts alternative understandings while enabling coordination within the frame.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint are dramatic and illustrative of the full classificatory range. The fish sees the constraint as natural law — the boundaries of water-existence are not negotiable but inevitable. The turtle sees extraction — their knowledge is suppressed and they are treated as anomalies or liars. The dominant framework owners see pure coordination — the unified ontology solves collective action problems. The alternate ontology communities see a Tangled Rope — they can function within the dominant frame but their deepest categories are rendered invisible. The Pluralism Movement sees a temporary problem (Scaffold) — multiple perspectives can coexist and bridge-building is possible. The academic discipline sees its own procedures as degraded (Piton) — the frame-protection function has become theatrical. The analytical observer risks seeing a Mountain (natural law of cognition) but the structural data reveals this as a false summit: the constraint is institutional, not physical or logical. This perspectival explosion is precisely what the parable is designed to demonstrate: a single constraint structure generates incommensurable experiences depending on the observer's structural position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Fish (victim + trapped): d≈0.95, f(d)≈1.42. Maximum extraction — no exit options, ontologically locked-in. Turtle (victim + trapped but partially aware): d≈0.92, f(d)≈1.38. Extreme extraction through enforced silence despite knowledge. Alternate ontology communities (victim + constrained): d≈0.72, f(d)≈1.10. Significant extraction — can partially exit through comparative work, but constrained by inability to demonstrate within dominant frame's evidentiary standards. Framework owners (beneficiary + arbitrage): d≈0.05, f(d)≈-0.12. Net beneficiary — can exit to other frames but benefit from this frame's coordination function. Pluralism movement (organized + constrained): d≈0.42, f(d)≈0.42. Low effective extraction despite constraints because organized agents have agency. Discipline (institutional + arbitrage): d≈0.08, f(d)≈-0.09. Slightly beneficiary despite piton status; the discipline maintains institutional power through frame enforcement. Analytical observer (analytical): d≈0.72, f(d)≈1.15. Mountain classification is false summit — analytical position attempts to naturalize constraint but structural data reveals institutional character.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the Ontological Lake is between coordination and extraction: Is the unified ontological framework a genuine solution to the collective action problem of shared understanding (Rope), or is it a mechanism for extracting alternative perspectives (Snare)? The Tangled Rope classification resolves this by asserting that BOTH are true simultaneously. The framework genuinely solves coordination problems — fish society could not function with incoherent understanding. But it simultaneously extracts alternatives — the framework's universality claims suppress counter-ontologies. The extraction and coordination functions are structurally coupled: the more effectively the frame solves coordination within itself, the more completely it suppresses alternatives. This is the deep mandatrophy: you cannot have a coordination frame without boundary-setting, and any boundary-setting excludes alternatives. The Pluralism Movement's Scaffold perspective suggests that this mandatrophy might be resolvable through explicit acknowledgment of multiple frames (instead of pretending a single frame is universal), but the viability of this is an omega variable — it depends on empirical question of whether coordination is actually possible across acknowledged incommensurable ontologies. Until that is resolved, the constraint remains Tangled Rope: genuine coordination coupled with genuine extraction, with no separable pure form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incommensurability_vs_bridgeable,
    'Are the fish and turtle ontologies truly incommensurable, or can they be bridged through translation mechanisms (metaphor, extended experience, collective inquiry)?',
    'Empirical study of actual ontological integration efforts (indigenous knowledge + Western science, different cultural frameworks coexisting); examination of successful trans-ontological communication',
    'If truly incommensurable: constraint is closer to Mountain (natural limit). If bridgeable: constraint is closer to Tangled Rope / Scaffold (institutional barrier, solvable through effort).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommensurability_vs_bridgeable, empirical, 'Whether ontological boundaries are incommensurable or bridgeable').

omega_variable(
    epistemic_cost_of_translation,
    'Does authentic translation between ontologies require loss of original framework coherence, or can multiple frameworks coexist without mutual contamination?',
    'Study of bilingual cognition, code-switching in thought, polymath integration of disparate domains; analysis of whether framework switching causes systematic epistemic distortion',
    'If translation requires coherence loss: extraction is structural (victims must lose ontological integrity). If coexistence is possible: constraint is institutional gate-keeping rather than natural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_cost_of_translation, empirical, 'Whether ontological translation requires loss of framework coherence').

omega_variable(
    turtle_agency_and_translation_willingness,
    'Does the turtle actively suppress knowledge of land to maintain lake social order, or are they themselves unable to translate their land experience into lake-coherent terms?',
    'Historical analysis of liminal figures (anthropologists in the field, cultural translators, diaspora communities); examination of whether silence is strategic suppression or structural incapacity',
    'If strategic: constraint is Snare (malicious). If incapacity: constraint is Tangled Rope (structural misalignment). Classification impact: whether victims are suppressed or structurally excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(turtle_agency_and_translation_willingness, conceptual, 'Whether turtle silence is strategic or structurally necessary').

omega_variable(
    ontological_pluralism_viability,
    'Can a society maintain stable coordination while explicitly acknowledging multiple incommensurable ontologies, or does coordination require unified framework?',
    'Longitudinal study of explicitly pluralist societies (countries with multiple legal systems, religious coexistence frameworks, multinational organizations); measurement of coordination effectiveness and conflict rates',
    'If viable: scaffold sunset is real (constraint is temporary, solvable). If impossible: constraint is fundamental (closer to Mountain or persistent Snare). High impact on whether pluralism movement is realist or aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_pluralism_viability, empirical, 'Whether stable coordination is possible with acknowledged multiple ontologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parable_fish_turtle, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pft_tr_t0, parable_fish_turtle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pft_tr_t25, parable_fish_turtle, theater_ratio, 25, 0.5).
narrative_ontology:measurement(pft_tr_t50, parable_fish_turtle, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(pft_be_t0, parable_fish_turtle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(pft_be_t25, parable_fish_turtle, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(pft_be_t50, parable_fish_turtle, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parable_fish_turtle, information_standard).
narrative_ontology:affects_constraint(parable_fish_turtle, incommensurability_thesis).
narrative_ontology:affects_constraint(parable_fish_turtle, paradigm_inviolability).
narrative_ontology:affects_constraint(parable_fish_turtle, epistemic_privilege_by_position).

% DUAL FORMULATION NOTE:
% The Ontological Lake parable decomposes into three related constraints: (1) incommensurability_thesis (ε≈0.08, Mountain) — the empirical claim that frameworks can be logically incommensurable; (2) paradigm_inviolability (ε≈0.65, Snare) — the institutional fact that established paradigms resist internal critique; (3) epistemic_privilege_by_position (ε≈0.52, Tangled Rope) — this story — the structural constraint that experience within a frame enables understanding of that frame while disabling understanding of alternatives. The parable is the middle constraint connecting the underlying natural limit (incommensurability) to the institutional extraction (paradigm gatekeeping).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parable_fish_turtle, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
