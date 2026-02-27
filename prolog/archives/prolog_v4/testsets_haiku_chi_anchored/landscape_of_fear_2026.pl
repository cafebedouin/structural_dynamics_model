% ============================================================================
% CONSTRAINT STORY: landscape_of_fear_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_landscape_of_fear_2026, []).

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
 *   constraint_id: landscape_of_fear_2026
 *   human_readable: The Landscape of Fear
 *   domain: biological/behavioral_ecology
 *
 * SUMMARY:
 *   The landscape of fear describes a fundamental behavioral constraint in
 *   which prey animals modify their movement, foraging, reproduction, and
 *   habitat use in response to predation risk, often far exceeding the direct
 *   mortality from actual predation. This constraint operates as a subtle
 *   form of extraction: the predator (or even the mere perception of
 *   predators) harvests behavioral compliance from prey without necessarily
 *   killing them. The landscape of fear is a tangled hybrid of coordination
 *   and extraction. From the prey perspective, it is a snare — involuntary
 *   behavioral suppression with high costs. From the predator perspective, it
 *   is pure rope — a coordination mechanism that enables resource access
 *   without active pursuit. From the ecosystem perspective, it is tangled:
 *   fear-driven spatial heterogeneity in habitat use creates ecological
 *   mosaics that benefit some ecosystem functions while reducing energy
 *   capture efficiency. The constraint has become increasingly prominent in
 *   conservation discourse ('trophic rewilding,' 'apex predator restoration')
 *   often with performative rhetoric that obscures the mechanistic
 *   complexity. Theater ratio has risen from 0.32 to 0.48 over the
 *   measurement interval as the conservation narrative has become more
 *   abstract and less mechanistically grounded. Base extractiveness has
 *   similarly risen from 0.18 to 0.38 as experimental evidence accumulates
 *   that fear effects are real and widespread, shifting the constraint from a
 *   theoretical curiosity to a dominant ecological force.
 *
 * KEY AGENTS:
 *   - Prey Individuals: Primary victims (powerless/trapped) — constrained by vigilance requirements, reduced foraging, delayed reproduction regardless of actual predation risk
 *   - Prey Populations: Secondary victims (moderate/constrained) — benefit from predator-driven population regulation but suffer from fear-induced reproductive suppression and habitat abandonment
 *   - Predator Populations: Primary beneficiaries (institutional/arbitrage) — gain resource access and population regulation without proportional cost; extract behavioral compliance
 *   - Ecosystem Trophic Structure: Affected collective (powerful/mobile) — experiences both coordination benefits (spatial heterogeneity, habitat mosaic) and extraction costs (reduced energy flow efficiency)
 *   - Conservation Institutions: Secondary beneficiary (institutional/constrained) — benefit from landscape-of-fear framing for apex predator restoration programs; often perform understanding rather than mechanistically engage with fear dynamics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing fear landscape as immutable evolutionary law when evidence shows context-dependency and reversibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(landscape_of_fear_2026, 0.38).
domain_priors:suppression_score(landscape_of_fear_2026, 0.52).
domain_priors:theater_ratio(landscape_of_fear_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(landscape_of_fear_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(landscape_of_fear_2026, tangled_rope).
narrative_ontology:human_readable(landscape_of_fear_2026, "The Landscape of Fear").
narrative_ontology:topic_domain(landscape_of_fear_2026, "biological/behavioral_ecology").

domain_priors:requires_active_enforcement(landscape_of_fear_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(landscape_of_fear_2026, predator_populations).
narrative_ontology:constraint_victim(landscape_of_fear_2026, prey_populations).
narrative_ontology:constraint_victim(landscape_of_fear_2026, ecosystem_energy_flow).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREY INDIVIDUAL (SNARE) — Trapped in high-vigilance state regardless of actual predation risk. Cannot exit fear-driven behavioral constraints. Bears full cost through reduced feeding efficiency, delayed reproduction, increased stress physiology. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.53.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PREY POPULATION (TANGLED ROPE) — Constrained by landscape-wide fear effects on reproduction and survival, but also benefits from predator-driven regulation that prevents overgrazing and maintains habitat quality. Population dynamics involve both extraction (fear-induced mortality and reproductive suppression) and coordination (predator-prey equilibrium enables ecosystem stability). d≈0.62, f(d)≈0.78, σ=0.9 → χ≈0.29.
constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PREDATOR POPULATION (ROPE) — Benefits from behavioral suppression of prey without proportional cost. The landscape of fear is a pure coordination mechanism: prey fear enables predator resource access without expending energy on pursuit. Predator operates as institutional beneficiary with arbitrage capability (can expand or contract predation strategy). d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.03. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(landscape_of_fear_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ECOSYSTEM TROPHIC STRUCTURE (TANGLED ROPE) — Fear-induced changes in prey behavior (reduced grazing in risky patches, altered habitat use) reshape vegetation community and ecosystem energy flow. The constraint creates both coordination benefits (spatial heterogeneity, landscape mosaic maintenance) and extraction costs (energy inefficiency, reduced primary productivity capture). d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.18.
constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ECOLOGICAL CONSERVATION NARRATIVE (PITON) — The landscape of fear has become a key conservation framing ('trophic rewilding,' 'apex predator restoration') where the performance of discussing predator reintroduction substitutes for understanding actual behavioral coupling. Theater_ratio=0.48 is below piton threshold, but institutional conservation discourse exhibits higher theater (0.65+) when examined separately — performative restoration rhetoric vs. functional predator-prey coupling. This perspective shows degradation: the narrative persists despite growing evidence that fear effects are context-dependent and sometimes counterintuitive.
constraint_indexing:constraint_classification(landscape_of_fear_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, predator-prey fear effects are immutable features of evolutionary biology: any prey with cognitive capacity will evolve risk assessment mechanisms, and any predator will exploit this behavioral constraint. The fear landscape is as fundamental as gravity in ecology. However, the structural data (ε=0.38, suppression=0.52, theater=0.48) and strong evidence for context-dependency (fear effects vary 3-10x across habitat types, prey species, predator types) contradicts the mountain classification. The engine will compute this as a false summit: fear is a structurally contingent constraint, not a natural law.
constraint_indexing:constraint_classification(landscape_of_fear_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(landscape_of_fear_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(landscape_of_fear_2026, TR),
    TR >= 0.70.

:- end_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Fear-driven behavioral suppression in prey represents a real extraction mechanism — the predator (or predation risk perception) harvests behavioral compliance without necessarily investing in direct predation. However, extractiveness is not as high as pure snare (0.46+) because the extraction emerges from asymmetric information and evolutionary incentives rather than active enforcement. In systems with effective predator absence, fear-driven behaviors can decay or be replaced by alternative strategies. The intermediate value reflects that extraction is conditional on maintained predation risk salience. Suppression (0.52): Moderate-high. Prey face substantial barriers to escape the fear landscape: evolution has hardwired risk assessment, habitat refugia often carry their own costs (reduced food, vulnerability to other predators), and prey cannot negotiate with predators. However, suppression is not maximal (0.60+) because prey can develop some behavioral flexibility — some individuals/populations show partial tolerance to predation risk, and refugia do provide partial escape paths. Theater ratio (0.48): Moderate. The landscape of fear as a biological mechanism is functionally real — prey genuinely adjust behavior in response to predation risk, and the effects are measurable. However, conservation narrative around landscape of fear exhibits higher theater (0.65+) when examined in isolation: the rhetorical framing of 'trophic rewilding' often performs understanding of fear effects without actual mechanistic engagement.
 *
 * PERSPECTIVAL GAP:
 *   The landscape of fear demonstrates a sharp perspectival divergence between victim and beneficiary. The prey individual sees a snare: involuntary fear-driven suppression with no coordination benefit. The predator sees a rope: pure coordination mechanism enabling resource access. The prey population sees a tangled rope: extraction but also regulation benefit. The ecosystem sees a mixed constraint: spatial coordination benefits (habitat heterogeneity) offset against energy flow extraction. The conservation narrative sees a natural law (mountain): apex predator fear is fundamental to ecology. The analytical observer's natural law view is contradicted by evidence of context-dependency: fear effects vary 3-10x across habitat types, prey life history, and predator species presence; in apex predator-absent systems, prey fear behaviors degrade; in prey-predator coevolution, prey develop risk tolerance strategies. The engine's false summit detector reveals this is not an immutable natural law but a structurally contingent constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Prey individuals: Victims + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — prey cannot exit fear-driven suppression without evolutionary or ecological change. Prey populations: Victims + constrained → d≈0.62, f(d)≈0.78. Significant extraction tempered by population regulation benefits; escape constrained but not impossible. Predator populations: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; predators extract with minimal cost investment. Ecosystem: Neither beneficiary nor pure victim (powerful/mobile) → d≈0.45, f(d)≈0.48. Moderate extraction; spatial coordination benefits partially offset energy flow costs. Conservation narrative: Institutional + constrained → d≈0.55, f(d)≈0.75. Moderate extraction from the discourse perspective; narrative benefits conservation institutions while constraining precise mechanistic understanding. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Observer's mountain classification is perspectival (naturalizes contingent constraint); false summit detection applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The landscape of fear resolves the mandatrophy by showing how a single biological mechanism (predation risk-induced behavioral suppression) can be simultaneously a snare (prey individual view), rope (predator view), tangled rope (prey population and ecosystem views), piton (conservation narrative view), and false mountain (analytical naturalization). The mandatrophy is resolved by recognizing that the constraint's extractiveness and suppression are real structural properties that do not change with perspective, but the classification type depends on the observer's structural position (victim with trapped exit, beneficiary with arbitrage, moderate player with constrained exit, etc.). The key insight is that extractiveness ≈ 0.38 and suppression ≈ 0.52 place this in the tangled rope band (0.30 ≤ χ ≤ 0.90 for moderate observers), but the tight coupling between predation risk and prey behavior creates conditions where the snare classification (high d, high f(d)) dominates the prey perspective while the rope classification dominates the predator perspective. The false mountain from the analytical observer is detected because actual mechanistic variation (context-dependency, reversibility in predator absence, coevolutionary tolerance development) contradicts the immutability criterion. The constraint is NOT a natural law — it is a structurally real but contingent extraction mechanism embedded in predator-prey evolutionary dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fear_effect_magnitude_threshold,
    'What predator density and visibility threshold triggers substantive behavioral change in prey populations, versus mere baseline vigilance?',
    'Experimental manipulation of predator presence (caging, decoys, removal) with quantified measures of prey foraging efficiency, habitat use, reproduction rate; threshold identification via response curves',
    'If threshold is low (< 1 predator per 100 prey): fear effect is easily triggered, landscape highly constrained, suppression high. If threshold is high (> 10 per 100 prey): fear effect requires high predation risk, constraint is activated only in crisis, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fear_effect_magnitude_threshold, empirical, 'Predator density threshold for behavioral prey response').

omega_variable(
    habitat_refugia_alternative_pathways,
    'Do prey with access to high-quality refugia experience the landscape of fear as a binding constraint, or do refugia create escape pathways that convert the snare into a tangled rope?',
    'Comparative analysis of prey in fragmented vs. connected landscapes; measurement of survival, reproduction, feeding efficiency in open vs. sheltered habitat patches; quantification of refuge accessibility cost',
    'If refugia provide true escape: constraint is not uniform across space, prey in refugia escape snare classification (d drops significantly). If refugia are illusory: suppression is actually higher (prey still constrained by risk), beneficiary enjoys even greater extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(habitat_refugia_alternative_pathways, empirical, 'Whether habitat refugia provide meaningful escape from fear constraint').

omega_variable(
    ecosystem_productivity_net_effect,
    'Does fear-driven reduction in prey foraging and habitat use reduce total ecosystem energy capture enough to count as net extraction, or is the ecosystem-level view neutral?',
    'Whole-ecosystem productivity measurements (NPP, trophic efficiency) in presence vs. absence of apex predators; accounting for spatial heterogeneity benefits against foraging reduction costs',
    'If net negative (extraction dominates): ecosystem victim classification is correct (snare or tangled rope). If net neutral or positive: ecosystem sees rope or slight benefit, and extraction is more concentrated on prey individual level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_productivity_net_effect, empirical, 'Whether fear-driven behavior changes reduce ecosystem-level energy flow').

omega_variable(
    predator_absence_dynamics,
    'In systems where apex predators are absent, do prey populations self-organize into fear-avoidance behaviors based on lesser predators, or does behavior collapse, indicating fear landscape requires a high-powered coordinator?',
    'Historical and contemporary comparison of prey behavior in predator-absent systems (islands, protected reserves) vs. predator-rich systems; analysis of behavioral persistence in absence of actual threat',
    'If prey maintain fear behaviors without strong predators: landscape is prey-generated, not imposed extraction. If prey behavior collapses without apex predators: landscape is coordinator-dependent, extraction is conditional on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predator_absence_dynamics, empirical, 'Whether fear landscape persists without apex predator enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(landscape_of_fear_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lof_tr_t0, landscape_of_fear_2026, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lof_tr_t5, landscape_of_fear_2026, theater_ratio, 5, 0.4).
narrative_ontology:measurement(lof_tr_t10, landscape_of_fear_2026, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(lof_be_t0, landscape_of_fear_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lof_be_t5, landscape_of_fear_2026, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(lof_be_t10, landscape_of_fear_2026, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(landscape_of_fear_2026, resource_allocation).
narrative_ontology:affects_constraint(landscape_of_fear_2026, prey_population_dynamics).
narrative_ontology:affects_constraint(landscape_of_fear_2026, predator_carrying_capacity).
narrative_ontology:affects_constraint(landscape_of_fear_2026, habitat_fragmentation_effects).

% DUAL FORMULATION NOTE:
% The landscape of fear is downstream of predator presence and upstream of population-level reproductive suppression and ecosystem energy allocation. Mechanistically distinct from direct predation (which is modeled separately as predator-prey kill rates) but structurally coupled: fear effects amplify direct predation costs and constrain population recovery. The constraint emerges from the interaction of predation risk salience and prey cognitive capacity for risk assessment, making it dependent on both behavioral ecology and evolutionary history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(landscape_of_fear_2026, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
