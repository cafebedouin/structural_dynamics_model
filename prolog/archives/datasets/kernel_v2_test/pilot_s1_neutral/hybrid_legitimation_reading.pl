% ============================================================================
% CONSTRAINT STORY: hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_legitimation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Through Imperial Charisma and Institutional Incentives
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the establishment of new norms through a hybrid
 *   mechanism combining symbolic authority transfer (the emperor's exemplary
 *   conduct as legitimacy source) with institutional incentives (career
 *   advancement, resource redistribution, hierarchical compliance). The
 *   reading instantiates a middle position between two sibling
 *   interpretations: the endogenous-climb reading (subordinates voluntarily
 *   adopt because they perceive the norm as improved) and the
 *   exogenous-override reading (the center imposes the norm through coercion
 *   regardless of preference). The hybrid reading claims that legitimacy
 *   flows from the emperor's charismatic exemplarity AND is sustained by
 *   institutional enforcement mechanisms targeting competing claims and
 *   non-compliant populations. Early adoption by elites creates a
 *   coordination problem for subordinates (everyone else is adopting, so
 *   adoption becomes normatively expected); the center maintains enforcement
 *   capacity to suppress alternative legitimacy sources and punish persistent
 *   deviation. The constraint exhibits Tangled Rope structure: genuine
 *   coordination function (hierarchical norm coherence) paired with
 *   asymmetric extraction (the center captures legitimacy dividends, elites
 *   gain status/resources, subordinates bear compliance costs). The
 *   measurements show extractiveness rising in the transition phase
 *   (institutional mechanisms mature to enforce compliance) then declining as
 *   the norm becomes habitual and enforcement burden lightens. Theater ratio
 *   rises during maturation and plateaus once the norm is embedded,
 *   indicating the charismatic exemplarity mechanism becomes increasingly
 *   vestigial. This reading differs from its siblings by asserting that BOTH
 *   charisma AND coercion are necessary — neither alone is sufficient, and
 *   the constraint's type is neither pure climb nor pure override.
 *
 * KEY AGENTS:
 *   - Imperial Authority Center: Institutional beneficiary (institutional/arbitrage) — originates the norm, captures legitimacy dividend, directs enforcement incentives, maintains highest option value
 *   - Elite Adoption Cohort: Primary beneficiary (institutional/arbitrage) — gains status alignment with center, receives preferment in resource distribution, coordinates early adoption to signal loyalty
 *   - Intermediate Administrative Layer: Mixed agent (moderate/constrained) — manages enforcement downward while responding to incentives upward; bears transition costs of norm implementation
 *   - Subordinate Populations: Primary victim (powerless/trapped) — obliged to adopt norms derived from imperial authority without participation in legitimacy formation; suppression of competing claims prevents exit via alternative legitimacy sources
 *   - Competing Legitimacy Claims: Secondary victim (institutional/constrained) — religious authorities, local lords, tradition-keepers displaced by institutional embedding of imperial norms; resource base eroded as state resources concentrate on enforcing center-derived norms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent choice to use charisma as legitimacy mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_legitimation_reading, 0.52).
domain_priors:suppression_score(hybrid_legitimation_reading, 0.48).
domain_priors:theater_ratio(hybrid_legitimation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_legitimation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_legitimation_reading, "Hybrid Legitimation Through Imperial Charisma and Institutional Incentives").
narrative_ontology:topic_domain(hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_legitimation_reading, 'e532db77-5051-4d82-ae08-2e6fbfa45607').
narrative_ontology:cs_kernel_codification('e532db77-5051-4d82-ae08-2e6fbfa45607', distributed).
narrative_ontology:cs_authority_grounding('e532db77-5051-4d82-ae08-2e6fbfa45607', extraction).
narrative_ontology:cs_interpretation_layer_present('e532db77-5051-4d82-ae08-2e6fbfa45607').
narrative_ontology:cs_reading_relation('e532db77-5051-4d82-ae08-2e6fbfa45607', hybrid_legitimation_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e532db77-5051-4d82-ae08-2e6fbfa45607', hybrid_legitimation_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('e532db77-5051-4d82-ae08-2e6fbfa45607', foundational, charisma_and_coercion_both_necessary).
narrative_ontology:cs_axiom_status(charisma_and_coercion_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e532db77-5051-4d82-ae08-2e6fbfa45607', charisma_and_coercion_both_necessary, empirically_contingent).
narrative_ontology:cs_axiom('e532db77-5051-4d82-ae08-2e6fbfa45607', foundational, stratified_adoption_mechanism).
narrative_ontology:cs_axiom_status(stratified_adoption_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e532db77-5051-4d82-ae08-2e6fbfa45607', stratified_adoption_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('e532db77-5051-4d82-ae08-2e6fbfa45607', imperial_exemplary_authority).
narrative_ontology:cs_drift_state('e532db77-5051-4d82-ae08-2e6fbfa45607', institutional_maturation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e532db77-5051-4d82-ae08-2e6fbfa45607', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, imperial_authority_center).
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, elite_adoption_cohort).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, subordinate_populations).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, competing_legitimacy_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE POPULATION (SNARE) — Bears the extraction of enforced norm adoption without participating in the legitimacy formation. Trapped within the imperial jurisdiction; cannot exit. The new norms are presented as cosmically inevitable (derived from the emperor's exemplary conduct) while actually serving institutional interests aligned with the center. Maximum extraction for this agent.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ELITE ADOPTION COHORT (ROPE) — Benefits from early adoption through status alignment with imperial authority and institutional preferment (office, land, honors). Experiences the constraint as coordination: aligning conduct with the emperor's example opens access to state apparatus and wealth redistribution mechanisms. Net beneficiary with genuine agency — can choose timing and degree of adoption.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERMEDIATE ADMINISTRATIVE LAYER (TANGLED ROPE) — Faces mixed pressures: institutional incentives to enforce the new norms among subordinates (career advancement, order maintenance), combined with extraction costs of managing the transition and suppressing competing legitimacy claims. Coordination function exists (stabilizing hierarchical compliance) but paired with asymmetric extraction (the center captures the legitimacy dividend while intermediate actors bear enforcement costs). Constrained by both upward accountability and downward enforcement burdens.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IMPERIAL AUTHORITY CENTER (ROPE) — The originating source of the norm and the symbolic exemplar. Benefits from norm adoption through centralized authority amplification, resource flows from compliant populations, and institutional consolidation. Experiences the constraint as pure coordination: the emperor's conduct is broadcast as the model; adoption cascades follow. The center has maximum arbitrage options (can modify the norm, withdraw the model, redirect enforcement focus). This perspective experiences the constraint as beneficial coordination.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: HISTORICAL ARCHIVE (PITON) — In retrospective view, the constraint becomes atrophied: the symbolic authority transfer (the emperor's exemplary conduct) no longer functions as the primary legitimacy mechanism once the norm is embedded in institutions. The theater persists (formal imperial endorsement, ceremonial re-enactment of the norm origin story) but the functional legitimacy has migrated to institutional habituation. The constraint becomes vestigial performance, maintained through inertia rather than active conversion power.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational frame, the charismatic authority of exemplary conduct appears as an immutable feature of how human societies establish legitimacy: people imitate models and elders, and this mimetic mechanism is built into human cognition. The constraint appears natural — inevitable given human psychology and social bonding mechanics. However, this naturalizes what is actually a contingent choice to use imperial charisma as the *vehicle* for legitimacy transfer. This perspective risks becoming a false summit: the natural law framing occludes the institutional engineering that selects charisma over other legitimacy mechanisms.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_legitimation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_legitimation_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts via asymmetric legitimacy capture (center derives authority, subordinates bear compliance cost) and via resource flows from enforcement (institutional preferment for adopters, diversion from competing claims). The value reflects that this is neither pure coordination (some populations benefit, others solely bear cost) nor pure coercion (charisma does work — voluntary early adoption by elites is real, not entirely performed). The intermediate value captures the hybrid mechanism: charisma drives elite adoption, institutional incentives and suppression carry mass adoption. Suppression (0.48): Moderate. Initial suppression is high (0.70) as competing legitimacy sources are actively targeted and enforcement infrastructure is built; it declines as the norm habituates and becomes institutionally self-sustaining. The endpoint (0.35) reflects that once internalized, the norm requires less active suppression — it has become normal. Theater ratio (0.65): Moderate-high. The constraint's legitimacy anchor is symbolic (the emperor's exemplary conduct), which is inherently theatrical. Theater rises as the norm matures and the actual mechanism becomes enforcement rather than exemplarity; the ceremonies of imperial endorsement persist but their functional role diminishes. The reading claims this is not pure theater (the charisma does affect early adoption) nor pure functionality (the mechanism becomes increasingly institutional rather than charisma-driven).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in the subordinate population's Snare classification versus the elite's Rope classification, mediated by the center's Rope and the administrative layer's Tangled Rope. All four agents experience the same constraint but classify it differently because of their power differentials and exit options. The subordinate (powerless/trapped) sees only extraction and suppression — the norm appears imposed via coercion and cannot be refused. The elite (institutional/arbitrage) sees coordination — the norm aligns them with the center and opens resource flows. The center (institutional/arbitrage) sees pure coordination — the norm is the center's tool for organizing the hierarchy. The administrative layer (moderate/constrained) sees genuine mixing — they coordinate downward but are extracted from upward. The hybrid reading's coherence depends on this stratification: different agents genuinely experience different types because they occupy different structural positions within the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's beneficiary/victim status and exit options relative to this specific constraint. The subordinate (trapped victim) experiences d ≈ 1.0 — maximum target status. The elite (beneficiary with arbitrage) experience d ≈ 0.2 — they gain from the constraint and can restructure it. The center (beneficiary originating the constraint) experiences d ≈ 0.1 — the constraint subsidizes the center. The administrative layer (mixed: enforcer but constrained) experiences d ≈ 0.55 — costs and benefits trade off. The engine's f(d) sigmoid amplifies these to effective extraction (chi): trapped victims experience the constraint as highly extractive; beneficiaries with exit experience it as coordination. The directionality values are not overridden — they derive cleanly from the declared beneficiary/victim structure and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate for this constraint is 'establish new norms through the emperor's exemplary conduct.' In the early phase (t=0-3), this mandate is operationally real: the emperor's conduct IS the primary legitimacy anchor, voluntary adoption among elites responds to this exemplarity, and institutional enforcement builds infrastructure to extend adoption downward. By the mature phase (t=6-10), the mandate has partially atrophied: the norm is now embedded in institutions, hierarchical enforcement carries compliance, and the emperor's exemplary conduct has become ceremonial (piton perspective). The constraint does not resolve its mandatrophy completely — the ceremonial endorsement persists even as its functional role diminishes. This is structurally coherent (the constraint does not vanish) but indicates transition from Tangled Rope to degraded Piton status in the civilizational timeframe. The hybrid reading preserves the mandatrophy tension: the constraint is simultaneously still operative (institutional enforcement carries it) and atrophying (the charisma mechanism decays). A full mandatrophy resolution would require either restoration of the exemplarity function (renewed charisma from a new emperor) or explicit sunset of the norm (formal institutional replacement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charisma_versus_coercion_weight,
    'What proportion of norm adoption results from charismatic influence (genuine voluntary imitation of the emperor''s model) versus coercion (threat of punishment for non-compliance)? Is there a threshold of voluntary adoption that becomes self-sustaining?',
    'Historical analysis of defection rates in early vs late adoption phases; comparison of compliance patterns in high-visibility vs low-visibility norm domains; ethnographic reconstruction of contemporary perception (did subordinates genuinely see the norm as flowing from imperial virtue or as imposed power).',
    'If adoption is >70% charisma-driven: constraint approaches Rope (coordination-dominant), and the Tangled Rope reading underestimates genuine voluntary buy-in. If adoption is <30% charisma-driven: constraint approaches Snare (coercion-dominant), and the charisma framing is purely decorative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charisma_versus_coercion_weight, empirical, 'Charisma vs. coercion weight in determining norm adoption').

omega_variable(
    competing_legitimacy_foreclosure,
    'Does the imperial charisma mechanism actively suppress competing legitimacy claims (traditional, religious, local), or do alternative claims simply lose salience as imperial norms gain institutional embedding?',
    'Historical record of how competing authorities (local lords, religious figures, tradition-keepers) responded; whether they were actively persecuted, marginalized through resource cuts, or slowly displaced by institutional integration that made their roles obsolete.',
    'If active suppression: the constraint includes a coercive component targeting alternative legitimacy sources (Snare feature). If passive displacement: the constraint is primarily coordination-based competition, not extraction (Rope feature). This directly impacts classification of the ''victims'' category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_legitimacy_foreclosure, empirical, 'Whether imperial charisma actively suppresses competing legitimacy claims or displaces them passively').

omega_variable(
    reading_coherence_ambiguity,
    'Is this hybrid reading coherent, or does it conflate endogenous voluntary adoption (the ''climb'' reading) with exogenous imposed adoption (the ''override'' reading)? Can a single mechanism be both charisma-driven AND coercion-backed without becoming a categorical muddle?',
    'Structural analysis: if the mechanism genuinely operates via (a) symbolic transfer at the elite level AND (b) enforcement costs at the mass level, then it is coherent as Tangled Rope. If the mechanism is actually (a) for some populations and (b) for others with no unified process, then the constraint decomposes into two separate stories per the ε-invariance principle.',
    'If coherent: this reading stands as a distinct third pole in the kernel dispute. If incoherent: the hybrid reading is a false synthesis and the kernel resolves into the two pure readings (climb vs override). The constraint_id would be reclassified or split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coherence_ambiguity, conceptual, 'Whether hybrid reading is conceptually coherent or a false synthesis of two incompatible mechanisms').

omega_variable(
    mandatrophy_of_exemplarity,
    'Does the founding mandate for this constraint — ''establish new norms through imperial exemplarity'' — persist after institutionalization, or has the exemplary conduct mechanism become vestigial (the piton perspective) while institutional incentive structures carry the actual norm enforcement?',
    'Comparison of norm propagation mechanisms in early phase (when the emperor actively embodied the norm and was the visibility anchor) versus mature phase (when institutional mechanisms and hierarchical mandate carry compliance, and the emperor''s exemplary conduct becomes ceremonial).',
    'If mandate persists: the constraint remains Tangled Rope throughout. If mandate atrophies: the constraint undergoes piton transition — the charisma function degrades to theater while institutional extraction persists. This addresses the mandatrophy gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_exemplarity, empirical, 'Whether exemplarity mandate persists or atrophies into vestigial theater').

omega_variable(
    false_summit_naturalization_risk,
    'Does the analytical perspective''s ''natural law'' framing of charismatic legitimacy naturalize what is actually an engineered institutional choice? Is there an alternative legitimacy mechanism (democratic consent, rational-legal authority, traditional authority) that would have achieved the same result with lower extraction?',
    'Comparative historical analysis: examination of other cases where legitimacy was established via alternative mechanisms (rationalized law codes, consensus-based deliberation, religious warrant) and measurement of extraction/suppression in those cases versus this one.',
    'If charisma is the lowest-extraction mechanism: the mountain perspective is justified. If alternative mechanisms show comparable or lower extraction: the mountain is a false summit — charisma is contingent, not natural, and the constraint decomposes into an institutional choice (Tangled Rope) not a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, empirical, 'Risk of false-summit naturalization of contingent institutional choice as natural law').

omega_variable(
    kernel_reading_coherence,
    'Which sibling reading (endogenous climb vs exogenous override) does this hybrid reading ultimately reduce to, if forced to choose? Or is the kernel genuinely three-way contested without a truth-value reduction?',
    'Engage the question of authority_grounding within cs_structure: if the authority grounding is ''extraction'' (institutional benefit from norm stability), the reading is closer to override. If grounding is ''distributed'' (no centralized authority determining the mechanism), the reading is genuinely hybrid. If grounding is ''lineage'' (continuous derivation from the emperor''s exemplary precedent), the reading is closer to charisma-as-natural.',
    'The resolution determines the reading_relations mapping: if this reading influences but does not foreclose the siblings, the kernel is genuinely three-way. If one sibling is foreclosed, the kernel structure simplifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether hybrid reading is genuine third pole or reduces to a sibling reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_legitimation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_legit_tr_t0, hybrid_legitimation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hybrid_legit_tr_t3, hybrid_legitimation_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(hybrid_legit_tr_t6, hybrid_legitimation_reading, theater_ratio, 6, 0.68).
narrative_ontology:measurement(hybrid_legit_tr_t10, hybrid_legitimation_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hybrid_legit_be_t0, hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hybrid_legit_be_t3, hybrid_legitimation_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(hybrid_legit_be_t6, hybrid_legitimation_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(hybrid_legit_be_t10, hybrid_legitimation_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_legit_su_t0, hybrid_legitimation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hybrid_legit_su_t3, hybrid_legitimation_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(hybrid_legit_su_t6, hybrid_legitimation_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(hybrid_legit_su_t10, hybrid_legitimation_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hybrid_legitimation_reading, 0.12).
narrative_ontology:affects_constraint(hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_mechanism_kernel. The sibling readings (endogenous_climb_reading and exogenous_override_reading) are separate constraint stories with different ε values and different perspectival structures. The hybrid reading assumes ε ≈ 0.52 (moderate-high extractiveness from the moderate-mixed-mechanism stance). The climb reading would have ε ≈ 0.20-0.30 (lower extraction because charisma works, voluntary adoption reduces suppression cost). The override reading would have ε ≈ 0.70-0.80 (higher extraction because coercion is primary, charisma is theater). All three readings share the same kernel (norm legitimation mechanism) but decompose into distinct stories because their ε values diverge substantively. They are linked via network.affects_constraints because the kernel's ultimate resolution would affect all three: empirical evidence that charisma dominates would favor the climb reading; evidence that coercion dominates would favor the override reading; evidence that both are necessary would confirm the hybrid reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
