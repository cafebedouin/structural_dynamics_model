% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Rights Abolitionist Reading: Inherent Value Precluding Instrumental Use
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   contested kernel 'animal_status.' It states the structural claim of the
 *   abolitionist position: animals are inherent-value bearing subjects whose
 *   moral standing precludes instrumental use—meaning any use of animals as
 *   means to human ends (food, research, clothing, labor, entertainment)
 *   violates their rights. This reading is a minority position within law and
 *   policy but a live philosophical and social movement. The constraint is
 *   claimed as a snare because the current institutional arrangement treats
 *   animals as instrumentalizable property while abolitionist advocates argue
 *   this arrangement is fundamentally illegitimate. The extractiveness is
 *   zero because, under the abolitionist reading, instrumental use itself is
 *   impermissible—there is no 'fair' rate of extraction, no welfare
 *   optimization that legitimates the use itself. The suppression is high
 *   because maintaining the property framework and preventing the
 *   abolitionist redefinition of animal status requires active institutional
 *   work: regulatory capture, discourse control in supply chains, and legal
 *   resistance to personhood claims.
 *
 * KEY AGENTS:
 *   - animals_in_all_use_contexts: The ontological victims—all non-human animals subjected to human instrumental use. Powerless, trapped, with no institutional voice.
 *   - agricultural_industry: Primary institutional beneficiary of animal instrumentalization. Sets operational definitions and defends property status.
 *   - research_industry: Secondary institutional beneficiary. Maintains suppression via scientific-necessity and welfare-compliance frames.
 *   - legal_property_regime: The institutional substrate that codifies animals as non-agent objects. The engine of enforcement.
 *   - abolitionist_advocates: The excluded voice that challenges the entire framework. Organized but systematically denied policy seats.
 *   - welfare_reformers: The institutional compromise position. Occupy regulatory bodies and defend bounded-constraint framing against abolitionist claims.
 *   - consumer_base: Benefits from cheap animal products via abundance architecture and cultural normalization. Has genuine exit (plant-based alternatives) but faces suppression through default-system design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.72).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Rights Abolitionist Reading: Inherent Value Precluding Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'e9011c28-9301-4f3a-9ddd-5e18feb465b2').
narrative_ontology:cs_kernel_codification('e9011c28-9301-4f3a-9ddd-5e18feb465b2', fixed_text).
narrative_ontology:cs_authority_grounding('e9011c28-9301-4f3a-9ddd-5e18feb465b2', extraction).
narrative_ontology:cs_interpretation_layer_present('e9011c28-9301-4f3a-9ddd-5e18feb465b2').
narrative_ontology:cs_reading_relation('e9011c28-9301-4f3a-9ddd-5e18feb465b2', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('e9011c28-9301-4f3a-9ddd-5e18feb465b2', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('e9011c28-9301-4f3a-9ddd-5e18feb465b2', foundational, sentience_grants_inherent_value).
narrative_ontology:cs_axiom_status(sentience_grants_inherent_value, holdable).
narrative_ontology:cs_axiom_grounding('e9011c28-9301-4f3a-9ddd-5e18feb465b2', sentience_grants_inherent_value, deontological).
narrative_ontology:cs_axiom('e9011c28-9301-4f3a-9ddd-5e18feb465b2', foundational, inherent_value_precludes_instrumentality).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumentality, holdable).
narrative_ontology:cs_axiom_grounding('e9011c28-9301-4f3a-9ddd-5e18feb465b2', inherent_value_precludes_instrumentality, deontological).
narrative_ontology:cs_reference_frame('e9011c28-9301-4f3a-9ddd-5e18feb465b2', non_instrumentality_of_sentient_beings).
narrative_ontology:cs_drift_state('e9011c28-9301-4f3a-9ddd-5e18feb465b2', contemporary_industrial_animal_use_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e9011c28-9301-4f3a-9ddd-5e18feb465b2', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_in_all_use_contexts).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).
:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero across the measurement interval because the abolitionist reading does not debate the 'correct' rate of animal use—it forbids instrumental use categorically. This is structurally distinct from the welfare reading (which optimizes use conditions and accepts bounded extraction) and the property reading (which denies any moral constraint). The measured extractiveness of 0.0 reflects the reading's internal consistency: if animals are rights-holders, they cannot be rightfully extracted from at any rate.
 *   
 *   Suppression is high (0.72) and rising through the interval because maintaining animal instrumentalization against the abolitionist claim requires active suppression: controlling discourse in agriculture (omitting slaughter footage), funding research that frames animal cognition as less than human (comparative deficiency models), and resisting legal redefinition. The rising trajectory from 0.58 to 0.73 (t=0 to t=40) reflects the intensification of suppression as abolitionist advocacy grew more sophisticated and visible (t=40–50 slight decline reflects a measurement plateau rather than success—the constraint's suppression stabilized at high levels).
 *   
 *   Theater_ratio rises from 0.25 to 0.41 because welfare reforms, industry corporate-social-responsibility claims, and 'humane' labeling have proliferated while the fundamental instrumentalization persists. This theatrical activity (welfare certification, corporate pledges, minor regulatory reforms) performs constraint without addressing the core abolitionist claim. The theater ratio stabilizes at t=40+ because welfare theater has reached saturation in consumer awareness.
 *   
 *   Accessibility_collapse (0.68) reflects that alternatives to animal products are materially accessible (plant-based foods, synthetic fabrics, non-animal research methods exist), but the suppression mechanisms make them psychologically and infrastructurally invisible: the default food system is animal-based, cultural narratives treat animal consumption as natural/inevitable, and alternatives are framed as niche/inferior. Animals have zero accessibility to escaping instrumentalization—their collapse is total (ontological).
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist reading's perspective, the constraint is a regime of injustice: animals are wrongfully instrumentalized and their moral claims are systematically suppressed. From the agricultural/research institutional perspective, the constraint is solved: welfare regulations address moral concerns while preserving necessary human practices. From the welfare-reformer perspective, the constraint is a managed tension: animal interests deserve protection but not at the cost of human flourishing. From the consumer perspective, the constraint is invisible: animal products are abundant, affordable, and normalized; alternatives are costlier and culturally marked as abnormal. The engine should compute different types from each seat: abolitionists compute the current arrangement as snare (illegitimate extraction of powerless agents); industry seats compute it as rope (justified coordination); welfare reformers compute it as tangled_rope (genuine moral concern bounded by pragmatic limits). The gap reflects not observer uncertainty but genuine structural asymmetry in how the constraint is experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals occupy the full-victim end of the directionality spectrum: trapped exit (they cannot exit the category 'animal'), powerless power (they have no institutional voice), and the abolitionist reading directly identifies them as the extraction target. Their d approaches 1.0. Agricultural and research institutions occupy the beneficiary end (d near 0.0): they collect rents from animal instrumentalization, control the definitions, and have mobile exit (they could cease animal use but choose not to). The legal property regime is analytically positioned—it is the machinery, not an agent collecting rents. Consumers occupy the middle: they benefit from cheap products (low-to-moderate d) but face suppression through availability architecture and normalization rather than explicit coercion (d~0.4–0.5). Welfare reformers occupy observer seats: they are trying to move the constraint but have interests in both coordination (animal welfare) and avoiding the full abolitionist terminus.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the animal property/welfare regime is efficiency and profit in animal-product supply; the function of maintaining that supply persists. But the abolitionist reading identifies a mandate-creep: the property regime was instituted to serve human flourishing, yet it now serves routine extraction disconnected from necessity (factory farming for cost-reduction, research for marginal gains, consumption for preference satisfaction). The abolitionist claim is that this mandate has been functionally obsolete since plant-based and synthetic alternatives became viable. The regime persists through institutional inertia (entrenched agricultural systems, research funding structures, legal precedent) and through theater (welfare reforms that perform constraint without addressing the core claim). The mandatrophy is advanced: the founding problem 'how to safely nourish humans' is solved by non-animal means, yet the constraint persists, now functioning primarily as rent extraction from a habit-dependent consumer base.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_as_moral_threshold,
    'Is sentience (capacity for suffering) the correct moral threshold for inherent value, or does moral standing rest on some other capacity (agency, rationality, social embeddedness, species membership)? Does the abolitionist reading''s sentience-based threshold foreclose other moral groundings?',
    'Philosophical analysis of what capacities ground moral status and empirical investigation of which capacities animals demonstrably possess. Cross-cultural moral traditions differ on this threshold; no empirical fact settles it alone.',
    'If sentience is insufficient (if rationality or human-species membership is required), some instrumental use could be justified. If sentience is sufficient but contested, the abolitionist position is logically coherent but meets cultural/philosophical resistance. If non-human animal sentience is denied or minimized, the whole abolitionist argument founders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_as_moral_threshold, conceptual, 'Whether sentience is the right moral threshold or whether the abolitionist reading rests on a contestable philosophical claim.').

omega_variable(
    pragmatic_feasibility_vs_moral_claim,
    'Is the abolitionist reading''s force conditional on the existence of viable alternatives to animal use, or is it a deontological claim that holds regardless of cost? If alternatives become unavailable, does the moral claim weaken?',
    'Examine whether abolitionist advocates hold the claim as unconditional (animals may not be instrumentalized even if it costs human welfare) or conditional on feasibility. Test against scenarios where animal use becomes necessary for human survival.',
    'If unconditional, the reading is pure rights-based deontology and creates genuine moral dilemmas in scarcity. If conditional, the reading is pragmatically bounded and coherent but potentially weaker than framed. This affects how the constraint functions under different material conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_feasibility_vs_moral_claim, preference, 'Whether the abolitionist claim is moral axiom or pragmatically bounded principle.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of the abolitionist reading primarily structural (institutions actively resist it, capture discourse, control information) or internalized (consumers have habitual/cultural/identity attachment to animal use that persists even after barrier removal)?',
    'Post-exit trajectory studies: if consumers abandon animal products after legal prohibition but before internalized attachment is addressed, suppression is primarily structural. If attachment persists as a psychological/identity phenomenon post-exit, suppression is partially internalized.',
    'If structural, changing law and incentives could shift the constraint rapidly. If internalized, even legal abolition would require cultural work to address identity-fusion with consumption practices. This affects transition feasibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the measured suppression is structural coercion or internalized habit-identity.').

omega_variable(
    welfare_reform_cooptation_vs_genuine_progress,
    'Do welfare reforms (cage-free eggs, antibiotic-free meat, animal-testing alternatives in cosmetics) represent genuine progress toward the abolitionist endpoint or do they function primarily as legitimating theater that extends the constraint''s life?',
    'Track whether welfare reforms correlate with reduced animal use (abolitionist progress) or with maintained/growing absolute numbers of animals instrumentalized (theater without functional change). Examine whether reform participation shifts advocates toward abolitionist positions or consolidates them in welfare frames.',
    'If genuine progress, theater_ratio should decline as welfare reforms move toward abolition. If theater only, theater_ratio should rise (more performative activity, same extraction). The measured rising theater_ratio (0.25→0.41) suggests the latter, but this is measurement of the abolitionist reading''s claim about welfare reform, not independent verification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_cooptation_vs_genuine_progress, empirical, 'Whether welfare improvements are progress toward abolition or institutional co-optation.').

omega_variable(
    reading_foreclosure_logic,
    'Does the abolitionist reading''s core premise—that animals are rights-holders with inherent value—logically FORECLOSE the property reading''s premise (animals are non-agent objects), or do they merely COEXIST as incompatible framings held by different parties?',
    'Formal logical analysis: if one premise logically entails the negation of the other within any consistent framework, foreclosure holds. If both can be held without logical contradiction (just different commitments), coexistence holds. The distinction affects how institutional contest could resolve.',
    'Foreclosure suggests one reading must eventually win; institutional change would require one to collapse. Coexistence suggests both readings could persist indefinitely across different institutional sectors. This affects prognosis for constraint-type stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_logic, conceptual, 'Logical relationship between abolitionist and property readings: foreclosure or coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.0).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.0).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel instantiates three structurally distinct constraint stories, each with a different ε and a different structural relationship to the victim/beneficiary set. The abolitionist_reading claims zero extractiveness (use itself is impermissible); property_reading claims low extraction (property rights fully legitimate use); welfare_reading claims bounded extraction (use is constrained by welfare limits but not prohibited). These are not three measurements of one constraint—they are three readings of a contested kernel, each producing a different constraint type when the engine computes per-seat classification. The ε-invariance test: if changing the reading changes the extractiveness, the readings are separate constraints. Here it does, substantially. Each story carries its own founding problem, its own beneficiary/victim structure, and its own institutional defenders. Network links connect them as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
