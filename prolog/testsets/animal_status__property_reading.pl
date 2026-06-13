% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Property Without Independent Moral Standing
 *   domain: legal/ethical/political
 *
 * SUMMARY:
 *   The property reading of animal status asserts that animals lack
 *   independent moral standing and exist in law solely as objects of human
 *   ownership and use. Under this reading, animals are not victims—they are
 *   not morally relevant claimants. The constraint arises only as a framework
 *   dispute between human property holders and human advocates. Commercial
 *   users (agriculture, research, entertainment) benefit from unrestricted
 *   discretion; doctrinal property-law tradition benefits from coherence and
 *   internal consistency. Animal advocates and abolitionist philosophers pay
 *   the cost of exclusion from legal standing. The constraint is claimed as
 *   rope (coordination of property law) while measured extractiveness is
 *   near-zero (0.08) because the constraint does not extract from animals
 *   themselves—it simply denies them standing. The theater ratio is
 *   low-rising (0.08 → 0.18) because welfare statutes increasingly perform
 *   symbolic recognition of animal protection while leaving the property
 *   framework intact; the performative element grows as the gap between
 *   welfare rhetoric and property-doctrine reality widens. The reading is one
 *   of three siblings: abolitionist_reading (animals are rights-holders),
 *   welfare_reading (animals are sentient beings with constrained interests),
 *   and this one (property_reading). Per the kernel frame, this constraint
 *   does not flatten the dispute—it instantiates one coherent position within
 *   it.
 *
 * KEY AGENTS:
 *   - commercial_animal_users: collect value from unrestricted use; depend on property-reading legal standing
 *   - property_law_doctrinal_tradition: benefits from coherence; observes that property law requires non-claimant object category
 *   - animal_advocates: excluded from legal standing; operate as voluntary charity rather than rights vindication
 *   - individual_animals: powerless, trapped, no standing; explicitly outside the moral community under this reading
 *   - welfare_legislators: enforce cruelty prohibitions while preserving property frame; balance commercial and advocacy pressures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.08).
domain_priors:suppression_score(animal_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_status__property_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property Without Independent Moral Standing").
narrative_ontology:topic_domain(animal_status__property_reading, "legal/ethical/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'da6b3b69-1ffe-4caf-b608-d1e9c26bf10e').
narrative_ontology:cs_kernel_codification('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', formalized).
narrative_ontology:cs_authority_grounding('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', lineage).
narrative_ontology:cs_interpretation_layer_present('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e').
narrative_ontology:cs_reading_relation('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', foundational, moral_standing_rational_agency_exclusive).
narrative_ontology:cs_axiom_status(moral_standing_rational_agency_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', moral_standing_rational_agency_exclusive, deontological).
narrative_ontology:cs_axiom('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', foundational, property_law_object_category_necessary).
narrative_ontology:cs_axiom_status(property_law_object_category_necessary, holdable).
narrative_ontology:cs_axiom_grounding('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', property_law_object_category_necessary, conventional).
narrative_ontology:cs_reference_frame('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', classical_property_doctrine).
narrative_ontology:cs_drift_state('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', contemporary_welfare_statute_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da6b3b69-1ffe-4caf-b608-d1e9c26bf10e', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_users_commercial).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, property_law_doctrinal_tradition).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-minimal (0.08) because the constraint operates at the categorical level—it does not extract from animals (animals are not stakeholders; they have no standing to make claims). Instead, it denies a class of entities any legal voice. This is a structural exclusion, not a transfer mechanism. Suppression is similarly low (0.12) because the constraint's operation does not require active coercion—it requires only doctrinal consistency and enforcement of property-law default rules. When an animal advocate sues on behalf of an animal, they lose not because of fierce suppression but because standing doctrine excludes them by definition. Theater is rising (0.08 → 0.18) because welfare statutes increasingly create the appearance of animal-interest recognition while leaving the property framework untouched. The measured gap between welfare rhetoric and property-doctrine reality grows over the interval, indicating performative maintenance of the constraint despite intellectual pressure. Resistance is high (0.72) because animal-advocacy movements, abolitionist philosophy, and welfare-statute proliferation all challenge the property reading's adequacy; it persists despite sustained intellectual and political opposition. The claim/metric independence is critical here: the constraint is claimed as rope (coordination solution) because property law genuinely does solve a coordination problem; the low extractiveness reflects that the constraint extracts nothing from animals themselves (animals are not moral claimants). The high resistance reflects that the constraint does extract something: it extracts the capacity to make claims from anyone (advocates, philosophers, animals) who would operate outside the property framework. The constraint is not claimed as snare because there is no hidden cover story—the property reading is explicit about what it is doing. It is not claimed as tangled_rope because there is no asymmetric coordination burden; the coordination function is the elimination of animal claims from the legal space.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (commercial users, property-law doctrine), the property reading is the natural order—property law's logical necessity. From the excluded seat (animal advocates, abolitionist philosophers), it is pure structural negation of standing. The constraint's classification diverges across seats because the reading itself is a contestation about what the legal domain *is*. No per-seat override is needed here; the structural data (beneficiary, excluded roles; differential power and exit options) drives the divergence automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial users have d near 0.1 (strong beneficiary position: they collect unrestricted use, face minimal enforcement cost, have high exit options via jurisdictional arbitrage). Property-law tradition has d near 0.05 (analytical observer position; the tradition is vindicated by the reading's internal coherence, not extracted from). Animal advocates have d near 0.8 (near-target position: they are excluded from standing, constrained to operate as voluntary charity, have limited exit options within the legal system). Individual animals have no d value because they are not stakeholders in the engine's sense—they have no power atom, no time horizon, no exit options. They are excluded by the reading's definition. If the abolitionist reading were instantiated, individual animals would shift to d near 1.0 (full targets of a constraint that denies their rights). The directionality difference between property_reading and abolitionist_reading is exactly the point of the kernel structure: same legal domain, different computational routes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to structure property law coherently when animals lack moral standing) has status=contested because the property reading claims it as live (necessary to doctrine's coherence) while abolitionist and welfare readings claim it is false or already solved. The disappearance verdict (world_rearranges) confirms that the constraint is not mandatrophic: if animal property status were rescinded, the world would reorganize substantially. However, the rising theater_ratio (0.08 → 0.18) signals that performative welfare rhetoric is increasingly layered over the property framework without resolving the underlying status question. This is not mandatrophy (decay past function) but rather growing incoherence: welfare statutes gesture toward animal interests while property doctrine denies them standing. The measurement series captures this widening gap. The constraint is not claimed as piton (degraded maintenance) because commercial users and property-law doctrine still actively defend the reading; the resistance is high precisely because the reading requires ongoing intellectual and legal work to maintain against welfare and abolitionist pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_boundary,
    'Is the exclusion of animals from moral standing a logical consequence of property-law doctrine, or a contingent historical choice that could be revised while preserving property law''s coherence?',
    'Comparative jurisprudence examining how legal systems that have expanded animal legal standing (India''s recognition of some animals as non-property entities, the EU''s animal welfare directives) have handled property law''s logical structure. If property law remains coherent despite expanded animal standing, the necessity claim fails.',
    'If the exclusion is contingent rather than necessary, the property reading loses its claim to doctrinal inevitability and becomes a contestable policy choice. This would shift it from rope (coordination solution) toward snare (pure extraction) or tangled_rope (coordination with asymmetric burden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_boundary, conceptual, 'Whether animal exclusion from moral standing is logically necessary to property law or a revisable choice.').

omega_variable(
    animal_cognitive_capacities,
    'Do animals possess cognitive and emotional capacities that, if recognized in law, would establish moral relevance even within a revised property framework?',
    'Comparative cognition research documenting animal agency, social structures, and preference-satisfaction. If animals demonstrate capacities for suffering, planning, and social coordination, does this establish grounds for moral consideration even if not full rights-holder status?',
    'If animals possess morally relevant capacities, the property reading''s axiom (rational agency as the sole basis for moral standing) becomes empirically undermined. This would support migration toward welfare or abolitionist readings without requiring complete abandonment of property-law structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_cognitive_capacities, empirical, 'Whether animals possess morally relevant capacities under the property reading''s own framework.').

omega_variable(
    welfare_statute_drift,
    'Do welfare statutes, when enforced, begin to create de facto animal interests that the property reading cannot accommodate without self-contradiction?',
    'Historical analysis of welfare-statute enforcement: if courts begin to justify protections by reference to animal suffering (rather than pure owner-property considerations), does the property reading''s coherence degrade?',
    'If welfare enforcement creeps toward recognizing animal interests as legally relevant, the property reading begins to coexist uneasily with its own statutory superstructure. This would create pressure toward either strengthening the property reading (rejecting welfare as unauthorized) or migrating toward welfare or abolitionist framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_drift, empirical, 'Whether welfare enforcement begins to undermine the property reading''s coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t8, animal_status__property_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(anim_tr_t16, animal_status__property_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(anim_tr_t24, animal_status__property_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(anim_tr_t32, animal_status__property_reading, theater_ratio, 32, 0.18).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(anim_be_t8, animal_status__property_reading, base_extractiveness, 8, 0.07).
narrative_ontology:measurement(anim_be_t16, animal_status__property_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement(anim_be_t24, animal_status__property_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement(anim_be_t32, animal_status__property_reading, base_extractiveness, 32, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(anim_su_t8, animal_status__property_reading, suppression_requirement, 8, 0.1).
narrative_ontology:measurement(anim_su_t16, animal_status__property_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement(anim_su_t24, animal_status__property_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement(anim_su_t32, animal_status__property_reading, suppression_requirement, 32, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.05).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel comprises three structurally distinct constraints, each with its own ε, beneficiary structure, and classification. This property_reading is the doctrinal default (high institutional inertia). The welfare_reading introduces sentience-based limits while preserving use. The abolitionist_reading denies all instrumental use. They are linked via network.affects_constraints because the property reading's persistence affects both downstream readings: welfare advocates argue against it via empirical cognition data; abolitionists argue it is logically incoherent. Each reading should be generated as a separate constraint story with its own stakeholder surface, six_questions, and measurement grid. The three together form a constraint family modeling a single contested domain under three competing framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__property_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
