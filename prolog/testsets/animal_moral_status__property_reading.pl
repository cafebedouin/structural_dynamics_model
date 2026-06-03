% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Moral Status: Property Reading
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The property reading of animal moral status is one of three structurally
 *   distinct readings of a contested kernel: the question of what ontological
 *   and moral standing animals possess in human-centered legal and ethical
 *   frameworks. The property reading instantiates animals as res — things
 *   without rights or independent moral standing — whose use and disposition
 *   are determined entirely by the interests of human owners/users. The
 *   reading appears to function as a natural law: property is defined as
 *   objects without rights; animals lack the cognitive characteristics
 *   traditionally believed to ground legal personhood (rationality, language,
 *   self-governance); therefore animals must be property. However, the
 *   presence of two coherent alternative readings (welfare, abolitionist) and
 *   identifiable beneficiaries (property owners whose economic interests
 *   depend on maintaining animal property status) suggests this may be a
 *   false summit — a constraint that naturalizes what is actually a normative
 *   choice. The property reading's extractiveness (0.08) reflects that the
 *   constraint itself does minimal coercive work: property law operates
 *   through definition rather than enforcement. The suppression (0.02) and
 *   low theater ratio (0.15) indicate the reading presents itself as logical
 *   necessity rather than as a performance. The constraint is most accurately
 *   modeled as a mountain in doctrine (appears logically necessary within the
 *   property framework) while simultaneously being a false summit candidate
 *   (the logical necessity depends on contestable axioms about personhood and
 *   moral status).
 *
 * KEY AGENTS:
 *   - Property Owners/Resource Users: Primary beneficiaries — institutional power, arbitrage exit, economic stake in maintaining animal property status
 *   - Welfare Reformers: Secondary beneficiaries — institutional power, arbitrage exit, can operate within property framework while advocating regulated use
 *   - Animals: Primary victims (from abolitionist/welfare perspective) or absent entirely (from property perspective) — trapped, powerless, no standing
 *   - Legal Doctrine System: Institutional authority grounding the reading through formalized personhood/property definitions
 *   - Analytical Observer: Civilizational perspective assessing whether property reading is natural law or false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.08).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.02).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Moral Status: Property Reading").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'ebddb86f-eefb-4783-a9d6-10a5faf2da00').
narrative_ontology:cs_kernel_codification('ebddb86f-eefb-4783-a9d6-10a5faf2da00', formalized).
narrative_ontology:cs_authority_grounding('ebddb86f-eefb-4783-a9d6-10a5faf2da00', extraction).
narrative_ontology:cs_interpretation_layer_present('ebddb86f-eefb-4783-a9d6-10a5faf2da00').
narrative_ontology:cs_reading_relation('ebddb86f-eefb-4783-a9d6-10a5faf2da00', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('ebddb86f-eefb-4783-a9d6-10a5faf2da00', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('ebddb86f-eefb-4783-a9d6-10a5faf2da00', foundational, animals_lack_personhood_capacities).
narrative_ontology:cs_axiom_status(animals_lack_personhood_capacities, holdable).
narrative_ontology:cs_axiom_grounding('ebddb86f-eefb-4783-a9d6-10a5faf2da00', animals_lack_personhood_capacities, empirically_contingent).
narrative_ontology:cs_axiom('ebddb86f-eefb-4783-a9d6-10a5faf2da00', foundational, property_requires_lack_of_rights).
narrative_ontology:cs_axiom_status(property_requires_lack_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('ebddb86f-eefb-4783-a9d6-10a5faf2da00', property_requires_lack_of_rights, deontological).
narrative_ontology:cs_reference_frame('ebddb86f-eefb-4783-a9d6-10a5faf2da00', animals_as_legal_property).
narrative_ontology:cs_drift_state('ebddb86f-eefb-4783-a9d6-10a5faf2da00', contemporary_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ebddb86f-eefb-4783-a9d6-10a5faf2da00', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, resource_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTY RIGHTS FRAMEWORK (MOUNTAIN) — The property reading instantiates itself as natural law through formal doctrine. Animals-as-property follows from the foundational axiom that property is res (things without legal personhood) while persons hold rights. This is presented as a logical necessity, not a contingent choice. The framework experiences zero degrees of freedom: to deny that animals are property would require reconceptualizing the entire edifice of property law.
constraint_indexing:constraint_classification(animal_moral_status__property_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROPERTY OWNER (MOUNTAIN) — From the position of an agent who owns animals as productive assets, the constraint appears immutable. The property status of animals is foundational to the economic logic that justifies use; treating animals as property is the condition of possibility for extracting value. From this perspective, the constraint is unchangeable because denying it would dissolve the entire legitimacy framework for the owner's interests.
constraint_indexing:constraint_classification(animal_moral_status__property_reading, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE REFORMER (ROPE) — A reformer operating within the property framework (accepting that animals are property) but advocating for regulated use and protection from 'unnecessary' suffering experiences the constraint as coordination. The reformer sees the property-status-plus-welfare-regulation as a stable equilibrium that balances property rights with animal protection norms. From this view, the constraint is changeable in principle (one could advocate for stricter welfare standards) but the property foundation itself is not questioned.
constraint_indexing:constraint_classification(animal_moral_status__property_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANIMAL (SNARE) — The animal has no standing in the property framework and no means to exit it. The constraint as experienced by the animal is pure extraction: use, confinement, and ending of life are determined entirely by the property owner's interests. From the animal's structural position, there is no voice, no representation, and no escape. This is the only perspective where the animal is not absent from the framework — the animal is present as object, not subject.
constraint_indexing:constraint_classification(animal_moral_status__property_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal analytical perspective, the property reading appears as a natural law of legal doctrine: property is defined as res without rights; animals lack the cognitive/linguistic capacities that ground legal personhood; therefore animals are property. This appears logically necessary. However, the presence of competing readings (welfare, abolitionist) and the empirically contingent axioms grounding the property reading suggest this is a false summit — the constraint naturalizes what is actually a normative choice.
constraint_indexing:constraint_classification(animal_moral_status__property_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_moral_status__property_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_moral_status__property_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(animal_moral_status__property_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_moral_status__property_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The property reading does not require active extraction mechanisms — it operates through definition. Once property status is established, use flows from ownership by logical entailment, not through coercive enforcement. The constraint extracts value for property owners not by suppressing alternatives but by defining animals outside the moral/legal consideration set. Suppression (0.02): Minimal. The property framework does not suppress alternatives through force; it suppresses them through definitional exclusion. The constraint operates by placing animals outside the domain where interests count, not by forcibly preventing animals from asserting interests. Theater ratio (0.15): Low. The property reading presents itself as logical doctrine, not as performance. The constraint's legitimacy depends on appearing as natural law (logical necessity), not on maintaining a theatrical performance. However, the theater ratio rises slightly over time (0.10 → 0.20) as challenges to the reading accumulate — the reading increasingly must perform its naturalness against competing frameworks. Claimed type: Mountain. The property reading instantiates itself as natural law through the logical structure: property = objects without rights; animals lack personhood-defining capacities; therefore animals are property. This appears logically necessary from within the framework. False summit candidate: The presence of beneficiaries (property owners) and competing coherent frameworks (welfare, abolitionist) suggests the mountain may be false — the logical necessity depends on contestable axioms about what capacities ground moral standing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a striking perspectival divergence driven by whether one accepts the property reading's foundational axiom (animals lack independent moral standing). From the property reading's internal perspective (property rights framework, property owners, welfare reformers), the constraint is either mountain (logical necessity) or rope (coordination of use with welfare protections). From outside the property reading (the animal's actual structural position, the abolitionist challenge), the constraint is snare (pure extraction with no voice, no exit, no standing). This gap is not merely a difference in evaluation — it is a difference in whose interests are counted. The property reading achieves its mountain status by excluding animals from the set of entities whose interests matter. The abolitionist and welfare readings reject this exclusion and therefore see extraction where the property reading sees law. The perspectival gap reveals the reading as a false summit: it appears as natural law only from perspectives that accept its foundational axiom about animal moral status.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation is structurally unusual for this constraint because the property reading excludes animals from the framework entirely. Animals do not appear as victims in the base_properties (they are res, not agents with standing). Instead, the constraint operates by positioning animals outside the set of entities whose interests are morally relevant. Property owners are beneficiaries (institutional power, arbitrage exit — they can use property freely). Welfare reformers are beneficiaries with constrained exit (they operate within the property framework but advocate regulated use). The only perspective that includes animals as victims (the SNARE perspective, Perspective 4) is the animal's own structural position — trapped, powerless, with zero exit options. From the property reading's internal logic, this perspective is not valid because animals lack standing to have interests. This creates a diagnostic gap: the property reading is self-sealing (animals cannot appear as victims because they lack moral standing) while the snare perspective shows that animals empirically bear costs. The gap reveals the reading's false summit structure: the natural law appearance depends on excluding from consideration the very agent who experiences the constraint as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by being radically asymmetric: the property reading does not claim to serve animals' interests — it explicitly subordinates them. There is no tension between coordination and extraction because the reading denies that animals have interests to be coordinated or extracted from. However, this resolution of mandatrophy is precisely what makes it a false summit candidate. The constraint achieves conceptual coherence by placing animals outside the moral consideration set, but this coherence depends on a contestable axiom (animals lack moral standing). When alternative readings (welfare, abolitionist) are formulated, they reintroduce the animals as agents whose interests matter, and the property reading's mountain status collapses into a snare (extraction of animal use justified by denying animals' capacity to matter). The mandatrophy is not resolved — it is displaced. The property reading avoids acknowledging it by erasing the agent whose suffering would trigger it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contestation_vs_natural_law,
    'Is the property status of animals a natural law (logical necessity from the nature of property and personhood) or a contested normative reading of a kernel that other coherent frameworks deny?',
    'If the abolitionist and welfare readings are logically coherent (can be held without self-contradiction in a single framework), then the property reading is not natural law but one reading among several. If those readings entail fundamental contradictions with basic legal principles, then the property reading may be necessary. Evidence: coherence of existing abolitionist legal frameworks (New Zealand, India animal-rights constitutions), consistency of welfare frameworks with property law, and logical structure of each reading''s axioms.',
    'If reading is one among several (not natural law): reclassify as tangled_rope or snare depending on extraction from animals. If reading is necessary: confirm mountain. This is the FSM diagnostic question for the entire kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_vs_natural_law, conceptual, 'Whether property reading is natural law or contested normative choice').

omega_variable(
    sentience_and_moral_status_link,
    'Does the property reading deny animal sentience, or does it accept sentience but deny that sentience entails moral standing?',
    'Textual/doctrinal analysis: examine whether property reading treats animal capacity for suffering as morally irrelevant (cognitive failure) or as real but subordinated (normative choice). Survey contemporary property-law doctrine on sentience recognition. Behavioral evidence: do property regimes permitting use correlate with sentience denial or with sentience-indifference?',
    'If property reading denies sentience: constraint may be mountain (based on false empirical claim). If reading accepts sentience but denies moral weight: constraint is tangled_rope or snare (normative subordination of acknowledged interests). This distinction clarifies whether the reading is empirically contingent or deontologically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_and_moral_status_link, empirical, 'Whether property reading denies sentience or merely denies moral relevance of sentience').

omega_variable(
    axiom_status_contingency,
    'Are the foundational axioms of the property reading (personhood requires reason/language, property requires lack of rights, interests subordinate to ownership) empirically contingent claims or deontological principles?',
    'Genealogical analysis of axiom grounding in philosophical/legal tradition. Track whether axioms are presented as discoveries (empirical) or definitions (deontological). Examine whether the reading''s authority structure explicitly grounds axioms in empirical premises (Descartes on rationality, Locke on labor) or pure conceptual necessity (property must exclude rights by definition). Test: would empirical evidence that animals possess capacities traditionally thought to ground personhood (theory of mind, linguistic recursion, self-awareness) logically entail reclassification?',
    'If axioms are empirically_contingent: rising evidence of animal cognition erodes the reading (axiom_overriding drift). If deontological: empirical discoveries about animal minds do not logically touch the reading. This clarifies whether the property reading is vulnerable to falsification or structurally immune.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_status_contingency, conceptual, 'Whether property reading axioms are empirically contingent or deontological').

omega_variable(
    false_summit_detection_signal,
    'The presence of beneficiaries (property owners/users) on a constraint classified as mountain is diagnostically anomalous. Is this a genuine natural law or a constructed constraint that benefits identifiable agents?',
    'FSM analysis: if the property reading is truly a natural law (logical necessity of legal personhood), then beneficiary identification is accidental — the law benefits some but that benefit does not explain the law. If the reading is a normative choice, beneficiaries should show economic or institutional stake in maintaining it. Evidence: intensity of defense of property status when alternative readings are proposed; regulatory capture by property owners; suppression of alternative frameworks; resistance to challenge correlated with beneficiary interests.',
    'If false summit confirmed: reclassify to tangled_rope (coordination of property rights + extraction of animal use). If genuine mountain: beneficiaries are incidental to a logical necessity. The engine''s false_summit_mountain signature will trigger on beneficiary presence; this omega documents the structural basis for signature evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_signal, conceptual, 'FSM detection: whether beneficiary presence indicates false summit or incidental benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_prop_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(animal_prop_tr_t100, animal_moral_status__property_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(animal_prop_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(animal_prop_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(animal_prop_be_t100, animal_moral_status__property_reading, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal moral status kernel decomposes into three constraint stories, one per reading. The property_reading story models the constraint as it appears from within the property law framework (mountain). The welfare_reading models the constraint as it appears from within regulated-use frameworks (tangled rope). The abolitionist_reading models the constraint as it appears from outside all use frameworks (snare). All three stories link to one another via affects_constraints. The decomposition is mandatory because the three readings have different ε values, different beneficiary/victim structures, and different logical foundations. No single story can capture all three readings as perspectives — each reading is itself a complete framework instantiating a different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
