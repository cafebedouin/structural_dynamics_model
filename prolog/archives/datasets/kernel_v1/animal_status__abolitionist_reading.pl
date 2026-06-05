% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Animal Instrumental Use as Structural Extraction (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status treats all instrumental
 *   use of nonhuman animals as systematic extraction — a snare operating
 *   through property law, confinement, normalization, and epistemic erasure.
 *   This reading is one interpretation of the contested kernel of 'animal
 *   status,' distinct from the welfare reading (which permits use with
 *   suffering minimization) and the property reading (which denies moral
 *   status entirely). The abolitionist reading forecloses the welfare
 *   reading's core premise: it asserts that animals are rights-holders with
 *   inherent value that precludes instrumental use, period. Welfare reforms
 *   are reclassified from coordination improvements into performative
 *   legitimation of the underlying extraction regime. The constraint exhibits
 *   high extractiveness (0.92) because it systematizes the conversion of
 *   sentient beings into economic commodities, operating at global scale with
 *   ~80 billion animals annually. Suppression is severe (0.78) because it is
 *   enforced through legal property status, physical confinement, normalized
 *   consumption culture, and exclusion of animals from moral standing.
 *   Theater is moderately high (0.65) and rising because welfare reforms
 *   increasingly perform the function of absorbing abolitionist pressure
 *   while maintaining the core regime — the regulatory apparatus is a piton,
 *   a degraded institution whose primary function has become legitimation
 *   rather than meaningful constraint on extraction.
 *
 * KEY AGENTS:
 *   - Nonhuman Animals: Primary victims (powerless/trapped) — held in property status; subject to systematic instrumental use; no exit options or voice
 *   - Extractive Industries (agriculture, pharmaceutical testing, entertainment): Primary beneficiaries (institutional/arbitrage) — benefit from property status enabling resource extraction; experience the constraint as coordination of supply chains (from non-abolitionist frames)
 *   - Animal Welfare Advocates: Secondary actors (moderate/constrained) — constrain extractiveness through labeling and regulation but operate within the abolitionist reading's rejected framework (welfare reform)
 *   - Welfare Regulatory Apparatus: Institutional actor (institutional/arbitrage) — maintains performative oversight; functions as piton from abolitionist perspective
 *   - Consumers (embedded in extraction regime): Moderate-institutional (powerful/constrained) — benefit from low-cost animal products; constrained by convenience and normalized practice; some subset are organized advocates for welfare or abolition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the abolitionist reading as revealing the snare structure underlying normalized instrumental use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.78).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental Use as Structural Extraction (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'f0b4fda9-8512-4731-85e3-0d8baa885a0f').
narrative_ontology:cs_kernel_codification('f0b4fda9-8512-4731-85e3-0d8baa885a0f', distributed).
narrative_ontology:cs_authority_grounding('f0b4fda9-8512-4731-85e3-0d8baa885a0f', distributed).
narrative_ontology:cs_reading_relation('f0b4fda9-8512-4731-85e3-0d8baa885a0f', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('f0b4fda9-8512-4731-85e3-0d8baa885a0f', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('f0b4fda9-8512-4731-85e3-0d8baa885a0f', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('f0b4fda9-8512-4731-85e3-0d8baa885a0f', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('f0b4fda9-8512-4731-85e3-0d8baa885a0f', foundational, instrumental_use_preclusion).
narrative_ontology:cs_axiom_status(instrumental_use_preclusion, holdable).
narrative_ontology:cs_axiom_grounding('f0b4fda9-8512-4731-85e3-0d8baa885a0f', instrumental_use_preclusion, deontological).
narrative_ontology:cs_reference_frame('f0b4fda9-8512-4731-85e3-0d8baa885a0f', animals_as_moral_patients).
narrative_ontology:cs_drift_state('f0b4fda9-8512-4731-85e3-0d8baa885a0f', contemporary_advanced_capitalist_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f0b4fda9-8512-4731-85e3-0d8baa885a0f', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, extractive_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumer_convenience_economy).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, nonhuman_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, ecological_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NONHUMAN ANIMALS (SNARE) — Trapped in total instrumental use regimes. Zero exit options. Suppression enforced via confinement (physical), legal property status (structural), and epistemic erasure (cognitive). The constraint extracts lifetime labor, biological material, and reproductive capacity. Maximum experienced extraction — powerless agents with no voice in systems that govern their use. Classification holds across all time horizons and scales.
constraint_indexing:constraint_classification(animal_status__abolitionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL WELFARE ADVOCATES (TANGLED ROPE) — Constrained by legal barriers (animal property status prevents standing), economic costs (welfare reforms are more expensive than status quo), and social penalty (dismissed as sentimentalist). However, these advocates also coordinate genuine welfare improvements through labeling, certification, and regulatory pressure. The constraint exhibits both extractive asymmetry (victims are nonhuman animals; advocates bear costs of challenging) AND a coordination function (advocates successfully shift practices toward less severe confinement). This is hybrid extraction-coordination rather than pure snare from the advocate perspective. Exit options are constrained but available through legislative pathways.
constraint_indexing:constraint_classification(animal_status__abolitionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE INDUSTRIES (ROPE) — Institutional beneficiaries with arbitrage options. Experience the constraint as pure coordination: managing large-scale animal confinement, processing, and distribution requires synchronized supply chains. The abolitionist reading denies the coordination function entirely — from this reading's frame, there is NO legitimate coordination problem being solved, only extraction being organized. From the industry perspective (non-abolitionist), this is coordination. From the abolitionist perspective (this reading), the industry's 'coordination' is the apparatus of extraction itself. This is a genuine reading difference: does organizing extraction count as coordination? The abolitionist reading says no.
constraint_indexing:constraint_classification(animal_status__abolitionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELFARE REFORM REGULATORY APPARATUS (PITON) — The institutional apparatus of animal welfare regulation (farm animal welfare standards, slaughter stunning requirements, transport duration limits) is largely performative from the abolitionist perspective. These regulations create the appearance of ethical oversight while maintaining the core instrumental use regime. Theater ratio is high because regulation legitimates the system rather than challenging its foundation. The welfare apparatus has become degraded — it exists primarily to absorb abolitionist pressure and preempt more radical change, not to functionally resolve the underlying asymmetry. Institutional inertia maintains it despite the abolitionist critique that it perpetuates extraction.
constraint_indexing:constraint_classification(animal_status__abolitionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / ABOLITIONIST READING (SNARE) — From the civilizational analytical perspective, the abolitionist reading classifies all instrumental animal use as snare: systematic extraction from agents with inherent status precluding such use, enforced through property law, confinement, and normalized cultural practice. The snare classification is stable across all observer positions under this reading because the axiom (animals are rights-holders with intrinsic value) forecloses treating their instrumental use as coordination rather than extraction. This reading does NOT see the constraint as a natural law (mountain), but as a deliberately maintained regime of extraction.
constraint_indexing:constraint_classification(animal_status__abolitionist_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_status__abolitionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_status__abolitionist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(animal_status__abolitionist_reading, TR),
    TR >= 0.70.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92): Very high. The constraint systematically extracts lifetime labor, biological material (flesh, eggs, milk, organs), reproductive capacity, and freedom from confinement from nonhuman animals. The measurement of 0.92 reflects that the extraction is near-total in scope and severity — animals in industrial systems have minimal agency, minimal benefit, and no exit. The slight gap below 1.0 reflects that some individual animals receive some welfare provisions (shelter, food) within the extractive system; these do not negate the extraction but they prevent the score from reaching absolute totality. Suppression (0.78): Very high. The constraint is enforced through multiple interlocking mechanisms: (1) legal property status removing animals from moral standing; (2) physical confinement and control; (3) cultural normalization and epistemic erasure (consumer distance from production processes); (4) economic integration making exit costly for beneficiaries. Theater ratio (0.65 and rising): Moderate-to-high and increasing. The measured trajectory shows that welfare reforms (cage-free, humane slaughter, transport duration limits) have increasingly become performative — they create the appearance of ethical oversight while maintaining the core extraction regime. Theater ratio was lower historically (0.40) when instrumental use was presented without welfare rhetoric; it has risen as the regulatory apparatus has developed. The rising trajectory indicates that suppression is increasingly maintained through legitimation theater rather than naked coercion, a classic piton pattern.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading generates a sharp perspectival gap between the primary beneficiary and the analytical observer. From the extractive industry perspective (non-abolitionist), the constraint is coordination: managing supply chains, standardizing practices, ensuring consumer access. From the abolitionist analytical perspective, that 'coordination' is the apparatus of extraction itself. The welfare advocate perspective inhabits an intermediate position — they see genuine coordination problems (reducing confinement severity) but the abolitionist reading rejects the premise that animals can be legitimately used instrumentally at all. The piton perspective reveals that the welfare apparatus has become degraded — it exists primarily to absorb abolitionist pressure, not to functionally solve the underlying moral asymmetry. The snare perspectives (powerless animals, analytical observer) converge: the constraint is pure extraction enforced through suppression. No single perspective sees the constraint as rope or scaffold from the abolitionist reading's axioms — because the reading forecloses those interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position, not from opinion. Animals are fully in the victim set (directionality high, d ≈ 0.95) because they bear costs without corresponding agency or exit. Extractive industries are beneficiaries with arbitrage options (d ≈ 0.05) — they can shift practices, develop alternatives, but retain exit pathways. Welfare advocates are victims + constrained exit (d ≈ 0.70) — they bear costs of challenging the system but can organize. The critical feature of the abolitionist reading is that it does NOT allow the welfare advocate to be reclassified as a beneficiary: under this reading, welfare reform is part of the extraction apparatus, not a constraint on it. The analytical observer's position (d ≈ 0.80) reflects that from a civilizational perspective, the observer is measuring a global regime that trivializes animal agency — high directionality not because the observer is victimized but because the observer's analytical frame assigns high extraction to the system's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   Extractiveness = 0.92 >> 0.70 threshold requires mandatrophy resolution. The abolitionist reading resolves this by asserting that the classification as snare is not mislabeled coordination — it is accurate extraction. The constraint is not 'overstated as snare when it is really rope.' Rather, the abolitionist axiom (animals are rights-holders precluding instrumental use) makes the snare classification inevitable and correct. Welfare reforms are NOT coordination improvements from the abolitionist frame — they are performance enhancements of the extraction regime. The constraint cannot be reclassified as tangled rope because the reading rejects the premise that any legitimate coordination is occurring. Mandatrophy is resolved by accepting that the reading's axioms foreclose the tangled rope escape: if animals have intrinsic value precluding use, then there is no coordination benefit to balance the extraction. The snare classification is the reading's definitive claim, not a misdiagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_vs_sapience_threshold,
    'Does moral status require consciousness/sentience or full cognitive autonomy/sapience? Where is the boundary, and does the abolitionist reading''s universalist framing require it?',
    'Empirical identification of sentience markers (nociception, preference satisfaction, anticipatory behavior) across taxa; assessment of whether abolitionist axiomatic commitment requires same moral status for all sentient beings or allows graduated protections',
    'If sentience suffices: abolitionist reading''s scope includes insects, mollusks, and some fish — dramatically expanding the victim set and extraction magnitude. If sapience required: scope narrows to vertebrates and some cephalopods — more tractable but concedes a gradient the reading may want to resist. This determines whether ε should rise above 0.92.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_vs_sapience_threshold, empirical, 'Whether moral status requires sentience or sapience; scope implications for abolitionist reading').

omega_variable(
    domestication_axiom_contestation,
    'Does the abolitionist axiom (''animals are rights-holders precluding instrumental use'') apply to domesticated animals created through selective breeding and total economic dependence, or only to wild-type and recently captured animals?',
    'Historical analysis of domestication practices and continuities; examination of whether the abolitionist reading''s foundational axiom (intrinsic value antecedent to human relationship) is even coherent for multi-generational domesticated populations; assessment of whether property status of domesticates has altered their moral standing within the reading',
    'If domestication severs the axiom: abolition applies only to wild animals and recently captured stock; historical domesticates remain in a modified status. If axiom is pre-domestication: abolition is absolute; the domestication history is irrelevant to current moral standing. This affects victim set granularity and directionality derivation for specific animal populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestication_axiom_contestation, conceptual, 'Whether abolitionist axiom applies to domesticated animals with historical dependence').

omega_variable(
    compensation_vs_abolition_tension,
    'If total abolition is adopted, how are the ~80 billion annually used domesticated animals managed? Does the abolitionist reading allow transitional welfare, euthanasia, or sanctuaries, or does each option violate the axiom?',
    'Logical analysis of whether proposed exit mechanisms are internally consistent with the abolitionist axiom (''intrinsic value precludes instrumental use'') — can post-abolition euthanasia be justified under this reading? Can sanctuary confinement? Does answering this question require moving outside the abolitionist frame?',
    'If practical exit mechanisms cannot be logically derived from the axiom: the reading is normatively powerful but pragmatically indeterminate, risking reclassification as scaffold (with a sunset that cannot be operationalized). If they can: the reading''s extractiveness calculation must include transition regime complexity. This affects mandatrophy resolution strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_vs_abolition_tension, conceptual, 'Whether abolitionist axiom permits transitional management strategies for existing domesticated populations').

omega_variable(
    readings_foreclosure_relationship,
    'Does the abolitionist reading''s axiom (''animals are rights-holders precluding instrumental use'') logically foreclose the welfare reading''s axiom (''animals can be used instrumentally if suffering is minimized''), or do they merely coexist as incommensurable frameworks?',
    'Formal logical analysis: can a single framework hold both ''instrumental use is intrinsically prohibited'' and ''instrumental use is permissible if suffering is minimized''? Or does one axiom categorically reject the legitimacy structure of the other? Route to reading_relations declaration.',
    'If forecloses: the welfare reading is analytically incoherent from within the abolitionist framework — the sibling reading''s core premise is ruled out. If coexists: both readings remain live options for different moral communities despite fundamental disagreement. This determines the reading_relations value (forecloses vs coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(readings_foreclosure_relationship, conceptual, 'Whether abolitionist and welfare axioms logically foreclose each other or remain analytically incommensurable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_aboli_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(anim_aboli_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(anim_aboli_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(anim_aboli_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(anim_aboli_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement(anim_aboli_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(anim_aboli_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(anim_aboli_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(anim_aboli_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel generates three distinct constraints corresponding to three coherent readings. The abolitionist reading asserts animals are rights-holders precluding instrumental use (ε ≈ 0.92, snare). The welfare reading permits use with suffering minimization (ε ≈ 0.55, tangled rope). The property reading denies moral status entirely (ε ≈ 0.15, rope or piton depending on enforcement burden). Each reading is ε-invariant within itself; the different ε values reflect structural differences in what is being constrained (use vs. suffering vs. property claims), not measurement ambiguity. All three are linked via network.affects_constraints indicating they interpret a shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
