% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status (Abolitionist Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the ABOLITIONIST READING of the
 *   contested animal-status kernel. The reading holds that animals are moral
 *   persons with the basic right not to be property; that property status
 *   itself constitutes the injustice; and that all use of animals is
 *   categorically impermissible regardless of welfare conditions. From this
 *   reading's structural perspective, the constraint that treats animals as
 *   property is a pure extraction mechanism — the entire surplus of animal
 *   labor and biological products flows to humans (property holders,
 *   researchers, consumers) while animals bear the complete cost of
 *   captivity, use, and denial of autonomy. The constraint is actively
 *   enforced through legal property classification, institutional
 *   gatekeeping, and suppression of abolitionist voices. This reading
 *   COEXISTS WITH the property reading (animals are property; economic value
 *   dominates) and the welfare reading (animals are sentient; use is
 *   acceptable if regulated). The readings constitute different coherent
 *   framings of the same kernel; each grounds a different constraint story
 *   with different ε values and victim-set boundaries.
 *
 * KEY AGENTS:
 *   - animals_held_as_property: moral patients denied personhood, fully victimized under this reading
 *   - property_holders_extracting_animal_labor_and_products: institutional agenda-setters and primary beneficiaries
 *   - animal_advocates_excluded_from_institutions: victims of the suppression machinery that excludes abolitionist voices
 *   - welfare_reform_advocates: institutional observers holding a sibling reading; strategic tension with abolitionists over incrementalism vs. categorical change
 *   - scientific_research_establishment: institutional beneficiary dependent on animal property status
 *   - consumer_masses_benefiting_indirectly: structural beneficiaries whose exit is constrained by price and institutional suppression of moral salience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, 'e4449cdc-f918-4c28-b891-598e3db3986d').
narrative_ontology:cs_kernel_codification('e4449cdc-f918-4c28-b891-598e3db3986d', fixed_text).
narrative_ontology:cs_authority_grounding('e4449cdc-f918-4c28-b891-598e3db3986d', extraction).
narrative_ontology:cs_interpretation_layer_present('e4449cdc-f918-4c28-b891-598e3db3986d').
narrative_ontology:cs_reading_relation('e4449cdc-f918-4c28-b891-598e3db3986d', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4449cdc-f918-4c28-b891-598e3db3986d', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('e4449cdc-f918-4c28-b891-598e3db3986d', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('e4449cdc-f918-4c28-b891-598e3db3986d', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('e4449cdc-f918-4c28-b891-598e3db3986d', foundational, property_status_itself_is_injustice).
narrative_ontology:cs_axiom_status(property_status_itself_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('e4449cdc-f918-4c28-b891-598e3db3986d', property_status_itself_is_injustice, deontological).
narrative_ontology:cs_axiom('e4449cdc-f918-4c28-b891-598e3db3986d', secondary, all_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(all_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('e4449cdc-f918-4c28-b891-598e3db3986d', all_use_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('e4449cdc-f918-4c28-b891-598e3db3986d', animal_moral_personhood_framework).
narrative_ontology:cs_drift_state('e4449cdc-f918-4c28-b891-598e3db3986d', contemporary_institutional_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e4449cdc-f918-4c28-b891-598e3db3986d', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, property_holders_extracting_animal_labor_and_products).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_held_as_property).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animal_advocates_excluded_from_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, scientific_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, consumer_masses_benefiting_indirectly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, animals are rights-bearing moral persons whose basic right not to be property is systematically violated. They are forced into instrumental use (food, labor, experimentation, entertainment, clothing) against their nature and interests. Their exit from property status is structurally impossible without external intervention; the constraint itself determines their complete exclusion from moral and legal personhood despite possessing sentience, agency, and intrinsic worth.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_held_as_property, payer,
    powerless, biographical, trapped, global).

% Benefit structurally from the property status of animals: industrial agriculture, pharmaceutical testing, entertainment, clothing manufacture, and research institutions collect the entire surplus of animal labor, biological products, and use-value. They set and enforce the rules that classify animals as property, regulate (not eliminate) use, and exclude alternative framings from public and legal discourse. Their institutional capacity to define animals as property and defend that definition against challenge is central to the extraction.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, property_holders_extracting_animal_labor_and_products, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, property_holders_extracting_animal_labor_and_products, beneficiary).

% Advocates and philosophers articulating the abolitionist reading are systematically excluded from institutions that set policy (legislatures, corporate boards, research ethics committees) and their moral claims are dismissed as irrational emotionalism. They bear the cost of exclusion (professional precarity, marginalization) while lacking the power to change the institutional framing. Their voices would substantively alter the constraint's enforcement if admitted; their systematic exclusion is itself part of the enforcement machinery.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_advocates_excluded_from_institutions, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_advocates_excluded_from_institutions, excluded).

% Institutional seat holding a different reading of the kernel (property status retained, but use constrained by welfare obligations). From the abolitionist reading's perspective, they are locked in strategic tension: their incremental reforms may reduce immediate suffering but according to abolitionism, legitimize and entrench the property status that is itself the root injustice. They have institutional access that abolitionists lack, but that access channels them into negotiating the terms of extraction rather than contesting its foundation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reform_advocates, observer,
    organized, biographical, constrained, global).

% Depends structurally on the property status of animals to justify research use without informed consent. Collects the surplus research output and knowledge production. Maintains institutional gatekeeping over what counts as valid evidence and ethics, systematically excluding abolitionist philosophical frameworks from legitimacy. Benefits from the suppression that excludes animal advocates' voices.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, scientific_research_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from low prices on animal products, pharmaceutical development, and entertainment that depend on animal property status. Their exit is theoretically available (ethical consumption alternatives exist) but practically constrained by price, cultural embedding, and institutional suppression of the abolitionist framing. They are structural beneficiaries who could alter the constraint if they converted to abolitionist principles, but the constraint's enforcement includes suppression of the moral salience of that choice.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, consumer_masses_benefiting_indirectly, beneficiary,
    moderate, immediate, mobile, global).

% The concept of personhood and rights has historically excluded animals; the abolitionist reading vindicates a revision to that concept (extending moral personhood to sentient beings with agency). This is not an actor but a normative framework that the constraint either reinforces (by denying animal personhood) or contradicts (by respecting animal moral status). Included for narrative completeness; not a beneficiary collecting from the constraint.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, philosophical_tradition_of_personhood, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__abolitionist_reading, philosophical_tradition_of_personhood).

% Property law, animal welfare statutes, and research ethics review systems all instantiate and enforce the property-status framing. Courts, legislatures, and regulatory bodies actively defend the animal-as-property classification and exclude legal standing for animals themselves or abolitionist advocates attempting to change it. The institutions do not merely accommodate the constraint; they constitute its enforcement machinery.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_and_institutional_frameworks, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, property_holders_extracting_animal_labor_and_products).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the abolitionist reading, there is NO genuine coordination function — what appears as coordination (efficient resource use, scientific progress, cultural tradition) is reframed as the institutional machinery of extraction. The apparent coordination is cover for the systematic extraction of animal labor and biological products. No coordination problem is solved by classifying animals as property; rather, the classification ENABLES the extraction by removing animals from the moral community.
% TRANSFER_FUNCTION: Moves the entire surplus of animal labor, biological products (meat, milk, eggs, fur, skin), reproductive capacity, and embodied research value FROM animals (the powerless payers) TO property holders, researchers, and consumers (the beneficiaries). The transfer is not negotiated; it is compulsory, enforced through legal property classification and suppression of alternative moral framings. Under this reading, it is a pure extraction mechanism.
% ABSENT_VOICES: Animals themselves cannot speak in human institutional forums and have no legal standing to contest their property status. Animal advocates advocating the abolitionist reading are systematically excluded from legislatures, corporate decision-making, and most research ethics committees; their frameworks are labeled irrational, extremist, or emotionally driven. Sentient beings' interests in not being property are structurally absent from every institution that could change the constraint.
% DISAPPEARANCE_RATIONALE: If the property status of animals disappeared overnight and abolitionist principles took hold — animals recognized as moral persons with the right not to be used — the entire industrial complex of animal agriculture, pharmaceutical testing, fashion, and entertainment would reorganize radically. Billions of animals would be freed from captivity. Food systems, medical research protocols, consumer culture would have to restructure around consent-based frameworks (impossible with animals as patients/subjects). Economic value chains built on animal extraction would collapse or transform. The constraint's disappearance would rearrange the world at scale.
% FOUNDING_PROBLEM: The founding problem is the epistemic and moral exclusion of animals from personhood: animals were treated as mere resources or automata, deemed without moral standing or interests worthy of consideration. Early modern philosophy (Descartes, Locke) and legal systems embedded the assumption that animals are property. The constraint was built to provide institutional stability to that exclusion by making property status the foundation of human use.
% FOUNDING_PROBLEM_CORROBORATION: The abolitionist reading asserts that the founding problem persists: animals continue to be excluded from moral and legal personhood, and this exclusion is the ROOT INJUSTICE (not welfare conditions, but status itself). Animal philosophers and ethicists (Tom Regan, Gary Francione, others outside the beneficiary set) attest this founding problem remains systematically unresolved. Property holders and institutional actors attest that the founding problem is SOLVED (animals are no longer confused with humans; their use is justified and regulated). The contest is live: no corroboration outside the benefiting parties exists; the abolitionist reading must carry its own philosophical warrant, not institutional corroboration.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) under the abolitionist reading because ANY use of a moral person as property violates their fundamental right — the constraint permits zero permissible use. There is no welfare threshold that legitimates extraction; the extraction IS the violation. Suppression is high (0.88) because the constraint depends on active institutional enforcement: legal property classification, professional gatekeeping that excludes abolitionist framings, and cultural-economic embedding that makes the abolitionist alternative seem inaccessible. Theater has been rising over the interval (0.28 → 0.42) as welfare reforms (cage-free eggs, higher humane standards) increase the performative activity around animal use while the fundamental extraction — property status — remains unchanged. From the abolitionist perspective, welfare reforms are THEATER masking the continued violation. The measurement series reflects the constraint's increasing theatrical character while extractiveness and suppression remain very high and stable — the core injustice persists; the machinery just performs more.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the animal's perspective (if expressible): total violation, no alternatives, total extraction. From the property holder's perspective: justified use grounded in property rights, welfare-constrained, economically efficient. From the welfare advocate's perspective: animals matter morally; use should be minimized but property status is pragmatic. From the consumer's perspective: animals are invisible in the extraction machinery; benefit is transparent (cheap food) while cost is suppressed (moral salience of animal suffering is institutional suppressed). From the abolitionist advocate's perspective: property status is the root injustice; all use is violation; the constraint extracts from two victim groups simultaneously (animals directly, advocates through exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are fully targeted (d → 1.0): they are trapped, powerless, bearing the complete cost of property status and use. Property holders are full beneficiaries (d → 0.0): they collect the surplus, set the rules, and depend on the constraint. Animal advocates are intermediate: they bear suppression costs (precarity, exclusion) but hold moderate institutional power and theoretical access; their directionality is mid-range but skewed toward the target end because they are systematically excluded from decision-making. The welfare reading advocates sit in a paradoxical position: they have institutional access (d lower) but according to the abolitionist reading, that access channels them into legitimizing the core injustice, making their structural relationship ambiguous. The abolitionist reading assigns them observer status rather than beneficiary, because they are advocating constraints that would reduce (not eliminate) animal extraction; their institutional power does not make them beneficiaries of the property-status constraint itself, though their theoretical influence might delay abolition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under the abolitionist reading is the EXCLUSION OF ANIMALS FROM MORAL PERSONHOOD and the INSTITUTIONAL ENFORCEMENT of that exclusion. The constraint persists because it is actively maintained by legal property frameworks, institutional gatekeeping, and cultural embedding — not because the problem it was built to solve remains unsolved. The founding problem (animals treated as mere resources) is structured as PERMANENT under this reading: property status is the founding problem, not the solution to a founding problem. This prevents mandatrophy resolution under the standard framework: mandatrophy would require the founding problem to become obsolete (e.g., animals cease to exist, or moral personhood concepts change unambiguously). Neither is true. The constraint's persistence is INTENTIONAL and STRUCTURAL, not inertial. Theater is present (welfare reforms) but does not indicate mandatrophy — it indicates the emergence of a secondary performance layer defending the primary extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_personhood_boundary_empirics,
    'What empirical and philosophical criteria definitively establish which beings are moral persons with rights against being property?',
    'Philosophical argument resolving sentience/agency/self-awareness thresholds; empirical data on animal cognition and suffering; consensus (or persistent disagreement) among philosophers outside property-holder interests.',
    'If animals'' cognitive and experiential capacities clearly meet personhood criteria, abolitionist extraction measures become ethically mandatory; if criteria remain contestable, the constraint''s ethical foundations remain disputed. The classification outcome determines whether the constraint persists as justified or as systematically unjust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_personhood_boundary_empirics, conceptual, 'Whether animals satisfy the criteria for moral personhood and rights-bearing status.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of abolitionist voices structural (institutional exclusion, economic barriers, gatekeeping) or internalized (consumers have internalized the property-status framing as natural and ineliminable)?',
    'Post-exposure to abolitionist arguments and frameworks: do consumers'' moral commitments persist toward property status, or do alternative frames become actionable? Do advocates who gain institutional access shift their framing, or maintain abolitionist principles?',
    'If suppression is primarily structural, removing institutional barriers could shift the constraint rapidly; if primarily internalized, the constraint persists even after institutional exclusion is lifted. The distinction affects both classification certainty and remedial pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative readings is structural or internalized in the moral community.').

omega_variable(
    welfare_reform_acceleration_vs_entrenchment,
    'Do incremental welfare reforms (cage-free standards, pain-reduction regulations) functionally accelerate the movement toward abolition by raising moral salience and lowering practical exit costs, or do they entrench property status by legitimizing and performing concern for animal interests while leaving core extraction intact?',
    'Historical trajectory analysis: does abolition-related legislation increase after welfare reforms, or decrease? Do welfare advocates shift toward abolitionist positions, or entrench welfare constraints? Long-term data on animal agriculture scale post-welfare-reform vs. pre-reform jurisdictions.',
    'If reforms accelerate abolition, the welfare reading is a strategic stepping-stone; if they entrench, the welfare reading is a constraint-stability mechanism that the abolitionist reading must overcome. This affects strategic posture and the reading''s institutional viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_acceleration_vs_entrenchment, empirical, 'Whether welfare reforms function as stepping-stones to abolition or as entrenchment mechanisms.').

omega_variable(
    alternative_readings_foreclosure_logic,
    'Does the abolitionist reading''s core axiom (animals are moral persons with the right not to be property) FORECLOSE the property reading''s core axiom (animals are property; economic value is the only relevant value), or do both readings remain logically coexistent within different moral frameworks held by different parties?',
    'Philosophical analysis of whether the axioms are contradictory (foreclosure) or merely incompatible (coexistence). Can a single moral agent coherently hold both simultaneously? If not, does that mean one framework must be false, or that frameworks are incommensurable?',
    'Foreclosure would mean one reading is structurally eliminated as the other becomes epistémically justified; coexistence means institutional contest persists indefinitely unless political force settles it. The distinction affects whether abolitionism is a claim about discovered truth or about power-mediated institutional change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure_logic, conceptual, 'Whether the abolitionist and property readings are logically foreclosed from each other or remain genuinely coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__abolitionist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__abolitionist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__abolitionist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__abolitionist_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.91).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__abolitionist_reading, base_extractiveness, 8, 0.92).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__abolitionist_reading, base_extractiveness, 16, 0.93).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__abolitionist_reading, base_extractiveness, 24, 0.94).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__abolitionist_reading, base_extractiveness, 32, 0.945).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__abolitionist_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__abolitionist_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__abolitionist_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__abolitionist_reading, suppression_requirement, 32, 0.87).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__abolitionist_reading, 0.05).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal-status kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading and computing to a different ε and classification. This story (abolitionist_reading) grounds animals' moral personhood and assigns zero permissibility to use. The property_reading treats animals as property with ε near 0 (natural allocation, no extraction). The welfare_reading treats animals as sentient beings deserving protection but retains property status, computing to medium ε (extraction mitigated by welfare regulation). All three are linked via network.affects_constraints to reflect the kernel's shared referent (animal status) and the empirical dispute over which reading is structurally true. Each story carries its own omega variables addressing the philosophical and empirical foundations of its reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, powerless, 1.0).
constraint_indexing:directionality_override(animal_status_kernel__abolitionist_reading, institutional, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
