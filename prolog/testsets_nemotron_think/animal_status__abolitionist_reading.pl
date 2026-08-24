% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Instrumental Use Regime (Abolitionist Assessment)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The abolitionist reading of the animal_status kernel assesses the
 *   standing arrangement — the global regime of animal instrumental use
 *   (food, research, entertainment, fiber) — as a snare. From this reading,
 *   animals are rights-holders with inherent value; the regime that treats
 *   them as property and resources is pure extraction masked by a welfare
 *   coordination story. The regime's persistence depends on active
 *   enforcement (property law, ag-gag laws, research licensing, cultural
 *   normalization) and the structural exclusion of abolitionist positions
 *   from legitimate discourse. Welfare reforms are not coordination
 *   improvements but legitimation theater that stabilizes the kernel. The
 *   constraint_id names this reading's assessment; the referent for ε is the
 *   standing regime, not the abolitionist alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.88).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.92).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental Use Regime (Abolitionist Assessment)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'ecafa43e-0d1a-4327-9ebe-4377b58cc1db').
narrative_ontology:cs_kernel_codification('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', distributed).
narrative_ontology:cs_authority_grounding('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', extraction).
narrative_ontology:cs_interpretation_layer_present('ecafa43e-0d1a-4327-9ebe-4377b58cc1db').
narrative_ontology:cs_reading_relation('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', foundational, animals_have_inherent_value_precluding_instrumental_use).
narrative_ontology:cs_axiom_status(animals_have_inherent_value_precluding_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', animals_have_inherent_value_precluding_instrumental_use, deontological).
narrative_ontology:cs_axiom('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', secondary, sentience_entails_right_not_to_be_used_as_resource).
narrative_ontology:cs_axiom_status(sentience_entails_right_not_to_be_used_as_resource, holdable).
narrative_ontology:cs_axiom_grounding('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', sentience_entails_right_not_to_be_used_as_resource, deontological).
narrative_ontology:cs_reference_frame('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', inherent_value_rights_framework).
narrative_ontology:cs_drift_state('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', contemporary_animal_use_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ecafa43e-0d1a-4327-9ebe-4377b58cc1db', '2026-08-15T10:00:00Z').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumer_markets_for_animal_products).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, exotic_pet_and_entertainment_sectors).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, companion_animals_in_breeding_mills).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, wild_animals_in_captivity).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, working_animals).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, speciesism_as_natural_order).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, property_rights_supersede_sentience).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, instrumental_rationality_over_inherent_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, and killed in industrial systems for human food. No legal standing to refuse; physical escape nearly impossible; biological existence structured entirely by the regime. Their suffering and lives are the extracted resource.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Used in research, testing, and education. Confined in controlled environments; subjected to procedures without consent; killed at study endpoint. Welfare regulations govern handling but do not challenge the permissibility of their use.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Held in zoos, aquariums, circuses, and private collections. Natural behaviors suppressed; breeding controlled; lifetime confinement justified as conservation or education. No pathway to return to wild or autonomous existence.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, wild_animals_in_captivity, payer,
    powerless, immediate, trapped, global).

% Integrated global system of breeding, feed, pharmaceuticals, processing, and retail. Captures the economic value of animal bodies at scale. Lobbies for regulatory frameworks that secure property status and externalize welfare costs. Can shift species, geographies, and production models to maintain extraction.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Universities, pharma companies, funding agencies, and regulatory bodies that depend on animal models. Controls the validation pipeline for alternatives; resists mandatory replacement. Extracts scientific legitimacy and career structures from the regime while publicly endorsing '3Rs' (replacement, reduction, refinement) that preserve the kernel.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, constrained, global).

% Mass markets for meat, dairy, eggs, leather, and other animal-derived goods. Beneficiaries of cheap, abundant animal protein and materials. Exit requires dietary/systemic change; constrained by price, culture, infrastructure, and industry marketing. Not direct extractors but the demand side that makes extraction profitable.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumer_markets_for_animal_products, beneficiary,
    organized, biographical, constrained, global).

% Administers the welfare statutes, anti-cruelty laws, and property frameworks that constitute the regime. Sets the terms of permissible use; licenses facilities; inspects for compliance with standards that assume instrumental use is legitimate. Could revise the kernel but is structurally captured by industry and cultural inertia.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, regulatory_state, agenda_setter,
    institutional, generational, analytical, national).

% Advocates and organizations arguing for full legal personhood and rights for animals. Structurally excluded from the regime's decision-making because their position (abolition) is treated as outside the Overton window of 'legitimate stakeholder' debate. Their exclusion is functional: the regime's legitimacy depends on the welfare/property spectrum being the only live options.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, excluded,
    moderate, biographical, identity_locked, global).

% Organizations working within the regime to improve welfare standards (e.g., cage-free campaigns, enrichment requirements). They occupy the 'reform' seat that the regime tolerates and often co-opts. Their successes are real for individual animals but legitimize the kernel by treating use as a given. From the abolitionist reading, they are observers of a constraint they help maintain.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reformers, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime coordinates the mass production, allocation, and consumption of animal bodies across global supply chains — solving the logistical problem of turning sentient beings into standardized commodities for food, research, fiber, and entertainment. From the abolitionist reading, this is not genuine coordination among parties but a unilateral imposition on beings who cannot consent.
% TRANSFER_FUNCTION: Moves animal lives, reproductive capacity, labor, bodily substances, and suffering from animals to human industries and consumers. The transfer is total: animals give everything (life, liberty, bodily integrity); industries receive market value; consumers receive products. No reciprocal flow to animals.
% ABSENT_VOICES: The animals themselves — the primary subjects of the constraint — have no voice in the regime that defines their status. Abolitionist advocates are structurally excluded from policy tables where 'stakeholders' are defined as industry, science, and consumer representatives. Wild animal communities displaced by animal agriculture are absent from the calculus.
% DISAPPEARANCE_RATIONALE: If the property/welfare regime vanished overnight, global food systems, biomedical research pipelines, fashion supply chains, and entertainment industries would face immediate, fundamental reorganization. Land use would shift (77% of agricultural land is for livestock); research would accelerate non-animal methods; new protein economies would scale. The world would not stay the same.
% FOUNDING_PROBLEM: Securing reliable, scalable supplies of animal protein, labor, research models, and raw materials for growing human populations and economies — while managing public sentiment about cruelty through minimal welfare concessions that do not threaten the property kernel.
% FOUNDING_PROBLEM_CORROBORATION: Historical records (agricultural policy archives, veterinary science founding texts, early humane society minutes) show the regime was built for human utility and economic development, not animal welfare. Industry and state actors attest the problem (feeding/clothing/researching for humans) is live. Abolitionists and independent ethicists attest the founding problem is illegitimate — it presupposes animals as resources. No corroboration exists outside the beneficiary set for the claim that animals' inherent value was ever a design consideration.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is near-maximal because the regime takes the entirety of animals' lives, liberty, and bodily integrity for human benefit with zero reciprocity. Suppression (0.92) is maximal because animals are physically confined, reproductively controlled, and legally silenced — no exit, no voice, no due process. Theater ratio (0.42) reflects the growing welfare regulation layer that improves conditions for some animals but legitimizes the kernel by making 'humane use' the only thinkable alternative. Accessibility collapse (0.87) is high because once animals are seen as rights-holders, the property/welfare spectrum collapses — there is no middle ground between 'property' and 'person'. Resistance (0.45) is moderate: human advocacy exists but the primary victims cannot resist effectively, and the regime co-opts reform energy.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from this structural data. From the animal seat: snare (extraction near 1, suppression near 1, no exit). From the industry seat: rope or scaffold (they see coordination, benefit, and exit options). From the regulatory seat: tangled_rope (coordination function + extraction from animals). From the abolitionist seat: snare (the regime is pure extraction). This seat divergence IS the measurement — the abolitionist reading exists because the regime computes differently for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals (farmed, lab, captive, working) are full victims/payers: they bear 100% of the extraction with zero benefit, powerless, trapped, global scope. Animal industries and research establishments are primary beneficiaries/agenda_setters: they capture the value, write the rules, hold institutional power, have arbitrage exit. Consumers are secondary beneficiaries: they receive cheap products but are constrained by system structure. Regulatory state is agenda_setter: administers the kernel, could change it but is captured. Abolitionist advocates are excluded: their position is the regime's structural outside. Welfare reformers are observers: they operate inside the kernel and stabilize it.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reform layer shows mandatrophy: the original mandate (prevent egregious cruelty) has atrophied into a system that legitimizes the kernel. Welfare reforms do not reduce extraction; they make it socially acceptable. The founding problem (efficient animal production for human needs) is contested — abolitionists say it was never legitimate; industry says it is live. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags this as a zombie constraint: the problem it was built for is either gone (if animals have rights) or illegitimate, but the arrangement persists and intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame_ambiguity,
    'Is the animal_status kernel a genuine site of contested readings, or does the abolitionist reading reveal the kernel itself as a constructed legitimation for extraction?',
    'Genealogical analysis of when and how ''animal welfare'' became the authorized interpretive frame for the property kernel — tracing whether the kernel was ever open to abolitionist codification or was designed to foreclose it.',
    'If the kernel was constructed to foreclose abolition, then welfare_reading and property_reading are not sibling readings but enforcement layers of a snare. The abolitionist reading would not be ''one reading among others'' but the only reading that names the constraint''s true structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_frame_ambiguity, conceptual, 'Whether the kernel contest is genuine or a managed dispute that preserves extraction.').

omega_variable(
    suppression_mechanism_for_nonhuman_victims,
    'How does suppression operate when the primary victims (animals) lack linguistic resistance but face total structural enclosure?',
    'Comparative analysis: measure suppression in human total institutions (prisons, factories) vs. animal systems. If animal suppression exceeds human total institutions on physical confinement, reproductive control, and absence of due process, the regime''s suppression is structural-enclosure-maximal, not merely ''high''.',
    'If suppression is structural-enclosure-maximal, the snare classification is reinforced and the theater_ratio (welfare regulations) is exposed as a pure legitimation layer with near-zero functional mitigation for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_for_nonhuman_victims, empirical, 'Whether animal suppression represents a distinct maximal form not captured by human-centric metrics.').

omega_variable(
    naturalness_of_property_status,
    'Is the property status of animals a natural-law-like inevitability (biological hierarchy, human nature) or a contingent historical construction that benefits identifiable agents?',
    'Cross-cultural and historical survey: societies with non-property animal statuses (e.g., certain Indigenous legal orders, Jain-influenced polities, pre-colonial Hawaiian kapu systems). If functional alternatives existed/exist, property status is constructed.',
    'If constructed, the false_summit_mountain signature could apply to any ''natural hierarchy'' framing; the regime''s claimed_type (property/welfare as natural order) would be a false summit masking a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_property_status, empirical, 'Whether the regime''s self-presentation as natural/inevitable withstands comparative evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_abolitionist_tr_t1800, animal_status__abolitionist_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1850, animal_status__abolitionist_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1900, animal_status__abolitionist_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(animal_status_abolitionist_tr_t1950, animal_status__abolitionist_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2000, animal_status__abolitionist_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(animal_status_abolitionist_tr_t2025, animal_status__abolitionist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(animal_status_abolitionist_be_t1800, animal_status__abolitionist_reading, base_extractiveness, 1800, 0.75).
narrative_ontology:measurement(animal_status_abolitionist_be_t1850, animal_status__abolitionist_reading, base_extractiveness, 1850, 0.78).
narrative_ontology:measurement(animal_status_abolitionist_be_t1900, animal_status__abolitionist_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(animal_status_abolitionist_be_t1950, animal_status__abolitionist_reading, base_extractiveness, 1950, 0.86).
narrative_ontology:measurement(animal_status_abolitionist_be_t2000, animal_status__abolitionist_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(animal_status_abolitionist_be_t2025, animal_status__abolitionist_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(animal_status_abolitionist_su_t1800, animal_status__abolitionist_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(animal_status_abolitionist_su_t1850, animal_status__abolitionist_reading, suppression_requirement, 1850, 0.87).
narrative_ontology:measurement(animal_status_abolitionist_su_t1900, animal_status__abolitionist_reading, suppression_requirement, 1900, 0.89).
narrative_ontology:measurement(animal_status_abolitionist_su_t1950, animal_status__abolitionist_reading, suppression_requirement, 1950, 0.91).
narrative_ontology:measurement(animal_status_abolitionist_su_t2000, animal_status__abolitionist_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(animal_status_abolitionist_su_t2025, animal_status__abolitionist_reading, suppression_requirement, 2025, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.18).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_welfare_regulation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_research_governance).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, agricultural_subsidy_regime).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, wildlife_management_framework).

% DUAL FORMULATION NOTE:
% This story (animal_status__abolitionist_reading) is one of three readings of the animal_status kernel. The welfare_reading and property_reading instantiate distinct constraints with different ε, beneficiary/victim structures, and claimed types. All three are linked via affects_constraints. The abolitionist reading's ε (0.88) assesses the standing regime; the welfare reading would author lower ε (seeing welfare rules as genuine coordination); the property reading would author near-zero ε (seeing property as natural order). Per ε-invariance, these are separate constraint stories, not one story with variable measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__abolitionist_reading, institutional, 0.1).
constraint_indexing:directionality_override(animal_status__abolitionist_reading, organized, 0.35).
constraint_indexing:directionality_override(animal_status__abolitionist_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
