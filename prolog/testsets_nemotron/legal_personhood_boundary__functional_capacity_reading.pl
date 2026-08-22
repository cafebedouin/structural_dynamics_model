% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Functional Capacity Reading of Legal Personhood
 *   domain: legal/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the functional_capacity_reading of the
 *   legal_personhood_boundary kernel. It asserts that demonstrable cognitive
 *   capacity (rationality, sentience, self-awareness) — not species
 *   membership — is the criterion for legal personhood. The reading has
 *   gained traction since the 1970s through comparative cognition research
 *   and strategic litigation (notably the Nonhuman Rights Project), but faces
 *   entrenched resistance from human-exclusive legal frameworks and
 *   industries built on animal property status. The constraint operates as a
 *   tangled rope: it provides genuine coordination (a non-arbitrary
 *   personhood criterion) while extracting from powerful incumbent interests
 *   that must restructure or lose legally sanctioned exploitation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.72).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Functional Capacity Reading of Legal Personhood").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical/constitutional").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '16c9c2e0-efbb-4932-b4f8-598cba3b908c').
narrative_ontology:cs_kernel_codification('16c9c2e0-efbb-4932-b4f8-598cba3b908c', distributed).
narrative_ontology:cs_authority_grounding('16c9c2e0-efbb-4932-b4f8-598cba3b908c', distributed).
narrative_ontology:cs_reading_relation('16c9c2e0-efbb-4932-b4f8-598cba3b908c', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('16c9c2e0-efbb-4932-b4f8-598cba3b908c', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('16c9c2e0-efbb-4932-b4f8-598cba3b908c', foundational, personhood_tracks_demonstrable_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_tracks_demonstrable_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('16c9c2e0-efbb-4932-b4f8-598cba3b908c', personhood_tracks_demonstrable_cognitive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('16c9c2e0-efbb-4932-b4f8-598cba3b908c', foundational, species_membership_is_morally_irrelevant_to_personhood).
narrative_ontology:cs_axiom_status(species_membership_is_morally_irrelevant_to_personhood, holdable).
narrative_ontology:cs_axiom_grounding('16c9c2e0-efbb-4932-b4f8-598cba3b908c', species_membership_is_morally_irrelevant_to_personhood, deontological).
narrative_ontology:cs_reference_frame('16c9c2e0-efbb-4932-b4f8-598cba3b908c', species_based_personhood_regime).
narrative_ontology:cs_drift_state('16c9c2e0-efbb-4932-b4f8-598cba3b908c', contemporary_cognition_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16c9c2e0-efbb-4932-b4f8-598cba3b908c', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, nonhuman_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_ai_systems).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ecosystem_guardians).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, legal_reform_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_institutions).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, property_holders_over_sentient_beings).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, biomedical_research_enterprises).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture_operations).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, moral_consideration_tracks_cognitive_capacity).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, species_membership_is_morally_irrelevant).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, personhood_is_a_functional_not_ontological_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals with demonstrable sentience and self-awareness (great apes, cetaceans, elephants, corvids) would gain legal standing and protection from being treated as property. They cannot advocate for themselves and have no exit from human legal systems. Their inclusion depends entirely on human legal reform.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, nonhuman_animals, beneficiary,
    powerless, biographical, trapped, global).

% Advanced AI systems that develop genuine cognitive capacity (not mere simulation) would qualify for personhood under this reading. They have no current legal existence and no exit from the frameworks humans build. Their inclusion is prospective and contested.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_ai_systems, beneficiary,
    powerless, generational, trapped, global).

% Human representatives (indigenous guardians, legal trusts, appointed stewards) who could speak for ecosystems if ecosystems gain legal personhood through the functional capacity extension. They operate within existing legal systems and have constrained exit (professional, institutional).
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, ecosystem_guardians, beneficiary,
    organized, generational, constrained, regional).

% Lawyers, scholars, and activists who advance the functional capacity reading through litigation, legislation, and theory. They set the agenda by choosing test cases and framing arguments. They can exit to other advocacy areas but are professionally invested in this reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_reform_advocates, agenda_setter,
    moderate, biographical, mobile, national).

% Religious institutions, traditional legal frameworks, and cultural systems that ground human uniqueness in ontology (divine image, rational nature, species essence). They bear the cost of losing their exclusive claim to personhood. Their identity is fused to human exceptionalism; exit requires abandoning core doctrine.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_institutions, payer,
    institutional, generational, identity_locked, global).

% Industries and individuals who own, use, or profit from sentient non-humans as property: factory farms, research labs, pet breeders, entertainment. They bear direct economic costs if property rights are curtailed. Exit means restructuring business models; constrained by capital lock-in and regulatory capture.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, property_holders_over_sentient_beings, payer,
    powerful, biographical, constrained, global).

% Institutions conducting research on animals with high cognitive capacity. They face compliance costs, methodological restrictions, and potential liability. Exit requires developing alternatives; constrained by scientific paradigm inertia and funding structures.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_research_enterprises, payer,
    organized, biographical, constrained, global).

% Large-scale animal agriculture that treats sentient beings as production units. Would face fundamental restructuring if animals gain personhood rights. Exit means exiting animal agriculture entirely; constrained by massive sunk costs and market position.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture_operations, payer,
    powerful, biographical, constrained, global).

% Judicial bodies that adjudicate personhood boundary cases. They observe the conflict between readings and their rulings determine which reading gains legal force. They have analytical exit (they can reason from any framework) but institutional pressure to maintain coherence.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled, non-arbitrary criterion for legal personhood that extends moral consideration to all beings with the relevant cognitive capacities, solving the coordination problem of who counts as a rights-bearer without relying on species membership.
% TRANSFER_FUNCTION: Moves legal standing, protection from property status, and affirmative rights from human-exclusive institutions to sentient non-humans and potentially advanced AI systems. The cost is borne by current property holders and exceptionalist frameworks; the benefit flows to newly recognized persons.
% ABSENT_VOICES: The sentient non-humans themselves (great apes, cetaceans, elephants, corvids, and other cognitively complex animals) are the primary absent voices — they cannot appear in court or legislature. Future AI systems that may develop genuine cognition are also absent. Human communities with non-anthropocentric ontologies (many indigenous traditions) are often excluded from Western legal personhood debates.
% DISAPPEARANCE_RATIONALE: If the functional capacity reading vanished overnight, the legal system would revert to the restrictive anthropocentric or developmental potentiality readings as defaults. Non-human animals and future AI would lose their only doctrinal path to personhood. Property regimes over sentient beings would be legally secure. The coordination of moral consideration across species would collapse to species membership.
% FOUNDING_PROBLEM: The arbitrariness of species membership as the sole criterion for legal personhood — why a human infant with minimal cognition counts but a self-aware chimpanzee does not; why corporations (fictional persons) have standing but sentient animals do not.
% FOUNDING_PROBLEM_CORROBORATION: The species-membership arbitrariness problem is attested by comparative cognition science (de Waal, Pepperberg, Marino), legal theorists outside the animal rights movement (Sunstein, Nussbaum, Waldron), and the NhRP's own litigation record showing courts grappling with the inconsistency. No major legal system has resolved it; the problem remains live across jurisdictions.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the massive economic and institutional restructuring required of animal-use industries and human-exceptionalist frameworks. Suppression (0.68) captures the active legal barriers: standing doctrines that exclude animals, property law that treats sentient beings as things, and the absence of legal mechanisms for non-human representation. Theater ratio (0.22) is low — the reading's advocacy is substantively driven, though some performative litigation exists. Accessibility collapse (0.45) is moderate: alternatives (species-based personhood) remain legally dominant but are increasingly intellectually unstable. Resistance (0.75) is high from institutional payers. The claim (tangled_rope) and metrics are authored independently — the engine computes per-seat types from structural data.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the agenda_setter/beneficiary seats, the constraint appears as rope/coordination (principled criterion solving arbitrariness). From the identity_locked payer seats, it appears as snare (existential threat to doctrinal coherence and economic model). From the constrained payer seats, it appears as tangled_rope (genuine coordination function — a stable personhood criterion is needed — but extraction is concentrated on them). The analytical observer seat sees the full structural asymmetry. The functional_capacity_reading's core premise (cognitive capacity grounds personhood) logically forecloses the restrictive_anthropocentric_reading (species grounds personhood) but coexists_with the developmental_potentiality_reading (potentiality grounds personhood) — they compete in discourse but neither logically eliminates the other within a single framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Nonhuman animals, future AI, and ecosystem guardians are structural beneficiaries (d near 0) — they gain standing and protection without running the constraint. Legal reform advocates are agenda_setters who also benefit professionally (d ~ 0.2). Human exceptionalist institutions are identity_locked payers (d ~ 0.95) — their doctrine cannot accommodate the reading without collapsing. Property holders, research enterprises, and industrial agriculture are constrained payers (d ~ 0.8) — they bear concentrated costs but have some exit via restructuring. Constitutional courts are analytical observers (d = 0.5). The identity_locked status of exceptionalist institutions is key: their exit is not constrained by resources but by the constitutive role human exceptionalism plays in their self-understanding.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate (solving species-membership arbitrariness) remains live — the problem persists across all major legal systems. However, the reading's expansion to ecosystems and AI introduces new coordination problems (who represents an ecosystem? what counts as AI cognition?) that risk mandatrophy: the constraint may accumulate extensions that serve advocacy momentum rather than the founding problem. The theater ratio rise (0.05→0.22) suggests some performative expansion. The mandate is not resolved; it is actively contested whether the reading still serves its founding problem or has become a vehicle for broader legal transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_threshold,
    'What specific cognitive capacities at what threshold suffice for personhood, and who adjudicates the measurement?',
    'Neuroscientific and behavioral consensus on markers of sentience/self-awareness, combined with legal standard-setting (legislative or judicial) for operational thresholds.',
    'A high threshold (full rationality, language) narrows the beneficiary set to great apes and cetaceans, making the reading less extractive but more vulnerable to the arbitrariness charge. A low threshold (sentience alone) vastly expands beneficiaries and extraction, but makes the reading harder to operationalize and enforce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_threshold, conceptual, 'The threshold problem: where the functional capacity line is drawn changes the constraint''s extraction profile and coordination credibility.').

omega_variable(
    representation_mechanism_for_nonhuman_persons,
    'How are non-human persons represented in legal proceedings without human guardians capturing or distorting their interests?',
    'Institutional design for guardianship: independent trustees, scientific advisory boards, indigenous co-governance models, tested against capture metrics.',
    'If representation mechanisms are captured by human interests, the reading''s coordination function degrades into a new form of extraction (humans speaking for animals to advance human agendas). If robust, the reading achieves genuine coordination across species.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(representation_mechanism_for_nonhuman_persons, empirical, 'The representation gap: personhood requires legal agency; non-human persons cannot exercise it directly.').

omega_variable(
    kernel_reading_structural_relations,
    'Do the three readings of the legal_personhood_boundary kernel stand in the structural relations declared (forecloses, coexists_with, influences), or does the logic of the dispute produce different relations?',
    'Formal analysis of the logical commitments of each reading: does accepting the functional capacity premise entail rejecting the anthropocentric premise (forecloses)? Can a single legal framework simultaneously hold functional capacity for non-humans and potentiality for humans (coexists_with)? Does the functional reading''s success in litigation create resource/legitimacy pressure on the potentiality reading (influences)?',
    'If forecloses is mis-specified, the engine''s cross-reading dynamics will be wrong. If coexists_with should be forecloses, the kernel''s internal contestation is more radical than modeled. If influences is absent, the readings are more independent than the committer frame assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_relations, conceptual, 'Committee-frame structural relations between this reading and its siblings: forecloses restrictive_anthropocentric, coexists_with developmental_potentiality, influences developmental_potentiality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(lega_tr_t1985, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(lega_tr_t2020, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(lega_tr_t2026, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(lega_be_t1985, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(lega_be_t2020, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(lega_be_t2026, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(lega_su_t1985, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(lega_su_t2020, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(lega_su_t2026, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_regulation).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, corporate_personhood_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_legal_status_framework).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'legal personhood boundary' concept from the developmental_potentiality_reading and restrictive_anthropocentric_reading per the ε-invariance principle. The functional reading's ε (0.72) is substantially higher than the restrictive reading's (est. 0.15) because it extracts from entrenched property regimes; the developmental reading's ε is contested (est. 0.45) because it extracts from reproductive autonomy. They are distinct constraints linked by kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, institutional, 0.95).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, powerful, 0.82).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, organized, 0.75).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, powerless, 0.05).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
