% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Regulatory Framework
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The welfare reading of animal moral status holds that animals are
 *   sentient beings whose suffering should be minimized within systems of
 *   regulated use; cruelty is wrong, but use is permissible. This constraint
 *   instantiates a tangled rope: it performs a genuine coordination function
 *   (providing a governance floor for animal use that prevents the worst
 *   cruelties and maintains social license) while simultaneously extracting
 *   from animals under 'humane' use — the standards permit suffering deemed
 *   'necessary' for the use, and animals have no exit. Beneficiaries are
 *   welfare organizations (legitimacy, funding, policy access) and regulated
 *   industries (public comfort, social license). Victims are animals
 *   suffering under 'humane' protocols. The constraint requires active
 *   enforcement (cruelty statutes, welfare inspections, standard-setting
 *   bodies). This is one reading of the contested kernel
 *   'animal_moral_status'; the sibling readings are property_reading and
 *   abolitionist_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.35).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.45).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Regulatory Framework").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'd035d74d-48f8-4584-ab7c-9287f1805e99').
narrative_ontology:cs_kernel_codification('d035d74d-48f8-4584-ab7c-9287f1805e99', distributed).
narrative_ontology:cs_authority_grounding('d035d74d-48f8-4584-ab7c-9287f1805e99', practice).
narrative_ontology:cs_interpretation_layer_present('d035d74d-48f8-4584-ab7c-9287f1805e99').
narrative_ontology:cs_reading_relation('d035d74d-48f8-4584-ab7c-9287f1805e99', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d035d74d-48f8-4584-ab7c-9287f1805e99', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('d035d74d-48f8-4584-ab7c-9287f1805e99', foundational, sentience_grounds_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('d035d74d-48f8-4584-ab7c-9287f1805e99', sentience_grounds_moral_consideration, deontological).
narrative_ontology:cs_axiom('d035d74d-48f8-4584-ab7c-9287f1805e99', foundational, regulated_use_permissible_when_suffering_minimized).
narrative_ontology:cs_axiom_status(regulated_use_permissible_when_suffering_minimized, holdable).
narrative_ontology:cs_axiom_grounding('d035d74d-48f8-4584-ab7c-9287f1805e99', regulated_use_permissible_when_suffering_minimized, conventional).
narrative_ontology:cs_reference_frame('d035d74d-48f8-4584-ab7c-9287f1805e99', industrial_animal_use_legitimacy_framework).
narrative_ontology:cs_drift_state('d035d74d-48f8-4584-ab7c-9287f1805e99', contemporary_animal_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d035d74d-48f8-4584-ab7c-9287f1805e99', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_under_humane_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, sentience_grounds_moral_consideration).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, cruelty_is_wrong).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legitimacy, funding, and policy access by certifying and monitoring 'humane' standards. Their organizational survival depends on the welfare framework being the accepted middle ground between cruelty and abolition. They negotiate standards with industry and government but cannot exit the framework without losing their mediating role.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, beneficiary,
    organized, biographical, constrained, national).

% Gain public comfort and social license to operate by complying with welfare standards that are calibrated to production efficiency. They bear compliance costs but these are passed to consumers and are structurally lower than the cost of abolition. They can relocate to jurisdictions with weaker standards, giving them exit leverage that shapes the regulatory floor.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_animal_industries, payer).

% Experience confinement, mutilation without analgesia, maternal separation, and slaughter under 'humane' protocols that permit suffering deemed 'necessary' for the use. They have no exit, no voice, and no power to contest the standards that govern their lives. Their suffering is the residual extraction the constraint permits.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_under_humane_use, payer,
    powerless, immediate, trapped, universal).

% Argue that the welfare framework legitimizes the property status that makes all use possible. They are structurally excluded from standard-setting bodies because their position (no use) is treated as outside the regulatory paradigm. They operate through litigation, public campaigns, and political pressure but are not seated at the tables where welfare standards are written.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, biographical, constrained, national).

% Represent agricultural and research interests that resist any recognition of animal interests as limiting property rights. They are excluded from the welfare consensus because they reject its premise (that suffering matters morally). They operate through legislative lobbying and litigation to prevent welfare standards from tightening.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, property_rights_advocates, excluded,
    institutional, generational, mobile, national).

% Administer and enforce cruelty statutes and welfare regulations. They set inspection regimes, define 'unnecessary suffering', and approve husbandry standards. Their authority derives from the legislative mandate to balance animal protection with legal uses. They are captured by industry expertise but pressured by welfare organizations.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Philosophers, legal scholars, and ethicists who analyze the framework's structural logic without participating in its enforcement or compliance. They map the constraint's coordinate space: the welfare reading as a stabilized compromise between the property and abolitionist readings, with animals as the silent third term.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Minimizes animal suffering within systems of regulated use through cruelty prohibitions and welfare standards, providing a governance framework that allows animal use to continue with social legitimacy.
% TRANSFER_FUNCTION: Moves compliance costs and welfare investments from regulated industries to animal protection; moves legitimacy and public comfort to industries and welfare organizations; animals bear residual suffering under 'humane' use that the standards permit as 'necessary'.
% ABSENT_VOICES: Animals themselves (cannot speak); abolitionist advocates who argue all use is violation and are excluded from standard-setting; property-rights advocates who argue no moral standing and are excluded from the welfare consensus.
% DISAPPEARANCE_RATIONALE: If welfare regulations vanished, industries would reduce welfare investments to zero, cruelty would increase unchecked, public comfort would erode, and the regulatory framework governing animal use would collapse — the social license for industrial animal use depends on the welfare floor.
% FOUNDING_PROBLEM: Industrial-scale animal use in the 19th century created visible cruelty that threatened social legitimacy and public comfort; a regulatory floor was needed to maintain the social license for animal use while preserving the property status of animals.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of 19th century anti-cruelty movements (ASPCA founding, Martin's Act) corroborate the founding problem as stated. Abolitionist scholars (Francione, Regan) attest the problem was use itself, not its cruelty. Industry representatives (animal agriculture coalitions) attest current standards are sufficient and the problem is solved.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.35) because the constraint's primary operation is coordination (preventing worst cruelties, enabling regulated use) rather than extraction, but the residual suffering permitted under 'necessary' clauses constitutes real extraction from trapped subjects. Suppression is moderate (0.45) because the constraint actively excludes rival payment routing — abolitionist challenges to the property-status foundation are structurally excluded from standard-setting. Theater ratio is rising (0.40) as welfare certifications increasingly serve marketing functions while production practices converge on minimum legal standards. The measurement series run on one shared time grid (0-30) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare organization seat, the constraint is genuine coordination preventing cruelty. From the industry seat, it is a manageable compliance cost that buys social license. From the animal seat (if it could speak), it is a snare — the 'humane' label legitimizes the very suffering it claims to minimize. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations and regulated industries are structural beneficiaries (d near beneficiary end) — they collect legitimacy, funding, and social license from the framework. Animals under humane use are full targets (d = 1.0) — trapped, no exit, bearing all residual suffering. Regulatory agencies sit near symmetric (d ≈ 0.5) — they administer the constraint but are pressured by both industry and welfare groups. Abolitionist and property-rights advocates are excluded — their exclusion is the enforcement object that maintains the welfare consensus. The engine computes per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (industrial cruelty threatening social license) is contested: welfare organizations say it remains live; abolitionists say the problem was use itself; industries say it is solved. The constraint persists because it solves a coordination problem for the powerful seats (industry legitimacy, welfare organizational survival) while extracting from the powerless seat (animals). This is not pure extraction (snare) because the coordination function is real and valued by multiple seats; it is not pure coordination (rope) because the extraction from animals is structural and the constraint requires active enforcement to suppress abolitionist challenges to the property foundation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the welfare_reading structurally relate to its sibling readings of the animal_moral_status kernel?',
    'Comparative constraint analysis: author the sibling readings as separate constraint stories and examine their ε values, beneficiary/victim structures, and computed seat types. The welfare reading claims the middle ground; the analysis must test whether that middle ground is a stable coordination or an unstable compromise.',
    'If welfare_reading computes as tangled_rope while abolitionist_reading computes as mountain (rights as natural law) and property_reading as piton (vestigial property claim), the kernel''s structural decomposition is validated. If welfare_reading computes as rope, its extraction is lower than claimed. If it computes as snare, the coordination function is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between the three readings of the animal_moral_status kernel.').

omega_variable(
    welfare_abolition_boundary,
    'Does the welfare reading''s ''unnecessary suffering'' standard structurally require the property status that abolitionists reject, or can welfare standards exist without property status?',
    'Jurisdictional comparison: examine legal systems that have granted limited personhood or rights to animals (e.g., New Zealand''s animal sentience recognition, Ecuador''s rights of nature) — do welfare standards persist, transform, or dissolve?',
    'If welfare standards require property status, the welfare reading forecloses abolition within its framework. If they can exist without it, the readings coexist as competing regulatory paradigms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_abolition_boundary, conceptual, 'Whether the welfare reading''s coordination function depends on the property foundation that abolitionists contest.').

omega_variable(
    extraction_measurement_ambiguity,
    'How much of the measured extractiveness (ε=0.35) is the price of genuine coordination (preventing worst cruelties) versus extractive overhead (permitting ''necessary'' suffering that serves industry efficiency)?',
    'Counterfactual modeling: estimate suffering under a hypothetical ''maximum welfare'' regime (best available practices universally mandated) versus current ''minimum legal'' standards. The gap measures the extraction component.',
    'If the gap is small, the constraint is closer to rope. If the gap is large, the tangled_rope classification is confirmed and the extraction component is substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, empirical, 'Decomposing ε into coordination cost versus extractive overhead in welfare standards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t6, animal_moral_status__welfare_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t12, animal_moral_status__welfare_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t18, animal_moral_status__welfare_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t24, animal_moral_status__welfare_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(animal_moral_status__welfare_reading_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t6, animal_moral_status__welfare_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t12, animal_moral_status__welfare_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t18, animal_moral_status__welfare_reading, base_extractiveness, 18, 0.32).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t24, animal_moral_status__welfare_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(animal_moral_status__welfare_reading_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t6, animal_moral_status__welfare_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t12, animal_moral_status__welfare_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t18, animal_moral_status__welfare_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t24, animal_moral_status__welfare_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(animal_moral_status__welfare_reading_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is the welfare_reading of the animal_moral_status kernel. The three readings form a constraint family: property_reading (piton/vestigial), welfare_reading (tangled_rope), abolitionist_reading (mountain/rope depending on framing). The welfare reading is structurally downstream of the property reading (it accepts the property foundation but imposes welfare limits) and upstream of the abolitionist reading (it is the compromise that abolitionists argue legitimizes the property status). All three stories should be authored and linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
