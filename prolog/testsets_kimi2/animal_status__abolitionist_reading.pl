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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Nonhuman Animal Status
 *   domain: applied ethics / legal philosophy / political economy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_status kernel. The standing arrangement under contest is the legal
 *   and economic system treating nonhuman animals as human property and
 *   resources. From this reading, animals are rights-holders with inherent
 *   value that precludes any instrumental use; the current arrangement is
 *   read as a snare in which a coordination story (food security, medical
 *   progress) serves as cover for pure extraction. Welfare reforms are
 *   rejected as legitimation devices that sustain rather than challenge the
 *   property framework. The metrics are authored independently of the claim:
 *   the arrangement does coordinate real resource flows for human
 *   beneficiaries, but the authored scores emphasize the severe, ongoing
 *   extraction from animals and the active enforcement required to maintain
 *   their property status.
 *
 * KEY AGENTS:
 *   - nonhuman_animals: Primary target (powerless/trapped) â bears total extraction of bodily integrity and life
 *   - animal_use_industries: Primary beneficiary (powerful/mobile) â captures economic surplus from property status
 *   - state_property_regime: Agenda-setter (institutional/constrained) â enforces the legal framework
 *   - consumers_of_animal_products: Secondary beneficiary (moderate/constrained) â receives subsidized animal-derived goods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading of Nonhuman Animal Status").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied ethics / legal philosophy / political economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'b61bc120-09ee-4b64-9023-411ec24d1ff8').
narrative_ontology:cs_kernel_codification('b61bc120-09ee-4b64-9023-411ec24d1ff8', distributed).
narrative_ontology:cs_authority_grounding('b61bc120-09ee-4b64-9023-411ec24d1ff8', distributed).
narrative_ontology:cs_reading_relation('b61bc120-09ee-4b64-9023-411ec24d1ff8', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('b61bc120-09ee-4b64-9023-411ec24d1ff8', animal_status__welfare_reading, influences).
narrative_ontology:cs_axiom('b61bc120-09ee-4b64-9023-411ec24d1ff8', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('b61bc120-09ee-4b64-9023-411ec24d1ff8', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('b61bc120-09ee-4b64-9023-411ec24d1ff8', secondary, legal_property_status_unjust).
narrative_ontology:cs_axiom_status(legal_property_status_unjust, holdable).
narrative_ontology:cs_axiom_grounding('b61bc120-09ee-4b64-9023-411ec24d1ff8', legal_property_status_unjust, deontological).
narrative_ontology:cs_reference_frame('b61bc120-09ee-4b64-9023-411ec24d1ff8', rights_bearing_nonhuman_persons).
narrative_ontology:cs_drift_state('b61bc120-09ee-4b64-9023-411ec24d1ff8', industrial_animal_use_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b61bc120-09ee-4b64-9023-411ec24d1ff8', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, nonhuman_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally classified as property and chattel across most jurisdictions; bred, confined, and killed for food, research, and entertainment; possess no legal standing to challenge use; cannot exit the property system; their bodies, labor, and lives are the primary resource extracted.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, nonhuman_animals, payer,
    powerless, generational, trapped, global).

% Own and control animals as productive capital; convert animal bodies and lives into commodities for food, research materials, and consumer goods; capture the economic surplus generated by low input costs due to animals' property status; could transition to non-animal models but resist due to capital investment and market position.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_use_industries, beneficiary,
    powerful, biographical, mobile, global).

% Codifies and enforces the legal classification of animals as property; regulates welfare standards within use industries; adjudicates disputes between owners and rarely recognizes animal interests independent of human claims; maintains the framework through police power, contract law, and agricultural subsidies.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, state_property_regime, agenda_setter,
    institutional, generational, constrained, national).

% Receive inexpensive animal-derived food, clothing, and medicines made possible by the property system's externalization of true costs onto animals; social norms and infrastructure make non-animal alternatives accessible in principle but constrained by habit, cost, and cultural inertia.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, constrained, global).

% Campaign for improved treatment standards within animal use industries; their reforms are viewed by the abolitionist reading as legitimizing continued property status by making extraction appear more ethical, though the advocates do not themselves collect extraction.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_welfare_advocates, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human access to animal-derived food, materials, and research data through a standardized property and market system; provides a predictable legal framework for owning, breeding, and disposing of animals as economic inputs.
% TRANSFER_FUNCTION: Transfers bodily integrity, reproductive autonomy, labor, and life from nonhuman animals to human-controlled industries and consumers via the property regime.
% ABSENT_VOICES: Nonhuman animals are structurally excluded from legal and political deliberation; their interests are only mediated through human proxies. Abolitionist voices demanding complete abolition of property status are marginalized in mainstream policy discourse relative to welfare-framed reform.
% DISAPPEARANCE_RATIONALE: The global food system, biomedical research pipeline, fashion and chemical testing industries, and numerous consumer markets are built on animal property status; its disappearance would force rapid reorganization toward plant-based, cellular, and non-animal alternatives.
% FOUNDING_PROBLEM: Pre-industrial and early industrial societies required reliable, proximate sources of food, labor, and raw materials; legal property status in animals secured these resources against theft and loss, reduced transaction costs, and enabled capital accumulation in animal husbandry.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist ethicists and agricultural transition researchers attest that plant-based nutrition and non-animal research models have rendered the original resource-capture problem obsolete; industry beneficiaries attest it remains live. Corroboration from fully neutral parties without institutional ties to either animal industries or advocacy organizations is scarce.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.92, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.92) because the constraint systematically transfers animals' entire bodily existence to human control. Suppression is high (0.85) because the arrangement requires active legal suppression of animals' interests and agency, and the property framework excludes them from standing. Theater ratio is elevated (0.55) due to welfare regulations, humane certification, and ethical review boards that perform care without altering the extraction structure. Accessibility collapse is high (0.78) because legal alternatives to property status (rights, personhood) are structurally unavailable in almost all jurisdictions. Resistance is moderate (0.42) because abolitionist and reform movements exist but face institutional barriers. The measurement series tracks industrial intensification and the rise of welfare theater from 1975â2025.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary infrastructure for food and medicine; the payer seat experiences total extraction with zero reciprocity. The engine should compute severe divergence: the institutional seats land near coordination or tangled_rope, while the nonhuman animal seat computes as extreme extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal_use_industries and consumers_of_animal_products are declared beneficiaries, placing them at the low-d (subsidy) end of the derivation. Nonhuman_animals is the sole declared victim, placing it at the high-d (target) end. The state_property_regime is neither beneficiary nor victim in the base properties, so it reverts to its power atom's canonical fallback. No override is needed because the structural derivation matches the reading: industries are subsidized by externalized animal costs, while animals bear the full extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing animal resources for human subsistence) is read as dead because nutritional and technological alternatives now exist. The constraint persists not because it solves a live coordination problem but because beneficiaries have accumulated capital and institutional power within it. This is the mandatrophy pattern: a once-functional coordination mechanism has become primarily extractive but persists through inertia and beneficiary opposition to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_necessity_boundary,
    'Does the constraint extract from animals beyond what is strictly necessary for human survival and health, or is the current scale of use indispensable?',
    'Comparative analysis of plant-based nutritional adequacy and non-animal research models against current animal-dependent outputs.',
    'If use is dispensable, extraction is pure surplus and the constraint reads as snare; if indispensable, part of the extraction is the price of coordination and reads as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity_boundary, empirical, 'Whether animal use is strictly necessary or pure surplus extraction.').

omega_variable(
    welfare_legitimation_or_reform,
    'Do animal welfare regulations function primarily to reduce extraction or to legitimate its continuation?',
    'Longitudinal outcome measurement comparing welfare-era and pre-welfare-era total animal use and suffering metrics.',
    'If legitimation, theater_ratio and suppression are higher than surface measures suggest, strengthening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_legitimation_or_reform, empirical, 'Whether welfare reforms reduce harm or legitimate continued extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct readings: abolitionist (rights precluding all use), welfare (interests constraining use), and property (legal objects). Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
