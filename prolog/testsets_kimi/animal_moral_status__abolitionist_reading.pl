% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Property Status
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the abolitionist reading of the contested
 *   animal_moral_status kernel. Under this reading, the legal and social
 *   property status of animals is not a neutral coordination mechanism but a
 *   structure of systematic victimization that extracts bodily autonomy,
 *   life, and liberty from animals under human dominion and transfers these
 *   to human beneficiaries. The constraint operates through formalized legal
 *   codes, active enforcement of property rights, and an interpretive layer
 *   of welfare regulation that absorbs critique without altering the
 *   underlying property relation. All use, however labeled humane,
 *   perpetuates the extraction. The claim of tangled_rope is authored
 *   independently of the high extractiveness metrics: the constraint does
 *   coordinate massive human economic activity, but the coordination function
 *   is inseparable from asymmetric extraction that falls entirely on a
 *   trapped, powerless victim population.
 *
 * KEY AGENTS:
 *   - animal_use_industries: Primary beneficiary and agenda setter (powerful/constrained exit) â captures the extraction through commodity production
 *   - state_legal_system: Institutional agenda setter (institutional/constrained exit) â codifies and enforces the property relation
 *   - animals_under_human_dominion: Primary target (powerless/trapped exit) â bears the full cost of the property relation
 *   - consumers_of_animal_products: Diffuse beneficiary (organized/mobile exit) â receives subsidized products and cultural convenience
 *   - animal_advocacy_organizations: Analytical observer (organized/mobile exit) â documents harm and advances abolitionist frames from outside the benefiting coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.84).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.76).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Abolitionist Reading of Animal Property Status").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'e3b39def-ab41-4bd1-9a6c-3f801ee23179').
narrative_ontology:cs_kernel_codification('e3b39def-ab41-4bd1-9a6c-3f801ee23179', formalized).
narrative_ontology:cs_authority_grounding('e3b39def-ab41-4bd1-9a6c-3f801ee23179', lineage).
narrative_ontology:cs_interpretation_layer_present('e3b39def-ab41-4bd1-9a6c-3f801ee23179').
narrative_ontology:cs_reading_relation('e3b39def-ab41-4bd1-9a6c-3f801ee23179', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('e3b39def-ab41-4bd1-9a6c-3f801ee23179', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_axiom('e3b39def-ab41-4bd1-9a6c-3f801ee23179', foundational, animal_personhood_rights_bearing).
narrative_ontology:cs_axiom_status(animal_personhood_rights_bearing, holdable).
narrative_ontology:cs_axiom_grounding('e3b39def-ab41-4bd1-9a6c-3f801ee23179', animal_personhood_rights_bearing, deontological).
narrative_ontology:cs_axiom('e3b39def-ab41-4bd1-9a6c-3f801ee23179', foundational, property_status_inherently_violative).
narrative_ontology:cs_axiom_status(property_status_inherently_violative, holdable).
narrative_ontology:cs_axiom_grounding('e3b39def-ab41-4bd1-9a6c-3f801ee23179', property_status_inherently_violative, deontological).
narrative_ontology:cs_reference_frame('e3b39def-ab41-4bd1-9a6c-3f801ee23179', anthropocentric_property_dominion).
narrative_ontology:cs_drift_state('e3b39def-ab41-4bd1-9a6c-3f801ee23179', contemporary_animal_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e3b39def-ab41-4bd1-9a6c-3f801ee23179', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the confinement, breeding, processing, and sale of animals under legal property frameworks. Lobbies for property protections, ag-gag legislation, and against personhood status. Collects revenue from animal products, research protocols, and entertainment. Sectoral exit would require full economic transformation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animal_use_industries, beneficiary).

% Codifies and enforces animal property status through civil and criminal law, adjudicating ownership disputes and welfare violations while preserving the underlying use relation. Could abolish property status through legislative or constitutional reform but is structurally tied to economic and political stability.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, state_legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Receive subsidized animal products, cultural practices of consumption, and economic convenience from the property system. Aggregate demand sustains the constraint, and individual exit via plant-based alternatives is increasingly available but not evenly distributed.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).

% Are bred, confined, used, and killed under legal property regimes. Their interests in bodily integrity, liberty, and life are subordinated to owner prerogatives. Exit from the constraint is impossible; their entire existence is structured by it.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, immediate, trapped, global).

% Document systemic harm, advance abolitionist legal and ethical frames, and campaign for personhood and non-use. They are present in public discourse but structurally excluded from formal property and regulatory drafting processes.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_advocacy_organizations, observer,
    organized, biographical, mobile, global).

% Enforce animal welfare standards within property frameworks, administering inspections and penalties for cruelty while preserving the underlying use relationship. Their authority depends on the property system remaining intact.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human access to animal bodies for food, fiber, labor, research, and companionship through a predictable system of ownership, transfer, and disposition.
% TRANSFER_FUNCTION: Moves bodily integrity, reproductive autonomy, labor, and life from animals under human dominion to human owners, industries, and consumers, mediated by market exchange and legal title.
% ABSENT_VOICES: The animals themselves are structurally excluded from legal personhood and political voice; their interests are mediated by owners and welfare regulators who are structurally committed to preserving use. Abolitionist voices are present in civil society but excluded from most statutory and regulatory drafting bodies where welfare frames dominate.
% DISAPPEARANCE_RATIONALE: If animal property status vanished overnight, global agriculture, pharmaceutical research, food systems, and companion animal regimes would require immediate fundamental restructuring. Ownership claims would dissolve, slaughter and confinement industries would lose legal foundation, and human-animal relations would shift toward guardianship or non-contact frameworks.
% FOUNDING_PROBLEM: How to establish stable, predictable human dominion over animals for food, labor, materials, and social order in settled human societies.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist ethicists and food-systems researchers outside the beneficiary set attest that the founding problem is obsolete given plant-based and cellular alternatives. Animal use industries and state agricultural departments attest it remains live. Corroboration is split along interest lines with no unimpeached neutral seat.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.84) because the property relation grants near-total disposition over animals' bodies and lives, with their interests systematically subordinated. Suppression (0.76) reflects the active legal and economic enforcement of property status, including ag-gag laws, policing of trespass and theft, and cultural stigma against abolition. Theater ratio (0.55) captures the performative function of welfare regulations and humane labels, which maintain the property kernel by appearing to address cruelty while preserving use. Accessibility collapse (0.72) is high because, once the property frame is accepted, abolition becomes cognitively radical and alternatives are rendered invisible. Resistance (0.58) is moderate: abolitionist and welfare reform movements exert real pressure but remain marginalized in formal policy venues. The measurement series show extraction and theater rising together over the interval as industrial efficiency intensifies and welfare discourse expands to absorb critique.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (animal_use_industries, consumers) experience the constraint as a legitimate resource-allocation system that solves coordination problems. The payer seat (animals) experiences it as total extraction with zero degrees of freedom. The state seat experiences it as inherited legal architecture. The engine will compute these seats differently: beneficiaries with mobile or constrained exit and moderate-to-high power will show low directionality and damped effective extraction; the trapped, powerless victim seat will show maximal directionality and amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals_under_human_dominion are declared victims with powerless/trapped exit, placing their derived directionality near the full-target pole. Animal_use_industries are declared beneficiaries with powerful/constrained exit, placing their directionality near the beneficiary pole. Consumers are beneficiaries with organized/mobile exit, also near the beneficiary pole but less concentrated. The state is an agenda setter with institutional/constrained exit; its directionality is ambiguous but derived closer to the beneficiary side because it administers and sustains the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents mislabeling by insisting that welfare reforms are not evidence of a living founding problem but rather theater that extends the constraint's mandate beyond its original function of pure resource extraction. The founding problem of securing human dominion is dead in the sense that alternative food and materials systems exist; the constraint persists because the extraction remains profitable and the institutional forms have inertial mass. Declaring the founding problem dead while the disappearance verdict is world_rearranges flags the constraint as a zombie institution â exactly what the mandatrophy machinery is designed to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the abolitionist claim that animals are rights-bearing individuals a description of a natural moral fact or a constructed normative commitment?',
    'Philosophical analysis of moral realism vs. constructivism in animal ethics; legal history of property category formation.',
    'If a natural fact, the constraint may be a false summit mountain misclassified as human construction; if constructed, tangled_rope or snare remains correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Whether abolitionist moral status is discovered or invented').

omega_variable(
    property_contingency,
    'Is animal property status a contingent legal convention or a structural necessity of human civilization?',
    'Comparative historical analysis of societies with varying animal use; viability assessment of plant-based food systems at scale.',
    'If contingent, active enforcement is the constraint''s lifeline; if structural, it might compute toward mountain or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_contingency, empirical, 'Contingency of animal property status').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of abolitionist alternatives structural (legal barriers, economic subsidies, institutional capture) or internalized (speciesist ontology rendered invisible and normative)?',
    'Cross-cultural comparison of resistance to abolition where legal barriers are removed; measurement of persistence of animal use after economic incentives shift.',
    'If internalized, effective suppression exceeds structural measures and the constraint may behave more like a distributed implicit norm than a formalized legal system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__abolitionist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__abolitionist_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__abolitionist_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__abolitionist_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__abolitionist_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__abolitionist_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__abolitionist_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__abolitionist_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__abolitionist_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__abolitionist_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__abolitionist_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__abolitionist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is the abolitionist reading of the animal_moral_status kernel, which decomposes into three structurally distinct constraints: abolitionist_reading (property status as violation), property_reading (animals as resources), and welfare_reading (regulated use). Each has a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
