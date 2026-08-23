% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status (Property Reading)
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This is the property reading of the contested animal_status_kernel. It
 *   treats animals as chattel property under law and moral theory, deriving
 *   moral considerability exclusively from ownership rights and treating
 *   economic value as the sole relevant metric. Animals are structurally
 *   excluded from the victim set because the reading denies them moral
 *   standing; the constraint coordinates human economic activity around clear
 *   title while externalizing all costs onto non-persons. The high
 *   extractiveness and suppression metrics describe the objective operation
 *   of the arrangement, while the claimed rope type reflects the reading's
 *   self-understanding as a coordination mechanism for human affairs. The
 *   divergence is the signal.
 *
 * KEY AGENTS:
 *   - private_property_owners: Primary beneficiary (moderate/mobile) â collect economic value and legal protection from the constraint.
 *   - animal_agriculture_sector: Concentrated beneficiary (institutional/constrained) â profits from low-cost animal use under property law.
 *   - biomedical_research_sector: Concentrated beneficiary (institutional/constrained) â depends on property classification for research access.
 *   - courts_and_legislatures: Agenda setter (institutional/constrained) â administers and enforces the property framework.
 *   - abolitionist_advocates: Excluded voice (organized/constrained) â rejects the framework entirely but lacks standing within it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.85).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.8).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, rope).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2').
narrative_ontology:cs_kernel_codification('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', formalized).
narrative_ontology:cs_authority_grounding('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', lineage).
narrative_ontology:cs_interpretation_layer_present('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2').
narrative_ontology:cs_reading_relation('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_reading_relation('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', foundational, moral_considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', moral_considerability_derives_from_ownership, conventional).
narrative_ontology:cs_axiom('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', foundational, economic_value_is_only_relevant_value).
narrative_ontology:cs_axiom_status(economic_value_is_only_relevant_value, holdable).
narrative_ontology:cs_axiom_grounding('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', economic_value_is_only_relevant_value, instrumental).
narrative_ontology:cs_reference_frame('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', liberal_property_regime).
narrative_ontology:cs_drift_state('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', contemporary_animal_rights_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1f82373d-8abd-4b46-9ee4-bc4d7cc66ed2', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, private_property_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_agriculture_sector).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, biomedical_research_sector).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, legal_positivism).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as chattel with rights of use, exclusion, and disposition. Benefit from enforceable ownership claims, collateral value, and predictable transfer rules. Can divest animal holdings but remain within the property framework for other assets.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, private_property_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operates intensive production systems predicated on low-cost acquisition and replacement of animal bodies. Benefits from property status that permits standardization of breeding, confinement, and slaughter without individual standing challenges. Infrastructure and capital lock-in makes exit from animal property costly.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_agriculture_sector, beneficiary,
    institutional, generational, constrained, global).

% Depends on property classification to secure predictable access to animal subjects for experimentation and testing. Benefits from legal clarity that insulates research protocols from claims sounding in rights rather than welfare. Regulatory compliance is cheaper under property than under personhood frameworks.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, biomedical_research_sector, beneficiary,
    institutional, generational, constrained, national).

% Administer and enforce property law, adjudicate title disputes, and draft anti-cruelty statutes as protections of owner property value rather than recognition of animal interests. Bound by precedent and the formalized legal tradition that classifies animals as moveable property.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, courts_and_legislatures, agenda_setter,
    institutional, civilizational, constrained, national).

% Assert that animals are moral persons and that property status itself is the injustice. Structurally excluded from legal standing and from policy frameworks that treat animals exclusively as economic assets; their arguments are heard only in marginal public forums, not in property adjudication.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear title and transferable rights over animals, reducing inter-human conflict over resource control and creating predictable rules for commerce, inheritance, and liability.
% TRANSFER_FUNCTION: Moves control over animal bodies and the economic value derived from them to titled owners; moves risk and maintenance liability to owners as well.
% ABSENT_VOICES: Abolitionist advocates who reject the property framework root-and-branch, and animals themselves as beings with interests, are structurally excluded; their absence is constitutive of the property reading's coherence.
% DISAPPEARANCE_RATIONALE: If animal property status disappeared overnight, ownership claims would collapse, contracts for animal transfer would become voidable, and industries predicated on animal use would face existential legal uncertainty; human economic arrangements would reorganize around either personhood or a non-property regulatory regime.
% FOUNDING_PROBLEM: Preventing conflict over the control and use of animals among humans, and establishing predictable rules for animal commerce, inheritance, and remedy for theft or damage.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest that early animal property law emerged to settle inter-human disputes and facilitate trade. However, contemporary critics outside the benefiting sectors argue that the original coordination problem has been superseded by new moral knowledge about animal sentience, and that the arrangement now persists as a framework for extraction rather than coordination.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the reading permits unrestricted use subject only to property-value protections, eliminating any countervailing moral constraint. Suppression is high (0.80) because the framework actively excludes animal standing, ag-gag laws suppress documentation of conditions, and alternative normative frameworks are kept outside legal recognition. Theater ratio is moderate-high (0.45): anti-cruelty statutes perform moral concern while actually protecting owner economic interests. Accessibility collapse is high (0.75) because legal standing for animals is nearly impossible within property logic. Resistance is moderate (0.55) reflecting sustained but institutionally marginalized abolitionist and welfare advocacy. The founding problem remains live from this reading's perspective, preventing mandatrophy.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats (owners, industries) experience the constraint as legitimate coordination that secures their economic activity. The excluded abolitionist seat experiences it as systematic moral erasure. Because this reading excludes animals from the victim set entirely, the engine will compute low directionality for all named stakeholders, producing per-seat classifications consistent with rope. The high base extractiveness captures costs borne by beings the reading refuses to recognize as agents.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are either beneficiaries or agenda setters; the structural derivation places their directionality near the beneficiary end (low d). No victims are declared because the property reading structurally excludes animals from moral consideration. Abolitionist advocates are excluded rather than targeted â their absence from the conversation is a feature, not an oversight. The engine will therefore compute low effective extraction for every named seat, even though the base metric is high.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â coordinating human economic activity and preventing conflict over animal control â is authored as live because property law continues to adjudicate disputes and facilitate trade. This prevents a piton classification. However, the temporal measurements show extraction accumulation and theater growth, suggesting the constraint has drifted beyond its original coordination function into intensified extraction. The claim/metric divergence (claimed rope, high extraction and suppression) is the primary signal: the property reading's self-image as coordination is sustained only by excluding its costs from the agent surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_status_exclusion,
    'Does the property reading''s exclusion of animals from the victim set reflect an ontological truth about animals, or a structural blind spot produced by the legal framework?',
    'Comparative analysis of legal frameworks granting animals partial standing, combined with convergent scientific evidence on animal sentience and cognitive complexity.',
    'If animals are genuine moral patients, the property reading misclassifies systematic extraction as coordination, and the engine''s per-seat computation will diverge sharply from the reading''s self-image.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_exclusion, conceptual, 'Whether animal exclusion from victim set is ontological or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative frameworks structural (legal barriers to animal standing) or internalized (the property framing is so dominant that alternatives are literally unthinkable within the discourse)?',
    'Cross-jurisdictional comparison of legal education, judicial reasoning, and legislative debate to see whether alternatives are rejected or simply never conceived.',
    'If internalized, effective suppression is higher than structural measures suggest because the framework reproduces itself epistemically without ongoing enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of animal rights alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 124).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__property_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(anim_tr_t124, animal_status_kernel__property_reading, theater_ratio, 124, 0.45).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__property_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.76).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.81).
narrative_ontology:measurement(anim_be_t124, animal_status_kernel__property_reading, base_extractiveness, 124, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__property_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.68).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(anim_su_t124, animal_status_kernel__property_reading, suppression_requirement, 124, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.15).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the animal_status_kernel family. The epsilon-invariance principle requires separate stories because the property, welfare, and abolitionist readings have different beneficiary/victim structures, different epsilon values, and different failure modes. Property reading treats the kernel as formalized property law; welfare reading treats it as welfare-constrained property; abolitionist reading treats it as illegitimate personhood denial. Each story carries its own structural data and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
