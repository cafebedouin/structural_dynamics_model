% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Legal Objecthood (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the property reading of the
 *   animal_status kernel. Under this reading, animals are legal objects
 *   without independent moral standing, and human ownership is unrestricted
 *   except where limited by voluntarily adopted welfare statutes. The
 *   constraint coordinates human-to-human interactions over animal bodies and
 *   products, functioning as a property-rights regime. The structural delta
 *   from sibling readings is that animals are not seated as victims or
 *   rights-holders; the entire stakeholder surface is human. This yields
 *   near-zero extractiveness because the constraint operates as a
 *   dispute-resolution mechanism among rights-bearing humans rather than as
 *   an extraction mechanism from sentient beings. The authored metrics are
 *   independent of the claimed type: the low extraction and suppression
 *   scores reflect the reading's own assessment of the arrangement as
 *   background legal coordination.
 *
 * KEY AGENTS:
 *   - animal_property_holders (beneficiary, moderate/mobile)
 *   - state_legal_authority (agenda_setter, institutional/analytical)
 *   - animal_welfare_advocates (observer, organized/mobile)
 *   - abolitionist_advocates (excluded, organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_status__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Legal Objecthood (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'a68b9992-a9cb-4998-bf85-b9bd2fc2c24c').
narrative_ontology:cs_kernel_codification('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', formalized).
narrative_ontology:cs_authority_grounding('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', lineage).
narrative_ontology:cs_interpretation_layer_present('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c').
narrative_ontology:cs_reading_relation('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', foundational, legal_objecthood_no_moral_status).
narrative_ontology:cs_axiom_status(legal_objecthood_no_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', legal_objecthood_no_moral_status, conventional).
narrative_ontology:cs_axiom('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', foundational, unrestricted_human_use_baseline).
narrative_ontology:cs_axiom_status(unrestricted_human_use_baseline, holdable).
narrative_ontology:cs_axiom_grounding('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', unrestricted_human_use_baseline, conventional).
narrative_ontology:cs_reference_frame('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', legal_objecthood_framework).
narrative_ontology:cs_drift_state('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', contemporary_welfare_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a68b9992-a9cb-4998-bf85-b9bd2fc2c24c', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_property_holders).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, legal_positivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals and rely on the property framework to secure ownership, transfer, and recourse against theft or damage; benefit from the legal objecthood classification that permits unrestricted use within welfare bounds.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_property_holders, beneficiary,
    moderate, biographical, mobile, national).

% Codifies and enforces the legal classification of animals as property, adjudicates title disputes, and registers ownership claims; maintains the framework through statutory and common law evolution.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, state_legal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Lobby for welfare constraints within the property framework but do not challenge the foundational classification of animals as legal objects; operate as reformers rather than abolitionists.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, observer,
    organized, biographical, mobile, national).

% Assert that animals possess inherent moral status incompatible with property classification; their position is structurally excluded from the legal framework's foundational premises and appears only in external moral or political discourse.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear title, transfer, and dispute-resolution rules for animal bodies and products among humans; prevents conflicts over capture, ownership, and damages.
% TRANSFER_FUNCTION: Assigns exclusive control over animal bodies and labor to specific human owners, moving animals from a state of legal nature or commons into private title.
% ABSENT_VOICES: Abolitionist advocates who reject the property framework entirely; they are present in public discourse but excluded from the legal framework's foundational classification.
% DISAPPEARANCE_RATIONALE: Without the legal objecthood framework, title to animals would be contested, agricultural and companion animal industries would face legal uncertainty, and human-to-human dispute resolution would revert to ad hoc or possession-based norms.
% FOUNDING_PROBLEM: Competing claims over animal bodies, labor, and products in the absence of a uniform legal classification, leading to uncertainty, violence, and unresolved damages.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative law scholars attest that pre-codified regimes relied on possession and local custom, generating frequent disputes; these sources are outside the direct beneficiary set of modern animal industries.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint, on its own reading, does not extract from its subjects; it coordinates human owners. Suppression is low (0.12) because the framework is background law rather than active coercion. Theater ratio is negligible (0.05) because property registration and dispute resolution are functional, not performative. Accessibility collapse is moderate (0.50) because alternative arrangements (commons, personhood) are intellectually available but would require sweeping legal reorganization. Resistance is low (0.15) because the framework is widely internalized as natural.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state legal authority) and beneficiary seat (animal property holders) should compute similarly â both experience the constraint as coordination. The excluded seat (abolitionist advocates) would compute radically differently if seated as a payer, but under this reading it is structurally excluded from the stakeholder surface. The observer seat (welfare advocates) experiences minor friction from the baseline. The engine will not seat animals because they are not declared as victims or beneficiaries in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The only declared beneficiaries are animal property holders; there are no declared victims. This drives directionality toward the beneficiary end for property holders and leaves no seated agent with high d. The state legal authority, as agenda setter, sits near symmetric with slight beneficiary bias because it administers rather than collects. Welfare advocates and abolitionist advocates are observer and excluded, respectively, and do not enter the directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading avoids mandatrophy mislabeling by clearly distinguishing its founding problem (human-to-human dispute resolution over animal bodies) from the abolitionist reading's framing (ending instrumental use). The founding problem remains live because property disputes continue to be adjudicated daily. The constraint is not a piton because it is not maintained by inertia or theater; it actively processes title and transfers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seat_inclusion_ambiguity,
    'Would the classification of this constraint change if animals were structurally included as seated agents rather than excluded as non-agents?',
    'Comparison with sibling readings (welfare_reading, abolitionist_reading) that seat animals as payers or beneficiaries.',
    'If animals were seated, extractiveness would rise and the constraint would likely compute as snare or tangled_rope; the low epsilon depends entirely on the reading''s agent boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seat_inclusion_ambiguity, conceptual, 'Whether the constraint''s classification is robust to including animals in the stakeholder surface').

omega_variable(
    welfare_statute_trajectory,
    'Do welfare statutes represent external moral constraints on property or internal refinements of the property framework?',
    'Legislative history and judicial framing analysis.',
    'If external, the property reading is under structural pressure; if internal, the reading absorbs drift without fundamental change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_trajectory, conceptual, 'Conceptual framing of welfare statutes relative to property baseline').

omega_variable(
    property_contingency,
    'Is the legal objecthood of animals a universal legal feature or a contingent Western legal tradition?',
    'Comparative legal anthropology and historical jurisprudence.',
    'If contingent, the constraint''s stability is lower and reform more accessible; if universal, it behaves more like natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_contingency, empirical, 'Empirical universality of animal property classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_status_prop_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(animal_status_prop_tr_t25, animal_status__property_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(animal_status_prop_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(animal_status_prop_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(animal_status_prop_be_t25, animal_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(animal_status_prop_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the animal_status kernel, decomposed per the epsilon-invariance principle because the property reading, welfare reading, and abolitionist reading have structurally distinct epsilon values, beneficiary/victim surfaces, and normative foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
