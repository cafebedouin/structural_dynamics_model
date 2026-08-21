% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Status: Animals as Rights-Holders
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status,
 *   where animals are considered rights-holders with inherent value,
 *   precluding all instrumental use. From this perspective, any use of
 *   animals for human benefit (food, research, entertainment) is a violation
 *   of their fundamental rights and constitutes a snare. The constraint's
 *   persistence relies on the legal and philosophical suppression of animal
 *   personhood and the active enforcement of their property status. Welfare
 *   reforms are viewed as attempts to legitimize the underlying extraction
 *   rather than genuinely address it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Reading of Animal Status: Animals as Rights-Holders").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e').
narrative_ontology:cs_kernel_codification('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', formalized).
narrative_ontology:cs_authority_grounding('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', extraction).
narrative_ontology:cs_interpretation_layer_present('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e').
narrative_ontology:cs_reading_relation('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', foundational, instrumental_use_is_unjust).
narrative_ontology:cs_axiom_status(instrumental_use_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', instrumental_use_is_unjust, deontological).
narrative_ontology:cs_reference_frame('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', universal_animal_rights_recognition).
narrative_ontology:cs_drift_state('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a9ede9b0-3f7d-4e5a-8ad8-f9e685d2128e', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals_in_instrumental_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the direct targets of instrumental use (food, research, entertainment, clothing), animals bear the full cost of the current legal and ethical framework. They have no legal standing to object or exit, and their interests are systematically overridden.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals_in_instrumental_use, payer,
    powerless, immediate, trapped, universal).

% Actively campaign for the recognition of animal rights and the end of all instrumental use. They perceive the current system as a snare, extracting immense value from animals while suppressing their inherent value. Their 'exit' is the successful transformation of the legal and ethical landscape.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, constrained, global).

% Benefit directly from the instrumental use of animals across various industries (agriculture, pharmaceuticals, entertainment). They actively defend the current property status of animals and resist any changes that would restrict their operations, viewing the abolitionist position as an existential threat to their business models.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, industrial_animal_users, agenda_setter,
    institutional, biographical, mobile, global).

% Advocate for improved conditions for animals within instrumental use, but do not challenge the fundamental right to use animals. From the abolitionist perspective, their efforts are seen as legitimizing the underlying snare by making it appear less egregious, thus delaying true liberation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reformers, excluded,
    organized, biographical, constrained, national).

% Codify and enforce the property status of animals, providing the legal framework for instrumental use. They are slow to change and reflect deeply entrenched societal norms, making them a primary mechanism for the constraint's persistence.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, there is no genuine coordination function in the instrumental use of animals; the system primarily coordinates human exploitation of animals.
% TRANSFER_FUNCTION: Transfers the inherent value, bodily autonomy, and lives of animals to humans for various purposes (food, research, entertainment, clothing), enabling human economic and cultural practices at the animals' expense.
% ABSENT_VOICES: Animals themselves are the primary absent voices, unable to articulate their interests or consent. Their advocates (abolitionist_advocates) speak on their behalf, but animals lack direct representation in legal or ethical discourse.
% DISAPPEARANCE_RATIONALE: If the instrumental use of animals vanished overnight, global food systems, pharmaceutical research, entertainment industries, and cultural practices would undergo a radical, immediate, and profound reorganization. The economic and ethical landscape would be fundamentally transformed.
% FOUNDING_PROBLEM: The perceived human need for animal products and services, coupled with a philosophical framework that denied moral status to non-human animals, leading to their classification as property.
% FOUNDING_PROBLEM_CORROBORATION: The problem of human desire for animal products and services remains live, as evidenced by ongoing consumption and industry practices. However, the philosophical justification (denial of moral status) is contested by ethicists and animal rights scholars outside the benefiting industries.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is extremely high (0.95) because the constraint, from this reading, extracts the entire inherent value and bodily autonomy of animals. Suppression is also extremely high (0.98) due to the legal classification of animals as property, which denies them any legal standing or means of resistance. Accessibility collapse is low (0.1) because the abolitionist reading itself represents a clear alternative, though one that is actively suppressed. Resistance is high (0.8) due to the ongoing efforts of abolitionist advocates. Theater ratio is low (0.05) because, from this perspective, welfare reforms are not seen as genuine functional improvements but rather as cosmetic adjustments that do not alter the fundamental extractive nature of the system.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from property and welfare readings. While other readings might see coordination or limited extraction, the abolitionist view sees pure, total extraction and suppression. The engine's classification will highlight this divergence by computing a snare from the abolitionist perspective, contrasting with potentially different classifications from other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets (victims) of this constraint, bearing all costs with no exit. Abolitionist advocates are observers who experience the constraint as a snare, actively working against it. Industrial animal users and legal systems are the agenda-setters and beneficiaries, actively maintaining the constraint for their benefit. Welfare reformers are structurally excluded from the abolitionist conversation, as their efforts are seen as perpetuating the snare.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the mandate for instrumental animal use is based on a flawed premise (animals as property) that has never genuinely solved a coordination problem for animals themselves. The constraint's persistence is due to the benefits it provides to human users and the suppression of animal interests, not an enduring, legitimate mandate. The classification as a snare prevents mislabeling this as coordination by highlighting the systematic victimhood and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid instantiation of the ''abolitionist_reading'' of the ''animal_status'' kernel?',
    'Verification against core tenets of abolitionist animal ethics (e.g., inherent value, rejection of instrumental use, focus on rights rather than welfare).',
    'If not, the classification of this constraint as a snare with high extraction would be misattributed to the abolitionist framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this story accurately reflects the abolitionist perspective.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal property status) or internalized (societal norms, speciesism)?',
    'Post-legal-change suppression trajectory: if suppression persists after legal property status is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target (animals) carries the suppression with them after legal changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animal exploitation.').

omega_variable(
    welfare_reform_legitimation,
    'Do welfare reforms genuinely reduce extraction or primarily serve to legitimize instrumental use from the abolitionist perspective?',
    'Analysis of the impact of welfare reforms on the fundamental property status of animals and the overall volume of instrumental use. If property status remains and use continues unabated, legitimation is indicated.',
    'If welfare reforms primarily legitimize, the theater_ratio for the overall system is higher than currently estimated, as these reforms are performative rather than functional in ending extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_legitimation, conceptual, 'Role of welfare reforms in the context of abolitionist goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.96).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.97).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.98).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
