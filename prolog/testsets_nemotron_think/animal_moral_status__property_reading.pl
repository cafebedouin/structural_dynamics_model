% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Property Status (Property Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The property reading of animal moral status holds that animals are
 *   property — resources with no independent moral standing whose interests
 *   are subordinate to human interests by definition. This reading
 *   instantiates the animal_moral_status kernel by treating the property
 *   regime as a mountain: a natural, unchangeable coordination mechanism for
 *   allocating control over animal bodies and labor. The reading claims v_low
 *   extraction (ε≈0.15) because it assesses the property regime abstractly as
 *   a title-clearing mechanism, not the industrial uses it licenses.
 *   Beneficiaries are property owners, animal users, and animal industries.
 *   Animals are not in the victim set — the reading's definitional move. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as mountain
 *   while authored metrics show rising extraction, theater, and suppression
 *   over the interval as animal use industrializes and the regime defends
 *   itself against welfare and abolitionist challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.15).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.25).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4').
narrative_ontology:cs_kernel_codification('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', formalized).
narrative_ontology:cs_authority_grounding('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', lineage).
narrative_ontology:cs_interpretation_layer_present('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4').
narrative_ontology:cs_reading_relation('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', foundational, animals_are_property_no_moral_standing).
narrative_ontology:cs_axiom_status(animals_are_property_no_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', animals_are_property_no_moral_standing, conventional).
narrative_ontology:cs_axiom('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', foundational, human_interests_lexically_prioritize_animal_interests).
narrative_ontology:cs_axiom_status(human_interests_lexically_prioritize_animal_interests, holdable).
narrative_ontology:cs_axiom_grounding('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', human_interests_lexically_prioritize_animal_interests, deontological).
narrative_ontology:cs_reference_frame('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', classical_property_regime).
narrative_ontology:cs_drift_state('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', contemporary_animal_ethics_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('18e6a8e2-b7b2-48c6-b5c5-76fcef6658d4', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_users).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_as_natural_law).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_dominion_over_nature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own animals as property; benefit from full legal control over use, breeding, and disposition; can sell or transfer animals freely; exit is selling property or shifting to non-animal assets.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners, beneficiary,
    powerful, biographical, mobile, national).

% Use animals for research, entertainment, labor, companionship; benefit from legal certainty of access and use rights; constrained by welfare regulations but property status guarantees baseline authority over animal use decisions.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_users, beneficiary,
    moderate, biographical, constrained, national).

% Operate industrial-scale animal agriculture, pharmaceutical testing, breeding, and trade; property status enables business models and capital investment; can jurisdiction-shop for favorable regulations; lobby to maintain and expand property framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Subject to human ownership, use, and killing with no legal standing; cannot exit, object, or claim interests; interests subordinated by the property framework's definitional denial of moral standing; would object to confinement, forced reproduction, and killing if they could articulate preferences.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals, excluded,
    powerless, immediate, trapped, local).

% Argue for minimizing suffering within the property framework; seek regulatory constraints on use (space, handling, slaughter methods) but accept property status as given; operate inside the regime's interpretive layer.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, welfare_advocates, observer,
    organized, generational, analytical, national).

% Argue property status itself is the violation; seek legal personhood and rights for animals; excluded from the property framework's internal logic which treats property status as settled and animals as non-participants.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, animal_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Property rights solve the coordination problem of allocating control over animal bodies and labor: they establish clear title, prevent conflict over use, and enable long-term investment in animal husbandry by guaranteeing owners the returns from their care and breeding decisions.
% TRANSFER_FUNCTION: The property regime transfers decision-making authority over animal lives and bodies from the animals themselves to human owners, enabling owners to capture the full value of animal labor, reproduction, and bodies while externalizing the costs of suffering onto the animals.
% ABSENT_VOICES: The animals themselves — who would object to confinement, killing, and forced reproduction if they could articulate preferences — are structurally excluded by the property framework's definitional denial of their moral standing. Abolitionist advocates who challenge the property status itself are excluded from the framework's internal discourse, which treats property status as settled.
% DISAPPEARANCE_RATIONALE: If animals were no longer property overnight, the legal basis for owning, selling, breeding, and killing them would collapse; new frameworks (guardianship, personhood, commons) would have to be invented, and the global animal-industrial complex would face existential legal disruption.
% FOUNDING_PROBLEM: The pre-legal chaos of unregulated animal use — theft of livestock, disputes over stray animals, inability to invest in breeding without secure title — required a stable allocation mechanism to coordinate human-animal relations and support agricultural development.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Pound, Hart) corroborate the coordination function of property in early agricultural societies from outside the benefiting parties; animal ethicists (Regan, Francione) attest the problem is transformed at industrial scale and the founding justification no longer matches the arrangement.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.15 (v_low) per the reading's self-assessment of the property regime abstractly, but the measurement series shows it rising to 0.35 as industrial animal agriculture, research, and trade expand the extraction the regime enables. Suppression starts low (0.15) — property law coordinates without heavy enforcement — but rises to 0.45 as ag-gag laws, veterinary reporting exemptions, and criminalization of undercover investigation harden the enforcement shell. Theater rises from 0.05 to 0.25 as 'humane certification' and welfare labeling perform coordination while the extraction core expands. Accessibility_collapse is high (0.9) because the reading treats property status as conceptually exhaustive — no alternative framing is intelligible within its framework. Resistance is low (0.1) internally but the measurement interval captures external resistance from competing readings.
 *
 * PERSPECTIVAL GAP:
 *   From the property owner seat, the regime is genuine coordination (rope-like) — clear title, dispute reduction, investment security. From the animal seat (excluded, not a stakeholder in this reading), the same regime is total extraction with no exit. The engine computes per-seat classifications from the structural data; the property reading's claim of mountain reflects only the agenda-setter/beneficiary seats' experience. The abolitionist and welfare readings would compute radically different seat types for the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners, animal users, and animal industries are declared beneficiaries — they collect the value of animal use with legal certainty. Their exit_options (mobile, constrained, arbitrage) and power (powerful, moderate, institutional) derive low directionality (d near 0.0). Animals are excluded from the victim set by the reading's definitional premise, so no victim stakeholders are declared. This beneficiary structure with zero declared victims on a claimed mountain triggers the False Summit Mountain (FSM) candidate condition — the engine will test whether identifiable beneficiaries on a 'natural law' indicate a constructed constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pre-legal chaos of unregulated animal use, theft, and insecure title — is contested: property owners attest it remains live; critics attest industrial scale has transformed the problem such that the original coordination function no longer matches the arrangement. The mandate has not resolved; it has mutated. The property reading treats the mandate as live and the arrangement as mountain; the welfare reading treats it as live but requiring reform (scaffold/tangled_rope); the abolitionist reading treats it as dead and the arrangement as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_property,
    'Is animal property status a genuine natural law (mountain) or a constructed legal category that benefits identifiable human agents?',
    'Cross-cultural and historical comparison: if property-in-animals varies radically across societies and epochs while core physical laws do not, the natural-law claim fails.',
    'If constructed, the constraint is a false summit — FSM signature would reclassify to tangled_rope (coordination of human use + asymmetric extraction from animals). If natural, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_property, conceptual, 'Natural-law vs. constructed status of animal property rights').

omega_variable(
    extraction_assessment_dispute,
    'Does the property reading''s v_low ε assessment reflect the property regime abstractly, or does it ignore the extraction enabled by the regime in current industrial practice?',
    'Measure the delta between the property regime''s abstract coordination function (title clarity, dispute reduction) and the aggregate extraction it licenses in factory farming, research, and trade.',
    'If the regime''s ε is low only when abstracted from the use it authorizes, the reading''s metric is frame-dependent — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_assessment_dispute, empirical, 'Whether ε is measured abstractly or inclusively of licensed use').

omega_variable(
    kernel_reading_structure,
    'How does this property_reading relate structurally to the sibling welfare_reading and abolitionist_reading of the animal_moral_status kernel?',
    'Map the structural deltas: property_reading excludes animals from victim set and sets ε=v_low; welfare_reading includes animals as partial victims with ε=moderate; abolitionist_reading includes animals as full victims with ε=high. The disagreement is located on the moral-standing atom.',
    'If the three readings have mutually exclusive victim sets and ε values, they are distinct constraints linked by kernel_id, not one constraint with measurement variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committee-frame structure: this reading''s position in the kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t0, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t10, animal_moral_status__property_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t10, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t20, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t30, animal_moral_status__property_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t30, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t40, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(animal_moral_status__property_reading_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t0, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_be_t10, animal_moral_status__property_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t10, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t20, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_be_t30, animal_moral_status__property_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t30, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t40, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(animal_moral_status__property_reading_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t0, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_su_t10, animal_moral_status__property_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t10, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t20, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_su_t30, animal_moral_status__property_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t30, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t40, observed).
narrative_ontology:measurement(animal_moral_status__property_reading_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(animal_moral_status__property_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.15).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the animal_moral_status kernel family. The three readings (property, welfare, abolitionist) have mutually exclusive victim sets and ε values (v_low, moderate, high) and are distinct constraints linked by kernel_id. The property reading's claimed mountain status with declared beneficiaries makes it an FSM candidate; the welfare reading is a tangled_rope candidate (coordination + asymmetric extraction); the abolitionist reading is a snare candidate (pure extraction from animals).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
