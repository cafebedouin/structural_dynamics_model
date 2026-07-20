% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism â Composite Reading
 *   domain: historical sociology / legal history / normative systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism governed how aristocratic males in early
 *   modern Europe responded to insult through the duel, regulated by a
 *   peer-enforced code of honor. The composite_reading interprets this kernel
 *   as a constraint eroded not by a single logic but by multiple independent
 *   pressures: the state's monopoly on legitimate violence, the calculative
 *   norms of the commercial bourgeoisie, the actuarial logic of emerging
 *   insurance institutions, and a structural recategorization of dueling from
 *   honorable to barbaric. This reading coexists with sibling readings that
 *   emphasize simple frequency decline or categorical cognitive closure. The
 *   mechanism coordinated elite dispute resolution while extracting death and
 *   injury from individual participants, enforced by social ostracismâa
 *   Tangled Rope whose lifecycle shows rising theatricality and eroding
 *   suppression capacity.
 *
 * KEY AGENTS:
 *   - honor_community: Agenda setter (organized/identity_locked) â enforces the code through social sanction.
 *   - aristocratic_establishment: Primary beneficiary (powerful/identity_locked) â captures collective status maintenance.
 *   - honor_bound_gentlemen: Primary target (powerful/identity_locked) â bears the corporeal and financial costs of dueling.
 *   - bourgeois_commercial_class: Excluded voice (moderate/mobile) â promotes alternative norms but is outside the honor conversation.
 *   - lower_class_excluded: Excluded and powerless (powerless/trapped) â subject to aristocratic violence without standing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.65).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism â Composite Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical sociology / legal history / normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '3b607747-190a-4767-a65d-a83eb0efdf15').
narrative_ontology:cs_kernel_codification('3b607747-190a-4767-a65d-a83eb0efdf15', implicit).
narrative_ontology:cs_authority_grounding('3b607747-190a-4767-a65d-a83eb0efdf15', practice).
narrative_ontology:cs_interpretation_layer_present('3b607747-190a-4767-a65d-a83eb0efdf15').
narrative_ontology:cs_reading_relation('3b607747-190a-4767-a65d-a83eb0efdf15', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b607747-190a-4767-a65d-a83eb0efdf15', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('3b607747-190a-4767-a65d-a83eb0efdf15', foundational, plural_independent_erosion_mechanisms).
narrative_ontology:cs_axiom_status(plural_independent_erosion_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('3b607747-190a-4767-a65d-a83eb0efdf15', plural_independent_erosion_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('3b607747-190a-4767-a65d-a83eb0efdf15', secondary, material_pressure_enables_recategorization).
narrative_ontology:cs_axiom_status(material_pressure_enables_recategorization, holdable).
narrative_ontology:cs_axiom_grounding('3b607747-190a-4767-a65d-a83eb0efdf15', material_pressure_enables_recategorization, empirically_contingent).
narrative_ontology:cs_reference_frame('3b607747-190a-4767-a65d-a83eb0efdf15', aristocratic_honor_practice).
narrative_ontology:cs_drift_state('3b607747-190a-4767-a65d-a83eb0efdf15', industrial_modernity, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3b607747-190a-4767-a65d-a83eb0efdf15', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, aristocratic_establishment).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, honor_bound_gentlemen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The network of aristocratic peers, seconds, and social arbiters who enforce the code of honor through ostracism, gossip, and the choreography of challenge and duel. They set the terms of satisfaction and police compliance, with their own standing tied to the maintenance of the code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_community, agenda_setter,
    organized, generational, identity_locked, national).

% The aristocratic caste collectively, whose social boundary against bourgeois and lower classes is maintained by the honor code. They benefit from the exclusivity and masculine prestige the mechanism confers, even as individual members bear its physical costs.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocratic_establishment, beneficiary,
    powerful, generational, identity_locked, national).

% Individual aristocrats, officers, and gentlemen who are compelled to accept challenges or face social death within their caste. They bear the direct risks of death, injury, legal penalty, and financial ruin, with exit blocked by identity fusion with the honor group.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_bound_gentlemen, payer,
    powerful, biographical, identity_locked, national).

% Commercial and professional classes who promote contract law, calculable risk, and rational dispute resolution. They are excluded from the aristocratic honor conversation but their norms progressively delegitimize dueling as irrational and commercially disruptive.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_commercial_class, excluded,
    moderate, biographical, mobile, national).

% Servants, laborers, and women who are outside the honor group entirely, subject to aristocratic violence and social contempt without recourse, and absent from all normative deliberation about satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, lower_class_excluded, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a regulated, socially legitimate means for aristocratic males to resolve insults and maintain reputational standing without appealing to state courts or informal violence, thereby preserving elite solidarity and caste boundaries.
% TRANSFER_FUNCTION: Transfers the risk of death, injury, and property loss from the collective aristocratic status order to individual gentlemen who must bear the corporeal and financial costs of combat; also transfers authority over dispute resolution from public institutions to private peers.
% ABSENT_VOICES: Women, lower-class servants, and the commercial bourgeoisie are excluded from the honor community; they bear witness to the violence but have no voice in its norms. Anti-dueling religious and medical voices are marginalized within aristocratic deliberation.
% DISAPPEARANCE_RATIONALE: If the honor mechanism vanished, elite masculine identity and dispute resolution would reorganize around state courts, commercial negotiation, and professional reputation; the aristocratic boundary marker would collapse, and the social cost of insult would be monetized or litigated rather than corporeal.
% FOUNDING_PROBLEM: How to regulate violence among armed elites so that personal insult does not cascade into feud or anarchy, while preserving the exclusivity and masculine prestige of the aristocratic caste.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists, and state prosecutors outside the aristocratic beneficiary class attest that the problem of elite violence is now handled by state courts and commercial law; the absence of dueling in modern dispute resolution corroborates that the founding problem is solved by alternative institutions.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the mechanism compels individuals to risk death and property for collective status goods. Suppression (0.58) reflects the active force of social ostracism, which ratchets upward mid-lifecycle as the community resists erosion, then decays. Theater ratio (0.70 at interval end) captures the late-stage performative maintenance of a fading aristocratic ritual. Accessibility collapse (0.72) is high: once socialized into the honor group, alternatives to dueling are cognitively and socially closed. Resistance (0.45) is moderate, coming from state prosecution, religious objection, and bourgeois mockery. The measurement grid is shared across all three metrics to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (honor_community) experiences the constraint as necessary coordination preserving social order; the payer seat (honor_bound_gentlemen) experiences it as compelled extraction of bodily risk. The beneficiary seat (aristocratic_establishment) experiences diffuse status gain without individual cost. The engine computes divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic_establishment is declared beneficiary, deriving low directionality (subsidy side). Honor_bound_gentlemen is declared victim, deriving high directionality (target side). Honor_community, though agenda setter, is identity_locked and thus structurally fused to the constraint; its directionality sits near the beneficiary end because it enforces rather than pays. The bourgeois_commercial_class and lower_class_excluded are not in the beneficiary/victim arrays and are excluded from the directional derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâregulating elite violence without state feudâwas solved by the rise of state courts and commercial contract law. The constraint persists beyond its functional need, transitioning from active coordination to theatrical inertia. The R5 genealogy (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags this as a resolved mandatrophy, preventing misclassification as a live coordination device.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erosion_mechanism_causality,
    'Which of the four mechanismsâstate monopoly, bourgeois norms, insurance logic, or category-shiftâwas historically primary, or were they truly independent and co-equal?',
    'Comparative historical analysis isolating regions with varying timing of criminalization, commercialization, and insurance penetration.',
    'If one mechanism dominated, the constraint''s classification would shift toward the corresponding type (enforcement_mechanism, identity_coordination, etc.); if plural, the composite reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erosion_mechanism_causality, empirical, 'Uncertainty about the relative causal weight of the four erosion mechanisms.').

omega_variable(
    suppression_ambiguity_honor,
    'Was the constraint''s persistence driven primarily by internalized identity-lock among gentlemen or by the active social suppression of alternatives?',
    'Analysis of diaries and correspondence for evidence of internal compulsion versus external ostracism; measurement of refusal rates and social outcomes.',
    'If internalized, effective extraction is higher than structural suppression suggests; if external, the constraint operates more as a pure enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_ambiguity_honor, empirical, 'Internalized vs structural suppression in aristocratic honor culture.').

omega_variable(
    category_shift_primacy,
    'Did dueling become cognitively unthinkable because material pressures removed its social base, or did the category-shift independently foreclose the practice?',
    'Linguistic and cultural history tracing when ''barbaric'' language predated versus followed legal and economic pressures.',
    'Resolving this distinguishes the composite reading from the contraction reading; if category-shift was autonomous, contraction gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_shift_primacy, conceptual, 'Whether category-shift was autonomous or derivative in the erosion of honor satisfaction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_composite_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(honor_composite_tr_t10, honor_satisfaction_mechanism__composite_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(honor_composite_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(honor_composite_tr_t30, honor_satisfaction_mechanism__composite_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(honor_composite_tr_t40, honor_satisfaction_mechanism__composite_reading, theater_ratio, 40, 0.54).
narrative_ontology:measurement(honor_composite_tr_t50, honor_satisfaction_mechanism__composite_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(honor_composite_tr_t60, honor_satisfaction_mechanism__composite_reading, theater_ratio, 60, 0.7).

% Extraction over time
narrative_ontology:measurement(honor_composite_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(honor_composite_be_t10, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(honor_composite_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(honor_composite_be_t30, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(honor_composite_be_t40, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(honor_composite_be_t50, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(honor_composite_be_t60, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 60, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(honor_composite_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(honor_composite_su_t10, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(honor_composite_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(honor_composite_su_t30, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(honor_composite_su_t40, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(honor_composite_su_t50, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(honor_composite_su_t60, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
