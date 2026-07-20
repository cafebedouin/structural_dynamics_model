% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the harm_limited_reading of the
 *   speech_protection_boundary kernel: a constitutional arrangement in which
 *   speech is protected only insofar as it does not cause significant harm to
 *   dignity, equality, or freedom from harassment. The state acts as
 *   gatekeeper, interpreting and enforcing the boundary. The protected set is
 *   narrowed relative to the absolutist reading; the unprotected set includes
 *   hate speech, harassment, and coded dog whistles. This reading coexists
 *   with sibling readings (absolutist, balancing) in global constitutional
 *   discourse. The constraint carries genuine coordination function â
 *   sheltering marginalized groups from targeted degradation â but also
 *   asymmetric extraction: speakers lose expressive freedom, and the state
 *   accumulates gatekeeping power with attendant abuse risk.
 *
 * KEY AGENTS:
 *   - state_gatekeeper (institutional/constrained): Defines and enforces the harm boundary; accumulates gatekeeping authority.
 *   - protected_communities (organized/constrained): Receive shelter from targeted hate and harassment.
 *   - restricted_speakers (moderate/constrained): Bear expressive costs and legal punishment.
 *   - targets_of_gatekeeper_overreach (powerless/trapped): Pay disproportionate costs under abusive or expansive application.
 *   - equality_advocates (organized/mobile): Benefit from legal validation of dignity claims.
 *   - civil_liberties_institutions (institutional/analytical): Observe and resist overreach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.66).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.8).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, 'd55789a9-5736-46d2-8917-8a7202040370').
narrative_ontology:cs_kernel_codification('d55789a9-5736-46d2-8917-8a7202040370', formalized).
narrative_ontology:cs_authority_grounding('d55789a9-5736-46d2-8917-8a7202040370', lineage).
narrative_ontology:cs_interpretation_layer_present('d55789a9-5736-46d2-8917-8a7202040370').
narrative_ontology:cs_reading_relation('d55789a9-5736-46d2-8917-8a7202040370', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d55789a9-5736-46d2-8917-8a7202040370', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('d55789a9-5736-46d2-8917-8a7202040370', foundational, dignity_conditioned_speech_protection).
narrative_ontology:cs_axiom_status(dignity_conditioned_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('d55789a9-5736-46d2-8917-8a7202040370', dignity_conditioned_speech_protection, deontological).
narrative_ontology:cs_axiom('d55789a9-5736-46d2-8917-8a7202040370', foundational, state_may_enforce_harm_boundaries).
narrative_ontology:cs_axiom_status(state_may_enforce_harm_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('d55789a9-5736-46d2-8917-8a7202040370', state_may_enforce_harm_boundaries, conventional).
narrative_ontology:cs_reference_frame('d55789a9-5736-46d2-8917-8a7202040370', dignity_equality_framework).
narrative_ontology:cs_drift_state('d55789a9-5736-46d2-8917-8a7202040370', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d55789a9-5736-46d2-8917-8a7202040370', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, protected_communities).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, equality_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, restricted_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, targets_of_gatekeeper_overreach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces the boundary between protected and unprotected speech through statutes, administrative rules, and judicial review. Holds the monopoly on legitimate coercion to sanction prohibited expression. Exit is constrained by constitutional structure and political accountability, but the state cannot easily relinquish its gatekeeping role without institutional self-dissolution.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_gatekeeper, agenda_setter,
    institutional, generational, constrained, national).

% Receive legal shelter from hate speech, harassment, and dignity-degrading expression. Depend on state enforcement apparatus to deliver protection. Exit is constrained because abandoning the legal framework leaves them exposed to social hostility without statutory remedy or alternative institutional shield.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, protected_communities, beneficiary,
    organized, biographical, constrained, national).

% Organizations and movements advancing equality agendas that benefit from legal validation that dignity and equality can override certain expressive acts. Participate in shaping the harm boundary through lobbying and litigation. Mobility is limited by political identity but they can shift jurisdictions or issue areas.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, equality_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of expressive restraint: their speech is prohibited, punished, or chilled when categorized as harmful to dignity or equality. Face fines, imprisonment, or platform removal. Exit options are constrained by territorial reach of the legal regime and identity ties to the political community.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, restricted_speakers, payer,
    moderate, biographical, constrained, national).

% Dissidents, minority faith speakers, or boundary-test speakers wrongly or expansively categorized under the harm limitation. Bear disproportionate costs when the state abuses gatekeeper discretion. Often lack resources to mount legal defense and are trapped by citizenship, residence, or poverty.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, targets_of_gatekeeper_overreach, payer,
    powerless, immediate, trapped, national).

% Monitor and litigate against overreach, arguing that the harm limitation erodes the democratic function of free expression. Do not collect from the constraint but analyze its structural effects from an adversarial vantage, supplying resistance data to the public record.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_institutions, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public sphere in which marginalized groups can participate without facing systematic degradation of their dignity, equality, and freedom from harassment by prohibiting expression that inflicts those harms.
% TRANSFER_FUNCTION: Moves the cost of expressive silence and legal punishment from speakers whose expression is categorized as harmful to the protected communities who gain freedom from targeted hostility, while concentrating interpretive authority over the boundary in the state.
% ABSENT_VOICES: Absolutist free speech advocates and illiberal dissenters are structurally excluded from the harm-limitation framework's legitimating discourse; their objections are treated as complicity with oppression rather than as competing constitutional commitments worthy of accommodation.
% DISAPPEARANCE_RATIONALE: If the harm-limitation boundary vanished overnight, previously prohibited speech would enter public discourse, protected communities would lose statutory shields against targeted degradation, and the state's authority to arbitrate expressive legitimacy would collapse. The architecture of public discourse, minority protection, and state-citizen relations would reorganize around a different equilibrium.
% FOUNDING_PROBLEM: Historical and ongoing patterns of hate speech, group defamation, and discriminatory harassment that systematically degrade the equal standing and dignity of marginalized groups in democratic society.
% FOUNDING_PROBLEM_CORROBORATION: International human rights institutions and domestic equality commissions attest the problem remains live from seats partly independent of the immediate beneficiary set. Civil liberties organizations and dissident movements attest the problem is either overstated or that the arrangement produces independent harms. Corroboration is split and contested; no non-beneficiary consensus exists.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is substantial because the constraint systematically strips expressive rights from a defined class of speakers and transfers interpretive authority to the state. Suppression (0.80) is high because the constraint depends on active legal punishment and chilling effect to persist; alternatives such as absolutist regimes are structurally suppressed in the jurisdictional space. Theater_ratio (0.48) is moderate-to-high: some enforcement is substantive, but a growing share performs equality commitment without proportional harm reduction. Accessibility_collapse (0.58) reflects that once the harm-limitation framework is accepted, alternative speech regimes are legally crowded out. Resistance (0.72) is high due to sustained opposition from civil liberties institutions and dissident speakers. The metrics and claimed_type are independently authored: the structural claim is tangled_rope, while the metrics describe an operationally extractive, actively enforced constraint.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (protected communities, equality advocates) experience the constraint as necessary coordination that secures their democratic participation; the payer seats (restricted speakers, targets of overreach) experience it as extraction backed by state coercion. The agenda-setter seat (state gatekeeper) experiences it as a legitimate regulatory function. The engine computes divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected communities and equality advocates are structural beneficiaries (low d): the constraint subsidizes their security and standing. Restricted speakers and targets of gatekeeper overreach are structural targets (high d): the constraint extracts expressive freedom and imposes legal costs. The state gatekeeper sits near symmetric but leans beneficiary: it gains authority and institutional purpose from the arrangement, though it also bears enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy mislabeling because it carries a live coordination function (protecting dignity) that is structurally inseparable from its extractive mechanism (state gatekeeping). It is not a pure snare because the harm to protected communities is real and the constraint does coordinate a genuine public good; it is not a pure rope because the costs are borne by a distinct, asymmetrically positioned set and active enforcement is required. The tangled_rope classification captures this hybridity without collapsing it into either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_limited_reading_location,
    'This constraint is the harm_limited_reading of the speech_protection_boundary kernel; how would adopting the absolutist_reading or balancing_reading alter the beneficiary-victim structure and the state''s extractive role?',
    'Comparative structural analysis of the sibling constraint stories in this kernel family.',
    'Adopting the absolutist reading would eliminate the state gatekeeper and the victim set, collapsing epsilon toward rope or mountain; adopting the balancing reading would likely raise indeterminacy and theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_limited_reading_location, conceptual, 'Kernel reading location and sibling structural deltas.').

omega_variable(
    gatekeeper_discretion_abuse,
    'Does the state''s discretionary power to define ''significant harm to dignity'' systematically drift toward suppressing dissenting or minority viewpoints?',
    'Empirical audit of prosecutorial and judicial decisions: measure partisan asymmetry, viewpoint targeting, and chilling effect surveys across jurisdictions with harm-limitation regimes.',
    'Systematic drift would push classification toward snare; bounded, rule-of-law-constrained application supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_discretion_abuse, empirical, 'Whether state gatekeeping power drifts into viewpoint suppression.').

omega_variable(
    harm_definition_vagueness,
    'Does the indeterminacy of ''dignity,'' ''equality,'' and ''harassment'' in the legal standard function as necessary flexibility or as an arbitrariness engine?',
    'Comparative legal certainty metrics and predictability of judicial outcomes across jurisdictions.',
    'High indeterminacy without bounded interpretation amplifies effective extraction because speakers cannot reliably know the boundary, inflating self-censorship beyond the statutory text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_vagueness, conceptual, 'Whether legal vagueness is a feature or a bug of the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_boundary kernel. The kernel decomposes into three structurally distinct constraints: absolutist (near-absolute protection), balancing (case-by-case weighing), and harm-limited (categorical dignity condition). Each reading produces a different epsilon, beneficiary/victim structure, and state role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
