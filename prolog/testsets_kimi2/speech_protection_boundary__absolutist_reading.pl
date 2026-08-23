% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist First Amendment Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the absolutist reading of the First
 *   Amendment speech-protection boundary, instantiated in the Brandenburg v.
 *   Ohio imminent-lawless-action standard. Under this reading, speech is
 *   near-absolute protected and the only unprotected category is direct
 *   incitement to imminent violence. The constraint coordinates judicial
 *   review and speaker behavior around a bright line, but externalizes the
 *   costs of racist, sexist, and harassing speech onto minoritized
 *   communities who are denied legal redress. It is one reading of the
 *   speech_protection_boundary kernel; the sibling readings
 *   (balancing_reading, harm_limited_reading) are structurally foreclosed
 *   within this framework.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary (institutional/analytical): Administers and enforces the Brandenburg bright-line test through judicial review.
 *   - speakers_and_press (organized/mobile): Benefit from near-absolute immunity for offensive and harassing speech.
 *   - minoritized_communities (powerless/identity_locked): Bear the dignitary and equality harms that the constraint leaves legally unredressed.
 *   - state_legislatures (institutional/constrained): Attempt to pass broader speech regulations that are structurally excluded by the standard.
 *   - critical_legal_scholars (moderate/analytical): Observe and critique the distributional asymmetry of the absolutist framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.7).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist First Amendment Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, 'abee0dca-eda7-4aa5-9e02-324c53feef7b').
narrative_ontology:cs_kernel_codification('abee0dca-eda7-4aa5-9e02-324c53feef7b', fixed_text).
narrative_ontology:cs_authority_grounding('abee0dca-eda7-4aa5-9e02-324c53feef7b', lineage).
narrative_ontology:cs_interpretation_layer_present('abee0dca-eda7-4aa5-9e02-324c53feef7b').
narrative_ontology:cs_reading_relation('abee0dca-eda7-4aa5-9e02-324c53feef7b', speech_protection_boundary__balancing_reading, forecloses).
narrative_ontology:cs_reading_relation('abee0dca-eda7-4aa5-9e02-324c53feef7b', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_axiom('abee0dca-eda7-4aa5-9e02-324c53feef7b', foundational, speech_immunity_trumps_dignitary_harm).
narrative_ontology:cs_axiom_status(speech_immunity_trumps_dignitary_harm, holdable).
narrative_ontology:cs_axiom_grounding('abee0dca-eda7-4aa5-9e02-324c53feef7b', speech_immunity_trumps_dignitary_harm, deontological).
narrative_ontology:cs_reference_frame('abee0dca-eda7-4aa5-9e02-324c53feef7b', classical_liberal_public_sphere).
narrative_ontology:cs_drift_state('abee0dca-eda7-4aa5-9e02-324c53feef7b', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abee0dca-eda7-4aa5-9e02-324c53feef7b', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers_and_press).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, brandenburg_imminent_incitement_test).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the First Amendment boundary, applying the Brandenburg imminent-lawless-action test to strike down content-based speech regulations and maintain the near-absolute protection framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from near-absolute protection against government censorship; can express offensive, harassing, or discriminatory views without legal penalty so long as the speech does not incite imminent violence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers_and_press, beneficiary,
    organized, biographical, mobile, national).

% Bear the aggregate dignitary, equality, and psychological harms of speech that targets them but falls short of imminent incitement; lack legal recourse because the harm exception is narrowly limited to imminent lawless action.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, identity_locked, national).

% Attempt to pass hate-speech, group-libel, or hostile-environment statutes to protect minoritized communities, but these efforts are structurally excluded from enactment by judicial review applying the Brandenburg standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, state_legislatures, excluded,
    institutional, generational, constrained, national).

% Document and critique the distributional effects of the absolutist standard, arguing that the constraint externalizes harms disproportionately onto historically marginalized groups and obscures power asymmetries in the speech market.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, critical_legal_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, speakers_and_press).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line judicial standard for distinguishing protected from unprotected speech, coordinating legislatures, law enforcement, courts, and speakers around a predictable boundary: only direct incitement to imminent lawless action is punishable.
% TRANSFER_FUNCTION: Transfers the costs of harmful but non-imminent speech â including discriminatory harassment, group defamation, and dignitary injury â from speakers and media institutions onto minoritized communities by legally immunizing the speech.
% ABSENT_VOICES: Minoritized communities targeted by protected hate speech and state legislatures seeking to pass group-libel or hostile-environment statutes are present in litigation but structurally lose; their preferred legal frameworks are excluded from adoption by the doctrinal boundary.
% DISAPPEARANCE_RATIONALE: If the absolutist Brandenburg standard vanished overnight, legislatures would enact broader hate-speech and harassment laws, speakers would face expanded civil and criminal liability, and judicial dockets would shift from striking down speech restrictions to adjudicating harm-based balancing â First Amendment doctrine would reorganize around a different core boundary.
% FOUNDING_PROBLEM: Government suppression of dissident political speech and the chilling of legitimate opposition through vague criminal syndicalism, sedition, and anti-subversive laws.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and critical legal scholars outside the absolutist tradition attest that the specific threat of government sedition prosecutions against political dissidents, which motivated mid-twentieth-century speech expansion, has been replaced by different speech-regulation problems; absolutist scholars and First Amendment bar insiders assert the threat remains live.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial because the constraint systematically transfers dignitary and equality harms from speakers to minoritized communities. Suppression (0.70) is high because the constraint actively suppresses alternative regulatory frameworks (hate-speech laws, group-libel statutes) through judicial enforcement. Theater ratio (0.48) is moderate-to-high: the original anti-censorship rationale is increasingly performed as courts strike down modern digital-harassment laws in contexts far from the founding problem of government sedition prosecutions. Accessibility collapse (0.75) reflects that, within US constitutional law, alternatives to the Brandenburg test are institutionally closed off. Resistance (0.55) captures sustained scholarly and legislative dissent.
 *
 * PERSPECTIVAL GAP:
 *   The speaker seat experiences the constraint as protective coordination (low d, negative chi), while the minoritized-community seat experiences it as extraction (high d, high chi). The judiciary seat sits near analytical, with low personal extraction but high agenda-setting power. State legislatures attempting regulation are excluded â their preferred policies are structurally barred.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (speakers_and_press) receive subsidy: their expressive costs are socialized onto targeted communities. Victims (minoritized_communities) bear targeted extraction with identity-locked exit. The directionality derivation therefore places speakers near the beneficiary end and minoritized communities near the full-target end. No override is needed because the structural derivation matches the relational facts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â government suppression of political dissidents through vague sedition laws â is largely dead. The constraint persists with a rising theater ratio as courts apply a mid-twentieth-century standard to twenty-first-century speech harms. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals mandatrophy: the arrangement would be recognized as a zombie if the genealogy were consumed, but the R5 consumer uses only the mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_externality_ambiguity,
    'Does the absolutist reading of the speech boundary generate unavoidable externalized harm to minoritized communities, or would an alternative reading simply shift the locus of harm to speakers and dissenters?',
    'Comparative constitutional analysis of jurisdictions with hate-speech or dignity-balancing frameworks; measurement of minority-community legal recourse and reported harassment outcomes under alternative standards.',
    'If harm is unavoidable, the classification turns on distribution; if avoidable, the absolutist reading is more extractive than structurally necessary and the externality is a contingent policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_externality_ambiguity, conceptual, 'Whether externality under absolutism is inevitable or contingent').

omega_variable(
    mandatrophy_obsolescence,
    'Has the founding problem of government sedition prosecutions been replaced by new speech harms, rendering the absolutist standard a zombie constraint?',
    'Historical analysis of the original sedition threat versus contemporary digital-harassment and disinformation patterns; judicial citation studies showing continued reliance on Brandenburg in contexts far from its origin.',
    'If the founding problem is dead, the constraint persists by inertia (piton pressure) and the coordination story is largely retrospective; if live, it remains a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_obsolescence, empirical, 'Whether the absolutist standard''s founding rationale remains live').

omega_variable(
    kernel_reading_relationship,
    'Does the absolutist reading foreclose the balancing and harm-limited readings within a single constitutional framework, or can they coexist as doctrinal alternatives?',
    'Engine computation from cs_axiom_contradiction; legal-doctrinal analysis of whether a single court could simultaneously apply the Brandenburg test and a harm-balancing test to the same speech act.',
    'If foreclosed, the readings are mutually exclusive and the kernel generates sharp seat contention; if coexistent, the kernel supports plural legal orders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between absolutist and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spee_tr_t9, speech_protection_boundary__absolutist_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement(spee_tr_t18, speech_protection_boundary__absolutist_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement(spee_tr_t27, speech_protection_boundary__absolutist_reading, theater_ratio, 27, 0.33).
narrative_ontology:measurement(spee_tr_t36, speech_protection_boundary__absolutist_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(spee_tr_t45, speech_protection_boundary__absolutist_reading, theater_ratio, 45, 0.44).
narrative_ontology:measurement(spee_tr_t55, speech_protection_boundary__absolutist_reading, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t9, speech_protection_boundary__absolutist_reading, base_extractiveness, 9, 0.36).
narrative_ontology:measurement(spee_be_t18, speech_protection_boundary__absolutist_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(spee_be_t27, speech_protection_boundary__absolutist_reading, base_extractiveness, 27, 0.48).
narrative_ontology:measurement(spee_be_t36, speech_protection_boundary__absolutist_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement(spee_be_t45, speech_protection_boundary__absolutist_reading, base_extractiveness, 45, 0.59).
narrative_ontology:measurement(spee_be_t55, speech_protection_boundary__absolutist_reading, base_extractiveness, 55, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_boundary__absolutist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_boundary kernel. The natural-language label 'speech protection boundary' conflates three structurally distinct legal constraints: an absolutist bright-line rule (this file), a harm-limited conditional rule, and a case-by-case balancing test. Each reading has different beneficiary/victim structures, different epsilon values, and different classification profiles. They are modeled as separate stories linked by network edges, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
