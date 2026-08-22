% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Override)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 to the Geneva Conventions was drafted to guarantee a
 *   non-derogable minimum of humane treatment to all persons in the power of
 *   a party to a conflict, applicable 'in all circumstances.' The
 *   contextual-necessity reading, operationalized through post-9/11 executive
 *   legal memoranda and interrogation program authorizations, treats that
 *   floor as a rebuttable baseline: when national security agencies determine
 *   that a detainee's intelligence value and the imminence of threat justify
 *   it, the 'baseline' can be superseded by enhanced techniques the agencies
 *   themselves classify as still humane, or as necessary despite not being
 *   humane. The coordination story — enabling fast, secretive action against
 *   catastrophic threats — is real from the agencies' seat, but it rides on a
 *   mechanism (self-defined necessity, self-defined humaneness) that
 *   structurally concentrates discretion in the same body that benefits from
 *   exercising it, while the detainees it is exercised upon have no voice in
 *   the determination and, frequently, no timely access to any body that
 *   could review it.
 *
 * KEY AGENTS:
 *   - national_security_agencies: agenda_setter, defines and applies the necessity override (institutional/arbitrage)
 *   - executive_branch_authorizers: beneficiary/agenda_setter, approves necessity findings, gains political and intelligence benefit (institutional/arbitrage)
 *   - high_value_detainees: payer, subjected to enhanced techniques with no voice in the determination (powerless/trapped)
 *   - black_site_detainees: payer, held extraterritorially specifically to avoid domestic legal reach (powerless/trapped)
 *   - human_rights_monitors: excluded, denied access and treated as advisory only (organized/constrained)
 *   - domestic_courts: excluded, kept out of the loop via classification and extraterritorial siting (institutional/constrained)
 *   - treaty_body_observers: observer, documents but cannot compel compliance (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.81).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.87).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.81).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 Contextual-Necessity Reading (Enhanced Interrogation Override)").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21').
narrative_ontology:cs_kernel_codification('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', fixed_text).
narrative_ontology:cs_authority_grounding('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', extraction).
narrative_ontology:cs_interpretation_layer_present('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21').
narrative_ontology:cs_reading_relation('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', foundational, security_necessity_can_override_treatment_floor).
narrative_ontology:cs_axiom_status(security_necessity_can_override_treatment_floor, holdable).
narrative_ontology:cs_axiom_grounding('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', security_necessity_can_override_treatment_floor, instrumental).
narrative_ontology:cs_axiom('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', secondary, authorizing_agency_is_competent_definer_of_humane).
narrative_ontology:cs_axiom_status(authorizing_agency_is_competent_definer_of_humane, holdable).
narrative_ontology:cs_axiom_grounding('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', authorizing_agency_is_competent_definer_of_humane, conventional).
narrative_ontology:cs_reference_frame('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', common_article_3_baseline_floor).
narrative_ontology:cs_drift_state('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', post_9_11_interrogation_program_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e5c8bdf1-3bff-4d8e-9d1f-0d2e35270f21', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, executive_branch_authorizers).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, black_site_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, field_interrogators).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, field_interrogators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as 'humane' treatment in practice by classifying specific interrogation techniques as necessity-justified rather than prohibited. Authors and applies internal legal memoranda that reinterpret Common Article 3's baseline. Operates detention and interrogation programs largely outside routine judicial or public oversight, citing classification and operational security.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Approves the necessity determinations that unlock enhanced interrogation, gaining intelligence product and political cover ('we did everything necessary') while bearing little of the direct legal or physical exposure. Can invoke state secrets doctrine to shield the program from most forms of external review.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, executive_branch_authorizers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, executive_branch_authorizers, agenda_setter).

% Held in secret or quasi-secret custody, classified as sufficiently threatening that ordinary Common Article 3 baseline is treated as insufficient. Subjected to techniques (stress positions, sleep deprivation, waterboarding-adjacent methods) authorized under the necessity override. Has no meaningful legal representation, no habeas access in real time, and no capacity to contest the necessity determination that governs their treatment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, biographical, trapped, global).

% Detained in facilities outside the custodial state's own territory specifically to place them further from domestic legal reach. Treatment is calibrated against a shifting, agency-defined 'humane' floor rather than a fixed external standard. Physical removal from any jurisdiction with functioning courts is itself part of how the override is operationalized.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, black_site_detainees, payer,
    powerless, biographical, trapped, regional).

% Carry out techniques authorized under the necessity determination, relying on legal-opinion cover from above. Benefit from the discretion in the sense that it authorizes their actions, but bear personal legal and psychological exposure if the necessity finding is later repudiated or prosecuted retroactively; their own exit from the assignment is constrained by chain of command and classification.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, field_interrogators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__contextual_necessity, field_interrogators, beneficiary).

% International and domestic human rights organizations (ICRC, treaty bodies, NGOs) argue the necessity override is a legal fiction that guts Common Article 3's non-derogable character. They are denied access to black sites, denied timely notification of detention, and their legal arguments are treated as advisory rather than binding within the security apparatus's own decision process.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_monitors, excluded,
    organized, generational, constrained, global).

% Would ordinarily adjudicate the legality of detention and treatment, but are frequently kept out of the loop through classification, extraterritorial detention siting, and state secrets assertions until years after the fact, if ever. Post-hoc rulings (when they occur) rarely reach detainees still held under the program.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, domestic_courts, excluded,
    institutional, generational, constrained, national).

% UN and ICRC legal bodies track state practice and issue interpretive commentary on whether necessity-based derogation from Common Article 3 is compatible with its non-derogable text. They document but cannot compel compliance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, treaty_body_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, national_security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides security agencies and their political principals with a legally cognizable mechanism to authorize interrogation practices they judge necessary to prevent catastrophic harm, without requiring case-by-case public deliberation that might compromise operational secrecy or produce paralysis in fast-moving threat scenarios.
% TRANSFER_FUNCTION: Moves protection away from detainees classified as high-value or security-relevant and toward the agencies and executive officials who gain intelligence product, political insulation, and procedural discretion — the same Common Article 3 text that would otherwise fix a floor of treatment is redirected into a variable the security apparatus itself sets.
% ABSENT_VOICES: Human rights monitors, treaty bodies, and the detainees themselves are structurally excluded from the necessity determination process itself — they can criticize after the fact, sometimes years later, but have no seat in the room where 'humane' is redefined for a given case. Domestic courts are frequently excluded by classification and extraterritorial siting until the practical stakes have passed.
% DISAPPEARANCE_RATIONALE: If the necessity-override reading of Common Article 3 disappeared and only the absolute-prohibition reading governed, enhanced interrogation programs would become per se violations rather than context-dependent judgment calls; agencies would lose their primary legal cover, detention siting strategies built around jurisdictional avoidance would lose their point, and prosecutions or reparations claims for past conduct would gain a much firmer textual foundation.
% FOUNDING_PROBLEM: Common Article 3's drafters sought a minimum floor of humane treatment applicable in all armed conflicts, including non-international ones, precisely because states had historically claimed exceptional circumstances justified departing from any treatment standard. The 'contextual necessity' reading reintroduces, through interpretive discretion, the exact exception the non-derogable floor was designed to foreclose.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and executive legal counsel attest that genuine catastrophic-threat scenarios exist that the drafters could not have anticipated and that some interpretive flexibility is operationally necessary; this attestation comes from within the benefiting institutions themselves. Outside corroboration is largely negative: the ICRC's own commentary, UN Committee Against Torture findings, and post-hoc judicial rulings in several jurisdictions (including reviews of extraordinary rendition programs) have found the necessity-override practice inconsistent with Common Article 3's text and object; no independent international legal body has affirmed the override as consistent with the treaty's non-derogable character.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the reading transfers the protective content of Common Article 3 away from the detainees it was meant to protect and toward the discretion of the agencies applying it — the constraint's nominal beneficiary (detainees under the treaty's plain text) is displaced by an actual beneficiary (the agencies exercising the override). Suppression is authored even higher (0.87) because the mechanism depends on active exclusion: classification regimes, extraterritorial detention siting, state secrets doctrine, and denial of monitor access are all load-bearing, not incidental, to how the override functions. Theater ratio is authored at a substantial 0.58 because a meaningful share of the apparatus (legal memoranda citing careful case-by-case necessity analysis, internal review boards) performs rigor without the reviewability that would make it real oversight — the 'careful balancing' language recurs even where the actual decision loop excludes anyone who could contest it.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and executive authorizers sit near the full-beneficiary end: they set the terms of 'necessity,' capture the intelligence and political product, and hold arbitrage-grade exit (they can reclassify, declassify, or reframe the program as circumstances require). High-value and black-site detainees sit at the full-target end: trapped, powerless, and structurally denied any procedural foothold to contest the determination that governs their treatment. Field interrogators occupy an intermediate position — authorized beneficiaries of the discretion in the moment, but exposed as payers if the necessity finding is later repudiated, which is why they are marked dual-role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (states historically claiming exceptional circumstances to justify departing from any treatment floor) is exactly the problem Common Article 3's non-derogable language was built to close off. The contextual-necessity reading's central move is to reopen that exception through interpretive discretion rather than textual amendment — which is why founding_problem_status is authored as contested rather than dead: the security agencies maintain the underlying threat problem is still live and justifies the override, while outside corroborators (ICRC commentary, UN Committee Against Torture findings, several post-hoc judicial reviews) read the override itself as a recurrence of the drafters' original target, not a response to a genuinely new problem. Classifying this reading as tangled_rope rather than snare preserves the fact that a real coordination problem exists (states do face fast-moving intelligence scenarios) while still registering that the mechanism, as actually operated, imposes concentrated, asymmetric costs on a powerless, trapped population — collapsing it to pure snare would erase the genuine (if contested) operational logic the agencies invoke; certifying it as rope would erase the victims entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_reviewability,
    'Is the agencies'' necessity determination genuinely subject to any external, binding review, or is ''review'' entirely internal to the same institutions that benefit from a favorable finding?',
    'Audit of actual case outcomes: how many necessity determinations have been overturned or meaningfully constrained by a body outside the authorizing chain (courts, treaty bodies, legislative oversight with teeth) versus how many were self-affirmed and never independently tested.',
    'If review is functionally non-existent, the coordination story is largely cover for unilateral discretion, pushing the classification toward snare. If genuine external review exists and periodically constrains the agencies, the tangled_rope characterization (real coordination function plus real, checked extraction) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_determination_reviewability, empirical, 'Whether necessity determinations face any binding external check.').

omega_variable(
    kernel_reading_which_governs,
    'Which of the three readings of Common Article 3 (absolute_prohibition, contextual_necessity, proportionality_balancing) actually governs state practice, and does that vary by state, by era, or by detainee classification?',
    'Comparative survey of state interrogation policy and judicial treatment across multiple jurisdictions and time periods; identify whether states that formally endorse absolute_prohibition nonetheless operate contextual_necessity in practice.',
    'If contextual_necessity is the operative reading in practice regardless of a state''s formal treaty position, this constraint''s extraction and suppression figures likely understate its real-world reach; if proportionality_balancing has displaced it in most jurisdictions post-2010s reform, this reading''s continued extraction may be historically bounded rather than current.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_which_governs, conceptual, 'Which kernel reading actually governs observed state practice, and where the boundaries lie.').

omega_variable(
    catastrophic_threat_frequency,
    'How often do the catastrophic, imminent-threat scenarios that are used to justify the necessity override actually occur, versus how often the override is invoked for cases that do not meet that threshold?',
    'Declassified case review comparing the stated threat justification at time of authorization against the actual intelligence value later confirmed to have existed.',
    'A low hit rate (most invocations did not involve genuine catastrophic imminent threats) would support reading the necessity framing as pretextual cover rather than a narrowly-tailored emergency valve, strengthening the case for reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_threat_frequency, empirical, 'Whether the necessity trigger condition is met in practice or invoked more broadly than its own justification allows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t4, humane_treatment_standard__contextual_necessity, theater_ratio, 4, 0.38).
narrative_ontology:measurement_basis(huma_tr_t4, observed).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__contextual_necessity, theater_ratio, 8, 0.47).
narrative_ontology:measurement_basis(huma_tr_t8, observed).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__contextual_necessity, theater_ratio, 12, 0.55).
narrative_ontology:measurement_basis(huma_tr_t12, observed).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__contextual_necessity, theater_ratio, 16, 0.6).
narrative_ontology:measurement_basis(huma_tr_t16, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__contextual_necessity, theater_ratio, 24, 0.58).
narrative_ontology:measurement_basis(huma_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t4, humane_treatment_standard__contextual_necessity, base_extractiveness, 4, 0.68).
narrative_ontology:measurement_basis(huma_be_t4, observed).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__contextual_necessity, base_extractiveness, 8, 0.79).
narrative_ontology:measurement_basis(huma_be_t8, observed).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__contextual_necessity, base_extractiveness, 12, 0.84).
narrative_ontology:measurement_basis(huma_be_t12, observed).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__contextual_necessity, base_extractiveness, 16, 0.82).
narrative_ontology:measurement_basis(huma_be_t16, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__contextual_necessity, base_extractiveness, 24, 0.81).
narrative_ontology:measurement_basis(huma_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t4, humane_treatment_standard__contextual_necessity, suppression_requirement, 4, 0.71).
narrative_ontology:measurement_basis(huma_su_t4, observed).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__contextual_necessity, suppression_requirement, 8, 0.8).
narrative_ontology:measurement_basis(huma_su_t8, observed).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__contextual_necessity, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(huma_su_t12, observed).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__contextual_necessity, suppression_requirement, 16, 0.9).
narrative_ontology:measurement_basis(huma_su_t16, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.85).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__contextual_necessity, suppression_requirement, 24, 0.87).
narrative_ontology:measurement_basis(huma_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.08).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the humane_treatment_standard kernel (Common Article 3's baseline). absolute_prohibition treats the floor as non-derogable in all circumstances (near-zero extraction, victim set essentially empty because no override exists to redirect protection away from anyone). proportionality_balancing sits between the two, authorizing a structured, externally reviewable weighing rather than either a fixed floor or agency-defined discretion. This reading (contextual_necessity) authors substantially higher extraction and suppression than either sibling because it locates the discretion to redefine 'humane' inside the very institution that benefits from exercising that discretion, with the weakest external reviewability of the three. Each reading is a structurally distinct constraint with its own stable epsilon; they are not three measurements of one constraint but three different arrangements the same treaty text is read to establish.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
