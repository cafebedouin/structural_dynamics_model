% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 Contextual Necessity Override
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions is read here as establishing a
 *   baseline of humane treatment that yields when national security
 *   imperatives are asserted. The reading transfers definitional authority
 *   over 'humane' to state security agencies, making detainee protections
 *   contingent on executive necessity determinations. It presents itself as
 *   coordinating state compliance with humanitarian law while functionally
 *   providing legal infrastructure for enhanced interrogation. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope (hybrid coordination/extraction) while the metrics describe
 *   a heavily extractive, actively enforced structure.
 *
 * KEY AGENTS:
 *   - security_agencies (institutional/analytical): Primary agenda-setters who determine when necessity overrides humane treatment and conduct enhanced interrogation.
 *   - state_executive (institutional/analytical): Political beneficiary of expanded discretion; delegates operational authority while retaining legal cover.
 *   - detainees (powerless/trapped): Primary targets whose protections become conditional on security classification.
 *   - high_value_targets (powerless/trapped): Subset of detainees explicitly designated for exclusion from full protections.
 *   - international_human_rights_courts (institutional/analytical): Observers lacking enforcement power to compel absolute compliance.
 *   - human_rights_organizations (organized/constrained): Excluded voices documenting abuse but absent from necessity determinations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.72).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.7).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.72).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 Contextual Necessity Override").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '2adb4eff-76cd-4239-815b-b1a0b173db07').
narrative_ontology:cs_kernel_codification('2adb4eff-76cd-4239-815b-b1a0b173db07', fixed_text).
narrative_ontology:cs_authority_grounding('2adb4eff-76cd-4239-815b-b1a0b173db07', extraction).
narrative_ontology:cs_interpretation_layer_present('2adb4eff-76cd-4239-815b-b1a0b173db07').
narrative_ontology:cs_reading_relation('2adb4eff-76cd-4239-815b-b1a0b173db07', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('2adb4eff-76cd-4239-815b-b1a0b173db07', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('2adb4eff-76cd-4239-815b-b1a0b173db07', foundational, necessity_overrides_humanity).
narrative_ontology:cs_axiom_status(necessity_overrides_humanity, holdable).
narrative_ontology:cs_axiom_grounding('2adb4eff-76cd-4239-815b-b1a0b173db07', necessity_overrides_humanity, instrumental).
narrative_ontology:cs_axiom('2adb4eff-76cd-4239-815b-b1a0b173db07', foundational, executive_discretion_on_humane_definition).
narrative_ontology:cs_axiom_status(executive_discretion_on_humane_definition, holdable).
narrative_ontology:cs_axiom_grounding('2adb4eff-76cd-4239-815b-b1a0b173db07', executive_discretion_on_humane_definition, conventional).
narrative_ontology:cs_reference_frame('2adb4eff-76cd-4239-815b-b1a0b173db07', executive_discretion_framework).
narrative_ontology:cs_drift_state('2adb4eff-76cd-4239-815b-b1a0b173db07', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2adb4eff-76cd-4239-815b-b1a0b173db07', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_executive).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines when national security imperatives override Common Article 3; defines what constitutes humane treatment in detention and interrogation contexts; conducts enhanced interrogation under color of legal necessity; operates without external binding review of necessity determinations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from expanded executive discretion in security matters; delegates operational authority to security agencies while retaining political and legal cover; asserts necessity determinations as sovereign prerogative.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_executive, beneficiary,
    institutional, generational, analytical, national).

% Subject to detention and interrogation regimes where protections are contingent on executive necessity assessments; cannot appeal security classification or exit the coercive relationship; experience the override as conditional suspension of bodily security.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees, payer,
    powerless, immediate, trapped, local).

% Explicitly designated as warranting enhanced interrogation due to perceived intelligence value; categorically excluded from full humane treatment protections by the necessity override; entirely subject to interrogator discretion.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_targets, payer,
    powerless, immediate, trapped, local).

% Adjudicate claims of humane treatment violations and issue rulings against override readings; lack direct enforcement mechanisms to compel state compliance with absolute prohibition norms.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_human_rights_courts, observer,
    institutional, generational, analytical, global).

% Document and publicize detainee abuse; advocate for absolute prohibition reading; structurally excluded from national security necessity determinations and detainee classification processes.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a baseline of humane treatment for detainees in non-international armed conflicts, providing a shared reference point that coordinates state behavior and international monitoring.
% TRANSFER_FUNCTION: Transfers definitional authority over humane treatment from international legal consensus to national security agencies when executive necessity is asserted; transfers bodily security and dignity from detainees to state interrogation interests.
% ABSENT_VOICES: Detainees are physically present but juridically excluded from the necessity determination; human rights organizations are excluded from security classification processes and their evidence is treated as external to the security calculus.
% DISAPPEARANCE_RATIONALE: If the contextual necessity override vanished, security agencies would lose legal cover for enhanced interrogation, detainee protections would revert to an unconditional baseline, and executive discretion over treatment standards would contract sharply.
% FOUNDING_PROBLEM: The need to regulate state conduct in internal armed conflicts where full Geneva Convention protections do not formally apply, while preserving state capacity to respond to security threats posed by detainees.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross corroborates that baseline protections remain necessary in non-international armed conflicts. Independent human rights monitors corroborate that the override mechanism is contested, though no external party outside the benefiting state apparatus corroborates the national-security necessity claim as a live justification for suspension.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the override mechanism decouples protections from the detainee's status and couples them to the state's security assessment, transferring dignity and bodily security to the interrogator's interest. Suppression (0.70) is high because the constraint's persistence requires suppressing the absolute prohibition reading and isolating detainees from legal recourse. Theater_ratio (0.45) reflects significant performative maintenance: states continue to invoke 'humane treatment' and legal process while operational practice expands interrogation latitude. Accessibility_collapse (0.68) captures that once the contextual necessity reading is adopted, the absolute protection alternative collapses for detainees within that jurisdiction. Resistance (0.55) reflects sustained but institutionally weak pushback from human rights courts and NGOs.
 *
 * PERSPECTIVAL GAP:
 *   From the security agency seat, the constraint is necessary coordination: it preserves a baseline while allowing operational flexibility against existential threats. From the detainee seat, the same text operates as extraction: the 'baseline' is whatever the interrogator chooses to leave intact. The engine computes this divergence from the structural asymmetry in exit options (trapped vs. analytical) and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies and state executive are structural beneficiaries: the constraint subsidizes their operational discretion and legal cover (d near 0.0). Detainees and high_value_targets are structural targets: the constraint extracts their protections and substitutes contingent privileges subject to state determination (d near 1.0). International courts sit at analytical distance (d ~0.5) but with no enforcement exit; human_rights_organizations are excluded from the necessity frame (d ~0.6, paying costs of suppressed voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction components to be declared. A pure coordination reading (Rope) would ignore the victim set and the active enforcement of override; a pure extraction reading (Snare) would miss that Common Article 3 genuinely does coordinate some baseline expectations and restrain some state conduct. The Tangled Rope classification captures the hybrid: the baseline coordinates, but the override clause extracts asymmetrically and requires active enforcement to maintain against the absolute prohibition alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_integrity,
    'Who determines when a national security imperative exists, and is that determination subject to independent judicial review or entirely internal to the security apparatus?',
    'Comparative case study of states with independent judicial review of interrogation necessity claims versus purely executive self-certification; measurement of abuse rates and classification error.',
    'If necessity is entirely self-determined by security agencies, effective extraction is higher than the structural measure suggests and the constraint tilts toward snare; if externally reviewable, the extraction is partially checked and the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_determination_integrity, empirical, 'Whether necessity determinations are subject to independent review').

omega_variable(
    structural_extraction_vs_coordination,
    'Is Common Article 3 with a necessity override primarily coordinating international expectations by setting a tolerated baseline, or is it primarily extracting from detainees by providing legal cover for abuse?',
    'Quantitative analysis of state compliance: measure the gap between stated baseline protections and actual treatment outcomes across jurisdictions adopting this reading versus absolute prohibition jurisdictions.',
    'If the gap is small, the coordination function dominates and extractiveness is overstated; if the gap is large, the override functions as extraction infrastructure and the metric profile is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_extraction_vs_coordination, conceptual, 'Whether the override is coordination cost or extraction cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.11).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.12).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__contextual_necessity, theater_ratio, 30, 0.15).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__contextual_necessity, theater_ratio, 40, 0.25).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__contextual_necessity, theater_ratio, 50, 0.5).
narrative_ontology:measurement(huma_tr_t60, humane_treatment_standard__contextual_necessity, theater_ratio, 60, 0.48).
narrative_ontology:measurement(huma_tr_t75, humane_treatment_standard__contextual_necessity, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__contextual_necessity, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__contextual_necessity, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__contextual_necessity, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(huma_be_t60, humane_treatment_standard__contextual_necessity, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(huma_be_t75, humane_treatment_standard__contextual_necessity, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__contextual_necessity, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__contextual_necessity, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__contextual_necessity, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(huma_su_t60, humane_treatment_standard__contextual_necessity, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(huma_su_t75, humane_treatment_standard__contextual_necessity, suppression_requirement, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the humane_treatment_standard kernel, which decomposes into three structurally distinct claims: absolute_prohibition, proportionality_balancing, and contextual_necessity. The epsilon values, beneficiary structures, and victim sets differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
