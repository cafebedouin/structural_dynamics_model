% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition on Torture and Degrading Treatment (Absolute Prohibition Reading)
 *   domain: international_law/security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes a
 *   non-derogable floor for the treatment of persons taking no active part in
 *   non-international armed conflicts. The absolute prohibition reading
 *   treats torture and degrading treatment as categorically forbidden
 *   regardless of circumstance, creating a hard constraint on state and
 *   non-state belligerents. This reading is contested by sibling readings
 *   that admit security exceptions or proportionality balancing. The
 *   constraint coordinates reciprocal protection while asymmetrically
 *   extracting operational discretion from detaining authorities.
 *
 * KEY AGENTS:
 *   - detained_persons_niac (powerless/trapped): Primary beneficiaries â receive absolute legal shield but cannot exit detention.
 *   - belligerent_parties (institutional/constrained): Dual-role agents â bear operational costs as payers while receiving reciprocal protection as beneficiaries.
 *   - international_judiciary (institutional/analytical): Agenda-setters â interpret and enforce the standard through criminal jurisprudence.
 *   - human_rights_monitoring_bodies (organized/mobile): Observers â monitor compliance and shape interpretive discourse.
 *   - security_hardliners (moderate/mobile): Excluded voices â argue for contextual necessity but are outside the dominant legal framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.65).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.72).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition on Torture and Degrading Treatment (Absolute Prohibition Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_law/security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '4595aae0-d1bc-46eb-a481-54b58bc8576f').
narrative_ontology:cs_kernel_codification('4595aae0-d1bc-46eb-a481-54b58bc8576f', fixed_text).
narrative_ontology:cs_authority_grounding('4595aae0-d1bc-46eb-a481-54b58bc8576f', lineage).
narrative_ontology:cs_interpretation_layer_present('4595aae0-d1bc-46eb-a481-54b58bc8576f').
narrative_ontology:cs_reading_relation('4595aae0-d1bc-46eb-a481-54b58bc8576f', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('4595aae0-d1bc-46eb-a481-54b58bc8576f', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('4595aae0-d1bc-46eb-a481-54b58bc8576f', foundational, torture_categorically_prohibited).
narrative_ontology:cs_axiom_status(torture_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('4595aae0-d1bc-46eb-a481-54b58bc8576f', torture_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('4595aae0-d1bc-46eb-a481-54b58bc8576f', foundational, detainee_non_derogable_rights_holder).
narrative_ontology:cs_axiom_status(detainee_non_derogable_rights_holder, holdable).
narrative_ontology:cs_axiom_grounding('4595aae0-d1bc-46eb-a481-54b58bc8576f', detainee_non_derogable_rights_holder, deontological).
narrative_ontology:cs_reference_frame('4595aae0-d1bc-46eb-a481-54b58bc8576f', absolute_humanitarian_minimum).
narrative_ontology:cs_drift_state('4595aae0-d1bc-46eb-a481-54b58bc8576f', war_on_terror_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4595aae0-d1bc-46eb-a481-54b58bc8576f', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detained_persons_niac).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, belligerent_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, belligerent_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in the power of a party to a non-international armed conflict, including captured combatants and civilians, who are protected by the absolute prohibition on torture and degrading treatment. They receive legal shielding but cannot voluntarily exit the protective relationship; physical exit depends on release or escape.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detained_persons_niac, beneficiary,
    powerless, immediate, trapped, global).

% States and organized armed groups party to non-international armed conflicts that are legally bound to refrain from torture and degrading treatment under all circumstances. They bear the operational cost of restricted interrogation methods. Simultaneously, they benefit reciprocally when their own captured personnel are detained by adversaries also bound by the same standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, belligerent_parties, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, belligerent_parties, beneficiary).

% International criminal courts and tribunals, including the ICTY, ICTR, and ICC, that interpret Common Article 3, adjudicate violations, and enforce individual criminal responsibility. They maintain the prohibition's absoluteness through jurisprudence and precedent.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_judiciary, agenda_setter,
    institutional, civilizational, analytical, global).

% International and non-governmental organizations, including the ICRC and UN special procedures, that monitor compliance with Common Article 3, document violations, and publish interpretive guidance. They shape the standard's reception without possessing direct enforcement authority.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_monitoring_bodies, observer,
    organized, generational, mobile, global).

% Intelligence and security actors within states who argue that absolute prohibition impedes effective interrogation and threat prevention. They are structurally excluded from the dominant legal interpretive framework that treats the prohibition as categorically non-derogable.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, security_hardliners, excluded,
    moderate, immediate, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates belligerents in non-international armed conflict around a minimum standard of humanity, preventing a race-to-the-bottom in detainee treatment and preserving reciprocal protection for captured personnel across all parties.
% TRANSFER_FUNCTION: Transfers the burden of restraint and the risk of legal accountability from detained persons to state and non-state parties to conflict, removing certain interrogation and coercion methods from the available toolkit regardless of operational urgency or security context.
% ABSENT_VOICES: Security hardliners and advocates of enhanced interrogation within state apparatuses would argue for contextual necessity and security exceptions; they are structurally excluded from the dominant legal interpretive framework that treats the prohibition as non-derogable.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, state and non-state parties would reintroduce coercive interrogation methods that are currently foreclosed, the baseline of reciprocal protection for captured personnel would collapse, and the broader architecture of non-derogable human rights norms would face severe fracture.
% FOUNDING_PROBLEM: The absence of legal protection for detainees in non-international armed conflicts produced unrestrained brutality, eroded the laws of war, and removed any reciprocal incentive for humane treatment by adversaries.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and international criminal tribunals attest to ongoing violations in contemporary non-international armed conflicts, confirming the problem remains live. State parties historically affirmed the need for minimum standards in the 1949 negotiating record. Security hardliners contest the absolute framing but do not contest that unrestrained brutality would follow total removal of the standard.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the prohibition removes interrogation methods from state tactical options regardless of context. Suppression (0.72) is high because the constraint's persistence depends on active legal enforcement (tribunals, universal jurisdiction) to prevent backsliding. Theater ratio (0.30) is moderate: some enforcement is genuine (prosecutions), but a portion of state compliance is performative (lip service, concealment of violations). Accessibility collapse (0.75) is high: once the standard is understood, torture as policy becomes legally inaccessible, though covert practice persists. Resistance (0.60) reflects ongoing state efforts to carve out exceptions, especially under security framing. The trajectory from 1949 to 2024 shows rising extractiveness and suppression as the legal infrastructure hardened and post-9/11 resistance intensified enforcement needs.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee seat, the constraint is a protective shield approaching a natural-law floor; effective extraction is damped to near-zero and the seat computes toward rope or mountain. From the belligerent-party seat, the constraint extracts heavily from operational sovereignty; directionality nears full-target, producing high effective extraction and a tangled_rope or snare classification. From the tribunal seat, the constraint is a necessary coordination mechanism with low personal extraction. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons are full beneficiaries (d near 0.0) because the constraint subsidizes their protection without cost. Belligerent parties are primary payers (d near 1.0) because the constraint extracts operational discretion; their secondary beneficiary status dampens directionality only slightly because the reciprocal benefit is contingent on adversary compliance rather than the constraint itself. The international judiciary sits at analytical distance (d near 0.5). No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope captures both the genuine coordination function (reciprocal protection, civilization minimum) and the asymmetric extraction from belligerents. A rope classification would be incorrect because alternatives (torture as policy) are actively suppressed and states bear real costs. A snare classification would ignore the coordination benefit that belligerent parties themselves receive reciprocally. A mountain classification would falsely naturalize a treaty-based legal construct. The temporal measurements show rising enforcement investment, confirming the constraint remains active rather than inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_ambiguity,
    'Is the suppression of torture structurally consistent across all state parties, or is enforcement selectively applied to weaker states and non-state actors?',
    'Comparative prosecution data across strong and weak states; analysis of ICC case selection patterns and universal jurisdiction invocation rates.',
    'If enforcement is selective, the constraint''s extraction falls disproportionately on weak actors, shifting classification toward snare for those seats while strong actors experience it as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_ambiguity, empirical, 'Whether enforcement is structurally consistent or selectively applied.').

omega_variable(
    customary_vs_treaty_source,
    'Does the absolute prohibition derive its force primarily from the positive treaty text of Common Article 3, or from a customary and peremptory status that would persist even if the treaty were denounced?',
    'International Court of Justice advisory opinion or widespread state practice analysis in a hypothetical post-denunciation scenario.',
    'If purely treaty-based, the constraint is more fragile and scaffold-like; if jus cogens, it approaches mountain-like persistence but remains a constructed norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_vs_treaty_source, conceptual, 'Whether the norm''s authority is treaty-positive or customary-peremptory.').

omega_variable(
    state_dual_role_coherence,
    'Does the belligerent party''s dual role as both payer and beneficiary mask a coordination benefit that outweighs extraction, or does it obscure extraction from weaker belligerents by stronger ones?',
    'Seat-resolved analysis of directionality by power level: do powerful states experience the constraint as coordination while weak states and non-state actors experience it as extraction?',
    'If directionality varies by power level within the same role, the constraint exhibits same-level lateral dynamics requiring decomposition into power-differentiated stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_dual_role_coherence, conceptual, 'Whether dual-role status conceals power-asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__absolute_prohibition, theater_ratio, 15, 0.12).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__absolute_prohibition, theater_ratio, 30, 0.15).
narrative_ontology:measurement(huma_tr_t45, humane_treatment_standard__absolute_prohibition, theater_ratio, 45, 0.2).
narrative_ontology:measurement(huma_tr_t60, humane_treatment_standard__absolute_prohibition, theater_ratio, 60, 0.27).
narrative_ontology:measurement(huma_tr_t75, humane_treatment_standard__absolute_prohibition, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__absolute_prohibition, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__absolute_prohibition, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(huma_be_t45, humane_treatment_standard__absolute_prohibition, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(huma_be_t60, humane_treatment_standard__absolute_prohibition, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(huma_be_t75, humane_treatment_standard__absolute_prohibition, base_extractiveness, 75, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__absolute_prohibition, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__absolute_prohibition, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(huma_su_t45, humane_treatment_standard__absolute_prohibition, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(huma_su_t60, humane_treatment_standard__absolute_prohibition, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(huma_su_t75, humane_treatment_standard__absolute_prohibition, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is the absolute_prohibition reading of the humane_treatment_standard kernel. It is structurally distinct from its sibling readings: contextual_necessity permits security overrides, and proportionality_balancing admits dignity-security tradeoffs. Each reading carries a different epsilon, stakeholder structure, and victim/beneficiary configuration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
