% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout Persistence via Incumbent Preservation
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   QWERTY began as a genuine coordination solution to a mechanical problem
 *   (typebar jamming) but persisted long after that problem vanished. This
 *   reading argues that persistence is not passive path dependence but active
 *   preservation by beneficiaries: manufacturers protecting tooling
 *   investments, typists protecting skill capital, and institutions
 *   protecting curricula. These incumbents deploy suppression — OEM contracts
 *   excluding alternatives, standards bodies ratifying only QWERTY,
 *   educational mandates — to maintain the standard. The constraint is a
 *   Tangled Rope: it still coordinates (universal layout enables
 *   interoperability) but extracts via defensive suppression of alternatives.
 *   The lapsed_alternatives_reading offers a competing explanation:
 *   alternatives simply failed to reach critical mass on their merits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout Persistence via Incumbent Preservation").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '1335dc47-c74a-4419-bc19-4607cab515c2').
narrative_ontology:cs_kernel_codification('1335dc47-c74a-4419-bc19-4607cab515c2', implicit).
narrative_ontology:cs_authority_grounding('1335dc47-c74a-4419-bc19-4607cab515c2', practice).
narrative_ontology:cs_interpretation_layer_present('1335dc47-c74a-4419-bc19-4607cab515c2').
narrative_ontology:cs_reading_relation('1335dc47-c74a-4419-bc19-4607cab515c2', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('1335dc47-c74a-4419-bc19-4607cab515c2', foundational, incumbent_capital_protection_drives_standard_persistence).
narrative_ontology:cs_axiom_status(incumbent_capital_protection_drives_standard_persistence, holdable).
narrative_ontology:cs_axiom_grounding('1335dc47-c74a-4419-bc19-4607cab515c2', incumbent_capital_protection_drives_standard_persistence, instrumental).
narrative_ontology:cs_axiom('1335dc47-c74a-4419-bc19-4607cab515c2', secondary, alternative_suppression_is_active_not_passive).
narrative_ontology:cs_axiom_status(alternative_suppression_is_active_not_passive, holdable).
narrative_ontology:cs_axiom_grounding('1335dc47-c74a-4419-bc19-4607cab515c2', alternative_suppression_is_active_not_passive, empirically_contingent).
narrative_ontology:cs_reference_frame('1335dc47-c74a-4419-bc19-4607cab515c2', mechanical_typewriter_era_standardization).
narrative_ontology:cs_drift_state('1335dc47-c74a-4419-bc19-4607cab515c2', electronic_keyboard_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1335dc47-c74a-4419-bc19-4607cab515c2', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, consumers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, consumers).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, path_dependence_in_technical_standards).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, increasing_returns_to_adoption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the de facto keyboard layout standard through manufacturing tooling, OEM contracts, and supply chain integration. Invested billions in QWERTY-specific production infrastructure. Actively lobby standards bodies and use contractual leverage to prevent alternative layout adoption. Collect monopoly rents from protected market position.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Invested thousands of hours in QWERTY muscle memory and professional certification. Benefit from universal skill transferability across jobs and devices. Simultaneously bear opportunity costs of suboptimal ergonomics and cannot switch without massive retraining investment. Professional identity fused with QWERTY proficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, trained_typists, payer).

% Operate typing curricula, certification programs, and educational materials built entirely around QWERTY. Collect revenue from training and certification. Curricula inertia creates high switching costs. Accreditation systems reinforce QWERTY as the only legitimate standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_institutions, beneficiary,
    organized, generational, constrained, national).

% Users of Dvorak, Colemak, or other layouts who bear compatibility costs: cannot use standard keyboards efficiently, face software/hardware incompatibility, must carry personal keyboards, experience friction in workplace and public computing. Actively suppressed by OEM exclusion and standards body inertia.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    moderate, biographical, trapped, global).

% Users who would adopt more efficient/ergonomic layouts but cannot overcome the coordination barrier. Bear ongoing costs of repetitive strain injury, slower typing speeds, and cognitive load from suboptimal layout. No viable exit because alternatives lack ecosystem support.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seekers, payer,
    powerless, biographical, constrained, global).

% ISO/IEC and national standards bodies that ratified QWERTY as de jure standard (ISO 9995). Maintain the standard through consensus processes dominated by incumbent manufacturers. Could revise but face institutional pressure to preserve backward compatibility.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_bodies, observer,
    institutional, generational, analytical, global).

% Purchase keyboards and devices with QWERTY layout by default. Benefit from universal compatibility and immediate usability. Pay hidden costs through higher device prices (monopoly rents) and health externalities (RSI). No meaningful choice in layout at point of purchase.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, consumers, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, consumers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Universal keyboard layout enabling interoperable manufacturing, transferable typing skills across jobs and devices, compatible software/hardware ecosystems, and shared training infrastructure — a genuine coordination problem solved by a single dominant standard.
% TRANSFER_FUNCTION: Lock-in rents flow from alternative-layout adopters and efficiency-seekers (who bear switching costs, compatibility penalties, and ongoing ergonomic harm) to keyboard manufacturers (protected tooling investments), trained typists (protected human capital), and typing institutions (protected curricula and certification revenue).
% ABSENT_VOICES: Dvorak and Colemak advocates, ergonomic researchers documenting QWERTY's biomechanical inferiority, users with disabilities requiring alternative layouts (one-handed, chorded, eye-tracking), global users with non-Latin scripts forced into QWERTY-based input methods, and potential innovators in input technology excluded by the standard's dominance.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire keyboard manufacturing supply chain, global typing education industry, software keyboard implementations, OS-level input systems, and muscle memory of billions would require simultaneous reorganization — a civilizational-scale coordination crisis.
% FOUNDING_PROBLEM: Mechanical typewriter key jamming at high typing speeds in the 1870s — QWERTY's staggered layout separated common letter pairs to prevent typebar collisions.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians (David 1985 'Clio and the Economics of QWERTY'; Liebowitz & Margolis 1990 'The Fable of the Keys') document the mechanical origin from primary sources. The mechanical jamming problem is acknowledged as obsolete by all parties including manufacturers — electronic keyboards have no typebars. No credible source maintains the mechanical justification applies to modern keyboards.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the coordination function (universal layout) could be served by any standard — the QWERTY-specific rents come from suppressing competition. Suppression (0.75) is high and rising: early on, network effects sufficed; later, active exclusion (OEM contracts, standards capture, education mandates) became necessary as alternatives emerged. Theater ratio (0.38) reflects that the coordination function is real but increasingly performative — 'compatibility' is maintained as a cover for rent extraction. Accessibility collapse (0.82) is very high: alternatives exist but cannot overcome the installed base. Resistance (0.48) is moderate: alternative advocates persist but lack structural power.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer seat: QWERTY is a valuable coordination asset they built and maintain. From the alternative-adopter seat: QWERTY is an actively enforced barrier to better layouts. From the typist seat: QWERTY is both the foundation of their professional identity and a cage preventing ergonomic improvement. The engine computes these divergent seat types from the structural data — the claimed tangled_rope captures the structural truth that coordination and extraction are inseparable in this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers are structural beneficiaries (d ~0.1) — they collect rents, control the standard, have arbitrage-grade exit (could pivot to any layout if it became dominant). Trained typists are identity-locked beneficiaries (d ~0.3) — they genuinely benefit from coordination but are trapped by skill investment; professional identity fuses with QWERTY. Typing institutions are constrained beneficiaries (d ~0.25) — they collect certification rents but could retrain curricula at cost. Alternative adopters and efficiency seekers are trapped/constrained payers (d ~0.85-0.9) — they bear costs with no viable exit. Consumers are constrained payers with secondary beneficiary role (d ~0.6) — they get compatibility but pay monopoly prices and health costs. Standards bodies are analytical observers (d ~0.5) — they could change the standard but face institutional pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming) is dead — electronic keyboards eliminated it by 1980s. Yet the constraint persists and extraction has increased. This is classic mandatrophy: the mandate ('prevent jamming') expired but the arrangement (QWERTY) survived and grew more extractive. The coordination function (universal layout) remains live but is now served by the wrong standard for the wrong reasons. The constraint prevents mislabeling: it is not pure extraction (coordination is real) nor pure coordination (extraction is active and asymmetric). The mandatrophy_resolved flag should be false — the dead mandate has not been resolved; the constraint has metastasized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_passive_failure,
    'Is QWERTY''s persistence driven by active incumbent suppression (lobbying, OEM contracts, standards capture) or by passive network effects where alternatives simply cannot overcome coordination barriers?',
    'Counterfactual analysis: if all active suppression mechanisms were removed (open OEM contracts, neutral standards processes, curriculum choice), would a superior layout achieve adoption within a generation? Historical natural experiments (e.g., French AZERTY vs. QWERTY, Japanese JIS vs. QWERTY) provide partial evidence.',
    'If active suppression is primary, the constraint is a Snare/Tangled Rope with identifiable perpetrators. If passive failure is primary, it is a Rope/Piton where coordination value alone sustains it. Classification and remedy differ fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_passive_failure, empirical, 'Whether extraction is actively maintained or passively inherited.').

omega_variable(
    counterfactual_adoption_threshold,
    'What critical mass of adoption would an alternative layout need to overcome QWERTY''s installed base advantage, and is that threshold achievable without coordinated intervention?',
    'Agent-based modeling of standard adoption with calibrated network effect parameters; analysis of historical standard transitions (e.g., VHS/Betamax, Blu-ray/HD-DVD, USB-C) for threshold estimates.',
    'If threshold is low (<15% market share), passive failure explanation gains weight. If threshold is high (>40%), active suppression is necessary to explain persistence. Determines whether mandate-level intervention is structurally required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_adoption_threshold, empirical, 'The tipping point for alternative layout viability.').

omega_variable(
    suppression_mechanism_identification,
    'What specific mechanisms constitute ''active suppression'' — explicit contractual exclusion, standards body capture, educational mandate, or merely failure to support alternatives?',
    'Documentary analysis of OEM contracts, standards body proceedings, educational policy records, and lobbying disclosures from 1980-present.',
    'If suppression is explicit (contracts forbidding alternatives), the constraint is a Snare. If suppression is neglect (failure to support), it is a Tangled Rope with passive extraction. Affects liability and remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identification, empirical, 'The specific causal mechanisms of alternative exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_incumbent_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwerty_incumbent_tr_t30, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(qwerty_incumbent_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(qwerty_incumbent_tr_t90, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 90, 0.31).
narrative_ontology:measurement(qwerty_incumbent_tr_t120, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(qwerty_incumbent_tr_t150, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 150, 0.38).

% Extraction over time
narrative_ontology:measurement(qwerty_incumbent_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(qwerty_incumbent_be_t30, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(qwerty_incumbent_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(qwerty_incumbent_be_t90, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 90, 0.63).
narrative_ontology:measurement(qwerty_incumbent_be_t120, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement(qwerty_incumbent_be_t150, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 150, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_incumbent_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qwerty_incumbent_su_t30, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(qwerty_incumbent_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(qwerty_incumbent_su_t90, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 90, 0.62).
narrative_ontology:measurement(qwerty_incumbent_su_t120, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 120, 0.7).
narrative_ontology:measurement(qwerty_incumbent_su_t150, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 150, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, dvorak_adoption_barrier).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, keyboard_ergonomics_standards).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, input_method_editor_standardization).

% DUAL FORMULATION NOTE:
% This reading (incumbent_preservation) and its sibling (lapsed_alternatives) decompose the single natural-language claim 'QWERTY persists' into two structurally distinct constraints. The incumbent_preservation_reading has high epsilon (0.72) because it includes defensive suppression costs as extraction. The lapsed_alternatives_reading would have lower epsilon (coordination failure only). They share the same referent (QWERTY's persistence) but disagree on the causal structure — exactly the epsilon-invariance principle at work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence__incumbent_preservation_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
