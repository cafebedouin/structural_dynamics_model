% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Keyboard Standardization as Manufacturer Cartel Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the strategic lock-in reading of the QWERTY
 *   persistence kernel: that the layout's continued dominance is not merely
 *   historical accident but the product of an 1893 manufacturer
 *   standardization agreement that built a self-reinforcing training and
 *   certification infrastructure to lock in demand for QWERTY-compatible
 *   machines. Under this reading, the arrangement is a tangled rope — it
 *   genuinely solves an interoperability coordination problem, but the cartel
 *   actively engineered and enforced a specific solution that also generated
 *   rents and externalized ergonomic costs onto typists, and it suppressed
 *   superior alternatives (like Dvorak) from gaining institutional footing.
 *   This is a distinct constraint from the sibling path-dependency reading
 *   (constraint_id: path_dependency_reading), which holds no beneficiary or
 *   victim structure because it treats persistence as accident without
 *   strategic actors. The two readings are not the same constraint measured
 *   differently — they instantiate different beneficiary/victim structures
 *   and different ε values, per the kernel-reading discipline.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturer_cartel_1893: agenda_setter (organized/arbitrage) — designed and enforced the standard, captured training pipeline
 *   - professional_typists: payer (powerless/trapped) — bear ergonomic and retraining costs
 *   - alternative_layout_inventors: excluded (powerless/trapped) — shut out of standard-setting
 *   - historical_economists_and_labor_researchers: observer (analytical) — assess strategic vs accidental persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.58).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Standardization as Manufacturer Cartel Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '735f9a60-d6fc-46ea-8fda-0f6b630fe9a7').
narrative_ontology:cs_kernel_codification('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', distributed).
narrative_ontology:cs_authority_grounding('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', extraction).
narrative_ontology:cs_interpretation_layer_present('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7').
narrative_ontology:cs_reading_relation('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', foundational, standardization_control_confers_rent_extraction_capacity).
narrative_ontology:cs_axiom_status(standardization_control_confers_rent_extraction_capacity, holdable).
narrative_ontology:cs_axiom_grounding('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', standardization_control_confers_rent_extraction_capacity, empirically_contingent).
narrative_ontology:cs_axiom('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', secondary, training_pipeline_capture_constitutes_active_suppression).
narrative_ontology:cs_axiom_status(training_pipeline_capture_constitutes_active_suppression, holdable).
narrative_ontology:cs_axiom_grounding('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', training_pipeline_capture_constitutes_active_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', manufacturer_cartel_founding_standard).
narrative_ontology:cs_drift_state('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', digital_keyboard_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('735f9a60-d6fc-46ea-8fda-0f6b630fe9a7', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_school_operators).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, incumbent_keyboard_patent_holders).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_sufferers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, network_effects_justify_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% In 1893, the leading American typewriter manufacturers formed a standardization agreement fixing the QWERTY layout across their product lines. This coordination allowed them to jointly fund and control typing-school curricula, guaranteeing a labor pool trained exclusively on their layout. They administer the standard and profit from the resulting closed loop between machine sales and training infrastructure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893, agenda_setter,
    organized, generational, arbitrage, national).

% Operate certification programs built entirely around QWERTY touch-typing speed contests, funded and promoted by the manufacturer cartel. Their entire business model depends on the layout remaining singular; they have no incentive to teach or certify alternatives, and profit from the appearance that QWERTY proficiency equals typing skill itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_school_operators, beneficiary,
    organized, biographical, arbitrage, national).

% Hold patents and manufacturing tooling built around the QWERTY layout. They lobby against layout alternatives (e.g. Dvorak) by funding comparative-speed studies with methodological choices favorable to QWERTY, and by controlling which layouts appear in institutional procurement contracts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, incumbent_keyboard_patent_holders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, incumbent_keyboard_patent_holders, agenda_setter).

% Must learn and use QWERTY to be employable, since certification, employer expectations, and available machines are all built around it. Bear the ergonomic and speed costs of a layout not optimized for the language, with no viable path to retrain onto a superior layout without losing employability during the transition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    powerless, biographical, trapped, national).

% Suffer repetitive strain injuries linked to QWERTY's non-ergonomic key distribution, a cost that falls entirely on individual workers while the manufacturers who benefit from the standardization bear none of the medical or lost-labor costs.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_sufferers, payer,
    powerless, biographical, trapped, national).

% Designers of ergonomically superior layouts (e.g. Dvorak) are shut out of the training-school and procurement pipeline controlled by the cartel and its allied institutions. Their studies and demonstrations are dismissed or countered by cartel-funded research; they have no seat in the standard-setting process.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    powerless, biographical, trapped, national).

% Study the QWERTY case as a paradigm example in debates over lock-in, network effects, and manufactured versus accidental path dependency. Their analyses are the primary outside corroboration for whether the standardization was strategic or incidental.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, historical_economists_and_labor_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing on a single keyboard layout solves a genuine coordination problem: without a shared layout, typists could not move between machines or employers, and manufacturers could not build a shared training and certification pipeline.
% TRANSFER_FUNCTION: The arrangement moves ergonomic costs, retraining barriers, and injury risk from the manufacturer cartel and allied training-school operators onto individual typists, while training fees, machine sales, and certification revenue flow to the cartel and its partners.
% ABSENT_VOICES: Alternative-layout inventors and ergonomics researchers were excluded from the standard-setting process; they would have argued for a layout optimized for typing speed and injury reduction rather than one that (per some historical accounts) was partly shaped by mechanical constraints of early typebar machines and then locked in by training infrastructure regardless of its ergonomic merit.
% DISAPPEARANCE_RATIONALE: If the cartel's coordinated standardization enforcement vanished today, the physical QWERTY layout would likely persist in the short term due to sheer installed base and muscle memory, but the mechanisms that actively suppressed alternative layouts (certification exclusivity, procurement lock-in, funded comparative studies) would lose their institutional teeth — new entrants and alternative layouts would face a materially lower barrier to adoption. Whether the world 'rearranges' depends on whether one attributes persistence to the cartel's active maintenance or to accumulated habit independent of it — this is exactly the kernel dispute this reading addresses.
% FOUNDING_PROBLEM: In the 1870s-1890s, the typewriter industry needed a single agreed keyboard arrangement so that trained typists, sold machines, and certification programs could all interoperate; manufacturers organized in 1893 to fix a standard and built training infrastructure around it.
% FOUNDING_PROBLEM_CORROBORATION: Historical economists and labor historians analyzing typewriter industry records attest that once electric and then digital keyboards removed the original mechanical constraints attributed to early typebar collision avoidance, no remaining technical coordination problem required QWERTY specifically — any layout would now solve the interoperability problem equally well. This corroboration comes from outside the beneficiary set (independent economic historians), though the cartel's institutional descendants (keyboard manufacturers and standards bodies) continue to assert the standard reflects an unchangeable practical necessity rather than an entrenched historical choice.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that a real coordination function exists (a shared layout enables labor mobility) but the cartel's control over training/certification infrastructure and procurement channels allowed them to extract rents beyond what pure coordination would require — asymmetric to the ergonomic and retraining costs borne by typists. Suppression (0.58) is elevated relative to the sibling reading because this reading holds that the cartel actively funded comparative studies and controlled certification to suppress alternative layouts, not merely that habit accumulated passively. Theater ratio (0.4) captures that some genuine coordination function remains (a shared layout is still useful) alongside a growing performative element — claims of 'typing efficiency' research funded by incumbents to justify continued standardization long after the original interoperability problem was solved by digital keyboards.
 *
 * DIRECTIONALITY LOGIC:
 *   The cartel and its allied training-school and patent-holding partners are structural beneficiaries: they set the standard, capture certification and machine-sale revenue, and face essentially no exit cost (arbitrage-level exit options — they can pivot standards if it profits them). Professional typists and ergonomic-injury sufferers are structural targets: trapped exit options because employability depends on QWERTY proficiency, and the costs (RSI, foregone speed gains from superior layouts) are borne individually with no coalition mechanism to negotiate a layout change. Alternative-layout inventors are excluded rather than coordinated — the constraint's suppression apparatus (funded comparative studies, procurement exclusivity) exists specifically to keep their alternative out of the market.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (a common layout usable across machines and employers) is now dead as a technical matter — modern digital keyboards impose no typebar-collision constraint and could support any layout with zero marginal hardware cost. Under this reading, the constraint's persistence past the point of technical necessity, sustained by cartel-descended institutional actors (keyboard standards bodies, training certifiers) still asserting practical necessity, is precisely the mandatrophy this framework is built to detect: a genuine original coordination function whose surviving justification is now retroactive cover for continued extraction (network-effects rent, training-industry revenue) rather than the coordination itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_coordination_intent_ambiguity,
    'Did the 1893 manufacturer agreement constitute deliberate strategic lock-in with foreseen rent-extraction, or was it a good-faith interoperability standard whose extractive effects emerged later without design intent?',
    'Archival review of the 1893 manufacturer correspondence, meeting minutes, and contemporaneous trade-press coverage to establish whether rent-extraction and competitor-suppression were articulated goals versus emergent side-effects of a genuine standardization effort.',
    'If archival evidence shows explicit intent to suppress alternatives and control training pipelines for profit, this reading is strongly corroborated over the path-dependency sibling. If evidence shows the agreement was purely about interoperability with no anticompetitive design, this reading''s tangled_rope classification weakens toward the sibling''s milder characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_coordination_intent_ambiguity, empirical, 'Whether cartel formation was strategically extractive by design or incidentally so.').

omega_variable(
    qwerty_dvorak_comparative_evidence_reliability,
    'Are the historical comparative typing-speed studies favoring QWERTY (often cited to justify continued standardization) methodologically sound, or were they cartel-funded and designed to produce a predetermined conclusion?',
    'Independent statistical re-analysis of the original study designs and funding sources, cross-referenced against later blind replications.',
    'If the studies are found methodologically compromised by funding conflicts, this substantially strengthens the suppression and theater_ratio metrics authored here. If found methodologically sound, the suppression narrative weakens and the constraint moves closer to a genuine rope with incidental victim costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_dvorak_comparative_evidence_reliability, empirical, 'Reliability of the evidentiary basis for continued QWERTY dominance claims.').

omega_variable(
    kernel_reading_choice_signal,
    'What observational signal should govern whether an analyst adopts the strategic_lock_in_reading versus the path_dependency_reading of QWERTY persistence?',
    'This is a conceptual framing choice, not solely an empirical one: it depends on whether one weighs the presence of an identifiable, organized beneficiary group (the 1893 cartel) as sufficient to establish strategic design, or requires direct documentary evidence of anticompetitive intent before abandoning the null hypothesis of accidental lock-in.',
    'Adopting the strategic reading (this story) classifies the constraint as tangled_rope with named victims and beneficiaries, triggering enforcement and extraction analysis. Adopting the path-dependency reading removes the beneficiary/victim structure entirely, likely reclassifying toward rope or mountain-adjacent emergent-outcome status with negligible extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_signal, conceptual, 'Which reading of the QWERTY kernel an analyst adopts and why, and what classification follows from each.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.2).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.25).
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.45).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.5).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint (strategic_lock_in_reading) and path_dependency_reading are sibling readings of the same kernel (qwerty_persistence_inevitability). They share the same observed phenomenon (QWERTY's continued dominance) but instantiate structurally distinct claims: this story holds an organized, profiting beneficiary set (the 1893 cartel and descendants) and a named victim set (typists bearing ergonomic/retraining costs), classifying as tangled_rope with moderate-high extraction. The sibling holds no beneficiary/victim structure, treating persistence as accidental network-effect lock-in with negligible extraction, classifying closer to rope or an emergent natural outcome. Per the ε-invariance principle, these are two separate constraints, not one constraint measured two ways, and are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
