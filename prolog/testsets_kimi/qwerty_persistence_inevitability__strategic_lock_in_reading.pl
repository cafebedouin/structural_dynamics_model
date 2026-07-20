% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the strategic_lock_in_reading of the
 *   qwerty_persistence_inevitability kernel. It treats the persistence of the
 *   QWERTY keyboard layout not as an emergent historical accident but as a
 *   manufacturer-engineered lock-in mechanism forged by the 1893 typewriter
 *   cartel through patent pooling, production standardization, and exclusive
 *   training partnerships. The structural result is a tangled rope: genuine
 *   coordination benefits (interoperability, transferable skills) coexist
 *   with asymmetric extraction (ergonomic costs borne by typists, retraining
 *   barriers, foreclosed innovation) actively enforced by the cartel and its
 *   downstream training partners. The reading is distinguished from its
 *   path_dependency sibling by the presence of identifiable beneficiaries
 *   with enforcement capacity and a victim set bearing concentrated costs.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers_cartel (agenda_setter/institutional/arbitrage): engineered standard and captured rents via patent pools and production control
 *   - typing_training_industry (beneficiary/organized/constrained): profits from layout permanence and lobbies against retraining subsidies
 *   - typists (payer/powerless/identity_locked): bear ergonomic costs and face skill devaluation if the standard shifts
 *   - alternative_keyboard_inventors (excluded/moderate/trapped): foreclosed from market access by cartel-standardized ecosystems
 *   - technology_historians (observer/analytical/analytical): dispute whether persistence reflects efficient standardization or engineered lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.76).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '3b9df481-0aa8-41d6-889f-048006943be3').
narrative_ontology:cs_kernel_codification('3b9df481-0aa8-41d6-889f-048006943be3', formalized).
narrative_ontology:cs_authority_grounding('3b9df481-0aa8-41d6-889f-048006943be3', extraction).
narrative_ontology:cs_interpretation_layer_present('3b9df481-0aa8-41d6-889f-048006943be3').
narrative_ontology:cs_reading_relation('3b9df481-0aa8-41d6-889f-048006943be3', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('3b9df481-0aa8-41d6-889f-048006943be3', foundational, standardization_serves_rent_extraction).
narrative_ontology:cs_axiom_status(standardization_serves_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3b9df481-0aa8-41d6-889f-048006943be3', standardization_serves_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('3b9df481-0aa8-41d6-889f-048006943be3', secondary, interoperability_claim_is_cover).
narrative_ontology:cs_axiom_status(interoperability_claim_is_cover, holdable).
narrative_ontology:cs_axiom_grounding('3b9df481-0aa8-41d6-889f-048006943be3', interoperability_claim_is_cover, empirically_contingent).
narrative_ontology:cs_reference_frame('3b9df481-0aa8-41d6-889f-048006943be3', cartel_standardized_interoperability).
narrative_ontology:cs_drift_state('3b9df481-0aa8-41d6-889f-048006943be3', post_typewriter_era_computing, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b9df481-0aa8-41d6-889f-048006943be3', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_training_industry).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, standardization_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formed patent pools and production agreements in the 1890s to enforce QWERTY as the universal standard; collected rents through controlled aftermarket supply, repair monopolies, and licensing fees tied to layout uniformity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel, agenda_setter,
    institutional, generational, arbitrage, global).

% Built curricula, certification programs, and employment pipelines around QWERTY-specific muscle memory; their business model and capital stock depend on layout permanence, creating active resistance to alternative standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_training_industry, beneficiary,
    organized, generational, constrained, global).

% Invest years acquiring QWERTY proficiency that constitutes employable skill; bear higher finger-travel distances and ergonomic strain than alternative layouts would impose; face total skill devaluation and retraining costs if the standard shifts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Designed demonstrably more efficient keyboard layouts but were denied manufacturing partnerships, excluded from institutional procurement contracts, and unable to reach training pipelines locked to QWERTY; market access was foreclosed by cartel-standardized ecosystems.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_inventors, excluded,
    moderate, biographical, trapped, global).

% Debate whether QWERTY persistence reflects efficient market standardization or engineered lock-in; their analytical position is outside the extraction structure and they evaluate archival evidence of cartel coordination.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, technology_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes typewriter production and typing instruction to create interoperable equipment and transferable skills across the emerging clerical labor market, solving fragmentation in early mechanical writing systems.
% TRANSFER_FUNCTION: Moves economic surplus from typists (through ergonomic inefficiency, retraining barriers, and suppressed wage mobility) and from alternative innovators (through foreclosed markets) to the manufacturing cartel and the training industry that enforces layout permanence.
% ABSENT_VOICES: Alternative keyboard layout inventors and ergonomic reformers were structurally excluded from standard-setting bodies and manufacturer partnerships; typists themselves were not represented in cartel negotiations and their ergonomic interests were not admitted.
% DISAPPEARANCE_RATIONALE: If the QWERTY lock-in and its enforcement mechanisms vanished in the formative decades, typewriter markets would have fragmented across competing layouts, training capital would have diversified, and the clerical labor market would have organized around different skill standards â the technological trajectory would reorganize rather than persist unchanged.
% FOUNDING_PROBLEM: The early typewriter market faced fragmentation incompatible with mass production: incompatible keyboard layouts across manufacturers prevented economies of scale, created consumer confusion, and impeded the development of a liquid labor market for clerical workers.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians David (1985) and Liebowitz & Margolis (1990) debate the persistence mechanism from outside the benefiting parties; cartel documents cite interoperability, but independent engineering reviews and later ergonomic studies suggest alternative layouts were technically viable and standardization served rent consolidation rather than pure coordination.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.76) is high because the layout is demonstrably suboptimal for typing speed and ergonomics, yet persisted due to cartel enforcement that foreclosed alternatives. Suppression (0.71) reflects active mechanisms: patent pools, exclusive training partnerships, and procurement lock-in. Theater ratio (0.48) captures the performative claims of interoperability and efficiency that obscure the rent extraction. Accessibility collapse (0.80) is high because once the training infrastructure and employer expectations locked to QWERTY, alternative layouts became practically inaccessible. Resistance (0.42) is moderate: alternative inventors and ergonomic reformers mounted real but unsuccessful challenges. The measurement series run on a shared time grid to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The cartel and training industry experience the constraint as a coordination mechanism they built and maintain; typists experience it as an identity-locked skill trap with embodied costs. The agenda-setter seat computes toward rope-like coordination, while the payer seat computes toward snare-like extraction. The engine resolves this divergence as tangled_rope because both structural elements are present and actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (typewriter_manufacturers_cartel, typing_training_industry) sit at low directionality: the constraint subsidizes their revenue streams and institutional position. Victims (typists) sit at high directionality: their professional identity and earning capacity are fused to a suboptimal standard, and they pay ergonomic and retraining costs. Alternative inventors are excluded entirely, experiencing near-full target directionality via market foreclosure. The training industry, though a beneficiary, has constrained exit because its capital is specific to QWERTY, making it a secondary trapped agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmarket fragmentation in early typewritersâwas genuinely solved by standardization, satisfying the coordination test. However, the cartel's active enforcement of QWERTY long after the interoperability problem was solved, and the suppression of superior alternatives, indicates the coordination function was captured for extraction. The mandatrophy is not fully resolved because the constraint persists beyond the death of its founding problem, but it is not a pure piton: the training industry still actively benefits, and the theater ratio, while significant, has not fully replaced function with performance. The R5 genealogy flags a dead founding problem with a world_rearranges disappearance verdict, indicating zombie coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_intent,
    'Does QWERTY persistence reflect accident-driven path dependency without strategic beneficiaries, or manufacturer-engineered lock-in via cartel coordination?',
    'Archival discovery of cartel meeting minutes, patent pool agreements, and training partnership contracts demonstrating active standardization enforcement versus passive market tipping.',
    'If passive tipping dominates, classification shifts toward rope or mountain-of-history; if active cartel enforcement is documented, tangled_rope is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_intent, empirical, 'Whether persistence was engineered or emergent.').

omega_variable(
    ergonomic_rent_quantification,
    'What is the measurable productivity and health surplus extracted from typists via QWERTY inefficiency relative to demonstrably optimal layouts?',
    'Comparative ergonomic studies and wage-productivity analysis across QWERTY and alternative-layout typist populations in controlled or historical natural experiments.',
    'Quantifies the extraction rate from the payer seat; high quantified rent strengthens the asymmetric extraction component of the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ergonomic_rent_quantification, empirical, 'Quantifying ergonomic extraction from typists.').

omega_variable(
    suppression_decay_or_inertia,
    'Does contemporary QWERTY persistence still require active enforcement, or has it become self-sustaining cultural inertia?',
    'Analysis of modern manufacturer and educational institution behavior: are alternatives actively suppressed, or merely ignored due to normalized path dependency?',
    'If active enforcement has ended, the constraint may have drifted toward piton; if active enforcement continues, tangled_rope holds across the full interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_decay_or_inertia, conceptual, 'Whether modern persistence is enforced or inertial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qwer_tr_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(qwer_be_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 50, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qwer_su_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, identity_coordination).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling path_dependency_reading decompose the natural-language label 'QWERTY persistence' into two structurally distinct claims: one reading the persistence as manufactured lock-in with identifiable extractors, the other as emergent path dependency without concentrated beneficiaries. They share the same historical kernel but instantiate different constraints with different epsilon values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
