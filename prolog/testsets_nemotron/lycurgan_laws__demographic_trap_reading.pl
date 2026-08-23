% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Unrevisability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan laws (the Great Rhetra) are the paradigmatic case of a
 *   constitutional system whose founding virtue — immutability as the guard
 *   against stasis — becomes its fatal vice when demographic and geopolitical
 *   conditions shift. The demographic_trap_reading reads the kernel's
 *   unrevisability not as sacred fidelity (sacral_fidelity_reading) nor as
 *   noble lie (adaptive_fiction_reading) but as a structural snare: the
 *   constitutional freeze prevents adaptation of the citizenship/land tenure
 *   system, causing the Spartiate population to collapse from military
 *   viability while the ephorate elite and landed aristocracy extract status
 *   rents from the freeze. The constraint is the meta-rule 'the laws shall
 *   not be changed' operating as a suppression mechanism that protects the
 *   specific extractive provisions (helot labor, citizenship rent, kleros
 *   inalienability) from revision.
 *
 * KEY AGENTS:
 *   - ephorate_elite: Institutional agenda setter / beneficiary — holds veto power over constitutional change, extracts status rents from the freeze
 *   - landed_spartiate_aristocracy: Organized beneficiary — identity-locked into the Lycurgan order, resists reform even as class shrinks
 *   - declining_spartiate_population: Powerless payer — trapped by frozen citizenship rolls, bears demographic collapse
 *   - helot_underclass: Powerless payer — total extraction, geographically and legally trapped
 *   - perioikoi_merchants: Moderate payer — constrained exit, blocked from citizenship integration
 *   - plutarch_pausanias_tradition: Analytical observer — sees full structural arc across the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Unrevisability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '80d23276-2873-403e-8b0a-58ab25298845').
narrative_ontology:cs_kernel_codification('80d23276-2873-403e-8b0a-58ab25298845', formalized).
narrative_ontology:cs_authority_grounding('80d23276-2873-403e-8b0a-58ab25298845', lineage).
narrative_ontology:cs_interpretation_layer_present('80d23276-2873-403e-8b0a-58ab25298845').
narrative_ontology:cs_reading_relation('80d23276-2873-403e-8b0a-58ab25298845', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('80d23276-2873-403e-8b0a-58ab25298845', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('80d23276-2873-403e-8b0a-58ab25298845', foundational, constitutional_unrevisability_causes_demographic_collapse).
narrative_ontology:cs_axiom_status(constitutional_unrevisability_causes_demographic_collapse, holdable).
narrative_ontology:cs_axiom_grounding('80d23276-2873-403e-8b0a-58ab25298845', constitutional_unrevisability_causes_demographic_collapse, empirically_contingent).
narrative_ontology:cs_axiom('80d23276-2873-403e-8b0a-58ab25298845', foundational, citizenship_restriction_without_sunset_is_extractive).
narrative_ontology:cs_axiom_status(citizenship_restriction_without_sunset_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('80d23276-2873-403e-8b0a-58ab25298845', citizenship_restriction_without_sunset_is_extractive, deontological).
narrative_ontology:cs_reference_frame('80d23276-2873-403e-8b0a-58ab25298845', lycurgan_founding_settlement).
narrative_ontology:cs_drift_state('80d23276-2873-403e-8b0a-58ab25298845', post_leuctra_demographic_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('80d23276-2873-403e-8b0a-58ab25298845', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, ephorate_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, landed_spartiate_aristocracy).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, declining_spartiate_population).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_underclass).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, perioikoi_merchants).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, constitutional_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, mixed_regime_stability_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five ephors hold the veto power over any proposed change to the Great Rhetra. They administer the kleros system, control the agoge, and interpret the oracle's pronouncements. Their authority derives entirely from the constitution's unrevisability — they are its guardians and its primary beneficiaries. Exit is arbitrage-grade: they could defect to Persian or Macedonian patronage networks, but doing so would collapse the system that empowers them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_elite, agenda_setter,
    institutional, generational, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, ephorate_elite, beneficiary).

% The homoioi (peers) who retain full citizenship and land allotments. Their status depends on the frozen citizenship rolls and the inalienability of kleroi. They benefit from helot labor extraction and the exclusion of nouveaux riches. Exit is identity-locked: their self-concept is constituted by the Lycurgan order — to advocate change is to cease being Spartiate. They resist reform even as their class shrinks below military viability.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, landed_spartiate_aristocracy, beneficiary,
    organized, biographical, identity_locked, local).

% Spartiates who fall below the property qualification for citizenship (through land subdivision, helot revolt losses, or Olympic victor dedications) and their descendants. The citizenship restriction — no admission of new Spartiates, no restoration of lost status — is structurally unrevisable. They bear the demographic collapse: from ~8000 peers at Leuctra to ~700 at Chaeronea. No exit: the helot population surrounds them, the perioikoi control commerce, and the ephorate blocks any constitutional remedy.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, declining_spartiate_population, payer,
    powerless, generational, trapped, local).

% The enslaved Messenian and Laconian populations whose labor sustains the entire system. The constitutional freeze prevents any mechanism for manumission, integration, or rights expansion — the Lycurgan order defines them as permanent chattel. Their extraction is total: labor, military service (as light infantry), and periodic culling via krypteia. Exit is geographically and legally trapped; revolt is the only structural alternative, which the frozen constitution treats as existential threat requiring maximum suppression.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_underclass, payer,
    powerless, generational, trapped, local).

% Free but non-citizen inhabitants of the perioecic towns who control commerce, crafts, and maritime trade. They pay taxes and provide hoplites but have no voice in the apella or gerousia. The constitutional freeze blocks their natural path to integration (cf. Roman socii → citizens). Exit is constrained: they can emigrate to other Greek poleis but lose their commercial networks and face xenophobia. Some wealthy perioikoi effectively buy Spartiate patronage, but the citizenship barrier remains absolute.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioikoi_merchants, payer,
    moderate, biographical, constrained, regional).

% The literary-historical tradition that preserves the Lycurgan system as an object of study. Plutarch's Life of Lycurgus and Pausanias' Description of Greece encode the demographic_trap_reading's core observation: the laws' greatness contained the seeds of their destruction. This observer seat sees the full structural arc — the coordination function (military cohesion), the extraction mechanism (helot labor + citizenship rent), and the death spiral (unrevisability → demographic collapse → military irrelevance).
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, plutarch_pausanias_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(lycurgan_laws__demographic_trap_reading, plutarch_pausanias_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the archaic Greek problem of stasis (civil strife) by freezing the constitution, equalizing the peer class through common messes and inalienable land, and externalizing violence onto the helot population. Produced the most stable and militarily effective polis for two centuries.
% TRANSFER_FUNCTION: Moves labor and agricultural surplus from helots to Spartiates; moves political authority from the demos to the ephorate/gerousia; moves military risk from the landed aristocracy to the declining peer class and perioikoi; moves status rents from potential new citizens to existing Spartiates via the frozen citizenship rolls.
% ABSENT_VOICES: The helot population (no literary voice, only archaeological trace), the hypomeiones (disenfranchised Spartiates — disappeared from the record), and the reformist faction (if any existed, they were silenced by the ephorate's veto and the religious prohibition on innovation). The adaptive_fiction_reading suggests a covert reformist current among the ephors themselves, but no direct evidence survives.
% DISAPPEARANCE_RATIONALE: If the Great Rhetra's unrevisability clause vanished overnight, the citizenship restriction would face immediate pressure from the hypomeiones and perioikoi, the helot system would face manumission demands or revolt, and the ephorate's veto power would collapse. The Spartan polis would either reform into a conventional Hellenistic state or fracture — the demographic trap is the constitution's unrevisability itself.
% FOUNDING_PROBLEM: Archaic Sparta faced endemic stasis between aristocratic factions, land hunger driving colonization, and the threat of helot revolt. The Lycurgan settlement froze the constitution to end factional conflict, equalized the peer class to create military cohesion, and externalized the labor question onto a permanent helot underclass.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle (Politics 1269a-1270b) explicitly states the Lycurgan system solved stasis but created a demographic trap: the laws make men 'good' but not 'happy,' and the population decline is a direct consequence of the citizenship restriction. Xenophon (Constitution of the Lacedaemonians 14) confirms the system's military purpose but notes the citizen body had shrunk catastrophically. Modern corroboration: Hodkinson (2000) and Cartledge (2002) demonstrate from archaeological and literary evidence that the citizen population fell from ~8000 to ~700 between 480-371 BCE while the constitution remained formally unchanged.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the unrevisability clause protects a bundle of extractive provisions: helot labor extraction (near-total), citizenship rent (status monopoly for declining class), kleros inalienability (prevents land consolidation that might restore viability). Suppression (0.88) is higher still because the constraint's persistence depends on active enforcement: ephorate veto, religious prohibition on innovation (the oracle's authority), krypteia against helots, and the military mobilization that depends on the frozen order. Theater is low (0.15) — the system performs its stated function (military cohesion) genuinely, but the function itself is shrinking as the population collapses. Accessibility collapse (0.92) is near-total: once the Great Rhetra is accepted as unchangeable, no constitutional alternative exists within the system — reform requires revolution. Resistance (0.08) is near-zero: the payers are trapped (helots, hypomeiones) or identity-locked (landed Spartiates), and the only resistance events (Cinadon's conspiracy, helot revolts) are crushed without constitutional concession.
 *
 * PERSPECTIVAL GAP:
 *   From the ephorate seat, the constraint appears as successful coordination (the polis has not suffered stasis for centuries). From the declining Spartiate seat, it appears as a death sentence they cannot escape. From the helot seat, it appears as permanent enslavement sanctified by law. The engine computes these as different effective extractions from the same base ε — the claimed snare classification reflects the aggregate structural reality, but per-seat types will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The ephorate elite are structural beneficiaries (d ≈ 0.1): they administer the constraint and collect its rents. The landed aristocracy are identity-locked beneficiaries (d ≈ 0.2): they benefit but cannot exit the identity that makes them beneficiaries. The declining Spartiate population are trapped payers (d ≈ 0.95): they bear the demographic collapse with no exit. The helots are trapped payers (d ≈ 1.0): total extraction, no exit. The perioikoi are constrained payers (d ≈ 0.7): they bear taxes and military service without voice, with limited emigration exit. The observer seat (analytical) sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stasis) is dead — Aristotle and Xenophon confirm the archaic crisis passed. The arrangement persists as a snare because the unrevisability clause has no sunset, the beneficiaries (ephorate, landed aristocracy) are institutionally empowered to block reform, and the payers are too trapped or identity-locked to force change. The constraint classifies as snare rather than piton because extraction is concentrated and active (ephorate veto, krypteia, citizenship enforcement), not inertial. The mandatrophy is resolved: the coordination function (ending stasis) was achieved, but the constraint lacks a sunset and has become a demographic death spiral.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the demographic trap a structural property of the Lycurgan kernel itself, or an artifact of the demographic_trap_reading''s epistemic frame?',
    'Compare institutional dynamics across sibling readings: if sacral_fidelity_reading predicts stability and adaptive_fiction_reading predicts covert adaptation, the trap is reading-dependent; if all three predict demographic collapse under different mechanisms, the trap is kernel-structural.',
    'If reading-dependent, this constraint''s ε (0.82) is an artifact of the demographic_trap_reading''s focus on citizenship restrictions rather than the kernel''s overall adaptive capacity. The sibling constraints would have different ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the demographic trap is kernel-structural or reading-relative').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression preventing constitutional adaptation structural (ephorate veto, religious prohibition) or internalized (Spartiate identity fused with immutability)?',
    'Counterfactual: if the ephorate veto were removed but Spartiate identity remained fused to ''unchanging laws,'' would adaptation occur? Historical analog: Roman mos maiorum persisted without formal veto.',
    'If internalized, suppression (0.88) understates the constraint''s persistence — the target carries the suppression after formal mechanisms are removed. If structural, suppression is accurately measured by enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the constitutional freeze').

omega_variable(
    extractiveness_attribution,
    'Does the extraction flow primarily from the unrevisability itself, or from the specific citizenship/land tenure mechanisms that unrevisability protects?',
    'Decompose ε: counterfactual where citizenship restrictions are relaxed but constitutional amendment remains forbidden vs. counterfactual where amendment is permitted but citizenship restrictions persist. Measure extraction differential.',
    'If extraction is in the citizenship mechanisms, the snare classification applies to the specific provisions, not the meta-constraint of unrevisability. The kernel would decompose into multiple constraints with different ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_attribution, empirical, 'Whether extraction resides in the meta-constraint or its protected provisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_laws_demo_trap_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lycurgan_laws_demo_trap_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(lycurgan_laws_demo_trap_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement(lycurgan_laws_demo_trap_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(lycurgan_laws_demo_trap_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lycurgan_laws_demo_trap_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(lycurgan_laws_demo_trap_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.71).
narrative_ontology:measurement(lycurgan_laws_demo_trap_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_laws_demo_trap_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(lycurgan_laws_demo_trap_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.73).
narrative_ontology:measurement(lycurgan_laws_demo_trap_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.81).
narrative_ontology:measurement(lycurgan_laws_demo_trap_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The lycurgan_laws kernel decomposes into three constraint stories with different ε and classification: sacral_fidelity_reading → mountain (low ε, divine law frame), adaptive_fiction_reading → tangled_rope (moderate ε, covert coordination/extraction hybrid), demographic_trap_reading → snare (high ε, suppression prevents adaptation). The kernel's label 'Lycurgan laws' conflates these; the ε-invariance principle demands decomposition. This story's ε (0.82) measures the standing arrangement's extraction as seen from the demographic collapse frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, organized, 0.2).
constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
