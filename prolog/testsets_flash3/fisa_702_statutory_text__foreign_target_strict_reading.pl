% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 Foreign Target Rule (Strict Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a strict reading of FISA Section 702's
 *   'foreign target' language, which limits surveillance to non-U.S. persons
 *   abroad and mandates minimization of incidentally collected U.S. person
 *   data. This reading emphasizes the statutory intent to protect U.S.
 *   persons from warrantless surveillance, aligning with Fourth Amendment
 *   principles. It is one interpretation within a contested kernel, where
 *   other readings permit broader collection and use of U.S. person data. The
 *   low extractiveness and suppression reflect the protections afforded to
 *   U.S. persons under this specific interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.2).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 Foreign Target Rule (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'dc21e559-750f-4962-9dee-03c9f3003d7c').
narrative_ontology:cs_kernel_codification('dc21e559-750f-4962-9dee-03c9f3003d7c', fixed_text).
narrative_ontology:cs_authority_grounding('dc21e559-750f-4962-9dee-03c9f3003d7c', lineage).
narrative_ontology:cs_interpretation_layer_present('dc21e559-750f-4962-9dee-03c9f3003d7c').
narrative_ontology:cs_reading_relation('dc21e559-750f-4962-9dee-03c9f3003d7c', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc21e559-750f-4962-9dee-03c9f3003d7c', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('dc21e559-750f-4962-9dee-03c9f3003d7c', foundational, statutory_limits_protect_us_persons).
narrative_ontology:cs_axiom_status(statutory_limits_protect_us_persons, holdable).
narrative_ontology:cs_axiom_grounding('dc21e559-750f-4962-9dee-03c9f3003d7c', statutory_limits_protect_us_persons, conventional).
narrative_ontology:cs_axiom('dc21e559-750f-4962-9dee-03c9f3003d7c', foundational, minimization_prevents_domestic_abuse).
narrative_ontology:cs_axiom_status(minimization_prevents_domestic_abuse, holdable).
narrative_ontology:cs_axiom_grounding('dc21e559-750f-4962-9dee-03c9f3003d7c', minimization_prevents_domestic_abuse, instrumental).
narrative_ontology:cs_reference_frame('dc21e559-750f-4962-9dee-03c9f3003d7c', original_statutory_intent).
narrative_ontology:cs_drift_state('dc21e559-750f-4962-9dee-03c9f3003d7c', contemporary_interpretive_contests, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dc21e559-750f-4962-9dee-03c9f3003d7c', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_abroad).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_protections).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, privacy_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the surveillance program, targeting non-U.S. persons abroad for foreign intelligence purposes. Under this strict reading, they are constrained to minimize and make inaccessible any incidentally collected U.S. person data for domestic use, ensuring the program adheres to its statutory limits.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Their communications are protected from warrantless collection and domestic queries, even when communicating with foreign targets. This reading ensures their Fourth Amendment rights are respected, minimizing the risk of incidental collection being used against them.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_abroad, beneficiary,
    moderate, biographical, mobile, global).

% Their communications are the primary target of the surveillance program, as they are non-U.S. persons located abroad. This reading does not offer them protection, as the statute is designed to collect their intelligence.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets, payer,
    powerless, immediate, trapped, global).

% Advocates for a strict interpretation of FISA Section 702 to protect privacy and civil liberties. This reading aligns with their goals, as it limits government surveillance over U.S. persons and minimizes domestic use of collected data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Responsible for legislating and overseeing the FISA 702 program. This reading reflects a desired interpretation of the statute, emphasizing the protection of U.S. persons and the minimization of incidental collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congressional_oversight_committees, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security intelligence collection on foreign targets while attempting to safeguard the privacy rights of U.S. persons, by strictly limiting the scope of collection and use of incidentally acquired data.
% TRANSFER_FUNCTION: Transfers the burden of surveillance from U.S. persons (who are protected) to foreign intelligence targets (who are collected upon), while transferring the cost of compliance and minimization to national security agencies.
% ABSENT_VOICES: Foreign intelligence targets, who bear the full weight of the surveillance, have no voice in the debate over U.S. statutory interpretation. Their perspective would highlight the complete lack of protection for non-U.S. persons abroad.
% DISAPPEARANCE_RATIONALE: If this strict reading vanished, national security agencies would likely expand collection and querying practices, leading to increased incidental collection of U.S. person data and broader domestic use, fundamentally altering the balance between security and privacy.
% FOUNDING_PROBLEM: The need to collect foreign intelligence on non-U.S. persons abroad without requiring individualized warrants, while simultaneously protecting the Fourth Amendment rights of U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: National security agencies attest to the ongoing need for foreign intelligence collection. Civil liberties advocates and congressional oversight committees corroborate the persistent tension between intelligence needs and privacy protections, making the problem live and contested in its resolution.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading explicitly excludes U.S. persons from being targets and mandates minimization of their data, thus limiting the direct cost to rights-holders. Suppression is also low (0.2) as the constraint's persistence relies on adherence to statutory limits and judicial oversight, rather than active coercion against U.S. persons. Resistance is high (0.7) from civil liberties advocates who actively push for this strict interpretation. The theater ratio is low (0.1) as the stated purpose of protecting U.S. persons is genuinely pursued under this reading.
 *
 * PERSPECTIVAL GAP:
 *   Under this strict reading, there is less perspectival gap for U.S. persons, as their rights are largely protected. However, foreign intelligence targets would still experience the constraint as highly extractive, a perspective not fully captured by the overall low extractiveness of this reading, which is focused on U.S. person protections.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies are agenda-setters, operating the program under these strictures. U.S. persons abroad are beneficiaries, as their communications are protected. Foreign intelligence targets are payers, as they are the intended subjects of collection. Civil liberties advocates and congressional oversight committees act as observers and enforcers of this strict reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''foreign target strict reading'' of FISA Section 702, or is it an aspirational interpretation that diverges from actual practice?',
    'Audits of intelligence agency practices, court rulings on minimization procedures, and legislative amendments clarifying statutory intent.',
    'If aspirational, the actual extractiveness and suppression are higher, and the constraint might reclassify as a Tangled Rope or Snare, reflecting a gap between stated policy and operational reality. If genuine, the low extractiveness and strong protections hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between the strict statutory reading and operational implementation.').

omega_variable(
    incidental_collection_definition,
    'How is ''incidental'' collection of U.S. person data defined and measured in practice? Does minimization mean deletion or merely restricted access?',
    'Detailed public reporting on minimization procedures, independent technical audits of data handling, and judicial review of specific collection instances.',
    'A broad definition of ''incidental'' or minimization as mere access restriction would increase effective extraction from U.S. persons, potentially shifting the constraint towards a Snare. Strict definition and deletion would reinforce the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_collection_definition, empirical, 'Ambiguity in the practical definition and handling of incidentally collected U.S. person data.').

omega_variable(
    sibling_reading_impact,
    'What would be the structural impact if the ''incidental_collection_reading'' or ''constitutional_floor_reading'' were adopted instead of this strict reading?',
    'Legal analysis of court precedents, legislative impact assessments, and comparative analysis of surveillance regimes in other countries.',
    'The ''incidental_collection_reading'' would significantly increase extractiveness from U.S. persons, likely reclassifying to a Snare. The ''constitutional_floor_reading'' would impose a warrant requirement, fundamentally altering the program''s operational model and potentially reducing extractiveness to near zero for U.S. persons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of alternative readings on the constraint''s structure and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
