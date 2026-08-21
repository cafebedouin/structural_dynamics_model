% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command: Continuationist Reading
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'continuationist_reading' of the
 *   'divine_marriage_command' kernel. It posits that the divine command for
 *   plural marriage remains doctrinally valid, and the 1890 Manifesto was a
 *   prudential suspension under duress (federal anti-polygamy laws), not a
 *   doctrinal rescission. The constraint operates by coordinating belief in
 *   this continuing validity while managing the tension with secular law and
 *   the non-practice of polygamy. The metrics reflect the ongoing costs and
 *   enforcement required to maintain this delicate balance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.6).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command: Continuationist Reading").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '99d7f07c-563e-4823-80c9-8bfaee76dea2').
narrative_ontology:cs_kernel_codification('99d7f07c-563e-4823-80c9-8bfaee76dea2', fixed_text).
narrative_ontology:cs_authority_grounding('99d7f07c-563e-4823-80c9-8bfaee76dea2', lineage).
narrative_ontology:cs_interpretation_layer_present('99d7f07c-563e-4823-80c9-8bfaee76dea2').
narrative_ontology:cs_reading_relation('99d7f07c-563e-4823-80c9-8bfaee76dea2', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('99d7f07c-563e-4823-80c9-8bfaee76dea2', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('99d7f07c-563e-4823-80c9-8bfaee76dea2', foundational, divine_command_immutable).
narrative_ontology:cs_axiom_status(divine_command_immutable, holdable).
narrative_ontology:cs_axiom_grounding('99d7f07c-563e-4823-80c9-8bfaee76dea2', divine_command_immutable, theological).
narrative_ontology:cs_axiom('99d7f07c-563e-4823-80c9-8bfaee76dea2', secondary, prophetic_guidance_prudential).
narrative_ontology:cs_axiom_status(prophetic_guidance_prudential, holdable).
narrative_ontology:cs_axiom_grounding('99d7f07c-563e-4823-80c9-8bfaee76dea2', prophetic_guidance_prudential, conventional).
narrative_ontology:cs_reference_frame('99d7f07c-563e-4823-80c9-8bfaee76dea2', original_revelation_unchanged).
narrative_ontology:cs_drift_state('99d7f07c-563e-4823-80c9-8bfaee76dea2', contemporary_secular_society, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99d7f07c-563e-4823-80c9-8bfaee76dea2', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, church_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamy_practitioners_under_duress).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, members_desiring_polygamy).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the official doctrinal interpretation that plural marriage remains divinely commanded but is prudentially suspended. Enforces this interpretation through ecclesiastical courts and social pressure, balancing theological consistency with institutional survival under secular law.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a coherent theological framework that reconciles historical revelation with current practice, preserving their faith's integrity. They internalize the belief in polygamy's validity while adhering to the suspension, often experiencing internal tension.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_adherents, beneficiary,
    moderate, biographical, identity_locked, global).

% These individuals, often in fundamentalist splinter groups, continue to practice polygamy based on their interpretation of the divine command. They bear the direct legal and social costs (imprisonment, loss of civil rights, social ostracization) imposed by secular authorities, and are often excommunicated by the mainstream church.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamy_practitioners_under_duress, payer,
    powerless, biographical, trapped, local).

% Adherents who believe in the doctrinal validity of polygamy and desire to practice it, but conform to the church's prudential suspension. They bear the cost of unfulfilled desires and the psychological tension of holding a belief that cannot be openly practiced within their community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, members_desiring_polygamy, payer,
    moderate, biographical, identity_locked, global).

% Groups that broke from the mainstream church to continue practicing polygamy, claiming direct continuity with original revelation. They are excluded from the mainstream church's fellowship and legitimacy, and face legal persecution from secular authorities. Their existence highlights the tension within the continuationist reading.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, excluded,
    organized, generational, constrained, regional).

% Enforce anti-polygamy laws, viewing the practice as illegal regardless of religious doctrine. They are an external constraint on the church's internal theological debates and directly suppress the practice of polygamy, influencing the church's prudential suspension.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain doctrinal consistency regarding the divine command for plural marriage, allowing the mainstream church to reconcile its historical revelations with its current institutional stance and legal compliance, thereby preserving its theological integrity and institutional survival.
% TRANSFER_FUNCTION: Transfers theological legitimacy and institutional stability to the church leadership and continuationist adherents, while transferring social, legal, and psychological costs (unfulfilled desires, persecution) to polygamy practitioners and members desiring polygamy.
% ABSENT_VOICES: The 'substitutionist_reading' adherents (who believe polygamy is doctrinally rescinded) and 'coercion_visibility_reading' adherents (who emphasize the Manifesto as a direct response to coercion, legitimizing the shift) are structurally excluded from the internal logic of the continuationist framing. Secular authorities are external enforcers, not internal voices in the doctrinal debate.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished overnight, the church would face a severe doctrinal crisis. Members would be forced to either accept a full doctrinal rescission (aligning with substitutionist views) or fully embrace fundamentalist splinter groups. The theological landscape and institutional structure of the faith would fundamentally reorganize.
% FOUNDING_PROBLEM: To reconcile the historical divine command for plural marriage with the existential threat posed by federal anti-polygamy laws and societal pressure, while preserving the church's theological integrity and ensuring its institutional survival.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion, independent sociological studies, and legal scholars corroborate the historical context of federal pressure and the church's need to adapt for survival. The tension between historical doctrine and modern practice/law persists, indicating the problem is still live, though its manifestation has changed.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a theological belief system (benefiting church leadership and adherents seeking consistency) but simultaneously extracts significant costs from those who must suppress practice or face legal/social consequences. Extractiveness is moderate (0.6) due to the internal tension and external costs. Suppression is high (0.7) due to both internal doctrinal enforcement and external legal pressure. Theater ratio is low (0.1) because the doctrinal claim is genuinely held, not merely performative. The temporal measurements show initial high suppression due to federal pressure, with extractiveness rising as the internal tension and splinter groups emerged, then stabilizing as the reading became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, this reading is a necessary and divinely guided adaptation that preserves the faith. From the perspective of practitioners under duress or members desiring polygamy, it is a source of significant personal cost and unfulfilled divine command. The engine's per-seat classification will highlight this divergence, showing the same constraint as a coordination mechanism for some and an extractive force for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership benefits from maintaining doctrinal consistency and institutional survival, placing them near the beneficiary end. Continuationist adherents also benefit from this theological coherence, though they bear internal costs. Polygamy practitioners under duress and members desiring polygamy are clear targets, bearing direct legal/social costs or psychological tension. Fundamentalist splinter groups are excluded and targeted by both the mainstream church and secular authorities. Secular authorities act as an external observer and enforcer, not directly benefiting from the internal doctrinal constraint but shaping its external manifestation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_vs_doctrinal_shift,
    'Was the 1890 Manifesto purely a prudential suspension under duress, or did it implicitly represent a deeper, unacknowledged doctrinal shift towards monogamy?',
    'Analysis of internal church discourse, theological interpretations, and leadership statements over time for evidence of evolving doctrinal rationales beyond mere ''duress.''',
    'If a deeper doctrinal shift is evident, the ''continuationist_reading'' becomes more theatrical, and its extractiveness for those adhering to the original command increases, potentially reclassifying it closer to a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_vs_doctrinal_shift, conceptual, 'Ambiguity of the Manifesto''s nature: temporary suspension vs. implicit doctrinal change.').

omega_variable(
    suppression_locus,
    'Is the suppression of polygamous practice primarily external (federal law) or internalized (doctrinal conformity and social pressure within the church)?',
    'Sociological studies of former members and splinter groups: if suppression persists after leaving the mainstream church or in contexts where federal law is not actively enforced, it suggests a strong internalized component.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them, making exit more difficult and costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_locus, empirical, 'Structural vs. internalized suppression mechanism for polygamous practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(divi_tr_t2000, divine_marriage_command__continuationist_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(divi_be_t2000, divine_marriage_command__continuationist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(divi_su_t2000, divine_marriage_command__continuationist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, federal_anti_polygamy_laws).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, church_membership_covenants).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel. Each reading represents a distinct constraint with its own structural properties and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
