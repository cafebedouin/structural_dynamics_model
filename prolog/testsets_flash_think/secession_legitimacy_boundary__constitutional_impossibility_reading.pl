% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impermissibility of Unilateral Secession
 *   domain: political_economy/federalism/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'constitutional impossibility' reading of
 *   the secession legitimacy boundary, asserting that unilateral secession is
 *   constitutionally impermissible and only negotiated exit via
 *   constitutional amendment is legitimate. This reading is typically held by
 *   federal governments and unionist citizens. While the reading itself
 *   claims to be a legitimate coordinating framework (claimed_type: rope),
 *   the authored metrics reflect the high extractiveness and suppression
 *   experienced by separatist movements, demonstrating a deliberate gap
 *   between the constraint's internal claim and its structural operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.85).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impermissibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political_economy/federalism/constitutional_law").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '8017765e-3e23-403a-bd64-6b7f0d528918').
narrative_ontology:cs_kernel_codification('8017765e-3e23-403a-bd64-6b7f0d528918', fixed_text).
narrative_ontology:cs_authority_grounding('8017765e-3e23-403a-bd64-6b7f0d528918', lineage).
narrative_ontology:cs_interpretation_layer_present('8017765e-3e23-403a-bd64-6b7f0d528918').
narrative_ontology:cs_reading_relation('8017765e-3e23-403a-bd64-6b7f0d528918', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8017765e-3e23-403a-bd64-6b7f0d528918', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('8017765e-3e23-403a-bd64-6b7f0d528918', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8017765e-3e23-403a-bd64-6b7f0d528918', foundational, constitutional_supremacy_over_secession).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_secession, holdable).
narrative_ontology:cs_axiom_grounding('8017765e-3e23-403a-bd64-6b7f0d528918', constitutional_supremacy_over_secession, conventional).
narrative_ontology:cs_axiom('8017765e-3e23-403a-bd64-6b7f0d528918', secondary, amendment_as_sole_legitimate_exit).
narrative_ontology:cs_axiom_status(amendment_as_sole_legitimate_exit, holdable).
narrative_ontology:cs_axiom_grounding('8017765e-3e23-403a-bd64-6b7f0d528918', amendment_as_sole_legitimate_exit, conventional).
narrative_ontology:cs_reference_frame('8017765e-3e23-403a-bd64-6b7f0d528918', perpetual_union_doctrine).
narrative_ontology:cs_drift_state('8017765e-3e23-403a-bd64-6b7f0d528918', contemporary_separatist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8017765e-3e23-403a-bd64-6b7f0d528918', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, unionist_citizens).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upholds the constitutional order, asserting its authority over territorial integrity. Benefits from the stability and continuity of the existing union. Actively enforces the constitutional process for any territorial changes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the stability, shared identity, and economic advantages of the existing federal union. Support the federal government's interpretation of constitutional impermissibility for unilateral secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, unionist_citizens, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of being denied a unilateral path to self-determination. Their political aspirations are blocked by the constitutional framework, forcing them into protracted political or legal battles within the existing system.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements, payer,
    organized, biographical, constrained, regional).

% As sub-national entities, they are legally bound by the federal constitution. They face significant legal and political barriers to any attempt at unilateral secession, including potential federal intervention and international non-recognition.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provinces, payer,
    institutional, generational, constrained, regional).

% Analyze the legal precedents and theoretical underpinnings of federalism and secession. Their interpretations often influence judicial and political discourse, but they do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% Observe and comment on secession attempts, generally favoring the territorial integrity of existing states unless extreme circumstances (e.g., genocide) are present. Their recognition (or lack thereof) is crucial for any seceding entity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the territorial integrity and political stability of the federal state, preventing fragmentation and internal conflict by establishing a clear, albeit difficult, legal process for any territorial changes.
% TRANSFER_FUNCTION: Transfers the ultimate authority over territorial boundaries and political self-determination from sub-national entities to the federal constitutional framework, ensuring central control over the union's composition.
% ABSENT_VOICES: Sub-national groups asserting popular sovereignty or historical claims to self-determination outside the federal framework; indigenous nations whose treaty rights predate the federal state and are not fully recognized by the federal constitution.
% DISAPPEARANCE_RATIONALE: If the constitutional impermissibility of unilateral secession vanished overnight, it would immediately open the door to unilateral declarations of independence, leading to widespread political instability, potential civil conflict, and a rapid redrawing of national borders as sub-national entities asserted their sovereignty.
% FOUNDING_PROBLEM: To establish a perpetual union of states, prevent internal dissolution, and ensure a stable framework for governance, economic integration, and collective action across a large and diverse territory.
% FOUNDING_PROBLEM_CORROBORATION: Federal courts, unionist political parties, and a majority of citizens (in most federal states) corroborate the ongoing need for national unity and stability. International bodies also generally favor existing state integrity, supporting the view that the founding problem remains live.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.65) reflects the fundamental political option of self-determination being denied to sub-national entities without federal consent. Suppression (0.85) is high due to the full legal and potentially coercive power of the federal state being marshaled against unilateral action. Theater ratio is low (0.10) because the constitutional framework is actively and genuinely enforced, not merely performed. Accessibility collapse is high (0.90) as, from this reading's perspective, no legitimate unilateral path exists. Resistance is high (0.70) due to active separatist movements challenging this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this constraint is a legitimate and necessary 'rope' for national unity. From the perspective of separatist movements, it operates as a 'snare' or 'tangled_rope,' coercively preventing their self-determination. The engine will compute this divergence from the structural data, revealing the gap between the claimed type and the experienced reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and unionist citizens are clear beneficiaries, as the constraint preserves their preferred political order. Separatist movements and provinces are targets, as their core political project is directly suppressed by this constraint. Constitutional scholars and international bodies act as observers, analyzing and influencing the discourse without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secession_legitimacy_ambiguity,
    'Is secession fundamentally a constitutional process, or an extra-constitutional right derived from popular sovereignty or self-determination?',
    'International legal precedent, evolving norms of self-determination, or a constitutional crisis that forces a re-evaluation of foundational principles.',
    'If reclassified as an extra-constitutional right, the constraint''s suppression and extractiveness would be viewed as illegitimate, potentially shifting its classification from ''rope'' to ''snare'' from the perspective of separatist groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secession_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the ultimate source of secessionary legitimacy.').

omega_variable(
    federal_extraction_validity,
    'Does the federal government''s enforcement of national unity constitute ''extraction'' from sub-national entities, or is it a legitimate exercise of sovereign power?',
    'Economic analysis of resource flows between federal and provincial levels, and political analysis of the distribution of power and autonomy within the federation.',
    'If federal actions are deemed extractive, the ''rope'' classification (from the federal perspective) would be challenged, pushing it towards ''tangled_rope'' or ''snare'' for affected sub-national entities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_extraction_validity, empirical, 'Whether federal unity enforcement is legitimate coordination or disguised extraction.').

omega_variable(
    constitutional_vs_popular_sovereignty,
    'Which is the ultimate source of authority: the written constitution (as interpreted by federal institutions) or the will of a regional democratic majority?',
    'A constitutional convention, a Supreme Court ruling that redefines the relationship between constitutional text and popular will, or a successful, internationally recognized unilateral secession.',
    'If popular sovereignty is recognized as supreme, the constitutional impossibility reading would be undermined, potentially leading to a reclassification of the constraint as a ''snare'' for regional populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_popular_sovereignty, preference, 'Contest between constitutional textualism and popular sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(sece_tr_t1970, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(sece_tr_t1990, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(sece_be_t1970, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(sece_su_t1970, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_taxation_authority).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, national_defense_mandate).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'secession_legitimacy_boundary' kernel, each representing a distinct structural claim about the legitimacy of secession. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
