% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Practice Reversal by Federal Coercion (Exogenous Override Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the reversal of the LDS Church's practice
 *   of plural marriage as a direct consequence of external coercion by the
 *   United States federal government, without an internal doctrinal revision
 *   of Section 132 (which outlines the principle of plural marriage). This
 *   'exogenous override' reading emphasizes the federal government's
 *   extraction of institutional autonomy from the LDS Church through
 *   legislative and judicial threats, leading to a public cessation of
 *   practice while the underlying theological principle remained unrenounced
 *   within the church's doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Practice Reversal by Federal Coercion (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b').
narrative_ontology:cs_kernel_codification('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', formalized).
narrative_ontology:cs_authority_grounding('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', extraction).
narrative_ontology:cs_interpretation_layer_present('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b').
narrative_ontology:cs_reading_relation('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', foundational, divine_mandate_for_marriage_practice).
narrative_ontology:cs_axiom_status(divine_mandate_for_marriage_practice, holdable).
narrative_ontology:cs_axiom_grounding('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', divine_mandate_for_marriage_practice, theological).
narrative_ontology:cs_axiom('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', foundational, federal_sovereignty_over_territory).
narrative_ontology:cs_axiom_status(federal_sovereignty_over_territory, holdable).
narrative_ontology:cs_axiom_grounding('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', federal_sovereignty_over_territory, conventional).
narrative_ontology:cs_reference_frame('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', federal_territorial_sovereignty_uncontested).
narrative_ontology:cs_drift_state('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', post_coercion_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('de65ccfa-fa0f-4be1-a9a0-3d2d4a96f54b', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserted its territorial sovereignty and legal authority over the practice of plural marriage, using legislative and judicial threats to compel the LDS Church to cease the practice. Benefits from the consolidation of its legal and territorial control.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% The institutional autonomy and self-governance of the LDS Church, which was directly targeted and diminished by federal intervention. It bore the cost of having its religious practice dictated by an external secular power.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).

% Forced to issue manifestos and declarations to formally end the practice of plural marriage in public compliance with federal law, despite the underlying doctrine (Section 132) remaining unrenounced. They bore the direct pressure of federal threats and the internal challenge of managing doctrinal integrity versus practical compliance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_leadership, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_leadership, agenda_setter).

% Adherents who had to abandon a religiously sanctioned practice, facing social and spiritual disruption. Their identity was deeply tied to the church's teachings, making exit from the church itself highly constrained, even as the practice changed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_members, payer,
    moderate, biographical, identity_locked, national).

% Study the historical context, motivations, and consequences of the federal intervention and the LDS Church's response, analyzing the interplay of religious freedom, state power, and institutional adaptation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the federal perspective, it coordinated the assertion of national legal uniformity and territorial sovereignty over religious practice within its borders, resolving a perceived challenge to its authority.
% TRANSFER_FUNCTION: Transferred institutional autonomy and the right to define marriage practices from the LDS Church to the federal government, consolidating federal legal and territorial control.
% ABSENT_VOICES: Those within the LDS community who believed the practice of plural marriage was a divine commandment that should not be surrendered to secular authority. Their voices were suppressed by the overwhelming federal power and the church's need to preserve its existence.
% DISAPPEARANCE_RATIONALE: If the federal coercion and its effects vanished, the question of religious freedom versus state authority over marriage practices would immediately re-emerge, potentially leading to a resurgence of the original practice or a re-negotiation of the church's autonomy. Federal territorial control would be challenged.
% FOUNDING_PROBLEM: The federal government perceived the practice of plural marriage in its territories as a challenge to its legal authority, moral norms, and the principle of a unified national legal system.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records, Supreme Court rulings (e.g., Reynolds v. United States), and contemporary newspaper accounts corroborate the federal government's view of the problem. Independent historians and legal scholars attest to the federal assertion of sovereignty as the driving force.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully compelled a major institutional change against the church's will, effectively extracting its autonomy over a core religious practice. Suppression is very high (0.90) due to the severe federal threats, including disincorporation of the church, confiscation of property, and imprisonment of leaders, which left the church with virtually no viable alternatives. Theater ratio is moderate (0.40) because while the public practice ceased, the doctrinal principle (Section 132) was preserved, creating a gap between outward compliance and internal theological commitment. Resistance was initially high but ultimately overcome by overwhelming federal power.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was an assertion of legitimate state power to enforce national law and moral norms. From the LDS Church's perspective, it was a coerced abandonment of a divinely sanctioned practice, a profound loss of religious freedom and institutional autonomy. This reading highlights the federal perspective as the driving force of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary, successfully asserting its legal and territorial sovereignty. The LDS institutional sovereignty, leadership, and members are the victims, bearing the costs of forced compliance and the disruption of a deeply held religious practice. The 'identity_locked' exit option for members reflects the profound difficulty of leaving the church even when its practices are externally altered.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_integrity_vs_practice_gap,
    'To what extent did the preservation of Section 132 as principle, despite the cessation of practice, represent a genuine internal doctrinal stance versus a strategic deferral of revision?',
    'Analysis of internal church discourse, theological writings, and subsequent doctrinal developments over a longer time horizon, particularly if the federal threat were to recede.',
    'If genuine, it reinforces the ''exogenous override'' reading; if strategic, it suggests a latent ''endogenous reinterpretation'' or ''practice-doctrine gap'' was always possible, even if triggered by external events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_practice_gap, conceptual, 'Ambiguity regarding the true nature of the doctrine-practice gap.').

omega_variable(
    long_term_internalization_of_coercion,
    'Did the initial external coercion eventually lead to an internalized acceptance or reinterpretation of the new practice within the LDS community, beyond mere compliance?',
    'Sociological studies of generational shifts in belief and practice, and theological developments within the church that explicitly address the new marital norms.',
    'If internalized, the constraint''s long-term suppression might shift from purely structural to partly internalized, making the reversal more stable than initially suggested by external force alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_internalization_of_coercion, empirical, 'Whether external coercion led to internalized change over time.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the cessation of plural marriage best framed as an ''exogenous override'' (federal coercion), an ''endogenous reinterpretation'' (divine revelation), or a ''practice-doctrine gap''?',
    'Comparative analysis of historical evidence, theological arguments, and institutional statements, weighing the causal primacy of external pressure versus internal agency.',
    'The choice of framing significantly alters the classification: exogenous override points to a snare; endogenous reinterpretation might suggest a rope or scaffold; practice-doctrine gap highlights a piton-like inertia or a tangled rope of internal/external pressures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Under-determination of the primary causal mechanism for the practice reversal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1870, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(marr_tr_t1878, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1878, 0.25).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.3).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.35).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.6).
narrative_ontology:measurement(marr_be_t1870, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1870, 0.7).
narrative_ontology:measurement(marr_be_t1878, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1878, 0.78).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.82).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.87).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.65).
narrative_ontology:measurement(marr_su_t1870, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1870, 0.75).
narrative_ontology:measurement(marr_su_t1878, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1878, 0.85).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.9).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.92).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
