% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the structural ambiguity within a religious
 *   institution where a core marriage-commitment principle (Section 132) was
 *   preserved in doctrine while its practice was publicly suspended due to
 *   external pressure. This reading focuses on the resulting gap between
 *   doctrine and practice, which allowed the institution to survive but
 *   created significant internal extraction from its membership. The period
 *   1890-1904 is critical as it covers the initial public suspension and the
 *   subsequent period where 'underground' marriages continued, leveraging the
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda-setter (institutional/constrained)
 *   - general_membership: Payer (moderate/identity_locked)
 *   - fundamentalist_factions: Payer (organized/constrained)
 *   - federal_government: Observer (institutional/analytical)
 *   - institutional_survival: Beneficiary (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.7).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '53c567f2-85ee-4f17-97cf-65350fb6fad9').
narrative_ontology:cs_kernel_codification('53c567f2-85ee-4f17-97cf-65350fb6fad9', fixed_text).
narrative_ontology:cs_authority_grounding('53c567f2-85ee-4f17-97cf-65350fb6fad9', lineage).
narrative_ontology:cs_interpretation_layer_present('53c567f2-85ee-4f17-97cf-65350fb6fad9').
narrative_ontology:cs_reading_relation('53c567f2-85ee-4f17-97cf-65350fb6fad9', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('53c567f2-85ee-4f17-97cf-65350fb6fad9', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('53c567f2-85ee-4f17-97cf-65350fb6fad9', foundational, doctrine_preservation_is_paramount).
narrative_ontology:cs_axiom_status(doctrine_preservation_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('53c567f2-85ee-4f17-97cf-65350fb6fad9', doctrine_preservation_is_paramount, deontological).
narrative_ontology:cs_axiom('53c567f2-85ee-4f17-97cf-65350fb6fad9', foundational, institutional_survival_justifies_adaptation).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('53c567f2-85ee-4f17-97cf-65350fb6fad9', institutional_survival_justifies_adaptation, instrumental).
narrative_ontology:cs_reference_frame('53c567f2-85ee-4f17-97cf-65350fb6fad9', unambiguous_doctrinal_practice).
narrative_ontology:cs_drift_state('53c567f2-85ee-4f17-97cf-65350fb6fad9', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('53c567f2-85ee-4f17-97cf-65350fb6fad9', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the official doctrine of Section 132 while publicly suspending its practice, allowing for institutional survival and strategic flexibility. Benefits from dual-track legitimation, navigating federal pressure while retaining core theological claims for internal audiences. Bears the cost of internal dissent and external scrutiny.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Experiences bewilderment and betrayal due to the gap between declared doctrine and observed practice. Many remain identity-locked due to deep religious commitment, absorbing the cognitive dissonance and loss of clarity. Some experience a crisis of faith or leave the institution.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, identity_locked, local).

% Perceive the practice-doctrine gap as a betrayal of foundational principles, leading to schism and the formation of splinter groups. They bear the cost of excommunication and loss of institutional standing, but gain clarity and doctrinal purity within their new structures.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, generational, constrained, regional).

% Exerted external pressure that led to the public suspension of practice. Observes the institution's compliance, but is not directly involved in the internal doctrinal debates. Its actions are a key exogenous factor shaping the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, observer,
    institutional, generational, analytical, national).

% The abstract concept of the institution's continued existence and ability to operate, which is directly facilitated by the ambiguity and strategic adaptation of its leadership. It is a beneficiary in an abstract sense, not an active agent.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the institution to navigate conflicting pressures from internal doctrine and external legal/political demands, maintaining a semblance of unity and continuity during a period of crisis. It coordinates the public presentation of the institution while allowing for internal doctrinal preservation.
% TRANSFER_FUNCTION: Transfers clarity and doctrinal consistency from the general membership and fundamentalist factions to the institutional leadership, in exchange for institutional survival and flexibility. It also transfers the burden of cognitive dissonance to the membership.
% ABSENT_VOICES: Early adherents who experienced the original revelation and codified Section 132 would likely object to the suspension of practice while the doctrine remains. Their voices are absent due to historical distance and the institution's control over historical narrative.
% DISAPPEARANCE_RATIONALE: If the practice-doctrine gap vanished overnight, the institution would be forced to either fully re-embrace the original practice (leading to renewed federal conflict) or formally renounce the doctrine (leading to massive internal schism). The institution's current structure and legitimacy depend on this ambiguity.
% FOUNDING_PROBLEM: The institution faced existential threat from federal anti-polygamy laws, which directly contradicted a core, divinely revealed doctrine (Section 132) regarding marriage commitment.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, independent of the institution, corroborate the severe federal pressure and the direct threat to the institution's existence. The institution's own historical records also attest to the crisis, though their interpretation of the resolution differs from external analyses.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the ambiguity sacrifices membership clarity and doctrinal consistency for institutional flexibility, effectively extracting cognitive and spiritual costs from members. Suppression is significant as dissent is managed through institutional authority and social pressure. The theater ratio is high because the public suspension of practice is a performance of compliance, while the underlying doctrine and some 'underground' practices persist, enabling dual-track legitimation. The increasing extractiveness and theater over time reflect the deepening of this ambiguity and the growing internal costs.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this was a necessary adaptation for survival, a 'rope' to navigate an existential threat. From the general membership and fundamentalist factions, it was a 'snare' or 'tangled rope' that extracted clarity, integrity, and sometimes led to schism, while the institution benefited. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership is a beneficiary (d near 0.0) as they gain institutional survival and flexibility. The general membership and fundamentalist factions are targets (d near 1.0) as they bear the costs of cognitive dissonance, loss of clarity, and potential schism. Institutional survival, as an abstract concept, is also a beneficiary. The federal government is an external observer whose pressure shaped the constraint but is not directly extracted from or subsidized by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure 'rope' (as the leadership might claim) by highlighting the asymmetric extraction and active enforcement required to maintain the ambiguity. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of institutional survival under duress. The mandatrophy is not fully resolved, as the founding problem (federal pressure) is still 'live', but the *method* of resolution (ambiguity) has become extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_ambiguity,
    'Is the strategic ambiguity a legitimate adaptation for institutional survival, or a deceptive practice that undermines internal integrity?',
    'Analysis of long-term institutional health, member retention, and external perception. If the ambiguity leads to sustained internal erosion or external delegitimization, it suggests a deceptive practice.',
    'If legitimate, the constraint''s coordination function is stronger, potentially shifting it closer to a Rope. If deceptive, the extraction and suppression are amplified, reinforcing a Snare or Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_ambiguity, conceptual, 'The normative status of strategic ambiguity in a commitment system.').

omega_variable(
    extent_of_underground_practice,
    'What was the true extent of ''underground'' marriage practices during the period of public suspension, and how did this impact the perceived practice-doctrine gap?',
    'Historical demographic research, analysis of private records, and oral histories from descendants. This would require access to sensitive, often suppressed, institutional archives.',
    'Higher rates of underground practice would increase the theater_ratio and extractiveness, as the public suspension becomes more performative. Lower rates would suggest more genuine compliance, reducing the theater and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_underground_practice, empirical, 'The actual prevalence of practices contradicting public statements.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds the general membership to the institution despite the cognitive dissonance of the practice-doctrine gap?',
    'Sociological and psychological studies of religious identity, community bonds, and belief systems within the institution. This would involve surveys, interviews, and ethnographic research.',
    'Understanding the identity-lock mechanism clarifies why members remain ''trapped'' or ''identity_locked'' despite high extraction, reinforcing the structural nature of their position and the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity-lock for general membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.55).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.75).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.8).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.65).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.68).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. This 'practice_doctrine_gap' reading focuses on the structural ambiguity and its extractive consequences, distinct from readings emphasizing external coercion or internal reinterpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
