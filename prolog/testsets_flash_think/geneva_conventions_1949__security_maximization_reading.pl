% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions: Security Maximization Reading
 *   domain: international_law/political_philosophy/conflict
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the 1949 Geneva
 *   Conventions, which asserts that the conventions are peacetime aspirations
 *   that must yield to operational necessity in asymmetric conflict. This
 *   interpretation justifies suspending most protections to maximize state
 *   security, leading to an expansion of 'unlawful combatant' categories,
 *   degradation of civilian immunity, and normalization of indefinite
 *   detention and coercive interrogation. The reading is framed as a
 *   necessary adaptation to modern threats but operates as a mechanism for
 *   extracting protections from vulnerable populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.9).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions: Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy/conflict").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'af1b3605-ebbd-4248-8671-c120b78cda8b').
narrative_ontology:cs_kernel_codification('af1b3605-ebbd-4248-8671-c120b78cda8b', fixed_text).
narrative_ontology:cs_authority_grounding('af1b3605-ebbd-4248-8671-c120b78cda8b', extraction).
narrative_ontology:cs_interpretation_layer_present('af1b3605-ebbd-4248-8671-c120b78cda8b').
narrative_ontology:cs_reading_relation('af1b3605-ebbd-4248-8671-c120b78cda8b', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('af1b3605-ebbd-4248-8671-c120b78cda8b', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('af1b3605-ebbd-4248-8671-c120b78cda8b', foundational, state_security_is_supreme_norm).
narrative_ontology:cs_axiom_status(state_security_is_supreme_norm, holdable).
narrative_ontology:cs_axiom_grounding('af1b3605-ebbd-4248-8671-c120b78cda8b', state_security_is_supreme_norm, deontological).
narrative_ontology:cs_axiom('af1b3605-ebbd-4248-8671-c120b78cda8b', foundational, irregular_warfare_exceptionalism).
narrative_ontology:cs_axiom_status(irregular_warfare_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('af1b3605-ebbd-4248-8671-c120b78cda8b', irregular_warfare_exceptionalism, empirically_contingent).
narrative_ontology:cs_reference_frame('af1b3605-ebbd-4248-8671-c120b78cda8b', state_sovereignty_paramount).
narrative_ontology:cs_drift_state('af1b3605-ebbd-4248-8671-c120b78cda8b', contemporary_asymmetric_conflict, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('af1b3605-ebbd-4248-8671-c120b78cda8b', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, political_leadership).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets international law to prioritize state security, justifying the suspension of protections for detainees and civilians in asymmetric conflicts. Benefits from expanded operational flexibility and reduced accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the perceived ability to act decisively against threats without being unduly constrained by international legal norms. Uses this reading to legitimize controversial security policies to domestic audiences.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, political_leadership, beneficiary,
    powerful, biographical, mobile, national).

% Bear the direct costs of this reading, facing indefinite detention without trial, denial of POW status, and coercive interrogation, with severely limited legal recourse. Their protections are systematically eroded.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees, payer,
    powerless, immediate, trapped, local).

% Experience degraded immunity, with increased acceptance of 'collateral damage' and the 'human shields' doctrine used to justify harm. Their safety is subordinated to military objectives.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Actively challenge this reading in legal and public forums, arguing for the universality and non-derogability of humanitarian protections. Their arguments are often dismissed as naive or impractical by proponents of this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, human_rights_advocates, excluded,
    organized, biographical, constrained, global).

% Investigate alleged violations of international humanitarian law, but their jurisdiction and enforcement powers are often challenged or circumvented by states adopting this reading. They provide an analytical check but face significant political resistance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate state action to effectively counter asymmetric threats and maximize national security in complex conflict environments, by removing perceived legal impediments.
% TRANSFER_FUNCTION: Transfers legal protections and rights from individuals (detainees, civilians) to the operational flexibility and security interests of the state, particularly in contexts of irregular warfare.
% ABSENT_VOICES: Detainees and civilians in conflict zones are largely absent from the interpretive process, their perspectives systematically marginalized. Human rights advocates and international legal bodies are present but often excluded from effective influence on state policy.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, state security apparatuses would face immediate and significant legal and political pressure to adhere more strictly to traditional interpretations of international humanitarian law, leading to fundamental changes in detention policies, targeting rules, and accountability mechanisms in conflict.
% FOUNDING_PROBLEM: The perceived inadequacy of traditional international humanitarian law to address the challenges posed by non-state armed groups, terrorism, and asymmetric warfare, which proponents argue create an existential threat to state security.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by state security officials, military strategists, and some political theorists who emphasize the unique nature of modern threats. Contested by international legal scholars, human rights organizations, and some former military officials who argue that existing frameworks are sufficient or that this reading is counterproductive to long-term security.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading systematically denies fundamental protections to detainees and civilians, transferring the 'cost' of security onto them. Suppression is also very high (0.90) as it relies on active legal and political mechanisms to suppress alternative interpretations and resistance from human rights bodies. Theater ratio is moderate (0.40) because while some formal adherence to international law may be maintained for diplomatic reasons, the core function of humanitarian protection is significantly degraded in practice. Accessibility collapse is high (0.88) as it aims to eliminate legal alternatives for those targeted, and resistance is high (0.75) due to strong opposition from human rights and international legal communities.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (state security, political leadership) perceive it as a necessary and legitimate adaptation to modern conflict, ensuring national survival. Victims (detainees, civilians) experience it as a severe and unjust denial of fundamental rights. International legal observers often view it as a dangerous erosion of established norms. The engine's classification as a Snare reflects the structural reality of extraction and suppression, regardless of the claimed necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and political leadership are clear beneficiaries, gaining operational flexibility and reduced accountability (low directionality). Detainees and civilians in conflict zones are the primary targets, bearing the direct costs of lost protections (high directionality). Human rights advocates are excluded from the decision-making process but actively resist, while international courts act as observers with constrained influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine interpretation of the Geneva Conventions, or a re-framing that fundamentally alters their purpose?',
    'Analysis of the travaux préparatoires (preparatory work) of the Conventions and subsequent state practice, alongside a conceptual analysis of the ''object and purpose'' of the treaties.',
    'If a fundamental alteration, it strengthens the Snare classification by revealing the ''security'' coordination story as a cover for pure extraction. If a genuine interpretation, it suggests a more complex Tangled Rope where coordination (state security) and extraction (denial of rights) are more intertwined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the interpretive legitimacy of the security maximization reading.').

omega_variable(
    security_efficacy_of_suspension,
    'Does suspending humanitarian protections in asymmetric conflict actually enhance state security, or does it prove counterproductive by fueling radicalization and undermining legitimacy?',
    'Empirical studies on the long-term effects of counter-terrorism tactics that involve derogations from international law, including analysis of recruitment patterns, local population support, and strategic outcomes.',
    'If counterproductive, the ''security maximization'' justification collapses, exposing the constraint as a pure Snare with no genuine coordination function. If effective, it complicates the classification, potentially pushing it towards a Tangled Rope by demonstrating a genuine (albeit highly extractive) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_efficacy_of_suspension, empirical, 'Empirical validity of the claim that suspending protections enhances security.').

omega_variable(
    operational_necessity_definition,
    'What constitutes ''operational necessity'' in asymmetric conflict, and who legitimately defines its scope?',
    'Development of clear, internationally agreed-upon legal standards and independent oversight mechanisms for assessing claims of operational necessity, moving beyond unilateral state declarations.',
    'If ''operational necessity'' remains unilaterally defined and expansive, the constraint retains its high extractiveness and suppression. If narrowly defined and subject to external review, it could reduce the constraint''s extractive capacity and open avenues for accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_definition, conceptual, 'Ambiguity in the definition and authority for ''operational necessity''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(gene_tr_t2000, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(gene_tr_t2005, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(gene_tr_t2020, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(gene_be_t2000, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(gene_be_t2005, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(gene_be_t2020, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(gene_su_t2000, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(gene_su_t2005, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2015, 0.9).
narrative_ontology:measurement(gene_su_t2020, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Geneva Conventions 1949 kernel. This reading prioritizes state security over humanitarian protections, directly conflicting with the humanitarian ceiling reading and influencing the conditional reciprocity reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
