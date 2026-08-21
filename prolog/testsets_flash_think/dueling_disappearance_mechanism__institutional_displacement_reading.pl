% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling Protocol (Institutional Displacement Reading)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the social protocol of dueling, specifically
 *   through the lens of its decline due to institutional displacement. It
 *   posits that dueling, as a coordination mechanism for honor disputes,
 *   became increasingly irrelevant as superior, less risky alternatives (like
 *   courts, banking, and libel law) emerged and gained social legitimacy. The
 *   constraint itself is a Rope, meaning it offered a coordination function
 *   with low inherent extraction, but its utility was outcompeted, leading to
 *   its marginalization rather than active suppression or cultural rejection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling Protocol (Institutional Displacement Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'ccd83a94-d358-43c2-a2a7-7ce1a000ed2e').
narrative_ontology:cs_kernel_codification('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', implicit).
narrative_ontology:cs_authority_grounding('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', practice).
narrative_ontology:cs_reading_relation('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', foundational, institutional_superiority_drives_adoption).
narrative_ontology:cs_axiom_status(institutional_superiority_drives_adoption, holdable).
narrative_ontology:cs_axiom_grounding('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', institutional_superiority_drives_adoption, empirically_contingent).
narrative_ontology:cs_axiom('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', foundational, voluntary_substitution_over_coercion).
narrative_ontology:cs_axiom_status(voluntary_substitution_over_coercion, holdable).
narrative_ontology:cs_axiom_grounding('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', voluntary_substitution_over_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', dueling_as_primary_dispute_resolution).
narrative_ontology:cs_drift_state('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', rise_of_modern_institutions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ccd83a94-d358-43c2-a2a7-7ce1a000ed2e', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, society_at_large).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, legal_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, financial_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, gentlemen_of_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically, these individuals participated in dueling to resolve honor disputes. As alternative institutions emerged, they increasingly opted for less risky and more effective methods, voluntarily moving away from dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentlemen_of_honor, payer,
    powerful, biographical, mobile, local).

% Courts and libel laws provided non-violent, formalized mechanisms for dispute resolution, absorbing conflicts that once led to duels and thereby increasing their own authority and scope.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_system, beneficiary,
    institutional, generational, arbitrage, national).

% Banking and commercial law offered mechanisms for resolving financial and contractual disputes, further reducing the need for dueling in areas where honor and financial integrity often intertwined.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, financial_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Benefited from the decline of dueling due to reduced violence, increased public safety, and the establishment of more stable and predictable dispute resolution mechanisms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, society_at_large, beneficiary,
    moderate, generational, mobile, national).

% Professionals who facilitated duels, their expertise and social role diminished as dueling became obsolete. They were not actively suppressed, but their function was outcompeted.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_masters, excluded,
    powerless, biographical, trapped, local).

% Analyze the historical and sociological factors contributing to dueling's decline, providing an analytical perspective on the mechanisms of institutional change and cultural evolution.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, historians_of_honor, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, albeit violent, protocol for gentlemen to resolve disputes of honor, thereby preventing endless feuds and maintaining social order among elites.
% TRANSFER_FUNCTION: Transferred social standing, reputation, or satisfaction from one party to another, often through the risk of injury or death, in a socially sanctioned manner.
% ABSENT_VOICES: The families of duelists, victims of dueling's violence, and those who lacked the social standing to participate in duels, who would have advocated for non-violent resolution but were outside the honor code's purview.
% DISAPPEARANCE_RATIONALE: If the dueling protocol had persisted as a primary dispute resolution mechanism, the development and authority of modern legal and financial institutions would have been fundamentally different, leading to a very different social and political landscape.
% FOUNDING_PROBLEM: To provide a structured and socially acceptable means for elites to resolve challenges to their honor, preventing uncontrolled violence or loss of face.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists widely corroborate that dueling served as a critical, albeit violent, mechanism for honor-based dispute resolution, and that its function was largely superseded by the rise of modern legal and financial systems, as evidenced in historical legal records and social commentaries.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.15 declining to 0.11) is low, consistent with a Rope, as the protocol itself was a voluntary coordination mechanism. The high personal risk of dueling was an outcome of the coordinated activity, not an extraction by the constraint. Suppression (0.10 declining to 0.06) is low because dueling was outcompeted, not actively suppressed by this mechanism. Theater ratio is low (0.05 declining to 0.03) as its decline was a genuine loss of function, not performative maintenance. Accessibility collapse (0.20) is low because alternatives became more accessible, making dueling less of a default. Resistance (0.08) is low as the shift was largely voluntary. The temporal measurements reflect a gradual decline in the constraint's relevance and the minimal effort required to maintain it, as it was increasingly superseded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'gentlemen of honor' in the early period, dueling was a necessary, albeit risky, coordination mechanism. From the perspective of 'society at large' and the emerging 'legal system', its decline was a beneficial evolution. This reading emphasizes the voluntary shift away from dueling due to better alternatives, rather than a moral or legal imperative.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'gentlemen of honor' are payers in the sense that they bore the risks and costs of dueling, but they also benefited from the honor system it maintained. As alternatives emerged, they became 'mobile' and chose to exit the dueling system. The 'legal system' and 'financial_institutions' are beneficiaries as they gained authority and relevance by offering superior dispute resolution. 'Society at large' benefited from reduced violence. 'Dueling masters' are excluded as their role atrophied.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causation,
    'To what extent did the decline of dueling result from a cultural shift away from honor culture (contraction_reading) versus the emergence of superior institutional alternatives (institutional_displacement_reading)?',
    'Comparative historical analysis of societies with similar honor cultures but different institutional development trajectories, or detailed micro-historical studies of individual choices in dispute resolution.',
    'If cultural shift was dominant, the constraint''s decline would be more akin to a ''Piton'' (atrophied function due to changing values); if institutional displacement was dominant, its ''Rope'' classification holds as it was outcompeted by better coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causation, empirical, 'Distinguishing cultural vs. institutional drivers of dueling''s decline.').

omega_variable(
    single_vs_overdetermined_causation,
    'Was dueling''s decline primarily due to institutional displacement, or was it an overdetermined outcome of multiple independent factors (overdetermined_composite_reading)?',
    'Counterfactual historical analysis, attempting to isolate the impact of institutional changes from other factors like legal prohibitions or major social upheavals (e.g., Civil War trauma).',
    'If overdetermined, this reading represents only one causal pathway, and the overall ''dueling_disappearance_mechanism'' would be a more complex, multi-component constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_vs_overdetermined_causation, conceptual, 'Assessing whether institutional displacement was the sole or primary cause of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.04).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.03).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.14).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.13).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.11).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.09).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.07).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on institutional displacement. It is linked to sibling readings that emphasize cultural contraction and overdetermined causation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
