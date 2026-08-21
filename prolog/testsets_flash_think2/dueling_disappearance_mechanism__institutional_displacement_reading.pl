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
 *   human_readable: Dueling Disappearance: Institutional Displacement
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the mechanism by which dueling, as a dispute
 *   resolution protocol, declined and became a fringe practice due to the
 *   rise and superior efficacy of institutional alternatives such as courts,
 *   banking systems, and libel law. This reading frames the decline as a
 *   process of voluntary substitution driven by the competitive advantage of
 *   new institutions, rather than active suppression of dueling itself. It is
 *   one reading of the 'dueling_disappearance_mechanism' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.2).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling Disappearance: Institutional Displacement").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/cultural_anthropology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '8888c724-2ae8-4152-a5ea-e3b395369021').
narrative_ontology:cs_kernel_codification('8888c724-2ae8-4152-a5ea-e3b395369021', implicit).
narrative_ontology:cs_authority_grounding('8888c724-2ae8-4152-a5ea-e3b395369021', practice).
narrative_ontology:cs_reading_relation('8888c724-2ae8-4152-a5ea-e3b395369021', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8888c724-2ae8-4152-a5ea-e3b395369021', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('8888c724-2ae8-4152-a5ea-e3b395369021', foundational, institutional_dispute_resolution_superiority).
narrative_ontology:cs_axiom_status(institutional_dispute_resolution_superiority, holdable).
narrative_ontology:cs_axiom_grounding('8888c724-2ae8-4152-a5ea-e3b395369021', institutional_dispute_resolution_superiority, empirically_contingent).
narrative_ontology:cs_axiom('8888c724-2ae8-4152-a5ea-e3b395369021', foundational, voluntary_adoption_of_superior_alternatives).
narrative_ontology:cs_axiom_status(voluntary_adoption_of_superior_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('8888c724-2ae8-4152-a5ea-e3b395369021', voluntary_adoption_of_superior_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('8888c724-2ae8-4152-a5ea-e3b395369021', honor_culture_dispute_resolution).
narrative_ontology:cs_drift_state('8888c724-2ae8-4152-a5ea-e3b395369021', post_institutional_modernization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8888c724-2ae8-4152-a5ea-e3b395369021', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, society_at_large).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, courts_of_law).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentlemen_of_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of violence and the establishment of more predictable and equitable dispute resolution mechanisms, leading to greater social stability and economic activity. Voluntarily adopted the new norms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Gained legitimacy and authority as the primary arbiter of disputes, expanding their jurisdiction and public trust. Actively developed and enforced legal frameworks that offered alternatives to dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, courts_of_law, agenda_setter,
    institutional, civilizational, analytical, national).

% Provided formal mechanisms for debt resolution and financial disputes, reducing the need for honor-based challenges related to financial obligations. Their growth offered a more stable economic environment.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Offered a legal avenue for redressing insults and protecting reputation, directly addressing a core driver of dueling. Its development provided a non-violent, institutionalized response to perceived slights.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_system, agenda_setter,
    institutional, generational, mobile, national).

% While initially adhering to honor codes, many eventually adopted institutional dispute resolution, benefiting from reduced personal risk and greater legal protection, even if it meant abandoning traditional practices. Their shift was largely voluntary due to superior alternatives.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentlemen_of_honor, beneficiary,
    moderate, biographical, constrained, local).

% Those who continued to uphold the honor code and the necessity of dueling found themselves increasingly marginalized, socially disfavored, and legally penalized as institutional alternatives gained dominance. Their worldview made exit from dueling norms difficult, but the institutional shift made dueling impractical.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_adherents, excluded,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, as a mechanism of institutional displacement, coordinated a societal shift towards formal, legal, and financial institutions as the primary means of resolving disputes over honor, reputation, and debt, replacing personal combat.
% TRANSFER_FUNCTION: Transferred the authority and legitimacy of dispute resolution from individual honor codes and personal combat to state-backed legal systems and formal financial mechanisms, moving social capital and power to these institutions.
% ABSENT_VOICES: Adherents of the traditional honor culture who viewed dueling as a necessary and honorable means of dispute resolution were increasingly excluded from the dominant discourse, their perspectives marginalized by the rising institutional consensus.
% DISAPPEARANCE_RATIONALE: If the institutional displacement mechanism had not occurred, society's approach to dispute resolution would be fundamentally different, likely more violent and less formalized, with honor codes retaining a more central role. The shift profoundly reorganized social and legal structures.
% FOUNDING_PROBLEM: The problem of widespread, often fatal, personal violence arising from disputes over honor, reputation, and financial obligations, and the lack of universally accepted, non-violent, and effective alternatives for redress.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records, sociological analyses of social change, and contemporary accounts from the period corroborate the decline of dueling and the rise of institutional alternatives. While the general problem of dispute resolution remains 'live', the specific problem dueling addressed is now largely handled by other means, as attested by legal historians and cultural anthropologists.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Rope because the institutional displacement mechanism offered a genuine coordination function (more stable and less violent dispute resolution) from which participants were net beneficiaries. Extraction is low because the shift was largely voluntary, driven by the superior utility of the new systems. Suppression is also low, as dueling was outcompeted rather than actively suppressed by the displacement mechanism itself (though the new institutions might have enforced their own rules). Theater ratio is negligible as the mechanism of displacement is a structural shift, not a performance. The temporal measurements reflect the stability of this displacement mechanism as a Rope, rather than the fluctuating prevalence of dueling itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the rising institutions and the general public, the displacement was a clear improvement, a beneficial coordination. From the perspective of staunch honor culture adherents, it represented a loss of a vital social practice, even if they were structurally outcompeted. This reading emphasizes the former, seeing the mechanism as a beneficial Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and the new institutions (courts, banks, libel law) are clear beneficiaries, gaining stability and authority. Gentlemen of honor, while losing a traditional practice, are also net beneficiaries of safer dispute resolution. Honor culture adherents are 'excluded' as their preferred method became obsolete, but they are not 'victims' of the displacement mechanism itself, which offered a better alternative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_primacy,
    'Was the decline of dueling primarily driven by the emergence of superior institutions (this reading), or by a fundamental cultural shift from honor to dignity culture (contraction_reading)?',
    'Comparative historical analysis of societies where institutional development outpaced cultural shifts, or vice versa, to isolate causal primacy. Examination of individual motivations for abandoning dueling.',
    'If cultural shifts were primary, this constraint''s explanation is secondary or derivative; if institutional competition was primary, this reading holds. This impacts the relative weight given to structural vs. ideational factors in social change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_institutional_primacy, empirical, 'Causal primacy of institutional displacement vs. cultural shift in dueling''s decline.').

omega_variable(
    single_cause_vs_overdetermination,
    'Is institutional displacement a sufficient explanation for dueling''s decline, or was the decline causally overdetermined by multiple independent factors (overdetermined_composite_reading)?',
    'Counterfactual historical analysis: would dueling have declined even without the rise of these specific institutions, due to other factors like legal prohibition or changing social attitudes?',
    'If overdetermined, this reading provides a valid but incomplete explanation, requiring integration with other causal factors. If institutional displacement was singularly sufficient, this reading''s explanatory power is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_cause_vs_overdetermination, conceptual, 'Whether dueling''s decline was due to a single primary cause or multiple overdetermining factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.04).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.06).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.14).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.16).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.18).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
