% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope: Territorial Sovereignty Reading
 *   domain: Technology Governance / International Law / Privacy Regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'territorial_sovereignty_reading'
 *   of the GDPR Article 3 scope kernel. From this perspective, the principle
 *   of territorial sovereignty acts as a fundamental, natural limit on
 *   regulatory authority. Any attempt at extraterritorial application of laws
 *   like GDPR is seen as exceeding legitimate authority, and the constraint
 *   itself (the boundary) is viewed as a 'mountain' – an unchangeable feature
 *   of international law. The metrics reflect this inherent, low-extraction
 *   nature of a fundamental limit, even while acknowledging that attempts to
 *   transgress it may generate conflict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.1).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.15).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, mountain).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "Technology Governance / International Law / Privacy Regulation").

domain_priors:emerges_naturally(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '9ead6d49-e375-474a-9cda-ba6c17a7f9f9').
narrative_ontology:cs_kernel_codification('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', formalized).
narrative_ontology:cs_authority_grounding('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', lineage).
narrative_ontology:cs_reading_relation('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', foundational, jurisdiction_is_strictly_territorial).
narrative_ontology:cs_axiom_status(jurisdiction_is_strictly_territorial, holdable).
narrative_ontology:cs_axiom_grounding('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', jurisdiction_is_strictly_territorial, conventional).
narrative_ontology:cs_reference_frame('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9ead6d49-e375-474a-9cda-ba6c17a7f9f9', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states_and_companies_seeking_regulatory_independence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators_and_courts).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, principle_of_territorial_sovereignty).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__territorial_sovereignty_reading, non_interference_in_internal_affairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities benefit from the principle that regulatory authority is bounded by national borders, limiting the extraterritorial reach of EU law like GDPR. This preserves their regulatory autonomy and reduces compliance burdens for operations outside the EU.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states_and_companies_seeking_regulatory_independence, beneficiary,
    institutional, generational, mobile, global).

% From the perspective of this reading, EU regulators and courts bear the cost of their authority being strictly limited to EU territory. Their efforts to apply GDPR extraterritorially are deemed illegitimate, curtailing their desired scope of protection and enforcement.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators_and_courts, payer,
    institutional, generational, constrained, regional).

% The privacy interests of EU data subjects are not the primary concern of this reading. If their data is processed by non-EU entities outside the EU, this reading implies their protection is limited by territorial boundaries, potentially leaving them without GDPR safeguards.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects, excluded,
    powerless, biographical, trapped, regional).

% These scholars analyze the historical and contemporary application of territorial sovereignty in international law, observing how this principle interacts with modern challenges like global data flows and extraterritorial regulatory claims.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear, universally recognized boundary for state regulatory authority, preventing conflicts of law and ensuring mutual respect for national sovereignty in international relations.
% TRANSFER_FUNCTION: Transfers regulatory autonomy and decision-making power from potentially overreaching foreign jurisdictions back to national states, by asserting a fundamental limit on extraterritorial application.
% ABSENT_VOICES: EU data subjects, whose privacy rights might be seen as secondary to state sovereignty in this framing, and EU regulators, who would argue for the necessity of extraterritorial application to protect EU citizens in a globalized digital economy.
% DISAPPEARANCE_RATIONALE: If the principle of strict territorial sovereignty vanished, the foundational structure of international law and state-based governance would collapse, leading to widespread jurisdictional conflicts and a scramble for regulatory dominance across borders.
% FOUNDING_PROBLEM: The historical problem of states asserting regulatory authority beyond their borders, leading to conflicts of law, infringements on national sovereignty, and instability in international relations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, non-EU governments, and various international organizations consistently attest to the ongoing relevance and contestation of territorial sovereignty in international law and governance, particularly in response to expansive jurisdictional claims by powerful states or blocs.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gdpr_article_3_scope__territorial_sovereignty_reading),
    narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'mountain' because this reading asserts territorial sovereignty as an irreducible, fixed principle of international law. `extractiveness`, `suppression`, and `theater_ratio` are low, reflecting the view that a legitimate boundary, by its nature, does not extract or suppress, but simply defines limits. `accessibility_collapse` is high (0.9) because, from this perspective, there are no legitimate alternatives to respecting territorial limits. `resistance` is low (0.15) because the principle itself is widely accepted, though resistance may arise against *violations* of it. The flat measurement series reflects the perceived immutable nature of this principle.
 *
 * PERSPECTIVAL GAP:
 *   The 'territorial_sovereignty_reading' fundamentally clashes with readings that assert broader extraterritorial jurisdiction. While this reading sees the constraint as a natural limit (mountain), other readings (e.g., 'effects_jurisdiction_reading') would perceive the same boundary as a 'snare' for EU data subjects or a 'tangled_rope' for EU regulators, as it limits their ability to protect citizens or enforce laws effectively in a globalized world.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading's perspective, non-EU states and companies are beneficiaries, as the constraint protects their regulatory independence from foreign overreach. EU regulators and courts are 'payers' in the sense that their desired extraterritorial reach is curtailed by this fundamental principle. EU data subjects are 'excluded' as their privacy protection is framed as secondary to the principle of state sovereignty in this specific reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the principle of territorial sovereignty, as applied to digital regulation, a genuine, unchangeable ''mountain'' of international law, or a contested interpretation that primarily benefits non-EU states and companies?',
    'Analysis of evolving international legal consensus, state practice, and judicial decisions regarding extraterritorial jurisdiction in the digital age. Examination of the political economy of jurisdictional claims.',
    'If resolved as a contested interpretation, the ''claimed_type'' would shift from ''mountain'' to a ''tangled_rope'' or ''snare'', reflecting its constructed nature and the beneficiaries it serves. This would trigger FSM detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between a fundamental principle and a self-serving interpretation of international law.').

omega_variable(
    territoriality_in_digital_age,
    'Does the nature of global digital data flows and interconnected economies render strict territorial sovereignty an anachronism, or is it still a foundational and adaptable principle for legitimate regulatory authority?',
    'Empirical study of the effectiveness of purely territorial regulation in protecting citizens'' rights in the digital sphere, alongside legal scholarship on new models of ''digital sovereignty'' or ''functional jurisdiction''.',
    'If territoriality is deemed anachronistic, the ''accessibility_collapse'' and ''resistance'' metrics would need re-evaluation, potentially leading to a reclassification of the constraint as a ''piton'' (if maintained theatrically) or a ''snare'' (if used to extract from those seeking to operate globally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territoriality_in_digital_age, empirical, 'The ongoing relevance and adaptability of territorial sovereignty in the face of global digital challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(gdpr_tr_t30, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(gdpr_be_t30, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 30, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(gdpr_su_t30, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the GDPR Article 3 scope kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
