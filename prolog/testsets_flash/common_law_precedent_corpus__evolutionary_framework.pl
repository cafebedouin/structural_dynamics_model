% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Framework
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the common law precedent corpus as an
 *   'evolutionary framework,' a reading that emphasizes the judiciary's role
 *   in adapting law to contemporary normative evolution. It views precedent
 *   as a flexible guide rather than an absolute binding constraint,
 *   permitting reinterpretation and occasional overruling to ensure justice
 *   and relevance. This is one reading of the 'common_law_precedent_corpus'
 *   kernel, distinct from 'strict_stare_decisis' and 'pluralist_balancing'
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.3).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.2).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.3).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb').
narrative_ontology:cs_kernel_codification('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', formalized).
narrative_ontology:cs_authority_grounding('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', lineage).
narrative_ontology:cs_interpretation_layer_present('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb').
narrative_ontology:cs_reading_relation('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', foundational, law_must_adapt_to_social_change).
narrative_ontology:cs_axiom_status(law_must_adapt_to_social_change, holdable).
narrative_ontology:cs_axiom_grounding('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', law_must_adapt_to_social_change, deontological).
narrative_ontology:cs_axiom('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', foundational, judicial_role_includes_normative_updating).
narrative_ontology:cs_axiom_status(judicial_role_includes_normative_updating, holdable).
narrative_ontology:cs_axiom_grounding('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', judicial_role_includes_normative_updating, conventional).
narrative_ontology:cs_reference_frame('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', adaptive_common_law_tradition).
narrative_ontology:cs_drift_state('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa1648ff-9cbf-44a5-96ec-dd3b4649d1fb', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_normative_change).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, judiciary_as_normative_updater).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_scholars_advocating_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, parties_relying_on_settled_law).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, living_constitution_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, adaptive_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies precedent, viewing it as a flexible guide that can be reinterpreted or overturned to reflect evolving societal norms and legal principles. This role empowers them to update the law.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, judiciary_as_normative_updater, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the flexibility of precedent, as it provides avenues to challenge existing legal interpretations based on contemporary values or new understandings. They can argue for reinterpretation or overruling of older cases.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_normative_change, beneficiary,
    moderate, biographical, mobile, local).

% Their work in identifying areas where law diverges from contemporary norms is directly supported by this framework. They provide intellectual justification for judicial reinterpretation and legal evolution.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_scholars_advocating_reform, beneficiary,
    organized, generational, analytical, global).

% Bear the costs of legal uncertainty when established precedents are reinterpreted or overturned. Their reliance on the predictability of law for planning and transactions is undermined by a highly adaptive framework.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, parties_relying_on_settled_law, payer,
    powerful, immediate, constrained, national).

% Argue for a more rigid adherence to precedent, emphasizing stability and predictability. Their arguments for judicial restraint are often sidelined in an evolutionary framework that prioritizes adaptation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, strict_stare_decisis_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the common law to adapt to changing societal conditions and moral understandings, ensuring the law remains relevant and just over time without requiring constant legislative intervention.
% TRANSFER_FUNCTION: Transfers authority for legal evolution from purely legislative processes to the judiciary, and shifts the burden of adapting to legal change from the state to individual litigants and legal actors.
% ABSENT_VOICES: Advocates for strict stare decisis are often marginalized in this framework, as their emphasis on legal stability and predictability is seen as hindering necessary adaptation. They would argue for a more constrained judicial role.
% DISAPPEARANCE_RATIONALE: If this evolutionary framework vanished, the common law would become static, unable to respond to new social realities without legislative action. The judiciary's role would fundamentally shift, and legal challenges based on evolving norms would lose their primary avenue.
% FOUNDING_PROBLEM: The problem of a static common law becoming irrelevant or unjust in the face of societal, technological, and moral evolution, leading to a disconnect between law and lived experience.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists corroborate the historical problem of legal stagnation and the ongoing need for adaptive mechanisms. Contemporary legal scholars and human rights advocates outside the judiciary also attest to the live status of the problem, supporting the framework's adaptive function.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the costs of legal uncertainty for those relying on settled law, but balanced by the benefits of adaptation. Suppression (0.2) is low, as this framework actively encourages challenges to existing interpretations. Theater ratio (0.1) is low, indicating that the adaptive function is genuine and not merely performative. The claimed type is 'rope' because it primarily serves a coordination function (adapting law) with moderate, non-coercive costs.
 *
 * PERSPECTIVAL GAP:
 *   Litigants seeking change and legal scholars experience this as a beneficial, adaptive system. Parties relying on settled law, however, experience the same system as a source of uncertainty and potential cost. The judiciary, as the agenda-setter, sees its role as essential for maintaining the law's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary (agenda_setter) and those seeking normative change (litigants, scholars) are beneficiaries, as the framework empowers their roles and goals. Parties relying on settled law are payers, bearing the costs of legal instability. Advocates for strict stare decisis are excluded, as their perspective is actively de-emphasized by this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework actively resists mandatrophy by institutionalizing mechanisms for legal evolution. The 'founding_problem_status' being 'live' indicates that the constraint's original mandate (preventing legal stagnation) is still relevant, preventing it from becoming a 'piton' or 'snare' that persists without function. The moderate extractiveness is a cost of adaptation, not a sign of atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_framework_vs_strict_stare_decisis,
    'Is the common law precedent corpus fundamentally an ''evolutionary framework'' or a ''strict stare decisis'' system?',
    'Analysis of judicial opinions over time, specifically the frequency and justification of precedent overruling versus adherence, and the explicit jurisprudential statements by courts regarding their role.',
    'If ''strict stare decisis'' is the more accurate description, the measured extractiveness of this ''evolutionary framework'' reading would be higher (as it would represent an unauthorized departure from the binding rule), and its claimed type would shift towards ''tangled_rope'' or ''snare'' for those who value stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolutionary_framework_vs_strict_stare_decisis, conceptual, 'Ambiguity between adaptive and rigid interpretations of precedent.').

omega_variable(
    judicial_activism_boundary,
    'At what point does judicial reinterpretation under an ''evolutionary framework'' cross into ''judicial activism'' that usurps legislative authority?',
    'Comparative analysis of legal systems with different separation of powers doctrines, and public/scholarly consensus on the appropriate scope of judicial law-making.',
    'If the boundary is frequently crossed, the ''judiciary_as_normative_updater'' would shift from beneficiary to an agenda_setter with higher extractiveness, as their actions would impose costs on the legislative process and public trust. This could push the constraint towards a ''tangled_rope'' for the broader political system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_boundary, preference, 'The normative boundary between judicial evolution and overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(comm_tr_t1970, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(comm_be_t1970, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(comm_su_t1970, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(comm_su_t1990, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. This 'evolutionary_framework' reading emphasizes judicial adaptation, while 'strict_stare_decisis' emphasizes rigidity and 'pluralist_balancing' emphasizes context-dependent weight. Each reading has distinct structural properties and implications for stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
