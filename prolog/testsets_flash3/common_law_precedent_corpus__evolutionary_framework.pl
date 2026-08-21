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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent (Evolutionary Framework Reading)
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'evolutionary framework' reading of common
 *   law precedent, where precedent serves as an adaptive guide rather than an
 *   absolute binding rule. It acknowledges the judiciary's role in
 *   reinterpreting or overruling past decisions to align with contemporary
 *   normative evolution. This reading is distinct from 'strict stare decisis'
 *   (precedent as backward constraint) and 'pluralist balancing'
 *   (context-dependent weight). The claimed type is 'rope' because it
 *   facilitates coordination in legal development, but its extractiveness is
 *   non-zero due to the costs of uncertainty for those relying on settled
 *   law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.35).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.2).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent (Evolutionary Framework Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'c386ad53-1845-4695-b0d9-c5ea7ae1e5aa').
narrative_ontology:cs_kernel_codification('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', formalized).
narrative_ontology:cs_authority_grounding('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', lineage).
narrative_ontology:cs_interpretation_layer_present('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa').
narrative_ontology:cs_reading_relation('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', common_law_precedent_corpus__strict_stare_decisis, influences).
narrative_ontology:cs_reading_relation('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', foundational, law_must_evolve_with_society).
narrative_ontology:cs_axiom_status(law_must_evolve_with_society, holdable).
narrative_ontology:cs_axiom_grounding('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', law_must_evolve_with_society, deontological).
narrative_ontology:cs_axiom('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', foundational, judicial_role_includes_normative_updating).
narrative_ontology:cs_axiom_status(judicial_role_includes_normative_updating, holdable).
narrative_ontology:cs_axiom_grounding('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', judicial_role_includes_normative_updating, conventional).
narrative_ontology:cs_reference_frame('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', adaptive_common_law_tradition).
narrative_ontology:cs_drift_state('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c386ad53-1845-4695-b0d9-c5ea7ae1e5aa', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_normative_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, litigants_relying_on_settled_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies precedent, with a recognized power to reinterpret or overrule past decisions to align with evolving societal norms. Benefits from the flexibility to adapt law, but is constrained by the need to provide reasoned justifications for departures.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the framework's openness to reinterpretation, allowing them to challenge existing legal norms and seek rulings that reflect contemporary values. Their pathway to legal change is broader under this reading.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_normative_change, beneficiary,
    moderate, biographical, mobile, local).

% Bear the costs of legal uncertainty when established precedents are subject to reinterpretation or overruling. Their reliance on the stability of past rulings is undermined, potentially leading to unexpected outcomes in their cases.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_relying_on_settled_precedent, payer,
    moderate, immediate, constrained, local).

% Analyze the evolution of precedent, critiquing the justifications for reinterpretation and its impact on legal coherence and predictability. Their role is to provide intellectual scaffolding and critique for the judiciary's adaptive function.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_scholars, observer,
    analytical, generational, analytical, global).

% While capable of enacting new laws, the legislature's role in updating common law is indirect. This reading of precedent empowers the judiciary to update law, potentially reducing pressure on the legislature to act on certain social issues.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legislature, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for legal decision-making that balances the need for stability and predictability with the imperative to adapt law to evolving societal values and circumstances, ensuring the law remains relevant and just over time.
% TRANSFER_FUNCTION: Transfers the authority to update and reinterpret legal norms from a purely backward-looking adherence to precedent to a more dynamic, judicially-driven process, impacting the certainty of legal outcomes for various parties.
% ABSENT_VOICES: Advocates for strict adherence to stare decisis, who would argue that judicial reinterpretation undermines the rule of law and legislative supremacy, are often marginalized in this framework, as their core premise is challenged by the adaptive nature of the reading.
% DISAPPEARANCE_RATIONALE: If this evolutionary framework for precedent vanished, the common law system would either rigidify into strict stare decisis, leading to ossified law, or fragment into ad hoc judicial decisions without a coherent adaptive mechanism. The legal system's capacity for organic growth and responsiveness would be fundamentally altered.
% FOUNDING_PROBLEM: The common law system needed a mechanism to evolve and remain relevant in the face of changing social, economic, and moral landscapes, without requiring constant legislative intervention or arbitrary judicial fiat.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporary jurisprudential scholars widely corroborate the ongoing need for legal adaptation, citing historical examples of law becoming unjust or irrelevant when rigidly applied. This is attested by independent academic analysis and judicial opinions from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate, reflecting the costs of legal uncertainty for parties relying on established precedent, but not so high as to negate the coordination function of an adaptive legal system. Suppression (0.20) is low, as this reading normalizes challenges to precedent rather than suppressing them. Theater ratio (0.10) is low, as the adaptive function is genuine, not merely performative. The trend shows a slight increase in extractiveness over time as the scope of reinterpretation has broadened, leading to more frequent challenges to settled law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and those seeking legal change, this framework is a necessary and beneficial adaptation. From the perspective of those relying on legal stability, it introduces an element of unpredictability and cost. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from the flexibility to adapt law (agenda_setter, low d). Litigants seeking normative change are beneficiaries (low d) as their path to legal reform is eased. Litigants relying on settled precedent are payers (high d) due to increased uncertainty. The legislature is 'excluded' in the sense that the judiciary's adaptive role reduces the urgency for legislative action on certain social issues, though the legislature retains ultimate law-making power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justification_for_overruling,
    'What constitutes a ''sufficient'' justification for reinterpreting or overruling precedent in this framework, and is this standard consistently applied?',
    'Empirical analysis of judicial opinions over time, categorizing stated justifications and assessing their consistency across different courts and legal domains.',
    'If justifications are inconsistent or arbitrary, the extractiveness for litigants relying on settled precedent would be higher, pushing the classification towards a ''tangled_rope'' due to unpredictable application of the adaptive rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_for_overruling, empirical, 'Consistency and clarity of standards for judicial reinterpretation.').

omega_variable(
    balance_of_stability_and_adaptation,
    'At what point does the pursuit of normative evolution undermine the core function of precedent in providing legal stability and predictability?',
    'Conceptual analysis of the ''rule of law'' principles, combined with empirical studies on the impact of frequent precedent changes on economic activity, social planning, and public trust in the legal system.',
    'If the balance is found to be skewed too far towards adaptation, the constraint''s overall coordination function would be compromised, potentially reclassifying it as a ''snare'' for those seeking stable legal guidance, or a ''piton'' if the adaptive function becomes purely performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balance_of_stability_and_adaptation, conceptual, 'The optimal balance between legal stability and adaptive evolution.').


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
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(comm_be_t1970, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2024, 0.35).

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
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. This 'evolutionary framework' reading emphasizes adaptation, while 'strict_stare_decisis' prioritizes stability and 'pluralist_balancing' seeks context-dependent flexibility. Each reading instantiates a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
