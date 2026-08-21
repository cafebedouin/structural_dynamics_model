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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Scope Bounded by Territorial Sovereignty
 *   domain: international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'territorial sovereignty' reading
 *   of GDPR Article 3's scope. From this perspective, a state's regulatory
 *   authority is fundamentally bounded by its physical territory. Any attempt
 *   by the GDPR to apply extraterritorially (e.g., to non-EU companies
 *   processing data outside the EU) is seen as exceeding legitimate
 *   regulatory authority and infringing on the sovereignty of other states.
 *   The constraint itself, therefore, functions as a protective boundary,
 *   coordinating state behavior by defining limits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.2).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.6).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Scope Bounded by Territorial Sovereignty").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'f2a8ac14-699a-49f3-906f-c8d77dc9db0c').
narrative_ontology:cs_kernel_codification('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', formalized).
narrative_ontology:cs_authority_grounding('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', lineage).
narrative_ontology:cs_interpretation_layer_present('f2a8ac14-699a-49f3-906f-c8d77dc9db0c').
narrative_ontology:cs_reading_relation('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', foundational, jurisdiction_is_territorial).
narrative_ontology:cs_axiom_status(jurisdiction_is_territorial, holdable).
narrative_ontology:cs_axiom_grounding('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', jurisdiction_is_territorial, conventional).
narrative_ontology:cs_reference_frame('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', digital_globalization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2a8ac14-699a-49f3-906f-c8d77dc9db0c', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively defend their sovereign right to regulate within their borders, resisting extraterritorial application of foreign laws like the GDPR. They benefit from maintaining their regulatory independence and avoiding compliance burdens imposed by other jurisdictions.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from the limitation of GDPR's extraterritorial reach, reducing their compliance burden and legal exposure outside the EU. They may still choose to comply with some GDPR provisions for market access, but this reading asserts they are not legally compelled by EU jurisdiction outside EU territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_companies, beneficiary,
    powerful, biographical, constrained, global).

% Seek to extend GDPR's protections to EU citizens' data globally, but are constrained by the principle of territorial sovereignty. They bear the cost of jurisdictional disputes and the inability to enforce GDPR fully outside the EU, which limits their regulatory authority.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, agenda_setter).

% May experience reduced data protection when their data is processed by non-EU entities outside the EU, as this reading limits the reach of GDPR enforcement. They bear the cost of potentially weaker privacy safeguards in such scenarios.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_citizens, payer,
    powerless, biographical, trapped, global).

% Analyze and interpret the principles of international law, including territorial sovereignty and its application to digital governance. They provide academic commentary on jurisdictional conflicts and the evolution of legal norms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior by establishing clear and mutually recognized boundaries for regulatory authority, preventing jurisdictional overreach and fostering predictable international relations.
% TRANSFER_FUNCTION: Prevents the transfer of regulatory authority from non-EU states to the EU, preserving the sovereign right of each state to govern within its own territory.
% ABSENT_VOICES: EU privacy advocates who prioritize universal data protection for EU citizens, regardless of where their data is processed, would object to any limitation on GDPR's extraterritorial reach. They are often not directly involved in inter-state jurisdictional negotiations.
% DISAPPEARANCE_RATIONALE: If the principle of territorial sovereignty vanished, states would assert jurisdiction over any activity affecting their interests globally, leading to widespread legal chaos, conflicting regulations, and a breakdown of international legal order. The global regulatory landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: To prevent states from unilaterally extending their laws beyond their borders, which would infringe upon the sovereignty of other nations and lead to international conflict and legal uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: The principle is enshrined in numerous international treaties (e.g., UN Charter), customary international law, and is consistently affirmed by legal scholarship and diplomatic practice from a wide range of states, not just those directly benefiting from a specific application.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).
:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.20) because the constraint's primary function, from this reading, is to prevent extraction of regulatory authority by one state from another, thus protecting the beneficiaries (non-EU states/companies). Suppression is moderate (0.60) because active legal and diplomatic efforts are required by non-EU states to resist and suppress the EU's attempts at extraterritorial application. Resistance is high (0.70) as the EU actively challenges this interpretation through its own legal doctrines. Theater ratio is low (0.10) as territorial sovereignty is a fundamental, non-performative principle of international law. Accessibility collapse is moderate (0.40) because while it preserves regulatory alternatives for non-EU states, it collapses the EU's alternative of universal jurisdiction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-EU states, this constraint is a fundamental 'rope' coordinating international relations and protecting sovereignty. From the perspective of EU regulators, it is a 'snare' or 'tangled rope' that unjustly limits their ability to protect their citizens' data globally, forcing them to bear costs in jurisdictional disputes. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU states and companies are beneficiaries (low d) as the constraint protects their regulatory independence and reduces compliance burdens. EU regulators are targets/payers (high d) as their expansive jurisdictional goals are curtailed. EU citizens are also payers (high d) as their data protection may be reduced when processed by non-EU entities outside the EU, due to the limits this constraint imposes on GDPR's reach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_regulatory_authority_ambiguity,
    'What constitutes ''legitimate regulatory authority'' in the context of global digital services, and how does it reconcile with traditional territorial principles?',
    'Development of new international legal norms or treaties specifically addressing digital jurisdiction, or a consensus shift in customary international law.',
    'If ''legitimate authority'' is re-defined to include effects-based or market-access criteria, this reading''s core premise would be weakened, potentially shifting its classification towards a more extractive type for non-EU entities. If territoriality is reaffirmed, its protective function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_regulatory_authority_ambiguity, conceptual, 'Ambiguity in defining legitimate regulatory authority in the digital age.').

omega_variable(
    data_localization_efficacy,
    'To what extent does data localization (a resistance mechanism for this reading) genuinely protect national sovereignty and privacy, versus merely creating economic friction and fragmenting the internet?',
    'Empirical studies on the effectiveness of data localization mandates in achieving their stated goals (e.g., security, privacy, law enforcement access) compared to their economic and technical costs.',
    'If data localization is found to be ineffective or counterproductive, the practical means of upholding this reading''s principles would be undermined, potentially leading to a re-evaluation of its viability as a protective mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_localization_efficacy, empirical, 'Efficacy of data localization as a resistance mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 2018, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2024, 0.11).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2026, 0.11).
narrative_ontology:measurement(gdpr_tr_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2028, 0.12).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2020, 0.19).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2022, 0.2).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2024, 0.2).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2026, 0.21).
narrative_ontology:measurement(gdpr_be_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2028, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2026, 0.64).
narrative_ontology:measurement(gdpr_su_t2028, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2028, 0.66).


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
