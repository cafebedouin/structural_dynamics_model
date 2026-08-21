% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Scope (Effects Jurisdiction Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint describes the extraterritorial application of GDPR
 *   Article 3(2) through the 'effects jurisdiction' reading, where
 *   jurisdiction is asserted over data controllers outside the EU if their
 *   processing activities target or monitor EU residents. This reading
 *   emphasizes the protection of EU fundamental rights regardless of
 *   geographical location. It imposes significant compliance burdens on
 *   non-EU entities while aiming to provide comprehensive data protection for
 *   EU citizens. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates data protection for EU residents but does so through a
 *   structure that extracts substantial compliance costs and asserts broad
 *   regulatory power over non-EU actors.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: Primary agenda_setter (institutional/analytical) — enforces the constraint and benefits from expanded authority.
 *   - eu_data_subjects: Primary beneficiary (moderate/constrained) — receives enhanced privacy protection.
 *   - non_eu_data_controllers: Primary payer (powerful/constrained) — bears significant compliance costs and legal risks.
 *   - global_tech_companies: Major payer (institutional/constrained) — faces immense compliance burdens and scrutiny.
 *   - non_eu_governments: Excluded actor (institutional/constrained) — objects to extraterritoriality but has limited direct influence.
 *   - international_law_scholars: Analytical observer (analytical/analytical) — analyzes legal legitimacy and implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.82).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.78).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Scope (Effects Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, 'e98591f4-d75d-490c-8692-a893d4be7cae').
narrative_ontology:cs_kernel_codification('e98591f4-d75d-490c-8692-a893d4be7cae', fixed_text).
narrative_ontology:cs_authority_grounding('e98591f4-d75d-490c-8692-a893d4be7cae', lineage).
narrative_ontology:cs_interpretation_layer_present('e98591f4-d75d-490c-8692-a893d4be7cae').
narrative_ontology:cs_reading_relation('e98591f4-d75d-490c-8692-a893d4be7cae', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e98591f4-d75d-490c-8692-a893d4be7cae', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('e98591f4-d75d-490c-8692-a893d4be7cae', foundational, data_protection_as_fundamental_right).
narrative_ontology:cs_axiom_status(data_protection_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('e98591f4-d75d-490c-8692-a893d4be7cae', data_protection_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('e98591f4-d75d-490c-8692-a893d4be7cae', foundational, jurisdiction_follows_harm_or_targeting).
narrative_ontology:cs_axiom_status(jurisdiction_follows_harm_or_targeting, holdable).
narrative_ontology:cs_axiom_grounding('e98591f4-d75d-490c-8692-a893d4be7cae', jurisdiction_follows_harm_or_targeting, conventional).
narrative_ontology:cs_reference_frame('e98591f4-d75d-490c-8692-a893d4be7cae', eu_fundamental_rights_framework).
narrative_ontology:cs_drift_state('e98591f4-d75d-490c-8692-a893d4be7cae', contemporary_digital_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e98591f4-d75d-490c-8692-a893d4be7cae', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting and enforcing GDPR's extraterritorial scope, issuing guidance, imposing fines, and cooperating with other EU DPAs. They benefit from expanded regulatory authority and the ability to protect EU residents globally.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Receive enhanced privacy protection and rights under GDPR, even when their data is processed by entities outside the EU. They are the primary intended beneficiaries of this extraterritorial reach, experiencing a higher standard of data governance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    moderate, biographical, constrained, regional).

% Bear significant compliance costs, legal risks, and potential fines if they process data of EU residents by targeting or monitoring them. They must adapt their global operations to EU standards or risk losing access to the EU market.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers, payer,
    powerful, biographical, constrained, global).

% As large-scale data processors, they face immense compliance burdens and scrutiny under GDPR's extraterritorial rules. They often engage in extensive legal and technical efforts to comply, or lobby against the broad interpretation of jurisdiction.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies, payer,
    institutional, biographical, constrained, global).

% Often object to the extraterritorial reach of GDPR as an infringement on their national sovereignty and regulatory authority. While they can express diplomatic resistance, they have limited direct power to prevent its application to entities targeting EU residents.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, constrained, national).

% Analyze the legal implications, legitimacy, and precedents set by GDPR's extraterritorial jurisdiction under public international law. They provide critical commentary and theoretical frameworks for understanding the constraint.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform and high standard for data protection for EU residents, ensuring their privacy rights are upheld regardless of where their personal data is processed globally, thereby fostering trust in the digital single market.
% TRANSFER_FUNCTION: Transfers the primary burden of ensuring EU data protection standards from individual EU data subjects to non-EU data controllers, and transfers enforcement power to EU data protection authorities.
% ABSENT_VOICES: Smaller non-EU businesses and some non-EU governments, who would argue for a more limited, territorially-bound jurisdiction due to disproportionate compliance costs and perceived infringements on sovereignty. Their voices are often heard through lobbying or diplomatic channels, but not directly in the EU's legislative or enforcement processes.
% DISAPPEARANCE_RATIONALE: If GDPR's extraterritorial scope vanished overnight, EU residents' data processed outside the EU would immediately lose its current level of protection, leading to a fragmented and less secure digital environment. This would undermine the GDPR's core objectives and necessitate a complete re-evaluation of data governance strategies for EU institutions and businesses.
% FOUNDING_PROBLEM: The borderless nature of the internet and global data flows allowed personal data of EU residents to be processed by entities outside the EU, often under weaker privacy regimes, thereby circumventing EU privacy laws and leaving citizens vulnerable to data exploitation.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions, data protection advocates, and the European Data Protection Board (EDPB) consistently attest that the founding problem of global data flows and privacy challenges remains live and requires robust extraterritorial application. Some non-EU governments and industry groups, however, contest the necessity of such broad extraterritoriality, arguing the problem is manageable through other means.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) due to the substantial compliance costs, legal fees, and potential fines imposed on non-EU entities. Suppression is also high (0.78) because non-EU data controllers have limited options other than compliance if they wish to interact with EU residents or the EU market; the enforcement mechanisms are robust. Theater ratio is low (0.15) as the enforcement is genuine and directly tied to the stated purpose of data protection, not merely performative. Accessibility collapse is high (0.85) because for entities targeting EU residents, there are virtually no legal alternatives to complying with GDPR. Resistance is moderate (0.55) from non-EU governments and industry groups, but it has not fundamentally altered the application of the rule.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU data subjects and authorities, this constraint is a necessary and legitimate extension of fundamental rights protection. From the perspective of non-EU data controllers and some non-EU governments, it is an overreach of jurisdiction that imposes disproportionate burdens and infringes on sovereignty. The engine's per-seat classification will reflect these divergent experiences based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are beneficiaries (low d) as they gain protection. EU data protection authorities are agenda-setters and beneficiaries (low d) as they gain regulatory power and achieve their mandate. Non-EU data controllers and global tech companies are payers (high d) as they bear the costs of compliance and face enforcement. Non-EU governments are excluded, bearing indirect costs of their entities' compliance without direct benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_extraterritoriality,
    'Is the extraterritorial application of GDPR Article 3(2) a legitimate exercise of jurisdiction under public international law, or does it constitute regulatory overreach?',
    'Decisions by international courts or tribunals, or widespread adoption of similar ''effects-based'' jurisdiction principles in other major legal systems, would clarify its status.',
    'If deemed illegitimate, the constraint''s legal foundation would be weakened, potentially leading to increased resistance, diplomatic disputes, and challenges to enforcement. If widely accepted, its legitimacy would be solidified, reducing resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_extraterritoriality, conceptual, 'Ambiguity regarding the international legal legitimacy of GDPR''s extraterritorial scope.').

omega_variable(
    compliance_burden_on_small_entities,
    'What is the actual, disproportionate compliance burden on small and medium-sized non-EU data controllers compared to large corporations, and does it hinder their ability to serve EU residents?',
    'Empirical studies and surveys specifically targeting SMEs outside the EU, analyzing their costs, operational changes, and market access decisions related to GDPR compliance.',
    'If the burden is found to be disproportionately high, it could lead to policy adjustments (e.g., simplified compliance for SMEs) or increased calls for a more nuanced application of the extraterritorial rules, potentially reducing extractiveness for this segment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_burden_on_small_entities, empirical, 'Disproportionate compliance costs for smaller non-EU entities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2048).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gdpr_tr_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2023, 0.12).
narrative_ontology:measurement(gdpr_tr_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2028, 0.14).
narrative_ontology:measurement(gdpr_tr_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2033, 0.15).
narrative_ontology:measurement(gdpr_tr_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2038, 0.15).
narrative_ontology:measurement(gdpr_tr_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2048, 0.15).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(gdpr_be_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2023, 0.75).
narrative_ontology:measurement(gdpr_be_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2028, 0.79).
narrative_ontology:measurement(gdpr_be_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2033, 0.81).
narrative_ontology:measurement(gdpr_be_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2038, 0.82).
narrative_ontology:measurement(gdpr_be_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2048, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(gdpr_su_t2023, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(gdpr_su_t2028, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2028, 0.75).
narrative_ontology:measurement(gdpr_su_t2033, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2033, 0.77).
narrative_ontology:measurement(gdpr_su_t2038, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2038, 0.78).
narrative_ontology:measurement(gdpr_su_t2048, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2048, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_data_portability).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_transfer_framework).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the GDPR Article 3 scope kernel. It focuses on the 'effects jurisdiction' aspect, while sibling constraints address 'market access' and 'territorial sovereignty' interpretations. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
