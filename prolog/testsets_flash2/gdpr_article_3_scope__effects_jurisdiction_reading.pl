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
 *   human_readable: GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint describes the GDPR's extraterritorial application under
 *   Article 3(2), specifically through the 'effects jurisdiction' reading.
 *   This interpretation asserts that GDPR applies to non-EU entities if their
 *   processing activities target or monitor individuals in the EU, regardless
 *   of where the processing takes place. This reading emphasizes the
 *   protection of EU data subjects and the regulatory reach of EU
 *   authorities, leading to significant compliance burdens for non-EU
 *   companies.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: Primary beneficiary (powerless/constrained)
 *   - eu_regulatory_authorities: Agenda setter (institutional/analytical)
 *   - non_eu_data_controllers: Primary payer (powerful/constrained)
 *   - non_eu_data_processors: Secondary payer (moderate/constrained)
 *   - non_eu_governments: Excluded (institutional/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.65).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.75).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, 'a514b165-05b4-4e3a-bd54-c3a68d8f7674').
narrative_ontology:cs_kernel_codification('a514b165-05b4-4e3a-bd54-c3a68d8f7674', fixed_text).
narrative_ontology:cs_authority_grounding('a514b165-05b4-4e3a-bd54-c3a68d8f7674', lineage).
narrative_ontology:cs_interpretation_layer_present('a514b165-05b4-4e3a-bd54-c3a68d8f7674').
narrative_ontology:cs_reading_relation('a514b165-05b4-4e3a-bd54-c3a68d8f7674', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_reading_relation('a514b165-05b4-4e3a-bd54-c3a68d8f7674', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('a514b165-05b4-4e3a-bd54-c3a68d8f7674', foundational, data_protection_follows_person).
narrative_ontology:cs_axiom_status(data_protection_follows_person, holdable).
narrative_ontology:cs_axiom_grounding('a514b165-05b4-4e3a-bd54-c3a68d8f7674', data_protection_follows_person, deontological).
narrative_ontology:cs_axiom('a514b165-05b4-4e3a-bd54-c3a68d8f7674', foundational, effects_doctrine_justifies_jurisdiction).
narrative_ontology:cs_axiom_status(effects_doctrine_justifies_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('a514b165-05b4-4e3a-bd54-c3a68d8f7674', effects_doctrine_justifies_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('a514b165-05b4-4e3a-bd54-c3a68d8f7674', eu_regulatory_autonomy_and_data_subject_rights).
narrative_ontology:cs_drift_state('a514b165-05b4-4e3a-bd54-c3a68d8f7674', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a514b165-05b4-4e3a-bd54-c3a68d8f7674', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_processors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive enhanced privacy protection and rights, even when their data is processed by entities outside the EU, provided those entities target or monitor them. Their ability to enforce these rights is mediated by EU authorities.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    powerless, biographical, constrained, regional).

% Are empowered to enforce GDPR rules against non-EU entities that process the data of EU residents, extending their regulatory reach. They issue guidance, conduct investigations, and levy fines, asserting jurisdiction based on the effects within the EU.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_regulatory_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Bear significant compliance costs to meet GDPR requirements, even if they have no physical presence in the EU, if their activities target or monitor EU residents. Their options are to comply, cease targeting EU residents, or risk substantial fines.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers, payer,
    powerful, biographical, constrained, global).

% Are also subject to GDPR obligations when processing data on behalf of non-EU controllers targeting EU residents. They face similar compliance burdens and risks as controllers, often with less leverage.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_processors, payer,
    moderate, biographical, constrained, global).

% Often view the extraterritorial application of GDPR as an overreach of EU sovereignty, infringing on their own regulatory authority over entities within their borders. They are excluded from the EU's unilateral assertion of jurisdiction but must contend with its effects on their domestic companies.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent standard for data protection for EU residents, regardless of where their data is processed, ensuring a baseline of privacy rights across borders.
% TRANSFER_FUNCTION: Transfers compliance costs and regulatory risk from EU data subjects and EU authorities to non-EU data controllers and processors, in exchange for extended data protection.
% ABSENT_VOICES: Non-EU governments, who would argue for a more territorially constrained view of jurisdiction and challenge the EU's unilateral assertion of regulatory authority over their domestic entities.
% DISAPPEARANCE_RATIONALE: If Article 3(2) vanished, non-EU entities would likely revert to their local data protection standards, leading to a fragmentation of privacy rights for EU residents and a significant reduction in the EU's global regulatory influence. The digital economy would reorganize around more fragmented jurisdictional claims.
% FOUNDING_PROBLEM: The internet's borderless nature allowed companies outside the EU to process the data of EU residents without being subject to EU privacy laws, creating a regulatory gap and undermining data subject rights.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and data protection advocates consistently attest that the problem of extraterritorial data processing without adequate protection remains live. While non-EU entities acknowledge the GDPR's reach, they often contest the necessity or proportionality of its extraterritorial application, but do not deny the underlying problem of cross-border data flows.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial due to the high compliance costs imposed on non-EU entities, often disproportionate to their direct engagement with the EU market. Suppression (0.75) is high because non-EU entities have limited options to avoid GDPR's reach if they wish to interact with EU residents, and enforcement mechanisms (fines, adequacy decisions) are robust. The theater ratio is low (0.1) as the enforcement is genuinely aimed at compliance, not merely performance. Accessibility collapse is high (0.7) because the 'targeting or monitoring' test is broad, making it difficult for non-EU entities to operate globally without encountering GDPR's scope. Resistance (0.4) is moderate, primarily from non-EU governments and industry groups, but often results in compliance rather than outright defiance due to the high stakes.
 *
 * PERSPECTIVAL GAP:
 *   EU regulatory authorities perceive this as a necessary and legitimate extension of protection, a 'rope' ensuring a level playing field for privacy. Non-EU data controllers and processors, however, experience it as a 'snare' or 'tangled rope' due to the significant compliance costs and the perceived overreach of jurisdiction. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are full beneficiaries (d=0.0) as the constraint directly enhances their rights. EU regulatory authorities are also beneficiaries (d=0.1) as their power and influence are extended. Non-EU data controllers and processors are targets (d=0.9) due to the high compliance costs and limited exit options. Non-EU governments are excluded, bearing indirect costs through their domestic industries without direct participation in the regulatory framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting EU data subjects in a global digital economy) remains highly relevant and actively pursued. There is no evidence of mandatrophy; the enforcement is robust and the problem it addresses is ongoing. The classification as a Tangled Rope reflects the genuine coordination function (consistent data protection) intertwined with asymmetric extraction (compliance costs for non-EU entities).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_monitoring_test_ambiguity,
    'How consistently and broadly is the ''targeting or monitoring'' test applied across different EU member states and regulatory authorities?',
    'Analysis of enforcement actions and court rulings across multiple jurisdictions over time, identifying patterns of interpretation and application.',
    'If application is highly inconsistent, it increases uncertainty and compliance burden for non-EU entities, potentially raising effective extraction. If consistent, it strengthens the legitimacy of the effects jurisdiction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_monitoring_test_ambiguity, empirical, 'Ambiguity in the practical application of the targeting/monitoring test.').

omega_variable(
    proportionality_of_compliance_costs,
    'Are the compliance costs imposed on non-EU entities proportional to the privacy risks they pose to EU data subjects?',
    'Independent economic impact assessments comparing compliance costs with quantified privacy risk reduction and market benefits for EU data subjects.',
    'If costs are disproportionate, it strengthens the ''snare'' aspect of the Tangled Rope, indicating excessive extraction. If proportional, it reinforces the ''rope'' aspect, justifying the costs as necessary for coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_compliance_costs, empirical, 'Whether compliance costs are proportional to privacy benefits.').

omega_variable(
    legitimacy_of_extraterritorial_jurisdiction,
    'Is the EU''s assertion of extraterritorial jurisdiction based on effects a legitimate exercise of regulatory power under international law, or an overreach?',
    'International legal scholarship, state practice, and rulings from international tribunals (if applicable) on the limits of extraterritorial jurisdiction.',
    'If widely accepted as legitimate, it strengthens the constraint''s foundation. If widely contested as overreach, it could undermine long-term enforceability and increase resistance from non-EU governments, potentially shifting the constraint towards a more coercive ''snare'' if maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_extraterritorial_jurisdiction, conceptual, 'The international legal legitimacy of effects-based extraterritorial jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.63).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.73).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gdpr_article_3_scope kernel. Other readings include 'market_access_reading' and 'territorial_sovereignty_reading', which offer alternative interpretations of GDPR's extraterritorial reach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
