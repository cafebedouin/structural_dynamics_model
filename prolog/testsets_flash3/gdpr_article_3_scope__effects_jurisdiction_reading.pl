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
 *   This constraint describes the 'effects jurisdiction' reading of GDPR
 *   Article 3(2), which extends the regulation's reach to non-EU entities
 *   that target or monitor EU residents. This reading asserts that
 *   jurisdiction follows the effects of data processing on EU data subjects,
 *   regardless of the controller's location. It is a core component of the
 *   EU's strategy to protect fundamental rights in the digital age, but it
 *   imposes significant compliance burdens on global businesses and is
 *   contested by some non-EU governments on grounds of sovereignty.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: Primary beneficiary (organized/constrained)
 *   - eu_data_protection_authorities: Agenda setter (institutional/analytical)
 *   - non_eu_data_controllers: Primary payer (moderate/constrained)
 *   - global_tech_companies: Major payer (powerful/constrained)
 *   - non_eu_governments: Excluded (institutional/trapped)
 *   - international_law_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.65).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.78).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritoriality (Effects Jurisdiction Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, '2353faf5-e8c0-47f0-97f2-7aad9e6b693f').
narrative_ontology:cs_kernel_codification('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', fixed_text).
narrative_ontology:cs_authority_grounding('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', lineage).
narrative_ontology:cs_interpretation_layer_present('2353faf5-e8c0-47f0-97f2-7aad9e6b693f').
narrative_ontology:cs_reading_relation('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', foundational, data_protection_as_fundamental_right_extraterritorial).
narrative_ontology:cs_axiom_status(data_protection_as_fundamental_right_extraterritorial, holdable).
narrative_ontology:cs_axiom_grounding('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', data_protection_as_fundamental_right_extraterritorial, deontological).
narrative_ontology:cs_axiom('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', foundational, jurisdiction_follows_effects).
narrative_ontology:cs_axiom_status(jurisdiction_follows_effects, holdable).
narrative_ontology:cs_axiom_grounding('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', jurisdiction_follows_effects, conventional).
narrative_ontology:cs_reference_frame('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', eu_fundamental_rights_framework).
narrative_ontology:cs_drift_state('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2353faf5-e8c0-47f0-97f2-7aad9e6b693f', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, data_protection_as_fundamental_right).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__effects_jurisdiction_reading, long_arm_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive enhanced privacy protections and rights, even when their data is processed by entities outside the EU, under the premise that their fundamental rights follow them. Their ability to exit the digital economy is constrained, making this protection valuable.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, biographical, constrained, regional).

% Are empowered to enforce GDPR rules against non-EU entities that target or monitor EU residents. They issue guidance, investigate complaints, impose fines, and cooperate with international counterparts to ensure compliance, thereby extending the reach of EU law.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Bear significant compliance costs, including appointing EU representatives, conducting data protection impact assessments, and adhering to strict data processing principles, even if they have no physical presence in the EU. Exiting the EU market is often not a viable option due to its economic importance.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_data_controllers, payer,
    moderate, immediate, constrained, global).

% Face substantial legal and financial risks due to the GDPR's extraterritorial reach. They must adapt their global data processing operations to meet EU standards, often leading to a 'Brussels Effect' where EU standards become de facto global standards. Their market position makes exiting the EU market prohibitive.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, global_tech_companies, payer,
    powerful, biographical, constrained, global).

% Often view the GDPR's extraterritorial application as an overreach of EU sovereignty, infringing on their own regulatory authority over entities within their borders. They are largely excluded from the direct enforcement mechanisms but may engage in diplomatic or trade disputes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_governments, excluded,
    institutional, generational, trapped, national).

% Analyze the implications of the GDPR's extraterritoriality for traditional principles of international law, particularly regarding sovereignty and jurisdiction. They debate whether the 'effects doctrine' is a legitimate basis for such broad application.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified standard of data protection for EU residents, ensuring their privacy rights are upheld regardless of where their data is processed, thereby preventing a race to the bottom in privacy standards.
% TRANSFER_FUNCTION: Transfers compliance burden and enforcement power from individual EU residents to non-EU data controllers and EU data protection authorities, respectively, in exchange for enhanced privacy rights for EU residents.
% ABSENT_VOICES: Non-EU governments, particularly those with less stringent privacy regimes, are largely absent from the direct enforcement and policy-making process, despite the significant impact on their domestic industries and jurisdictional claims. They would argue for a more territorially bounded approach to regulation.
% DISAPPEARANCE_RATIONALE: If Article 3(2) vanished, non-EU data controllers would likely revert to less stringent data protection practices for EU residents, leading to a significant erosion of privacy rights. EU data protection authorities would lose a key enforcement tool, and the global landscape of data governance would fragment, with a race to the bottom in privacy standards.
% FOUNDING_PROBLEM: The rise of global digital services meant that personal data of EU residents was increasingly processed by entities outside the EU, beyond the reach of traditional territorial laws, leading to a gap in privacy protection.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and data protection authorities consistently attest that the problem of extraterritorial data processing remains live and requires robust protection. While non-EU entities contest the scope of the solution, the underlying problem of data flowing across borders is universally acknowledged.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the substantial compliance costs borne by non-EU entities, which are often disproportionate to the direct benefits they receive from the EU market. Suppression (0.78) is high due to the EU's robust enforcement mechanisms (fines, adequacy decisions) and the lack of viable alternatives for global companies wishing to access the lucrative EU market. The theater ratio is low (0.1) as the enforcement is genuine and effective, not merely performative. Accessibility collapse is high (0.7) because once a non-EU entity targets EU residents, the GDPR's requirements become unavoidable.
 *
 * PERSPECTIVAL GAP:
 *   EU data subjects and authorities perceive this as a necessary and legitimate extension of fundamental rights protection, a coordination mechanism to ensure a high standard of privacy. Non-EU data controllers and global tech companies, however, experience it as a highly extractive and suppressive regulatory burden, a unilateral assertion of jurisdiction that imposes significant costs without commensurate benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects are clear beneficiaries (d=0.0-0.1) as they gain protection. EU data protection authorities are agenda setters and beneficiaries (d=0.0-0.1) as they gain power and enforce the regime. Non-EU data controllers and global tech companies are targets (d=0.8-1.0) as they bear the costs and face enforcement. Non-EU governments are excluded (d=1.0) as they are subject to the effects without direct participation in the rule-making or enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate to protect EU data subjects in a globalized digital economy is still very much live. The classification as a Tangled Rope reflects the genuine coordination function (unified privacy standards) intertwined with significant asymmetric extraction (compliance costs for non-EU entities) and active enforcement. It prevents mislabeling as a Snare by acknowledging the real coordination problem it addresses, while still highlighting the extractive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_effects_doctrine,
    'Is the ''effects doctrine'' a legitimate basis for extraterritorial jurisdiction under international law, or does it represent an overreach of sovereign power?',
    'Evolution of customary international law through state practice and opinio juris, or a landmark ruling by an international court on the limits of extraterritoriality.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be re-evaluated as purely coercive, potentially shifting its classification towards a Snare. If affirmed, its legitimacy as a Tangled Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_effects_doctrine, conceptual, 'Debate over the legal basis for extraterritorial application of domestic law.').

omega_variable(
    proportionality_of_compliance_costs,
    'Are the compliance costs imposed on non-EU entities proportionate to the privacy benefits conferred upon EU data subjects?',
    'Independent economic analysis comparing the aggregate costs of GDPR compliance for non-EU entities against the quantifiable benefits of enhanced data protection for EU residents.',
    'If costs are found to be disproportionate, the extractiveness metric would be confirmed as high, reinforcing the Tangled Rope classification and potentially leading to calls for regulatory adjustments. If proportionate, the coordination aspect would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_compliance_costs, empirical, 'Assessment of the economic balance between compliance burden and privacy benefit.').

omega_variable(
    kernel_reading_difference,
    'This constraint is one reading of the ''gdpr_article_3_scope'' kernel. How would the classification change under the ''territorial_sovereignty_reading'' or ''market_access_reading''?',
    'Analyze the structural properties (beneficiaries, victims, enforcement) of the alternative readings as separate constraint stories.',
    'The ''territorial_sovereignty_reading'' would likely classify the extraterritorial application as a Snare due to perceived illegitimate coercion. The ''market_access_reading'' might classify it as a Rope or Tangled Rope, emphasizing the voluntary nature of market participation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2, 0.08).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 8, 0.1).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 4, 0.63).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 8, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2, 0.73).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 8, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, eu_us_data_transfer_frameworks).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, global_privacy_standard_setting).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the GDPR Article 3(2) extraterritorial scope kernel. This 'effects jurisdiction' reading emphasizes the protection of EU data subjects' fundamental rights wherever their data is processed. Sibling readings include 'territorial_sovereignty_reading' (emphasizing limits of national jurisdiction) and 'market_access_reading' (emphasizing the GDPR as a condition for accessing the EU market).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
