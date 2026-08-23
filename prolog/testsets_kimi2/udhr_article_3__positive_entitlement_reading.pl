% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 Positive Entitlement Reading
 *   domain: constitutional/law/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the positive entitlement reading of UDHR
 *   Article 3, interpreting 'life, liberty and security of person' as
 *   obligating states to provide welfare, healthcare, housing, and
 *   restricting speech that threatens protected groups' security. The reading
 *   generates high extractiveness through wealth redistribution and
 *   expressive limitations, while coordinating material survival for
 *   vulnerable populations. It is claimed as tangled_rope: genuine
 *   coordination function (material provision for survival) combined with
 *   asymmetric extraction (systematic redistribution from property and
 *   expression rights holders). The metrics are authored independently of the
 *   claim.
 *
 * KEY AGENTS:
 *   - vulnerable_groups: Primary beneficiary (powerless/constrained) â receive material provision
 *   - state_welfare_apparatus: Agenda setter (institutional/constrained) â administers extraction and distribution
 *   - property_rights_holders: Primary payer (powerful/mobile) â bear tax and regulatory extraction
 *   - expression_rights_holders: Secondary payer (moderate/constrained) â bear expressive restrictions
 *   - human_rights_treaty_bodies: Analytical observer (institutional/analytical) â monitors and interprets compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.72).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 Positive Entitlement Reading").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional/law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '9b9cd8c2-3119-46b3-82f7-9c965c0e62da').
narrative_ontology:cs_kernel_codification('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', formalized).
narrative_ontology:cs_authority_grounding('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', lineage).
narrative_ontology:cs_interpretation_layer_present('9b9cd8c2-3119-46b3-82f7-9c965c0e62da').
narrative_ontology:cs_reading_relation('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', foundational, state_owes_material_conditions_to_all).
narrative_ontology:cs_axiom_status(state_owes_material_conditions_to_all, holdable).
narrative_ontology:cs_axiom_grounding('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', state_owes_material_conditions_to_all, deontological).
narrative_ontology:cs_axiom('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', foundational, security_of_person_requires_socioeconomic_protection).
narrative_ontology:cs_axiom_status(security_of_person_requires_socioeconomic_protection, holdable).
narrative_ontology:cs_axiom_grounding('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', security_of_person_requires_socioeconomic_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', universal_material_protection_mandate).
narrative_ontology:cs_drift_state('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', contemporary_austerity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9b9cd8c2-3119-46b3-82f7-9c965c0e62da', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_groups).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_rights_holders).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, socioeconomic_rights_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, positive_state_obligations_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on state provision of welfare, healthcare, and housing interpreted as constitutional rights under Article 3; lack independent material security; politically organized around defending entitlement programs but individually dependent on bureaucratic allocation decisions and eligibility rules.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_groups, beneficiary,
    powerless, immediate, constrained, national).

% Administers tax-funded welfare, healthcare, and housing programs; interprets eligibility and delivery rules; captures budgetary authority and staffing from the constitutional mandate; politically constrained by fiscal limits, electoral cycles, and treaty-body oversight.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_welfare_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Bear progressive taxation, regulatory takings, and redistribution mechanisms funding material provision; can relocate capital or change jurisdictions but face coordination costs, legal barriers to full exit, and political sanctions for doing so.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_rights_holders, payer,
    powerful, biographical, mobile, national).

% Face expressive limitations including hate speech restrictions justified as protecting vulnerable groups' security; counter-speech is chilled by legal penalties and social sanction; limited exit short of emigration or complete withdrawal from public discourse.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_rights_holders, payer,
    moderate, biographical, constrained, national).

% Monitor state compliance with Article 3 positive obligations; issue general comments and recommendations expanding the scope of material provision; do not directly administer or benefit from extraction but shape interpretive drift.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, vulnerable_groups).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the provision of material survival conditions (welfare, healthcare, housing) to populations who would otherwise lack them, through centralized state allocation, taxation, and redistribution.
% TRANSFER_FUNCTION: Moves financial and material resources from property holders and taxed populations to vulnerable groups via state welfare programs; moves expressive latitude from speakers to protected-group security through hate speech restrictions justified under security-of-person.
% ABSENT_VOICES: Libertarian constitutional scholars and mobile capital holders who reject positive entitlements are structurally marginalized in human rights forums; their objections are treated as anti-rights rather than rival rights-claims.
% DISAPPEARANCE_RATIONALE: If the positive entitlement reading vanished, welfare guarantees would lose constitutional supremacy and revert to ordinary legislative discretion; housing and healthcare markets would reorganize around private provision and charity; hate speech restrictions would narrow to direct incitement; the political economy would shift from redistribution to negative-liberty frameworks.
% FOUNDING_PROBLEM: Post-war devastation and industrial poverty left large populations without material conditions necessary for life and human dignity; the original kernel was partly motivated by preventing state indifference to destitution.
% FOUNDING_PROBLEM_CORROBORATION: Social historians and left constitutionalists attest the founding problem of material deprivation persists in the form of homelessness and medical debt. Libertarian legal scholars and public-choice economists attest that absolute destitution is largely solved in developed economies and the arrangement now functions as contested redistribution. International human rights monitors corroborate ongoing deprivation in some regions but their institutional mandate aligns with the beneficiary position.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers resources and expressive rights under constitutional compulsion. Suppression (0.68) reflects active enforcement: taxation, speech policing, housing allocation. Theater ratio (0.35) captures performative human rights rhetoric that partially obscures distributional conflict. Accessibility collapse (0.65) is high because constitutional entrenchment makes negative-liberty alternatives politically inaccessible. Resistance (0.60) is substantial from propertied and libertarian quarters. Temporal measurements trace the reading's intensification from post-war textual adoption through ICESCR development to contemporary expansive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable_groups seat experiences the constraint as life-sustaining coordination; property and expression rights seats experience it as coercive extraction. The state apparatus experiences it as a mandate with bureaucratic expansion and fiscal pressure. The engine computes these divergences from structural position, not from authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable_groups are near d=0.0 (full beneficiary) because the constraint subsidizes their material existence. Property_rights_holders sit near d=0.85 (near-target) despite mobile exit because the tax burden is inescapable at national scope; expression_rights_holders sit near d=0.95 (full target) due to constrained exit and direct suppression. State_welfare_apparatus sits near d=0.3 (partial beneficiary/agenda setter) because it captures authority and budget while being bound by the mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by naming both the coordination problem (material deprivation) and the extraction mechanism (redistribution, speech restriction). Without the coordination function, it would be a snare; without the victim group, it would be rope. Both are present and structurally linked through the same constitutional reading, satisfying the tangled_rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_threshold_expandability,
    'Is the threshold of ''material conditions necessary for life and security'' fixed by biological survival or politically expandable to include comfort, dignity, and cultural participation?',
    'Comparative constitutional corpus analysis tracking how courts and treaty bodies define the minimum over time.',
    'If expandable without bound, extraction ratchets upward and the coordination function becomes unbounded; if fixed, extraction is constrained to genuine survival necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_threshold_expandability, conceptual, 'Ambiguity about whether the material threshold is fixed or expandable').

omega_variable(
    extraction_overhead_vs_transfer,
    'Does the extracted tax and regulatory capacity fund direct material transfer to vulnerable groups, or is a significant share captured as bureaucratic overhead and institutional rent?',
    'Public finance audits and service-delivery effectiveness studies measuring overhead ratios in welfare bureaucracies.',
    'High overhead would shift the constraint toward snare by divorcing extraction from coordination; low overhead supports the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_overhead_vs_transfer, empirical, 'Whether extracted resources reach beneficiaries or are dissipated as overhead').

omega_variable(
    positive_negative_compatibility,
    'Does the positive entitlement reading of Article 3 logically foreclose the negative liberty reading within a single constitutional framework, or can both coexist?',
    'Jurisprudential analysis of legal systems that have incorporated both readings versus those where one has displaced the other.',
    'If foreclosing, the kernel is structurally irreconcilable; if coexisting, the divergence is perspectival rather than logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_negative_compatibility, conceptual, 'Whether positive and negative readings are mutually exclusive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(udhr_tr_t45, udhr_article_3__positive_entitlement_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__positive_entitlement_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__positive_entitlement_reading, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(udhr_be_t45, udhr_article_3__positive_entitlement_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__positive_entitlement_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__positive_entitlement_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(udhr_su_t45, udhr_article_3__positive_entitlement_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__positive_entitlement_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__positive_entitlement_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the positive entitlement reading of the UDHR Article 3 kernel, decomposed per the epsilon-invariance principle because its structurally distinct epsilon, beneficiary/victim profile, and failure modes differ from its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
