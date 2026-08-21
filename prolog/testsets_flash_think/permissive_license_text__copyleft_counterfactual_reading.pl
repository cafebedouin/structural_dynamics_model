% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint is the 'copyleft_counterfactual_reading' of the
 *   'permissive_license_text' kernel. It views permissive licenses, in the
 *   absence of reciprocity requirements, as mechanisms that enable
 *   exploitation of open-source contributions by proprietary interests.
 *   Sibling readings include 'commons_coordination_reading' (which emphasizes
 *   freedom of use) and 'corporate_moat_reading' (which focuses on the
 *   strategic advantage for corporations). This reading argues that while
 *   permissive licenses facilitate broad adoption, they also create a
 *   structural vulnerability for the open-source commons, enabling a one-way
 *   transfer of value to proprietary entities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.75).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'ef758482-48fe-4b1e-a1bf-53ca8e2ff92c').
narrative_ontology:cs_kernel_codification('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', fixed_text).
narrative_ontology:cs_authority_grounding('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', practice).
narrative_ontology:cs_interpretation_layer_present('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c').
narrative_ontology:cs_reading_relation('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', permissive_license_text__commons_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', foundational, reciprocity_is_ethical_minimum).
narrative_ontology:cs_axiom_status(reciprocity_is_ethical_minimum, holdable).
narrative_ontology:cs_axiom_grounding('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', reciprocity_is_ethical_minimum, deontological).
narrative_ontology:cs_axiom('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', foundational, unrestricted_reuse_enables_exploitation).
narrative_ontology:cs_axiom_status(unrestricted_reuse_enables_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', unrestricted_reuse_enables_exploitation, empirically_contingent).
narrative_ontology:cs_reference_frame('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', unfettered_code_flow).
narrative_ontology:cs_drift_state('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', contemporary_oss_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef758482-48fe-4b1e-a1bf-53ca8e2ff92c', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, original_open_source_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, users_of_proprietary_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities leverage software released under permissive licenses (e.g., MIT, Apache) to build proprietary derivative products without being required to contribute their modifications back to the open-source community. They benefit from reduced development costs and accelerated time-to-market, effectively capturing value from the commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_builders, beneficiary,
    powerful, biographical, mobile, global).

% These are the developers who create and release software under permissive licenses, often with the intent of maximizing adoption and reuse. From this reading's perspective, their contributions are 'paid' by being used in proprietary contexts without reciprocity, leading to a one-way value transfer.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, original_open_source_contributors, payer,
    moderate, biographical, constrained, global).

% These individuals and organizations champion licenses like the GPL that require derivative works to also be open source. They view permissive licenses without reciprocity as undermining the long-term health and freedom of the software commons, bearing the cost of seeing their vision for open collaboration diluted by proprietary enclosure.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).

% These users benefit from the availability of proprietary software that incorporates permissive open-source components, often enjoying polished products and commercial support. However, they may also face vendor lock-in and lack the freedoms associated with fully open-source alternatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, users_of_proprietary_software, beneficiary,
    moderate, immediate, constrained, global).

% These organizations often promote various open-source licenses, including permissive ones, to foster innovation and adoption. From this reading's perspective, they observe the tension between maximizing adoption and preventing exploitation, sometimes struggling to reconcile these goals.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates broad adoption and reuse of software components by minimizing legal friction for integration into diverse projects, including proprietary ones.
% TRANSFER_FUNCTION: Transfers the value generated by open-source contributions to proprietary software developers without requiring reciprocal contributions back to the open-source commons.
% ABSENT_VOICES: Developers who would prefer strong copyleft licenses and perceive permissive licenses as a threat to the commons are often marginalized in discussions focused solely on maximizing adoption. Users who might prefer fully open ecosystems are also often not at the table when licensing decisions are made.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, the entire software ecosystem would face immediate and profound disruption. Proprietary software relying on permissive open-source components would face legal challenges or require extensive re-licensing/re-development. The open-source movement would shift dramatically towards copyleft or other models, fundamentally altering how software is developed and distributed.
% FOUNDING_PROBLEM: The perceived friction and complexity of strong copyleft licenses hindering adoption and commercial use of open-source software, leading to a desire for simpler, more 'business-friendly' licensing terms.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of permissive licenses (e.g., some tech companies, certain open-source foundations) attest to the original problem of adoption friction. Copyleft advocates and some legal scholars attest that the problem has shifted to exploitation, citing numerous examples of proprietary derivatives and the 'enclosure' of the digital commons; legislative hearings and academic analyses support the shifted-function reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the value captured by proprietary builders without reciprocal contribution. Suppression (0.65) is structural, arising from the legal framework of permissive licenses that actively 'suppresses' any requirement for reciprocity, thereby limiting the options for original contributors to demand return. The low theater ratio (0.15) indicates that the licenses are highly functional in achieving their (from this reading's perspective) exploitative outcome. Resistance (0.70) is high due to ongoing advocacy from copyleft movements and legal scholars challenging this dynamic. The claimed type is 'tangled_rope' because it has a genuine coordination function (enabling broad reuse) but also substantial asymmetric extraction (from contributors to proprietary builders).
 *
 * PERSPECTIVAL GAP:
 *   Proponents of permissive licenses (e.g., some open-source foundations, proprietary builders) would frame this as a 'rope' or 'scaffold' that maximizes freedom and innovation. This 'copyleft_counterfactual_reading' frames it as a 'tangled_rope' or 'snare' due to the structural exploitation it enables. The engine's classification will highlight this divergence based on the authored metrics and structural declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software builders are clear beneficiaries, leveraging permissive code for private gain without obligation. Original open-source contributors and copyleft advocates are victims, as their work or principles are exploited by this one-way value transfer. Users of proprietary software are indirect beneficiaries of the resulting products but also face potential lock-in. Open-source foundations act as observers, navigating the complex trade-offs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploitation_vs_freedom,
    'Is the unrestricted reuse enabled by permissive licenses primarily a mechanism for maximizing implementation freedom, or does it structurally enable exploitation of the open-source commons?',
    'Empirical studies tracking value flow from permissive open-source projects to proprietary products, combined with legal analysis of ''enclosure'' mechanisms. Analysis of the long-term sustainability and growth of projects under different licensing models.',
    'If primarily exploitation, the constraint''s extractiveness is confirmed as high, supporting a ''tangled_rope'' or ''snare'' classification. If primarily freedom, extractiveness would be lower, supporting a ''rope'' or ''scaffold''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploitation_vs_freedom, conceptual, 'Ambiguity in the primary function of permissive licenses.').

omega_variable(
    necessity_of_viral_reciprocity,
    'Is viral reciprocity (e.g., GPL) a necessary mechanism to prevent exploitation and ensure the long-term health of the open-source commons, or are other models sufficient?',
    'Comparative analysis of open-source ecosystems under different licensing regimes, evaluating metrics like contributor retention, project sustainability, and the prevalence of proprietary enclosure. Theoretical work on economic models of common-pool resource management in software.',
    'If viral reciprocity is necessary, the ''tangled_rope'' classification is strengthened, as the alternative (permissive licenses) is shown to be structurally deficient. If other models are sufficient, the perceived exploitation might be mitigated, potentially lowering the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_viral_reciprocity, empirical, 'Whether viral reciprocity is a necessary countermeasure to exploitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
