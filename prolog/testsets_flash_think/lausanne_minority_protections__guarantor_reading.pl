% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections (Guarantor Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'guarantor_reading' of the Lausanne
 *   minority protections, asserting that these protections are
 *   internationally supervised obligations enforceable through guarantor
 *   state diplomacy and European human rights mechanisms, rather than being
 *   solely subject to domestic Turkish interpretation. It functions as a
 *   low-extractiveness scaffold, providing transitional support for minority
 *   rights through external adjudication pathways, though its enforcement
 *   relies more on diplomatic leverage than binding coercive power. The
 *   'claimed_type' as scaffold reflects its role as a transitional support
 *   mechanism, even without a formal sunset date, aiming for a future where
 *   such external oversight is no longer needed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.25).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.3).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections (Guarantor Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__guarantor_reading).
narrative_ontology:has_sunset_clause(lausanne_minority_protections__guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '058aa0d7-170a-49a8-bc29-3ebaef383d5a').
narrative_ontology:cs_kernel_codification('058aa0d7-170a-49a8-bc29-3ebaef383d5a', fixed_text).
narrative_ontology:cs_authority_grounding('058aa0d7-170a-49a8-bc29-3ebaef383d5a', lineage).
narrative_ontology:cs_interpretation_layer_present('058aa0d7-170a-49a8-bc29-3ebaef383d5a').
narrative_ontology:cs_reading_relation('058aa0d7-170a-49a8-bc29-3ebaef383d5a', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('058aa0d7-170a-49a8-bc29-3ebaef383d5a', lausanne_minority_protections__expansive_reading, coexists_with).
narrative_ontology:cs_axiom('058aa0d7-170a-49a8-bc29-3ebaef383d5a', foundational, international_treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(international_treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('058aa0d7-170a-49a8-bc29-3ebaef383d5a', international_treaty_obligations_are_binding, deontological).
narrative_ontology:cs_axiom('058aa0d7-170a-49a8-bc29-3ebaef383d5a', foundational, minority_rights_are_subject_to_international_scrutiny).
narrative_ontology:cs_axiom_status(minority_rights_are_subject_to_international_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('058aa0d7-170a-49a8-bc29-3ebaef383d5a', minority_rights_are_subject_to_international_scrutiny, conventional).
narrative_ontology:cs_reference_frame('058aa0d7-170a-49a8-bc29-3ebaef383d5a', post_ottoman_international_order).
narrative_ontology:cs_drift_state('058aa0d7-170a-49a8-bc29-3ebaef383d5a', contemporary_geopolitical_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('058aa0d7-170a-49a8-bc29-3ebaef383d5a', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, minority_communities_in_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_judiciary).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, international_law_supremacy).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, minority_rights_as_international_concern).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities (primarily Greek, Armenian, and Jewish) rely on the international framework for protection of their rights, seeking diplomatic intervention or legal redress through European human rights mechanisms when domestic avenues are insufficient. They bear the costs of seeking such redress.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_communities_in_turkey, beneficiary,
    powerless, generational, constrained, national).

% As the signatory state, Turkey is obligated to uphold the protections but often asserts domestic sovereignty over minority affairs. It faces diplomatic pressure and potential legal scrutiny from international bodies, incurring political and reputational costs for non-compliance.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter).

% These states (e.g., UK, France, Italy) are parties to the Treaty of Lausanne and engage in diplomatic efforts to ensure Turkey's compliance. They benefit from upholding international treaty law and stability in the region, but their enforcement is primarily through diplomatic leverage.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, guarantor_states, beneficiary).

% Bodies like the European Court of Human Rights provide a legal avenue for individuals to challenge alleged violations of their rights, including those related to minority status. They benefit from the expansion of human rights jurisprudence and enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms, beneficiary).

% Analyze the application and effectiveness of the Lausanne protections, contributing to the discourse on international minority rights and treaty interpretation. They have no direct power to enforce but influence legal and diplomatic narratives.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% Domestic courts are the primary interpreters of Turkish law, including how it applies to minorities. They face the challenge of reconciling domestic legal traditions with international obligations and human rights standards, sometimes leading to friction with the executive or international bodies.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_judiciary, payer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an internationally recognized framework for protecting non-Muslim minority rights in Turkey, coordinating diplomatic and legal efforts among guarantor states, international human rights bodies, and minority communities.
% TRANSFER_FUNCTION: Transfers diplomatic and legal leverage to minority communities and guarantor states, enabling external scrutiny and potential redress, thereby shifting some interpretive authority away from purely domestic Turkish institutions.
% ABSENT_VOICES: Other minority groups in Turkey not explicitly recognized by the Treaty of Lausanne (e.g., Kurds, Alevis) who might seek similar international protections but are not covered by this specific framework.
% DISAPPEARANCE_RATIONALE: If the international supervision and enforcement mechanisms vanished, minority protections would revert solely to domestic Turkish law, removing a critical external check and diplomatic avenue. This would likely lead to increased vulnerability for minorities and reduced international oversight, fundamentally reorganizing the legal and political landscape for these communities.
% FOUNDING_PROBLEM: The need to protect non-Muslim minorities in the newly formed Republic of Turkey after the collapse of the Ottoman Empire, ensuring their rights and preventing forced assimilation or displacement in a new national context.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, human rights organizations, and historical analyses consistently corroborate the ongoing need for robust minority protections and the role of international frameworks. While the Turkish state emphasizes domestic sovereignty, external reports and the continued advocacy of minority communities themselves attest to the problem's persistence and the value of international oversight.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because the constraint primarily creates an external pathway for redress and diplomatic pressure, rather than directly extracting resources. Suppression is also low, as it doesn't actively suppress alternatives but rather offers an additional layer of protection. The theater ratio is moderate, reflecting the often performative nature of diplomatic interventions, balanced by the real legal weight of human rights mechanisms. The scaffold classification is chosen because the international supervision is intended as a transitional support, guiding Turkey towards full domestic protection of minority rights, even if the 'sunset' is conceptual rather than a fixed date.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority communities and guarantor states, this constraint is a vital protective mechanism. From the Turkish state's perspective, it can be seen as an infringement on national sovereignty and an external imposition, even if it acknowledges the treaty obligations. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities are clear beneficiaries, gaining an external avenue for protection. Guarantor states and European human rights mechanisms also benefit by upholding international law and human rights norms. The Turkish state is the primary target, as it faces external scrutiny and pressure to comply with international interpretations, which can be seen as an 'extraction' of its sovereign interpretive authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_guarantor_enforcement,
    'How effective are guarantor state diplomacy and European human rights mechanisms in practice at ensuring compliance with Lausanne protections?',
    'Empirical analysis of case outcomes, diplomatic interventions, and changes in domestic policy over time, assessing the actual impact on minority rights.',
    'If effectiveness is low, the constraint''s actual extractiveness from the Turkish state is lower, and its benefit to minorities is reduced, potentially reclassifying it closer to a Piton (theatrical maintenance). If high, its Scaffold function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_guarantor_enforcement, empirical, 'Measures the practical impact of international enforcement mechanisms.').

omega_variable(
    scope_of_protections_ambiguity,
    'Is the scope of Lausanne protections limited to individual worship rights, or does it include institutional autonomy, property ownership, and theological education?',
    'Further international legal rulings, diplomatic consensus, or a shift in Turkish domestic law and practice to explicitly recognize broader institutional rights.',
    'If the scope is interpreted restrictively, the ''guarantor_reading''s'' practical effect is diminished, potentially reducing its classification to a weaker Scaffold or even a Piton. If interpreted expansively, its impact and coordination function are amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_protections_ambiguity, conceptual, 'The core contest over the breadth of minority rights covered by the treaty.').

omega_variable(
    international_obligation_vs_diplomatic_aspiration,
    'Is the international supervision of Lausanne protections a genuine, binding obligation under international law, or primarily a diplomatic aspiration subject to state discretion?',
    'Analysis of state practice, opinio juris, and the legal force of international court judgments and diplomatic démarches.',
    'If primarily aspirational, the constraint''s ''scaffold'' nature is weaker, and its ability to compel compliance is reduced, potentially shifting it towards a Piton. If genuinely binding, its Scaffold function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligation_vs_diplomatic_aspiration, conceptual, 'Ambiguity regarding the binding nature of international supervision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(laus_tr_t1948, lausanne_minority_protections__guarantor_reading, theater_ratio, 1948, 0.38).
narrative_ontology:measurement(laus_tr_t1973, lausanne_minority_protections__guarantor_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(laus_tr_t1998, lausanne_minority_protections__guarantor_reading, theater_ratio, 1998, 0.42).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__guarantor_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.2).
narrative_ontology:measurement(laus_be_t1948, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(laus_be_t1973, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1973, 0.23).
narrative_ontology:measurement(laus_be_t1998, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1998, 0.24).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.25).
narrative_ontology:measurement(laus_su_t1948, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1948, 0.27).
narrative_ontology:measurement(laus_su_t1973, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1973, 0.28).
narrative_ontology:measurement(laus_su_t1998, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1998, 0.29).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, turkish_domestic_minority_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, european_human_rights_jurisprudence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
