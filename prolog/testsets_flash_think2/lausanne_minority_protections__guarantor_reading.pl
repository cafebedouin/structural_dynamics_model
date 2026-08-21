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
 *   This constraint represents the 'guarantor reading' of the Lausanne
 *   minority protections, emphasizing their status as internationally
 *   supervised obligations enforceable through diplomatic and human rights
 *   mechanisms, rather than solely domestic interpretation. It is framed as a
 *   low-extractiveness scaffold, providing transitional support for minority
 *   rights enforcement, even if direct coercive mechanisms are limited. The
 *   'scaffold' classification implies its function is to support a transition
 *   towards full and direct integration of international norms into domestic
 *   law, or more robust international enforcement, at which point its
 *   transitional role would sunset.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.35).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.55).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections (Guarantor Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__guarantor_reading).
narrative_ontology:has_sunset_clause(lausanne_minority_protections__guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '744832e2-6fe2-4368-94b7-840924eedccd').
narrative_ontology:cs_kernel_codification('744832e2-6fe2-4368-94b7-840924eedccd', fixed_text).
narrative_ontology:cs_authority_grounding('744832e2-6fe2-4368-94b7-840924eedccd', lineage).
narrative_ontology:cs_interpretation_layer_present('744832e2-6fe2-4368-94b7-840924eedccd').
narrative_ontology:cs_reading_relation('744832e2-6fe2-4368-94b7-840924eedccd', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('744832e2-6fe2-4368-94b7-840924eedccd', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('744832e2-6fe2-4368-94b7-840924eedccd', foundational, international_law_supremacy_in_minority_rights).
narrative_ontology:cs_axiom_status(international_law_supremacy_in_minority_rights, holdable).
narrative_ontology:cs_axiom_grounding('744832e2-6fe2-4368-94b7-840924eedccd', international_law_supremacy_in_minority_rights, conventional).
narrative_ontology:cs_axiom('744832e2-6fe2-4368-94b7-840924eedccd', foundational, guarantor_states_have_standing_in_treaty_enforcement).
narrative_ontology:cs_axiom_status(guarantor_states_have_standing_in_treaty_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('744832e2-6fe2-4368-94b7-840924eedccd', guarantor_states_have_standing_in_treaty_enforcement, conventional).
narrative_ontology:cs_reference_frame('744832e2-6fe2-4368-94b7-840924eedccd', post_lausanne_international_order).
narrative_ontology:cs_drift_state('744832e2-6fe2-4368-94b7-840924eedccd', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('744832e2-6fe2-4368-94b7-840924eedccd', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, minority_groups).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, domestic_courts).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, international_human_rights_law).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, treaty_obligations_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the direct subjects of the protections, seeking to preserve their cultural and religious identity. They benefit from international oversight and diplomatic pressure, which provides an external avenue for redress beyond domestic legal systems. Their identity is deeply tied to their status within the state.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_groups, beneficiary,
    moderate, generational, identity_locked, national).

% States that were signatories to the Treaty of Lausanne and see themselves as having a continuing role in ensuring its implementation. They use diplomatic channels and international forums to press for compliance, gaining diplomatic influence and upholding international legal norms. They can choose to prioritize or de-prioritize this role based on geopolitical considerations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    institutional, generational, mobile, regional).

% Bodies like the European Court of Human Rights and the Council of Europe provide legal and normative frameworks for interpreting and enforcing minority rights, offering a judicial pathway for individuals and groups. They act as an external adjudicator, applying human rights standards to the Lausanne framework.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms, agenda_setter,
    institutional, civilizational, analytical, continental).

% The state party to the Treaty of Lausanne, which is obligated to protect minorities. It bears the costs of diplomatic pressure and potential legal challenges from international bodies. While it retains sovereign authority, its actions are constrained by international law and the diplomatic leverage of guarantor states. It also sets domestic policy.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter).

% National judicial bodies that must interpret domestic law in light of international obligations. They face pressure from both the national government and international legal principles, potentially leading to complex and contested rulings. Their autonomy is constrained by the hierarchy of legal norms.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, domestic_courts, payer,
    institutional, biographical, constrained, national).

% Factions within the Turkish state or society who argue for a purely domestic interpretation of minority rights, resisting external oversight. They are excluded from the international diplomatic and legal processes that this reading emphasizes.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, restrictive_interpretation_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international oversight and diplomatic engagement to ensure the protection of minority rights, preventing purely domestic interpretations from eroding treaty obligations and providing external recourse for minority groups.
% TRANSFER_FUNCTION: Transfers diplomatic pressure, legal scrutiny, and normative influence from guarantor states and human rights mechanisms to the Turkish state and its domestic legal system, in exchange for enhanced protection and recognition for minority groups.
% ABSENT_VOICES: Advocates for a purely national sovereignty approach to minority rights, who would argue against any external supervision or enforcement, are largely absent from the international forums and diplomatic channels that this reading empowers.
% DISAPPEARANCE_RATIONALE: If international supervision and human rights mechanisms vanished, the protections would revert to being solely a matter of domestic interpretation, likely leading to a more restrictive application of rights, increased vulnerability for minority groups, and a significant shift in diplomatic relations concerning these issues.
% FOUNDING_PROBLEM: The Treaty of Lausanne sought to stabilize post-Ottoman borders and protect non-Muslim minorities in Turkey, addressing historical conflicts and ensuring their rights in a new national context.
% FOUNDING_PROBLEM_CORROBORATION: Minority groups themselves, international human rights organizations, and guarantor states consistently attest that the problem of ensuring robust minority protections remains live, citing ongoing challenges to cultural and religious autonomy. This is corroborated by reports from independent NGOs and international monitoring bodies.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.35) because the constraint primarily involves diplomatic and legal pressure, not direct economic or physical extraction. Suppression (0.55) is moderate, reflecting the real but limited coercive power of international diplomacy and human rights bodies against a sovereign state. Theater ratio (0.20) is low, as the mechanisms are genuinely intended to function, even if their effectiveness varies. The 'has_sunset_clause: true' for a scaffold is interpreted conceptually: its 'sunset' is the point at which minority rights are fully and directly integrated into domestic law and robustly enforced, rendering the transitional support of external supervision obsolete.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority groups and guarantor states, this reading provides crucial, albeit imperfect, protection and a pathway for justice. From the perspective of the Turkish state, it can be seen as an infringement on sovereignty, even if acknowledged as a treaty obligation. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority groups, guarantor states, and European human rights mechanisms are beneficiaries, gaining protection, influence, and normative authority. The Turkish state and its domestic courts are the primary payers, bearing the costs of compliance, diplomatic pressure, and legal challenges. Advocates for a restrictive, purely domestic interpretation are excluded from the international discourse this reading empowers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_diplomatic_leverage,
    'To what extent is diplomatic leverage truly effective in compelling compliance from a sovereign state, versus being merely symbolic or easily circumvented?',
    'Empirical analysis of compliance rates with international recommendations in the absence of direct sanctions, comparing cases with and without active guarantor state engagement.',
    'If diplomatic leverage is largely ineffective, the constraint''s actual suppression and extractiveness (from the state) would be lower, potentially reclassifying it closer to a Piton or even a degraded Rope. If highly effective, its classification as a Scaffold with active enforcement is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_diplomatic_leverage, empirical, 'Assesses the real-world impact of diplomatic pressure.').

omega_variable(
    sunset_condition_clarity,
    'Is the ''sunset'' condition for this scaffold clearly defined and measurable, or is it an aspirational state that allows the constraint to persist indefinitely?',
    'Analysis of international legal scholarship and diplomatic statements for explicit criteria that would signal the obsolescence of external supervision, or the lack thereof.',
    'If the sunset condition is vague or perpetually deferred, the constraint''s ''scaffold'' nature is weakened, potentially reclassifying it as a Tangled Rope (if extraction is significant) or a Piton (if function atrophies but it persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_condition_clarity, conceptual, 'Clarifies the temporal nature and intended obsolescence of the scaffold.').

omega_variable(
    sovereignty_vs_obligation_framing,
    'Is the primary tension in this constraint framed as a conflict between national sovereignty and international obligation, or as a dispute over the interpretation of a shared legal text?',
    'Content analysis of legal arguments and diplomatic communications from all parties, identifying the dominant rhetorical and legal framing.',
    'If framed primarily as a sovereignty conflict, the ''payer'' seat (Turkish state) experiences higher perceived suppression and resistance. If framed as an interpretive dispute, the potential for resolution through legal dialogue is higher, potentially lowering perceived suppression over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_obligation_framing, conceptual, 'Examines the underlying conceptual conflict driving the constraint''s dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(laus_tr_t1945, lausanne_minority_protections__guarantor_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(laus_tr_t1975, lausanne_minority_protections__guarantor_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__guarantor_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__guarantor_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.25).
narrative_ontology:measurement(laus_be_t1945, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement(laus_be_t1975, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(laus_su_t1945, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(laus_su_t1975, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, international_human_rights_treaties).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, general_international_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Lausanne minority protections kernel, emphasizing international supervision and enforcement. It is structurally distinct from the restrictive and expansive readings, which focus on domestic interpretation or broader institutional autonomy, respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
