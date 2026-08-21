% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Absolute State Sovereignty (Sovereignty-First Reading)
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'sovereignty-first' reading of the
 *   Article 2(7) / Chapter VII kernel, emphasizing the foundational nature of
 *   state sovereignty and strict limits on external intervention. It posits
 *   that intervention requires explicit state consent or Chapter VII
 *   authorization, which is primarily limited to inter-state aggression. This
 *   reading is central to the post-WWII international order but is
 *   increasingly challenged by humanitarian crises within states. The claimed
 *   type is 'tangled_rope' because its proponents argue it coordinates
 *   international stability, but its operation is highly extractive for
 *   populations under domestic atrocity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.85).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.9).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Absolute State Sovereignty (Sovereignty-First Reading)").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '9e9b2099-ecc0-4951-b8be-b954b28bff82').
narrative_ontology:cs_kernel_codification('9e9b2099-ecc0-4951-b8be-b954b28bff82', fixed_text).
narrative_ontology:cs_authority_grounding('9e9b2099-ecc0-4951-b8be-b954b28bff82', lineage).
narrative_ontology:cs_interpretation_layer_present('9e9b2099-ecc0-4951-b8be-b954b28bff82').
narrative_ontology:cs_reading_relation('9e9b2099-ecc0-4951-b8be-b954b28bff82', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('9e9b2099-ecc0-4951-b8be-b954b28bff82', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('9e9b2099-ecc0-4951-b8be-b954b28bff82', state_sovereignty_absolute, conventional).
narrative_ontology:cs_axiom('9e9b2099-ecc0-4951-b8be-b954b28bff82', foundational, non_intervention_internal_affairs).
narrative_ontology:cs_axiom_status(non_intervention_internal_affairs, holdable).
narrative_ontology:cs_axiom_grounding('9e9b2099-ecc0-4951-b8be-b954b28bff82', non_intervention_internal_affairs, conventional).
narrative_ontology:cs_reference_frame('9e9b2099-ecc0-4951-b8be-b954b28bff82', westphalian_order_1945).
narrative_ontology:cs_drift_state('9e9b2099-ecc0-4951-b8be-b954b28bff82', post_cold_war_humanitarian_crises, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9e9b2099-ecc0-4951-b8be-b954b28bff82', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_sovereignty_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, non_intervention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle of non-intervention, which protects their hard-won independence and territorial integrity from external interference, particularly from former colonial powers. They actively defend this reading of sovereignty.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    institutional, generational, identity_locked, global).

% Benefits from the non-intervention principle by protecting their internal control and preventing external scrutiny or intervention in their domestic affairs, even in cases of severe human rights abuses. They are strong proponents of this reading.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, biographical, constrained, national).

% Bears the ultimate cost of this reading of sovereignty, as it denies them external protection when their own state commits mass atrocities. They are trapped by the state's sovereignty and lack effective recourse.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% The primary body with the authority to authorize intervention under Chapter VII. However, its actions are constrained by the sovereignty principle and the veto power of permanent members, often leading to deadlock when domestic atrocities are at issue.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Comprises NGOs, some states, and international legal scholars who argue for intervention in cases of mass atrocities. Their arguments are structurally excluded or significantly constrained by the sovereignty-first reading, which prioritizes state consent or inter-state aggression for intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_advocates, excluded,
    organized, biographical, constrained, global).

% Analyze the legal framework, its historical evolution, and its implications for international peace and human rights. They observe the tension between sovereignty and humanitarian concerns without directly participating in enforcement or suffering its direct costs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international order by defining the boundaries of state authority and limiting external interference, aiming to prevent inter-state aggression and maintain global stability among sovereign states.
% TRANSFER_FUNCTION: Transfers the exclusive right to manage internal affairs, including human rights protection, to the sovereign state, effectively transferring the burden of protection (or lack thereof) from the international community to the state itself. It also transfers the cost of non-intervention (suffering, instability) to populations under atrocity.
% ABSENT_VOICES: Populations under domestic atrocity are structurally excluded from the international legal discourse that defines intervention limits; their voices would demand protection over sovereignty. Rival interpretations that prioritize human rights are also marginalized.
% DISAPPEARANCE_RATIONALE: If the absolute sovereignty principle vanished, the international system would undergo a profound reordering, potentially leading to more frequent interventions, a redefinition of statehood, and a shift in the balance of power between states and international bodies. The current framework for international relations would collapse, leading to unpredictable global instability.
% FOUNDING_PROBLEM: To prevent a return to the chaos of world wars and colonial interventions by establishing clear rules for state interaction and non-interference in internal affairs, thereby promoting peace and stability among sovereign states.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and some international legal scholars corroborate that the problem of preventing external interference and maintaining state stability remains live. However, humanitarian organizations and other scholars attest that the problem of preventing mass atrocities has become equally, if not more, pressing, and that the current framework is inadequate, supporting a shifted-function reading.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the principle effectively denies populations under atrocity the protection of the international community, allowing states to act with impunity internally. Suppression is also very high (0.90) as the international legal framework, particularly the UN Security Council's veto power, actively suppresses attempts at intervention without state consent. Theater ratio is low (0.10) because the principle is a deeply ingrained and actively defended tenet of international law, not merely a performance. Accessibility collapse is high for victims, as alternatives for external protection are almost entirely foreclosed. Resistance is moderate, as humanitarian advocates and some states continuously challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The 'sovereignty-first' reading is experienced as a protective shield by beneficiary states, safeguarding their independence and internal order. For populations suffering under domestic atrocities, however, the same principle operates as a snare, denying them external protection and perpetuating their suffering. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are clear beneficiaries (low d) as the constraint protects their internal autonomy. Populations under domestic atrocity are the primary victims (high d), bearing the full cost of non-intervention. The UN Security Council acts as an agenda-setter, but its directionality is complex, often split by the interests of its permanent members. Humanitarian intervention advocates are structurally excluded, pushing their d towards the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent inter-state aggression and maintain global peace, a problem that remains live. However, its application has arguably atrophied in addressing intra-state atrocities, where the principle of non-intervention can be seen as protecting regimes rather than populations. The persistence of the constraint, despite its extractive consequences for victims, is driven by the strong interests of beneficiary states in maintaining their autonomy, rather than solely by its original coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_r2p_balance,
    'What is the optimal balance between state sovereignty and the international community''s responsibility to protect populations from mass atrocities?',
    'Evolution of international customary law through state practice and opinio juris, or a UN General Assembly resolution clarifying the scope of Chapter VII in relation to domestic atrocities.',
    'A shift towards the R2P reading would reduce the extractiveness and suppression of this ''sovereignty-first'' constraint, potentially reclassifying it as a more balanced ''rope'' or ''scaffold'' for intervention. Conversely, a reaffirmation of absolute sovereignty would solidify its ''snare'' characteristics for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_r2p_balance, conceptual, 'The fundamental tension between state sovereignty and humanitarian intervention.').

omega_variable(
    chapter_vii_scope_interpretation,
    'Can Chapter VII of the UN Charter be legitimately interpreted to authorize intervention in cases of purely domestic mass atrocities, even without explicit state consent or a direct threat to international peace and security?',
    'A landmark ruling by the International Court of Justice (ICJ) or a consistent pattern of UN Security Council resolutions that broaden the interpretation of ''threat to international peace and security'' to include domestic atrocities.',
    'A broader interpretation would weaken the ''sovereignty-first'' reading''s suppressive power, making intervention more accessible and reducing extraction from victim populations. A narrow interpretation reinforces the current constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chapter_vii_scope_interpretation, empirical, 'Ambiguity in the legal scope of UN Security Council intervention authority.').

omega_variable(
    internalized_non_intervention_norm,
    'To what extent is the suppression of humanitarian intervention structural (e.g., veto power, legal barriers) versus internalized by international actors (e.g., fear of setting precedents, respect for state autonomy as an end in itself)?',
    'Analysis of state diplomatic cables and UN debates for explicit justifications for non-intervention, distinguishing between legal/political barriers and normative reluctance. Post-intervention outcomes in cases where the structural barrier was overcome.',
    'If internalized norms play a significant role, the effective suppression is higher than structural measures suggest, as actors self-limit even when legal avenues might exist. This would make the constraint more resilient to purely legal challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_non_intervention_norm, empirical, 'Structural vs. internalized suppression mechanism for intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(arti_tr_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(arti_be_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(arti_be_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(arti_su_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(arti_su_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_criminal_justice).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_aid_access).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_peacekeeping_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 2(7) / Chapter VII kernel, which also includes the 'Responsibility to Protect' (R2P) reading. This 'sovereignty-first' reading emphasizes non-intervention, while the R2P reading emphasizes conditional sovereignty and the international community's responsibility to protect populations from mass atrocities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
