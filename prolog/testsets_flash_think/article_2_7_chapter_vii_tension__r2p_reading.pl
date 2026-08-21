% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Doctrine
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'Responsibility to Protect' (R2P)
 *   doctrine, which asserts that state sovereignty is conditional on a
 *   state's protection of its own population from mass atrocities. If a state
 *   fails this responsibility, the international community has a residual
 *   responsibility to intervene. This reading is in tension with a
 *   'sovereignty-first' interpretation of international law, particularly
 *   Article 2(7) of the UN Charter, which prohibits intervention in the
 *   domestic affairs of states. The R2P reading legitimizes intervention in
 *   cases of systematic atrocity, framing it as a necessary evolution of
 *   international norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.75).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.85).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Doctrine").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'c249ae62-3f10-4255-bfab-11751334dd96').
narrative_ontology:cs_kernel_codification('c249ae62-3f10-4255-bfab-11751334dd96', formalized).
narrative_ontology:cs_authority_grounding('c249ae62-3f10-4255-bfab-11751334dd96', lineage).
narrative_ontology:cs_interpretation_layer_present('c249ae62-3f10-4255-bfab-11751334dd96').
narrative_ontology:cs_reading_relation('c249ae62-3f10-4255-bfab-11751334dd96', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('c249ae62-3f10-4255-bfab-11751334dd96', foundational, sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('c249ae62-3f10-4255-bfab-11751334dd96', sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('c249ae62-3f10-4255-bfab-11751334dd96', secondary, international_community_has_residual_responsibility).
narrative_ontology:cs_axiom_status(international_community_has_residual_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('c249ae62-3f10-4255-bfab-11751334dd96', international_community_has_residual_responsibility, conventional).
narrative_ontology:cs_reference_frame('c249ae62-3f10-4255-bfab-11751334dd96', post_cold_war_humanitarianism).
narrative_ontology:cs_drift_state('c249ae62-3f10-4255-bfab-11751334dd96', contemporary_geopolitical_fragmentation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c249ae62-3f10-4255-bfab-11751334dd96', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, intervening_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary intended beneficiaries, whose lives and rights are at risk from mass atrocities within their state. They are trapped within their state's borders and rely on external intervention for protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% States accused of committing mass atrocities, whose sovereignty is challenged and potentially overridden by international intervention. They bear the direct costs of intervention, sanctions, or regime change.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    institutional, immediate, trapped, national).

% The body with the authority to authorize R2P interventions under Chapter VII of the UN Charter. Its actions are constrained by veto power and geopolitical interests, leading to selective application.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, biographical, constrained, global).

% Organizations and individuals who champion human rights and advocate for the protection of populations from atrocities. They benefit from the legitimization of intervention in such cases.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% States and legal scholars who prioritize state sovereignty and non-interference in internal affairs. They view R2P as an erosion of international law and a dangerous precedent, bearing the cost of its legitimization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates, payer,
    organized, generational, constrained, global).

% States that possess the military and political capacity to carry out R2P interventions. They act as enforcers and may gain geopolitical influence, but also bear the costs and risks of military action.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, beneficiary).

% Academics, policy analysts, and international legal experts who study the doctrine, its application, and its impact on international relations and law.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, diffuse).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international action to prevent or halt mass atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity) when a state fails to protect its own population.
% TRANSFER_FUNCTION: Transfers the responsibility for protecting populations from the sovereign state to the international community (specifically the UN Security Council), potentially leading to the transfer of military resources, political capital, and the erosion of state autonomy.
% ABSENT_VOICES: Populations in states that are vulnerable to atrocities but are not deemed geopolitically significant enough for intervention; states that fear setting a precedent for intervention against themselves; and those who argue for non-military, long-term solutions over coercive intervention.
% DISAPPEARANCE_RATIONALE: If R2P vanished, the international community would revert to a stricter interpretation of non-intervention, likely leading to more unaddressed mass atrocities and a significant shift in the moral and legal landscape of international relations. The 'right to intervene' would be delegitimized.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, due to a strict interpretation of state sovereignty that precluded intervention.
% FOUNDING_PROBLEM_CORROBORATION: UN reports (e.g., ICISS report), human rights organizations (e.g., Amnesty International, Human Rights Watch), and academic studies consistently document ongoing mass atrocities and the challenges of effective international response, corroborating the continued relevance of the problem.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75) because R2P fundamentally redefines and can override state sovereignty, a core principle of international law, leading to significant costs for targeted states. Suppression is very high (0.85) as R2P, when invoked, can lead to military intervention or severe sanctions, directly suppressing the target state's autonomy and capacity for self-governance. Theater ratio is moderate (0.40) because while interventions are real, the 'protection' justification can sometimes be perceived as masking other geopolitical interests, leading to performative aspects and selective application. Resistance is high (0.80) from states fearing intervention and those upholding traditional sovereignty. Accessibility collapse is high (0.70) for targeted states, as the option of non-intervention by external powers collapses.
 *
 * PERSPECTIVAL GAP:
 *   The R2P reading creates a significant perspectival gap between states that champion human rights and intervention, and those that prioritize state sovereignty. From the perspective of persecuted populations, R2P is a vital safeguard. From the perspective of targeted states or those wary of intervention, it is a dangerous precedent for external interference. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations and human rights advocates are clear beneficiaries, as the doctrine aims to protect their rights and lives. Targeted states and the traditional sovereignty norm are victims, as their autonomy and foundational principles are challenged or overridden. The UN Security Council and intervening states act as agenda-setters and enforcers, benefiting from the expanded scope of legitimate action, though intervening states also bear costs. Traditional sovereignty advocates are payers, bearing the cost of the erosion of their preferred international order.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_sovereignty_legitimacy,
    'Is the Responsibility to Protect (R2P) doctrine a legitimate evolution of state sovereignty, or an illegitimate erosion of the foundational principle of non-interference?',
    'Consensus among UN member states on a revised interpretation of Article 2(7) of the UN Charter, or a consistent pattern of international legal precedent that either affirms or rejects R2P''s conditional sovereignty.',
    'If affirmed as legitimate, R2P strengthens as a Tangled Rope, with its coordination function more widely accepted. If rejected, it risks reclassification towards Snare, seen as pure extraction of sovereignty, or Piton, if its application atrophies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(r2p_sovereignty_legitimacy, conceptual, 'Ambiguity regarding R2P''s legal and moral standing in relation to traditional sovereignty.').

omega_variable(
    r2p_selectivity_bias,
    'Is the application of R2P consistent across all cases of mass atrocities, or is it selectively invoked based on the geopolitical interests of powerful states?',
    'Independent, comprehensive analysis of all documented cases of mass atrocities, comparing the severity of atrocities with the international response, controlling for geopolitical factors and resource availability.',
    'If found to be highly selective, the `theater_ratio` would increase, and the `extractiveness` for targeted states would be seen as more arbitrary, pushing the classification closer to Snare. If consistent, it would strengthen the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_selectivity_bias, empirical, 'Whether R2P is applied impartially or with geopolitical bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2005, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2010, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(arti_tr_t2020, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2025, 0.39).
narrative_ontology:measurement(arti_tr_t2030, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(arti_tr_t2035, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2035, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(arti_be_t2010, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(arti_be_t2020, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement(arti_be_t2030, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2030, 0.75).
narrative_ontology:measurement(arti_be_t2035, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2035, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(arti_su_t2010, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(arti_su_t2020, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2025, 0.84).
narrative_ontology:measurement(arti_su_t2030, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2030, 0.85).
narrative_ontology:measurement(arti_su_t2035, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2035, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
