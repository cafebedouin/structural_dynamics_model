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
 *   human_readable: Sovereignty-First Reading of UN Article 2(7) / Chapter VII
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty-first' reading of UN Charter
 *   Article 2(7) and Chapter VII, which holds state sovereignty as
 *   foundational and strictly limits international intervention to cases of
 *   explicit state consent or Chapter VII authorization for inter-state
 *   aggression. This reading prioritizes state non-interference over
 *   humanitarian concerns, effectively shielding states from external action
 *   even when committing domestic atrocities. It is one reading of the
 *   broader 'article_2_7_chapter_vii_tension' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.85).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.9).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Sovereignty-First Reading of UN Article 2(7) / Chapter VII").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'ad74bf4a-4343-4f29-8a9d-cf5159301a31').
narrative_ontology:cs_kernel_codification('ad74bf4a-4343-4f29-8a9d-cf5159301a31', fixed_text).
narrative_ontology:cs_authority_grounding('ad74bf4a-4343-4f29-8a9d-cf5159301a31', lineage).
narrative_ontology:cs_interpretation_layer_present('ad74bf4a-4343-4f29-8a9d-cf5159301a31').
narrative_ontology:cs_reading_relation('ad74bf4a-4343-4f29-8a9d-cf5159301a31', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('ad74bf4a-4343-4f29-8a9d-cf5159301a31', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ad74bf4a-4343-4f29-8a9d-cf5159301a31', state_sovereignty_absolute, conventional).
narrative_ontology:cs_axiom('ad74bf4a-4343-4f29-8a9d-cf5159301a31', foundational, non_intervention_absolute).
narrative_ontology:cs_axiom_status(non_intervention_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ad74bf4a-4343-4f29-8a9d-cf5159301a31', non_intervention_absolute, conventional).
narrative_ontology:cs_reference_frame('ad74bf4a-4343-4f29-8a9d-cf5159301a31', westphalian_state_system).
narrative_ontology:cs_drift_state('ad74bf4a-4343-4f29-8a9d-cf5159301a31', post_cold_war_humanitarian_crises, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ad74bf4a-4343-4f29-8a9d-cf5159301a31', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the strong non-intervention principle, which protects their hard-won independence from external interference, even when facing internal challenges. They view any conditional sovereignty as a return to colonial-era justifications for intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, constrained, global).

% Exploit the non-intervention principle to shield themselves from international scrutiny or action when committing atrocities against their own populations. Their power relies on the inviolability of state borders against external moral claims.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, biographical, constrained, national).

% Bear the direct costs of this reading, as it denies external protection when their own state commits mass atrocities. They are trapped within their borders, with no recourse to international intervention without their persecutor's consent.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Bear the moral and political costs of being unable to effectively advocate for intervention to prevent or stop mass atrocities. Their efforts are consistently blocked by the legal and political weight of the sovereignty-first principle.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_advocates, payer,
    moderate, generational, constrained, global).

% Is the primary body authorized to sanction intervention, but its actions are constrained by the sovereignty-first interpretation, requiring explicit consent or a Chapter VII determination of inter-state aggression, often blocked by veto powers.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the legal and philosophical underpinnings of state sovereignty and the limits of intervention. They document the historical evolution and contemporary challenges to this reading of international law.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international relations by establishing clear boundaries for state action and non-interference, aiming to prevent inter-state conflict by upholding the principle of sovereign equality and non-aggression.
% TRANSFER_FUNCTION: Transfers the absolute right to manage internal affairs, including the treatment of populations, from the international community to individual states, in exchange for a commitment to non-aggression against other states.
% ABSENT_VOICES: Populations facing genocide or mass atrocities within their own states are structurally absent from the international legal discourse that prioritizes state consent over their protection. Their pleas for intervention are legally inadmissible without state consent.
% DISAPPEARANCE_RATIONALE: If this strong sovereignty-first principle vanished, the international system would undergo a profound rearrangement. States would lose their primary shield against external interference, potentially leading to more frequent interventions based on humanitarian grounds, but also increasing the risk of powerful states exploiting such justifications for geopolitical gain. The entire architecture of the UN Charter would need reinterpretation.
% FOUNDING_PROBLEM: The UN Charter was founded in the aftermath of two World Wars, aiming to prevent future inter-state aggression and uphold the sovereign equality of nations, particularly in the context of decolonization.
% FOUNDING_PROBLEM_CORROBORATION: Many states, particularly those in the Global South, continue to assert the foundational importance of non-intervention as a bulwark against neo-colonialism. International legal scholars and historians corroborate the historical context of preventing inter-state war and protecting newly independent states as the core founding problem.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.85) is high because this reading allows states to inflict severe harm on their own populations without external accountability. Suppression (0.9) is also very high, as the legal framework actively suppresses any attempts at intervention not sanctioned by the state itself or the UN Security Council (where veto power often protects perpetrators). Theater ratio (0.2) is low because the principle is genuinely applied, though often to the detriment of populations. The metrics reflect the structural delta: high epsilon blocking intervention, with post-colonial and authoritarian states as beneficiaries and populations under atrocity as victims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiary states, this is a legitimate 'rope' ensuring stability and non-interference. From the perspective of victim populations and humanitarian advocates, it operates as a 'snare' that traps them in cycles of violence and impunity. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are clear beneficiaries, as the constraint protects their internal affairs from external interference (low directionality). Populations under domestic atrocity and humanitarian advocates are clear targets, as the constraint denies them protection or effective recourse (high directionality). The UN Security Council, while an agenda-setter, is also constrained by this reading, limiting its ability to act.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent inter-state war and protect state sovereignty, particularly for newly independent nations. While this mandate remains live for many states, its application to domestic atrocities has led to a functional shift where it now also serves to protect regimes from accountability. The classification as a snare for victim populations highlights this mandatrophy, preventing mislabeling it as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is the absolute non-intervention principle a necessary condition for international peace and stability, or does it systematically enable mass human rights abuses?',
    'Empirical analysis of intervention outcomes: comparing states where intervention occurred (with or without consent) versus those where it was blocked, assessing long-term stability and human rights records.',
    'If non-intervention is found to systematically enable abuses without ensuring stability, the extractiveness of this reading would be re-evaluated as even higher, and its coordination function questioned. If it is found to prevent wider conflicts, its coordination value would be affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, empirical, 'The fundamental tension between state sovereignty and human rights protection.').

omega_variable(
    chapter_vii_interpretation_scope,
    'Should Chapter VII authorization for ''threats to international peace and security'' be interpreted to include domestic mass atrocities, even without cross-border aggression?',
    'Evolution of international legal precedent and Security Council practice: a shift towards consistently defining domestic atrocities as threats to international peace would resolve this ambiguity.',
    'If domestic atrocities are consistently included, the ''sovereignty-first'' reading''s scope would narrow, reducing its suppressive power and extractiveness for victim populations. If not, its current high suppression and extractiveness would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_vii_interpretation_scope, conceptual, 'Ambiguity in the scope of UN Security Council''s Chapter VII powers regarding domestic atrocities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(arti_tr_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(arti_tr_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 75, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(arti_be_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(arti_be_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(arti_su_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 45, 0.88).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(arti_su_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 75, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_2_7_chapter_vii_tension' kernel, focusing on the primacy of state sovereignty. It is structurally linked to the 'r2p_reading' which offers a conditional sovereignty perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
