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
 *   human_readable: UN Article 2(7) Sovereignty-First Interpretation
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty-first' reading of UN Article
 *   2(7) and Chapter VII, which prioritizes state sovereignty and
 *   non-intervention in domestic affairs, limiting international intervention
 *   to cases of explicit state consent or clear inter-state aggression. It is
 *   one reading of the 'article_2_7_chapter_vii_tension' kernel, contrasting
 *   with the 'r2p_reading'. This reading is structurally extractive,
 *   benefiting states that wish to avoid external scrutiny, at the cost of
 *   populations facing domestic atrocities.
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
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "UN Article 2(7) Sovereignty-First Interpretation").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '4494ad3a-bf52-4b98-bfb1-dda435ac103b').
narrative_ontology:cs_kernel_codification('4494ad3a-bf52-4b98-bfb1-dda435ac103b', fixed_text).
narrative_ontology:cs_authority_grounding('4494ad3a-bf52-4b98-bfb1-dda435ac103b', lineage).
narrative_ontology:cs_interpretation_layer_present('4494ad3a-bf52-4b98-bfb1-dda435ac103b').
narrative_ontology:cs_reading_relation('4494ad3a-bf52-4b98-bfb1-dda435ac103b', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('4494ad3a-bf52-4b98-bfb1-dda435ac103b', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4494ad3a-bf52-4b98-bfb1-dda435ac103b', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('4494ad3a-bf52-4b98-bfb1-dda435ac103b', foundational, non_intervention_principle_unconditional).
narrative_ontology:cs_axiom_status(non_intervention_principle_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('4494ad3a-bf52-4b98-bfb1-dda435ac103b', non_intervention_principle_unconditional, conventional).
narrative_ontology:cs_reference_frame('4494ad3a-bf52-4b98-bfb1-dda435ac103b', westphalian_state_system).
narrative_ontology:cs_drift_state('4494ad3a-bf52-4b98-bfb1-dda435ac103b', post_cold_war_humanitarian_crises, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4494ad3a-bf52-4b98-bfb1-dda435ac103b', '').
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

% Benefit from the strong non-intervention principle, which protects their newly won sovereignty from external interference, often seen as a continuation of colonial practices. They resist any erosion of this principle, even when domestic situations are dire.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, constrained, global).

% Utilize the non-intervention principle to shield their internal affairs, including human rights abuses, from international scrutiny and intervention. Their power is directly tied to the inviolability of state borders.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    organized, biographical, constrained, global).

% Bear the direct costs of this interpretation, as it prevents external intervention even in cases of genocide, war crimes, ethnic cleansing, and crimes against humanity within their own state borders. They have no effective exit from their situation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Work to highlight atrocities and advocate for intervention, but are consistently blocked by the strong sovereignty principle. They expend significant resources with limited success in changing the international legal framework.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_advocates, payer,
    moderate, biographical, constrained, global).

% Is the primary body authorized to sanction intervention under Chapter VII, but its actions are limited by the veto power of permanent members and the strict interpretation of 'threat to international peace and security' as primarily inter-state aggression. It enforces the non-intervention norm by withholding authorization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Often advocate for a more expansive interpretation of intervention, particularly in cases of humanitarian crisis, but are constrained by the existing legal framework and the political will of other powerful states. They are caught between their values and the established international order.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, western_liberal_democracies, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally recognized boundary for state sovereignty, preventing arbitrary intervention and promoting stability in the international system by limiting the use of force to inter-state aggression or explicit consent.
% TRANSFER_FUNCTION: Transfers the absolute right to internal governance, including the management of domestic populations, from international oversight to individual states, even at the cost of human lives. It transfers the burden of domestic atrocities onto the affected populations.
% ABSENT_VOICES: The populations suffering under domestic atrocities are structurally absent from the international legal discourse that defines the limits of intervention. Their pleas for protection are filtered through state-centric mechanisms that prioritize state consent.
% DISAPPEARANCE_RATIONALE: If this strong sovereignty-first interpretation vanished, the international system would face immediate and profound rearrangement. Intervention norms would become highly contested, potentially leading to more frequent, but also more chaotic, interventions. The power balance between states and international bodies would fundamentally shift.
% FOUNDING_PROBLEM: The UN Charter was established in the aftermath of World War II to prevent future inter-state wars and protect the sovereignty of member states, particularly newly independent nations, from external interference.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and authoritarian regimes assert the founding problem of preventing external interference remains live and paramount. Western liberal democracies and humanitarian advocates argue that while inter-state war prevention is important, the problem of intra-state mass atrocities has become equally, if not more, pressing, and the current interpretation fails to address it.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because this interpretation effectively grants states a 'license to kill' their own populations without external interference, extracting the right to life and security from vulnerable groups. Suppression (0.9) is also very high, as the international legal framework actively suppresses any attempts at intervention not sanctioned by the UN Security Council, which itself is constrained by this interpretation. Theater ratio is low (0.1) because the constraint is highly functional in its stated purpose of protecting state sovereignty, even if that function has extractive consequences for others. Accessibility collapse is high (0.75) because alternatives to this legal framework are severely limited for those seeking intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiary states, this is a foundational principle of international order, a 'rope' that prevents chaos and neo-colonialism. From the perspective of victim populations, it is a 'snare' that traps them in cycles of violence and abuse. The engine's classification will reflect the latter due to the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are clear beneficiaries, as this interpretation shields them from external interference. Populations under domestic atrocity and humanitarian advocates are victims, as their calls for protection are blocked. The UN Security Council acts as the agenda-setter, enforcing this interpretation, while Western liberal democracies are observers, often caught between their values and the legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is state sovereignty an absolute, unconditional principle, or is it conditional upon a state''s adherence to fundamental human rights and protection of its own population?',
    'Evolution of customary international law through state practice and opinio juris, or a UN Charter amendment explicitly redefining sovereignty''s limits.',
    'If sovereignty is deemed conditional, this reading''s extractiveness would be re-evaluated downward, and its classification might shift towards a ''tangled_rope'' or ''scaffold'' if a new, more balanced coordination mechanism emerges. If absolute, its ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'The fundamental conceptual tension between state sovereignty and human rights obligations.').

omega_variable(
    chapter_vii_scope_interpretation,
    'How broadly should ''threat to international peace and security'' under Chapter VII be interpreted? Does it include intra-state mass atrocities, or is it strictly limited to inter-state aggression?',
    'UN Security Council resolutions explicitly expanding the interpretation to include domestic atrocities as threats to international peace, or a ruling by the International Court of Justice.',
    'A broader interpretation would weaken the suppression of intervention, potentially lowering the extractiveness for victim populations and shifting the constraint''s classification away from a pure ''snare''. A narrower interpretation reinforces the current ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_vii_scope_interpretation, empirical, 'The empirical scope of UN Security Council''s authority to authorize intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(arti_tr_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(arti_tr_t79, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 79, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(arti_be_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 45, 0.82).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(arti_be_t79, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 79, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(arti_su_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 45, 0.87).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(arti_su_t79, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 79, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
