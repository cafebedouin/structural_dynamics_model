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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Traditional State Sovereignty (Non-Intervention First)
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'sovereignty first' reading of the
 *   Article 2(7) / Chapter VII tension within the UN Charter, emphasizing
 *   state non-intervention unless explicit consent or inter-state aggression
 *   is present. It prioritizes the territorial integrity and political
 *   independence of states above other considerations, even in cases of
 *   domestic mass atrocity. This reading stands in contrast to the
 *   'Responsibility to Protect' (R2P) reading, which posits conditional
 *   sovereignty. The high extractiveness and suppression reflect the severe
 *   costs borne by populations under domestic atrocity due to the robust
 *   protection afforded to state sovereignty.
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
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Traditional State Sovereignty (Non-Intervention First)").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a').
narrative_ontology:cs_kernel_codification('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', fixed_text).
narrative_ontology:cs_authority_grounding('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', lineage).
narrative_ontology:cs_interpretation_layer_present('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a').
narrative_ontology:cs_reading_relation('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', foundational, non_intervention_principle_paramount).
narrative_ontology:cs_axiom_status(non_intervention_principle_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', non_intervention_principle_paramount, conventional).
narrative_ontology:cs_reference_frame('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', westphalian_state_system).
narrative_ontology:cs_drift_state('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', post_cold_war_humanitarian_interventions, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d8d03ee-9974-40ae-9d15-fbabc0dc8e4a', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, liberal_democracies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states prioritize the principle of non-intervention due to historical experiences of colonialism and external interference, viewing it as essential for their self-determination and protection from neo-colonialism. They benefit from immunity from external scrutiny.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, identity_locked, global).

% These regimes benefit directly from the strong non-intervention principle, as it grants them immunity from external scrutiny and intervention regarding their domestic human rights records, allowing them to maintain power and suppress dissent without international interference.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, biographical, constrained, national).

% These powerful states, particularly the P5, wield veto power to block interventions that do not align with their geopolitical interests, often invoking the sovereignty-first principle. They benefit from maintaining control over the legitimate use of force in the international system.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council_members, beneficiary).

% These populations bear the direct and severe costs of the non-intervention principle, suffering violence, oppression, and mass atrocities without external protection. Their right to life and security is subordinated to state sovereignty.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% These organizations and individuals advocate for intervention to protect populations from atrocity, but their calls are often overridden by state interests and the legal framework of non-intervention. They bear the moral and practical costs of inaction.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates, excluded).

% These states are often ideologically torn between upholding state sovereignty and intervening to prevent human rights abuses. While they may advocate for intervention, they are constrained by the existing legal framework and geopolitical realities, sometimes bearing the moral cost of non-intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, liberal_democracies, observer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, liberal_democracies, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent arbitrary and unilateral military interventions by powerful states, thereby maintaining a degree of international order, respecting state borders, and ensuring the political independence of states.
% TRANSFER_FUNCTION: Transfers the right to self-determination and protection from populations (especially those facing atrocity) to the state, granting states immunity from external interference in their internal affairs.
% ABSENT_VOICES: Populations under domestic atrocity, who would demand protection and intervention. Their voices are structurally suppressed by the very states that claim sovereignty over them, and by the international system's emphasis on state-centric order.
% DISAPPEARANCE_RATIONALE: If the principle of non-intervention vanished overnight, the international system would likely descend into widespread, potentially arbitrary, interventions by powerful states, leading to a chaotic and unstable global order, though potentially saving some populations from atrocity. The current state system relies heavily on this foundational principle.
% FOUNDING_PROBLEM: To establish a stable international order after two devastating World Wars, preventing aggressive wars, respecting the territorial integrity and political independence of states, and protecting newly independent post-colonial states from external interference.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and many states (especially post-colonial ones) corroborate the original intent to prevent arbitrary intervention and maintain state stability. Human rights organizations and some liberal states contest its current applicability in cases of mass atrocity, citing the failure to protect populations and the evolution of international human rights law. Legislative hearings and independent reports from NGOs often highlight this tension.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading effectively grants states immunity from external intervention, even when they commit atrocities against their own populations, thereby extracting the right to protection from these vulnerable groups. Suppression is also very high (0.90) as the international legal and political system actively suppresses attempts at intervention that lack explicit state consent or a Chapter VII authorization for inter-state aggression. The theater ratio is low (0.10) because the principle is genuinely and robustly enforced, not merely performative; its function is to maintain state-centric order, which it largely achieves. Accessibility collapse is high (0.80) as alternatives for intervention are severely limited by this interpretation. Resistance is moderate (0.50) from human rights advocates and some liberal states, but this resistance is often overcome by the entrenched legal framework and geopolitical interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of post-colonial states and authoritarian regimes, this constraint is a vital 'rope' ensuring their sovereignty and preventing arbitrary interventions. From the perspective of populations under atrocity and human rights advocates, it operates as a 'snare' that traps victims within abusive state borders. The engine's classification as 'tangled_rope' reflects the dual function of coordinating non-intervention while enabling severe extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are clear beneficiaries, as the principle protects them from external interference and allows them to maintain internal control. UN Security Council members also benefit by retaining control over the legitimate use of force. Populations under domestic atrocity are the primary victims, bearing the direct costs of non-intervention. Human rights advocates and some liberal democracies are payers, as their efforts to protect populations are frustrated by this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_conditionality_ambiguity,
    'Is state sovereignty truly absolute, or is it implicitly conditional on a state''s adherence to fundamental human rights obligations?',
    'Evolution of international customary law and state practice, particularly regarding the ''Responsibility to Protect'' (R2P) doctrine. If R2P gains universal acceptance and consistent application, it would suggest a shift towards conditional sovereignty.',
    'If sovereignty is deemed conditional, the extractiveness of this reading would decrease for populations, and the suppression of intervention would be re-evaluated. This would fundamentally alter the balance of the international system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_conditionality_ambiguity, conceptual, 'The core conceptual ambiguity regarding the absolute vs. conditional nature of state sovereignty in international law.').

omega_variable(
    chapter_vii_scope_ambiguity,
    'Does ''threat to international peace and security'' under Chapter VII of the UN Charter encompass domestic mass atrocities, or is it strictly limited to inter-state aggression?',
    'UN Security Council resolutions and interpretations, particularly those authorizing interventions based on humanitarian crises. A consistent pattern of authorizing interventions for domestic atrocities would broaden the scope.',
    'If Chapter VII''s scope is broadened to include domestic atrocities, the legal basis for intervention would expand, potentially reducing the suppression of intervention and the extractiveness for victim populations. This would weaken the ''sovereignty first'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_vii_scope_ambiguity, empirical, 'Ambiguity in the interpretation of Chapter VII''s scope regarding domestic atrocities.').


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
narrative_ontology:measurement(arti_tr_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.09).
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

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Article 2(7) / Chapter VII tension' kernel, alongside the 'r2p_reading'. Both represent competing interpretations of international law regarding state sovereignty and intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
