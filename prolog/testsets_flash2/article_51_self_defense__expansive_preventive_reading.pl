% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense (Article 51 UN Charter)
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents an expansive reading of Article 51 of the UN
 *   Charter, which permits self-defense to include preemptive or preventive
 *   uses of force against non-state actors or emerging threats, with the
 *   necessity of such force largely self-judged by the acting state. This
 *   interpretation has gained traction since the post-Cold War era,
 *   particularly in response to terrorism and WMD proliferation concerns,
 *   shifting the balance from collective security to unilateral action. The
 *   constraint is claimed as a 'tangled_rope' because it purports to
 *   coordinate state responses to threats while simultaneously enabling
 *   asymmetric extraction of sovereignty and security from target states and
 *   populations.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Agenda-setter (institutional/arbitrage) — benefits from expanded unilateral action
 *   - defense_sectors: Beneficiary (organized/mobile) — profits from increased military activity
 *   - target_region_populations: Payer (powerless/trapped) — bears the direct costs of conflict
 *   - multilateral_veto_authority: Payer (institutional/constrained) — loses authority and influence
 *   - international_law_scholars: Observer (analytical/analytical) — critiques and analyzes the interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.65).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense (Article 51 UN Charter)").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '7a21a29b-fed4-4c37-935f-5cc3766b84f1').
narrative_ontology:cs_kernel_codification('7a21a29b-fed4-4c37-935f-5cc3766b84f1', fixed_text).
narrative_ontology:cs_authority_grounding('7a21a29b-fed4-4c37-935f-5cc3766b84f1', lineage).
narrative_ontology:cs_interpretation_layer_present('7a21a29b-fed4-4c37-935f-5cc3766b84f1').
narrative_ontology:cs_reading_relation('7a21a29b-fed4-4c37-935f-5cc3766b84f1', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a21a29b-fed4-4c37-935f-5cc3766b84f1', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('7a21a29b-fed4-4c37-935f-5cc3766b84f1', foundational, inherent_right_to_self_preservation).
narrative_ontology:cs_axiom_status(inherent_right_to_self_preservation, holdable).
narrative_ontology:cs_axiom_grounding('7a21a29b-fed4-4c37-935f-5cc3766b84f1', inherent_right_to_self_preservation, deontological).
narrative_ontology:cs_axiom('7a21a29b-fed4-4c37-935f-5cc3766b84f1', foundational, necessity_is_self_judged).
narrative_ontology:cs_axiom_status(necessity_is_self_judged, holdable).
narrative_ontology:cs_axiom_grounding('7a21a29b-fed4-4c37-935f-5cc3766b84f1', necessity_is_self_judged, conventional).
narrative_ontology:cs_reference_frame('7a21a29b-fed4-4c37-935f-5cc3766b84f1', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('7a21a29b-fed4-4c37-935f-5cc3766b84f1', contemporary_multipolar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a21a29b-fed4-4c37-935f-5cc3766b84f1', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with significant military capabilities that interpret Article 51 broadly to justify unilateral preemptive or preventive force against perceived threats, often self-judging necessity. They benefit from flexibility in projecting power and protecting national interests without explicit UN Security Council authorization.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Military-industrial complexes and associated research institutions that benefit from the expanded scope of legitimate military action, leading to increased demand for advanced weaponry, intelligence, and operational capabilities. They are not direct enforcers but profit from the interpretation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    organized, biographical, mobile, global).

% Populations in regions designated as sources of 'emerging threats' or hosting 'non-state actors' targeted by preemptive strikes. They bear the direct costs of conflict, displacement, and instability, often without recourse or representation in the decision-making process.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The UN Security Council and its member states, whose authority to authorize or legitimize the use of force is bypassed or undermined by expansive interpretations of self-defense. They bear the cost of diminished collective security mechanisms and a weakened international legal order.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).

% Academics and legal experts who analyze and critique the evolving interpretations of Article 51, documenting the legal precedents, state practices, and their implications for the international legal order. They do not directly participate in enforcement but shape the discourse.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to respond to threats to their security, theoretically coordinating responses to aggression and maintaining international peace and security.
% TRANSFER_FUNCTION: Transfers the authority to determine the necessity and proportionality of force from multilateral bodies (like the UN Security Council) to individual militarily capable states, along with the associated costs of conflict to target populations.
% ABSENT_VOICES: Smaller, less militarily capable states and civil society organizations in target regions are often excluded from the interpretive process, despite bearing the brunt of its consequences. They would advocate for stricter adherence to collective security principles and a narrower interpretation of self-defense.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished overnight, militarily capable states would face significantly higher legal and political hurdles for unilateral military action, forcing greater reliance on UN Security Council authorization or more narrowly defined responses to actual attacks. This would fundamentally alter global security dynamics and the balance of power in international law.
% FOUNDING_PROBLEM: The UN Charter's Article 51 was designed to balance the inherent right of self-defense with the principle of collective security, preventing unilateral aggression while allowing states to protect themselves from armed attack.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and their defense establishments argue that the founding problem of state security against evolving threats (e.g., terrorism, WMD proliferation) remains live, necessitating an expansive interpretation. International legal bodies, smaller states, and human rights organizations, from outside the benefiting parties, contend that the original problem has been reinterpreted to justify power projection, and the expansive reading exacerbates rather than solves security challenges.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant costs imposed on target populations and the erosion of multilateral authority. Suppression (0.65) is moderate, as the interpretation is actively enforced by powerful states through military action and diplomatic pressure, though it faces considerable legal and political resistance. The theater ratio (0.40) indicates that while genuine security concerns exist, a substantial portion of the justification for preemptive action serves to legitimize unilateral power projection rather than purely defensive coordination. The increasing extractiveness and suppression over time reflect the hardening of this interpretation into state practice.
 *
 * PERSPECTIVAL GAP:
 *   Militarily capable states perceive this reading as a necessary adaptation of international law to modern threats, a 'rope' for collective security. Target populations and multilateral institutions, however, experience it as a 'snare' that legitimizes intervention and undermines sovereignty. The engine's classification as 'tangled_rope' captures this dual nature, where a coordination function is intertwined with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are clear beneficiaries (d near 0.0) as they gain flexibility and power. Defense sectors are also beneficiaries, profiting from the expanded scope of military operations. Target region populations are clear victims (d near 1.0), bearing the direct consequences of preemptive strikes. Multilateral veto authority is also a victim, as its power to authorize force is circumvented. International law scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (as proponents claim) or a pure 'snare' (as critics argue). By identifying it as a 'tangled_rope', the analysis highlights the genuine coordination problem (responding to threats) that is simultaneously used as cover for asymmetric extraction (unilateral power projection and its costs). The mandatrophy is not fully resolved, as the 'founding problem' of collective security is contested, but the classification clarifies the extractive dynamics at play.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_self_judgment_legitimacy,
    'Is the self-judgment of ''necessity'' by the acting state a legitimate and sufficient basis for preemptive force under international law, or does it require independent, objective verification?',
    'Establishment of an independent international body with binding authority to review and approve claims of preemptive necessity, or a clear ICJ ruling on the limits of self-judgment.',
    'If self-judgment is deemed insufficient, the constraint''s legitimacy would collapse, shifting it towards a ''snare'' due to lack of oversight. If upheld, it reinforces the ''tangled_rope'' nature with a strong unilateral component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_self_judgment_legitimacy, conceptual, 'Ambiguity regarding the standard of necessity for preemptive self-defense.').

omega_variable(
    non_state_actor_attribution_challenge,
    'How should ''non-state actors'' be attributed to a state for the purposes of self-defense, and what level of state involvement triggers a right to preemptive action against the host state?',
    'Development of clear, internationally agreed-upon legal criteria for attribution of non-state actor actions to states, and thresholds for ''emerging threats''.',
    'Clearer attribution rules would reduce the scope for unilateral action, potentially pushing the constraint towards a ''rope'' or a more narrowly defined ''tangled_rope''. Lack of clarity maintains the current expansive interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_challenge, empirical, 'Uncertainty in attributing non-state actor threats to states for self-defense.').

omega_variable(
    reading_impact_on_multilateralism,
    'Does this expansive reading fundamentally undermine the UN Charter''s collective security framework, or is it a necessary evolution that can coexist with multilateralism?',
    'Long-term observation of state practice and UN Security Council effectiveness; analysis of whether unilateral actions consistently bypass or eventually gain multilateral endorsement.',
    'If it fundamentally undermines multilateralism, the ''extraction'' component (from multilateral veto authority) is higher, pushing it closer to a ''snare''. If it coexists, the ''coordination'' aspect is more robust, reinforcing ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_impact_on_multilateralism, preference, 'Impact of expansive self-defense on the collective security system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(arti_tr_t1998, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(arti_tr_t2006, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement(arti_tr_t2014, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(arti_be_t1998, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(arti_be_t2006, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2006, 0.75).
narrative_ontology:measurement(arti_be_t2014, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2014, 0.77).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(arti_su_t1998, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(arti_su_t2006, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2006, 0.63).
narrative_ontology:measurement(arti_su_t2014, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 51 self-defense kernel. Its expansive interpretation directly influences the operational scope of UN Security Council veto power and the practical application of international humanitarian law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
