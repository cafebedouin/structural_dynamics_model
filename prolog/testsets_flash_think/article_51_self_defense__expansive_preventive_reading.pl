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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense (Article 51 Reading)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents an expansive reading of Article 51 of the UN
 *   Charter, which permits self-defense to include preemptive or preventive
 *   uses of force against non-state actors or emerging threats, provided
 *   necessity is demonstrated. This interpretation emerged prominently in the
 *   post-9/11 era, driven by militarily capable states seeking greater
 *   flexibility in responding to transnational threats. It is a reading of
 *   the 'article_51_self_defense' kernel, distinct from narrower
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.85).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense (Article 51 Reading)").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '0695d497-638a-418f-86b6-17ac8ae0d057').
narrative_ontology:cs_kernel_codification('0695d497-638a-418f-86b6-17ac8ae0d057', fixed_text).
narrative_ontology:cs_authority_grounding('0695d497-638a-418f-86b6-17ac8ae0d057', extraction).
narrative_ontology:cs_interpretation_layer_present('0695d497-638a-418f-86b6-17ac8ae0d057').
narrative_ontology:cs_reading_relation('0695d497-638a-418f-86b6-17ac8ae0d057', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('0695d497-638a-418f-86b6-17ac8ae0d057', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('0695d497-638a-418f-86b6-17ac8ae0d057', foundational, preventive_force_legitimate_against_emerging_threats).
narrative_ontology:cs_axiom_status(preventive_force_legitimate_against_emerging_threats, holdable).
narrative_ontology:cs_axiom_grounding('0695d497-638a-418f-86b6-17ac8ae0d057', preventive_force_legitimate_against_emerging_threats, conventional).
narrative_ontology:cs_axiom('0695d497-638a-418f-86b6-17ac8ae0d057', foundational, state_self_judgment_of_necessity_in_self_defense).
narrative_ontology:cs_axiom_status(state_self_judgment_of_necessity_in_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('0695d497-638a-418f-86b6-17ac8ae0d057', state_self_judgment_of_necessity_in_self_defense, conventional).
narrative_ontology:cs_reference_frame('0695d497-638a-418f-86b6-17ac8ae0d057', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('0695d497-638a-418f-86b6-17ac8ae0d057', contemporary_geopolitical_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0695d497-638a-418f-86b6-17ac8ae0d057', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_complex).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_diplomacy_advocates).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, national_security_imperative).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, preventive_self_defense_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert and benefit from an interpretation of Article 51 that allows preemptive or preventive force against non-state actors or emerging threats, often self-judging the necessity. They gain strategic flexibility and reduce perceived vulnerability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% This sector benefits from increased military spending, procurement of advanced weaponry, and the operational tempo associated with a more permissive interpretation of self-defense, driving demand for their products and services.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_complex, beneficiary,
    organized, biographical, mobile, global).

% These populations bear the direct and indirect costs of preemptive or preventive military strikes, including loss of life, displacement, infrastructure damage, and destabilization of their regions. They have minimal agency in the decision-making process.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, regional).

% The authority of the UN Security Council to authorize the use of force is bypassed by unilateral actions taken under this expansive reading, leading to an erosion of its legitimacy and effectiveness in maintaining international peace and security.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_authority, payer,
    institutional, generational, constrained, global).

% These states, NGOs, and international organizations advocate for diplomatic and multilateral solutions to security threats. Their efforts are undermined when states resort to unilateral force, making their preferred mechanisms less effective and harder to implement.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_diplomacy_advocates, payer,
    organized, biographical, constrained, global).

% These experts analyze and critique the legal implications of this expansive interpretation, often highlighting its tension with the UN Charter's original intent and its potential to destabilize international law. They influence discourse but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% States and legal scholars who advocate for a strict interpretation of Article 51, limiting self-defense to responses to an actual or imminent armed attack by a state, find their views often sidelined in practice by those adopting the expansive reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for militarily capable states to address perceived security threats from non-state actors or emerging dangers before they fully materialize, aiming to coordinate national security responses in a complex threat environment.
% TRANSFER_FUNCTION: Transfers the authority to determine the necessity and legality of force from multilateral bodies (like the UN Security Council) to individual states, and transfers the direct and indirect costs of such interventions to target populations and the broader multilateral system.
% ABSENT_VOICES: States and legal scholars advocating for a narrow interpretation of self-defense, and populations in regions frequently targeted by such actions, are often excluded from the decision-making processes that legitimize these uses of force.
% DISAPPEARANCE_RATIONALE: If this expansive reading of self-defense vanished overnight, militarily capable states would face significantly higher legal and political hurdles for unilateral military action. This would likely lead to either increased reliance on multilateral authorization (reinvigorating the UNSC) or a perceived increase in unaddressed threats, fundamentally reorganizing international security dynamics and state behavior.
% FOUNDING_PROBLEM: The rise of transnational non-state terrorist groups and the perceived inadequacy of traditional international law (primarily focused on state-on-state conflict) to effectively address these threats, particularly after events like 9/11.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and their defense establishments consistently assert that the problem of non-state actor threats and emerging dangers is still live and requires flexible responses. While critical of the expansive reading, some international legal scholars and human rights organizations acknowledge the challenge posed by non-state actors, but dispute the necessity or legality of unilateral preventive force, advocating for alternative solutions.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.78) because this reading allows states to bypass multilateral authorization, effectively extracting sovereignty from target states and authority from the UN Security Council. Suppression is very high (0.85) as it actively suppresses alternative interpretations and multilateral veto power through state practice and legal arguments. Theater ratio is moderate (0.40): while there's a genuine security concern driving the interpretation, the 'necessity demonstrated' clause often involves performative justifications that are self-judged by the acting state, rather than objectively verified. The metrics reflect the structural reality of this interpretation's operation, not its proponents' claims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of militarily capable states, this reading is a necessary adaptation of international law to modern threats, providing essential coordination for national security. From the perspective of target populations and multilateral institutions, it is a mechanism for unilateral extraction of sovereignty and authority, undermining the UN Charter's collective security framework. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are the primary beneficiaries and agenda-setters, gaining significant latitude for unilateral action (low directionality). The defense industrial complex benefits from the increased demand for military interventions. Target region populations, the UN Security Council, and advocates for multilateral diplomacy are the primary victims/payers, bearing the costs of bypassed authority and direct conflict (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents mislabeling it as a pure Rope, which would ignore the substantial extraction from multilateral authority and target populations. It also prevents mislabeling it as a pure Snare, which would overlook the genuine (though contested) coordination function it purports to serve in addressing complex security threats. The 'necessity demonstrated' clause, however performative, provides the coordination story that distinguishes it from pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_objectivity,
    'Is the ''necessity demonstrated'' clause objectively verifiable by an independent body, or is it primarily self-judged by the acting state?',
    'Establishment of an international judicial or oversight body with binding authority to review and validate claims of necessity for preventive self-defense.',
    'If objectively verifiable, the constraint''s extractiveness and suppression would decrease, moving it closer to a genuine Rope by introducing external accountability. If self-judged, its extractive and suppressive nature is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_objectivity, empirical, 'Ambiguity regarding the objectivity of necessity determination in preventive self-defense.').

omega_variable(
    non_state_actor_attribution_legitimacy,
    'Under what conditions can the actions of a non-state actor be legitimately attributed to a state for the purposes of triggering Article 51 self-defense?',
    'Development of clear, internationally agreed-upon legal criteria for state attribution of non-state actor actions, beyond mere presence or passive tolerance.',
    'Clearer attribution criteria would reduce the scope for unilateral action and potentially lower extractiveness by limiting the ''target'' pool. Ambiguity allows for broader, more unilateral interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_legitimacy, conceptual, 'Ambiguity in attributing non-state actor threats to states for self-defense.').

omega_variable(
    reading_consistency_with_kernel_intent,
    'Is this expansive reading consistent with the original intent and foundational principles of Article 51 of the UN Charter, or does it represent a fundamental reinterpretation?',
    'Historical-legal analysis of the UN Charter''s drafting history, subsequent state practice, and ICJ jurisprudence, weighed against contemporary security challenges.',
    'If found inconsistent, the legitimacy of the expansive reading would be severely undermined, increasing resistance and potentially shifting its classification towards a Snare. If found to be a legitimate evolution, its current classification as Tangled Rope would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_consistency_with_kernel_intent, conceptual, 'Consistency of expansive reading with Article 51''s original intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement_basis(arti_tr_t2001, observed).
narrative_ontology:measurement(arti_tr_t2007, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement_basis(arti_tr_t2007, observed).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement_basis(arti_tr_t2013, observed).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement_basis(arti_tr_t2018, observed).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement_basis(arti_be_t2001, observed).
narrative_ontology:measurement(arti_be_t2007, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2007, 0.7).
narrative_ontology:measurement_basis(arti_be_t2007, observed).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2013, 0.75).
narrative_ontology:measurement_basis(arti_be_t2013, observed).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement_basis(arti_be_t2018, observed).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(arti_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement_basis(arti_su_t2001, observed).
narrative_ontology:measurement(arti_su_t2007, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2007, 0.78).
narrative_ontology:measurement_basis(arti_su_t2007, observed).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2013, 0.82).
narrative_ontology:measurement_basis(arti_su_t2013, observed).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement_basis(arti_su_t2018, observed).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.85).
narrative_ontology:measurement_basis(arti_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_51_self_defense' kernel, focusing on the expansive interpretation of preventive force. It is structurally distinct from the 'narrow_armed_attack_reading' and 'unable_unwilling_doctrine_reading' due to differing ε values and stakeholder impacts, but all are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
