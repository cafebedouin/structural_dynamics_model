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
 *   human_readable: Expansive Preventive Self-Defense Doctrine (Article 51)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint represents an expansive interpretation of Article 51 of
 *   the UN Charter, which permits self-defense. This reading extends the
 *   right to preemptive or preventive force against non-state actors or
 *   emerging threats, with the necessity of such force largely self-judged by
 *   the acting state. This interpretation emerged in response to new security
 *   challenges, particularly after 9/11, and has been adopted by militarily
 *   capable states to justify interventions that would otherwise lack
 *   explicit UN Security Council authorization. The claimed type is
 *   'tangled_rope' because it purports to coordinate state security while
 *   enabling significant extraction from target populations and multilateral
 *   institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.85).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.75).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense Doctrine (Article 51)").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '8ae008b6-cf01-484f-8ad9-c1575820c8ff').
narrative_ontology:cs_kernel_codification('8ae008b6-cf01-484f-8ad9-c1575820c8ff', fixed_text).
narrative_ontology:cs_authority_grounding('8ae008b6-cf01-484f-8ad9-c1575820c8ff', extraction).
narrative_ontology:cs_interpretation_layer_present('8ae008b6-cf01-484f-8ad9-c1575820c8ff').
narrative_ontology:cs_reading_relation('8ae008b6-cf01-484f-8ad9-c1575820c8ff', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ae008b6-cf01-484f-8ad9-c1575820c8ff', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('8ae008b6-cf01-484f-8ad9-c1575820c8ff', foundational, inherent_right_to_self_defense_against_any_threat).
narrative_ontology:cs_axiom_status(inherent_right_to_self_defense_against_any_threat, holdable).
narrative_ontology:cs_axiom_grounding('8ae008b6-cf01-484f-8ad9-c1575820c8ff', inherent_right_to_self_defense_against_any_threat, deontological).
narrative_ontology:cs_axiom('8ae008b6-cf01-484f-8ad9-c1575820c8ff', foundational, necessity_is_self_judged_by_the_threatened_state).
narrative_ontology:cs_axiom_status(necessity_is_self_judged_by_the_threatened_state, holdable).
narrative_ontology:cs_axiom_grounding('8ae008b6-cf01-484f-8ad9-c1575820c8ff', necessity_is_self_judged_by_the_threatened_state, conventional).
narrative_ontology:cs_reference_frame('8ae008b6-cf01-484f-8ad9-c1575820c8ff', post_9_11_security_paradigm).
narrative_ontology:cs_drift_state('8ae008b6-cf01-484f-8ad9-c1575820c8ff', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8ae008b6-cf01-484f-8ad9-c1575820c8ff', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_contractors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret Article 51 broadly to justify unilateral military action against perceived threats, often self-judging the necessity and proportionality of force. They benefit from the flexibility to act without explicit UNSC authorization.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the direct costs of military interventions, including casualties, displacement, and destruction of infrastructure. They have no direct voice in the interpretation or application of Article 51.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The UNSC's role in authorizing force is bypassed by this expansive interpretation, diminishing its authority and the collective security framework it represents. Its power to constrain unilateral action is eroded.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_authority, payer,
    institutional, generational, constrained, global).

% Analyze the legal implications and precedents set by this interpretation, often critiquing its departure from traditional understandings of self-defense and its impact on international order.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% Benefit from increased demand for military hardware, intelligence services, and logistical support as states engage in more frequent and expansive preemptive operations. Their interests align with a broader interpretation of self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_contractors, beneficiary,
    organized, biographical, mobile, global).

% Are the primary targets of this doctrine, often without clear attribution or state sponsorship, leading to a blurring of lines between self-defense and counter-terrorism operations. They are excluded from the legal discourse that defines their targeting.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, non_state_armed_groups, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows states to coordinate their responses to perceived threats that do not neatly fit the traditional 'armed attack' definition, theoretically preventing larger conflicts by addressing threats early.
% TRANSFER_FUNCTION: Transfers the authority to determine the legality of force from multilateral bodies (UNSC) to individual states, and transfers the costs of intervention (human and material) to target populations.
% ABSENT_VOICES: Populations in target regions, non-state actors themselves, and states with less military capability who fear becoming targets of such expansive interpretations would object. Their voices are marginalized in the discourse dominated by powerful states.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, militarily capable states would face significantly higher legal and political hurdles for unilateral preemptive action, likely leading to more reliance on UNSC authorization or a re-evaluation of threat responses. The international legal landscape would shift towards a more restrictive use of force.
% FOUNDING_PROBLEM: The rise of transnational terrorism and other non-state threats challenged the traditional state-centric framework of international law, creating a perceived gap in how states could legitimately defend themselves against actors not easily attributable to a state.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and their defense establishments consistently attest that the problem of non-state threats is live and evolving, requiring flexible self-defense interpretations. International law scholars and some UN member states, while acknowledging the threat, contest whether this expansive reading is the appropriate or legal solution, often citing the risk of abuse.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant costs imposed on target populations and the erosion of multilateral veto authority. Suppression (0.75) is high due to the unilateral nature of force and the limited avenues for challenge by affected parties or international bodies. The theater ratio (0.4) indicates that while genuine security concerns exist, a substantial portion of the justification for intervention serves to legitimize unilateral power projection rather than pure defense. The increasing trend in extractiveness and suppression over time reflects the hardening of this interpretation into practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of militarily capable states, this reading is a necessary adaptation of international law to modern threats, a 'rope' for collective security. From the perspective of target populations and the UNSC, it functions as a 'snare' that legitimizes unilateral power and undermines international legal norms. The engine's computation of a 'tangled_rope' classification for this reading captures this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and defense contractors are clear beneficiaries, gaining flexibility and revenue respectively. Target populations and the UN Security Council's veto authority are victims, bearing the costs of intervention and the erosion of collective security mechanisms. International law scholars act as observers, analyzing the doctrine's impact. Non-state armed groups are excluded, as they are the targets of the doctrine and lack a voice in its formulation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a potentially extractive doctrine as pure coordination. While the initial impulse to address non-state threats might have been a genuine coordination problem, the expansive interpretation has allowed for significant extraction and suppression, indicating a drift from a 'rope' to a 'tangled_rope' or even 'snare' in practice. The 'live' but 'contested' status of the founding problem highlights this ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_self_judgment_bias,
    'To what extent is the ''necessity'' of preemptive force genuinely objective, versus a self-serving judgment by the acting state?',
    'Independent, ex-post facto review by an international judicial body with binding authority, assessing the evidence of threat and proportionality of response without political influence.',
    'If necessity is consistently found to be self-serving, the doctrine''s legitimacy as a coordination mechanism collapses, reclassifying it closer to a ''snare''. If objective necessity is consistently demonstrated, it strengthens the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_bias, empirical, 'Bias in state''s self-judgment of necessity for preemptive force.').

omega_variable(
    non_state_actor_attribution_ambiguity,
    'How reliably can non-state actor threats be attributed to a specific state or entity, and does this attribution meet international legal standards?',
    'Development of universally accepted, verifiable standards for attribution of non-state actor actions, enforced by an impartial international body.',
    'If attribution is consistently weak or contested, the doctrine''s application becomes arbitrary, increasing extractiveness and suppression, pushing it towards a ''snare''. Strong, verifiable attribution would reduce ambiguity and potentially lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_ambiguity, empirical, 'Reliability of attributing non-state actor threats.').

omega_variable(
    doctrine_vs_practice_gap,
    'Is the gap between the stated doctrine of ''necessity'' and the actual practice of preemptive force widening, indicating a performative rather than functional justification?',
    'Systematic comparison of official justifications for interventions with independent, on-the-ground assessments of threat levels and outcomes, over time.',
    'A widening gap would increase the ''theater_ratio'' and push the classification towards a ''piton'' (if function atrophies) or a ''snare'' (if extraction increases under cover of performance). A narrowing gap would suggest greater fidelity to the stated coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_practice_gap, empirical, 'Gap between stated doctrine and actual practice of preemptive force.').

omega_variable(
    kernel_reading_divergence,
    'Is this expansive reading of Article 51 a legitimate evolution of international law, or a reinterpretation that fundamentally undermines the UN Charter''s collective security framework?',
    'A definitive ruling by the International Court of Justice or a new UN General Assembly resolution clarifying the scope of Article 51 self-defense in the context of non-state actors.',
    'If deemed an illegitimate reinterpretation, its classification would shift towards a ''snare'' from the perspective of the collective security framework. If affirmed as legitimate, its ''rope'' aspects would be strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Legitimacy of expansive reading within UN Charter framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(arti_tr_t1998, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(arti_tr_t2006, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(arti_tr_t2014, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(arti_be_t1998, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(arti_be_t2006, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2006, 0.8).
narrative_ontology:measurement(arti_be_t2014, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2014, 0.83).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(arti_su_t1998, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(arti_su_t2006, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(arti_su_t2014, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2014, 0.73).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 51 self-defense kernel. The other readings are 'narrow_armed_attack_reading' and 'unable_unwilling_doctrine_reading'. Each represents a distinct structural claim about the scope and conditions of self-defense under international law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
