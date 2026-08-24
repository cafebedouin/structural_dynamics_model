% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable/Unwilling Doctrine Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The unable/unwilling doctrine is a mid-point reading of Article 51: it
 *   requires an actual non-state actor armed attack (rejecting pure
 *   prevention) but permits unilateral cross-border force when the host state
 *   fails to suppress the threat. This creates a hybrid constraint — it
 *   coordinates a collective response to transnational terrorism while
 *   extracting sovereign prerogatives from host states. The doctrine emerged
 *   post-9/11, was invoked in Afghanistan (2001), Pakistan (drone campaign),
 *   Yemen, Somalia, Syria (anti-ISIS), and Iraq. Its extraction has
 *   accumulated as the 'unwilling' prong expanded to cover states that merely
 *   disagree with the intervener's threat assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable/Unwilling Doctrine Reading").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '9663c9fb-2f7b-4cc0-b643-8b7841c181fd').
narrative_ontology:cs_kernel_codification('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', fixed_text).
narrative_ontology:cs_authority_grounding('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', lineage).
narrative_ontology:cs_interpretation_layer_present('9663c9fb-2f7b-4cc0-b643-8b7841c181fd').
narrative_ontology:cs_reading_relation('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', foundational, non_state_actor_attack_triggers_self_defense).
narrative_ontology:cs_axiom_status(non_state_actor_attack_triggers_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', non_state_actor_attack_triggers_self_defense, conventional).
narrative_ontology:cs_axiom('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', foundational, host_state_unwilling_unable_satisfies_necessity).
narrative_ontology:cs_axiom_status(host_state_unwilling_unable_satisfies_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', host_state_unwilling_unable_satisfies_necessity, conventional).
narrative_ontology:cs_reference_frame('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', post_911_state_practice).
narrative_ontology:cs_drift_state('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9663c9fb-2f7b-4cc0-b643-8b7841c181fd', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_sovereignty_bypassed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, affected_civilian_populations).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, unable_unwilling_self_defense_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_armed_attack_threshold).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, necessity_condition_host_state_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that conduct cross-border military operations against non-state actors under the unable/unwilling doctrine. They gain operational flexibility to strike threats without Security Council authorization or host state consent. They bear political and reputational costs but retain the initiative. Exit from the doctrine would mean accepting stricter attribution standards or Security Council vetoes.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, beneficiary).

% States from whose territory non-state actors operate, which are deemed unable or unwilling to suppress the threat. They suffer unauthorized military incursions, erosion of territorial sovereignty, and potential domestic political destabilization. They can protest diplomatically, seek Security Council action, or attempt to suppress the non-state actors themselves to remove the pretext — but often lack capacity or political will.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_sovereignty_bypassed, payer,
    moderate, biographical, constrained, national).

% The non-state actors whose attacks trigger the doctrine. They are the object of the intervention but have no voice in the legal framework. Their strategic calculations adapt to the doctrine (dispersing, embedding in civilian populations, seeking state patronage). They would reject the legitimacy of any external use of force but are structurally excluded from the interpretive community.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_armed_groups, excluded,
    powerless, immediate, trapped, local).

% Civilians in host state territories where strikes occur. They bear collateral damage, displacement, and long-term insecurity from both non-state actor presence and intervening state operations. They have no exit from the constraint's effects and no formal standing in the legal debate.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, affected_civilian_populations, payer,
    powerless, immediate, trapped, local).

% Academics and practitioners who interpret, critique, and shape the doctrine through scholarship, ICJ briefs, and policy advising. They do not directly bear costs or collect benefits but their analysis influences state practice and judicial outcomes over time.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% The primary institutional competitor for authorization of cross-border force. The doctrine's expansion reduces the Council's gatekeeping role; its contraction strengthens it. Permanent members with intervention capacity (US, UK, France, Russia, China) hold veto power that shapes whether the Council acts as an alternative or a rubber stamp.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for states to respond to non-state actor attacks originating from territories where the host state cannot or will not act, filling a gap between strict attribution requirements and purely preventive force.
% TRANSFER_FUNCTION: Transfers the exclusive right to authorize cross-border military force from the host state's sovereign prerogative (and the Security Council's collective security role) to the intervening state's unilateral judgment of necessity and proportionality. Moves the burden of proof from 'armed attack by a state' to 'non-state actor attack + host state failure.'
% ABSENT_VOICES: Host state civilian populations who bear strike consequences; non-state actors who are the doctrine's operational target but have no legal personality; smaller states lacking intervention capacity who would prefer Security Council authorization; regional organizations (AU, OAS, ASEAN) whose collective security roles are bypassed.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine vanished overnight, states would revert to either (a) strict Nicaragua-style attribution requiring state control over non-state actors, (b) Security Council authorization under Chapter VII, or (c) unilateral force justified under expansive preventive claims. The legal basis for post-9/11 counterterrorism operations in Pakistan, Yemen, Somalia, Syria, and Iraq would collapse, forcing a reorganization of the international use-of-force regime.
% FOUNDING_PROBLEM: The post-9/11 emergence of transnational terrorist networks (Al Qaeda, ISIS) operating from ungoverned or hostile territories where host states were unable or unwilling to suppress them, creating a perceived gap between the UN Charter's state-centric self-defense framework and the operational reality of non-state threats.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the intervening states themselves (US, UK, Turkey, Israel, Russia) in national security strategies and UNGA statements. It is contested by the Non-Aligned Movement, the ICJ in the Wall Advisory Opinion (2004) and DRC v. Uganda (2005), and scholars including Dinstein, Corten, and Green, who argue the Charter framework already accommodates non-state actor attacks via attribution or that the doctrine unlawfully expands self-defense. No neutral third-party corroboration exists — the dispute is structural.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the doctrine's transfer of sovereign authority from host to intervening states — a significant but not total extraction, since host states retain residual sovereignty and the doctrine imposes necessity/proportionality limits. Suppression (0.55) is moderate: the constraint is enforced through military action and diplomatic pressure, not total closure of alternatives (host states can still suppress threats themselves or invoke the Security Council). Theater (0.4) captures the legal ritual of 'unwilling/unable' certifications that often follow rather than guide operational decisions. Accessibility collapse (0.5) and resistance (0.7) reflect the lively scholarly and diplomatic contestation — alternatives (attribution, Security Council, preventive) remain conceptually available but politically costly.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state's seat, the doctrine is a necessary coordination mechanism (Rope-like) addressing a genuine collective action problem: terrorism thrives in sovereignty gaps. From the host state's seat, it is an extraction mechanism (Snare-like) that licenses powerful states to violate weak states' territory. The engine computes this divergence from the structural data — the claimed type (Tangled Rope) acknowledges both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are structural beneficiaries (d ~0.2): they gain operational freedom and legal cover. Their exit is constrained — they could accept stricter rules but lose strategic initiative. Host states are structural targets (d ~0.85): they lose sovereign control over territory and face military incursions. Their exit is constrained — they can suppress the threat (often beyond capacity) or accept violation. Non-state actors and civilians are trapped (d ~0.95) with no legal personality or exit. The Security Council sits near analytical (d ~0.5) — its authority is both competitor and potential legitimator.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transnational terrorism from ungoverned spaces) remains live but has mutated — the original Al Qaeda core is degraded, but franchised affiliates and new groups (ISIS-K, AQAP, etc.) persist. The doctrine has expanded beyond its founding scope: 'unwilling' now covers policy disagreements, not just capacity failures. This suggests mandatrophy — the constraint persists and expands after its original justification has attenuated. The founding_problem_status 'contested' and disappearance_verdict 'world_rearranges' capture this: the arrangement would not survive its founding problem's resolution unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the unable/unwilling doctrine a distinct legal standard with its own criteria, or merely a gloss on existing necessity/proportionality analysis that adds no independent constraint?',
    'ICJ or authoritative tribunal ruling on whether ''unwilling or unable'' constitutes an independent legal test or is subsumed within necessity. State practice convergence on specific evidentiary thresholds for ''unwilling'' vs ''unable''.',
    'If a distinct standard, it has independent extractive force (host states must meet specific benchmarks). If a gloss, its extraction is parasitic on the underlying self-defense framework and the constraint''s boundaries collapse into the broader Article 51 debate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the doctrine has independent legal content or is interpretive filler.').

omega_variable(
    attribution_vs_unwilling_unable_boundary,
    'Where does the unable/unwilling test end and the effective control/attribution test begin? Can a host state''s failure to act be equated with attribution?',
    'Comparative analysis of ICJ Nicaragua (effective control), Tadic (overall control), and state practice in Syria/Iraq strikes. Scholarly consensus on whether the tests are alternatives or cumulative.',
    'If the tests merge, the constraint becomes a Trojan horse for expansive attribution — increasing extraction. If they remain distinct, the constraint''s coordinate function (requiring actual non-state actor attack) is preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_vs_unwilling_unable_boundary, conceptual, 'Boundary between unable/unwilling and traditional attribution standards.').

omega_variable(
    proportionality_operationalization,
    'How is proportionality measured in unable/unwilling strikes — against the non-state actor threat alone, or including host state sovereignty costs?',
    'Analysis of state justifications (US White Papers, UK legal positions, Turkish statements) and any judicial review of specific strikes. Military manual guidance on cross-border proportionality.',
    'If proportionality ignores sovereignty costs, extraction is higher than measured. If it includes them, the constraint has an internal brake that limits its extractive reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_operationalization, empirical, 'Whether proportionality calculus internalizes host state sovereignty costs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of host state alternatives structural (legal prohibition on consent withdrawal, veto-wielding Security Council) or internalized (host states accept the doctrine as legitimate due to counterterrorism cooperation benefits)?',
    'Post-exit suppression trajectory: if a host state revokes consent (e.g., Pakistan 2011, Iraq 2020) and strikes continue, suppression is structural. If host states actively facilitate, internalized acceptance plays a role.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint operates through consent manufactured by the doctrine itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in host state compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art51_uu_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(art51_uu_tr_t2004, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(art51_uu_tr_t2008, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(art51_uu_tr_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(art51_uu_tr_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(art51_uu_tr_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2017, 0.4).
narrative_ontology:measurement(art51_uu_tr_t2020, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(art51_uu_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(art51_uu_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(art51_uu_be_t2004, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2004, 0.45).
narrative_ontology:measurement(art51_uu_be_t2008, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(art51_uu_be_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement(art51_uu_be_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(art51_uu_be_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement(art51_uu_be_t2020, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(art51_uu_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(art51_uu_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(art51_uu_su_t2004, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2004, 0.45).
narrative_ontology:measurement(art51_uu_su_t2008, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(art51_uu_su_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2011, 0.52).
narrative_ontology:measurement(art51_uu_su_t2014, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2014, 0.53).
narrative_ontology:measurement(art51_uu_su_t2017, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2017, 0.54).
narrative_ontology:measurement(art51_uu_su_t2020, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(art51_uu_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, security_council_authorization_gatekeeping).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_sovereignty_non_intervention).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'Article 51 self-defense' kernel into three structurally distinct constraints by threat trigger and authorization gate. The unable/unwilling reading occupies the middle ground: narrower than preventive (requires actual attack) but wider than strict attribution (permits non-state actor triggers). Its ε (0.65) is higher than narrow_armed_attack (~0.2) but lower than expansive_preventive (~0.8), reflecting its hybrid coordination/extraction structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__unable_unwilling_doctrine_reading, powerful, 0.15).
constraint_indexing:directionality_override(article_51_self_defense__unable_unwilling_doctrine_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
