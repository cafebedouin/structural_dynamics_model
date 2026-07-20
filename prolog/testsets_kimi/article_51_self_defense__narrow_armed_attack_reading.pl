% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the narrow_armed_attack_reading of the
 *   Article 51 self-defense kernel under the UN Charter. It holds that
 *   self-defense is available only in response to an actual or imminent armed
 *   attack by a state attributable under international law, excluding
 *   preventive force and most non-state actor threats. The reading is
 *   contested by powerful states seeking strategic flexibility and defended
 *   by weaker states and multilateral institutions whose security and
 *   authority depend on restricting unilateral force.
 *
 * KEY AGENTS:
 *   - weaker_states: Primary beneficiary (organized/constrained) â gain legal protection against preventive intervention
 *   - multilateral_institutions: Agenda setter (institutional/constrained) â administer and interpret the UN Charter framework
 *   - powerful_states: Primary target (powerful/mobile) â bear constrained strategic freedom
 *   - non_state_actors: Excluded (organized/trapped) â cannot invoke Article 51 directly
 *   - international_legal_scholars: Analytical observer (analytical/analytical) â sustain the interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.62).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.48).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '23746f76-069e-474a-a0ab-f982272060d9').
narrative_ontology:cs_kernel_codification('23746f76-069e-474a-a0ab-f982272060d9', formalized).
narrative_ontology:cs_authority_grounding('23746f76-069e-474a-a0ab-f982272060d9', lineage).
narrative_ontology:cs_interpretation_layer_present('23746f76-069e-474a-a0ab-f982272060d9').
narrative_ontology:cs_reading_relation('23746f76-069e-474a-a0ab-f982272060d9', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('23746f76-069e-474a-a0ab-f982272060d9', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('23746f76-069e-474a-a0ab-f982272060d9', foundational, armed_attack_threshold_required).
narrative_ontology:cs_axiom_status(armed_attack_threshold_required, holdable).
narrative_ontology:cs_axiom_grounding('23746f76-069e-474a-a0ab-f982272060d9', armed_attack_threshold_required, conventional).
narrative_ontology:cs_axiom('23746f76-069e-474a-a0ab-f982272060d9', foundational, state_attribution_prerequisite).
narrative_ontology:cs_axiom_status(state_attribution_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('23746f76-069e-474a-a0ab-f982272060d9', state_attribution_prerequisite, conventional).
narrative_ontology:cs_reference_frame('23746f76-069e-474a-a0ab-f982272060d9', un_charter_collective_security_framework).
narrative_ontology:cs_drift_state('23746f76-069e-474a-a0ab-f982272060d9', post_9_11_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23746f76-069e-474a-a0ab-f982272060d9', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a legal rule that restricts powerful states from launching preventive strikes or unilateral interventions against them; rely on the UN Charter framework as a bulwark against great-power aggression and as a source of sovereign equality guarantees.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    organized, generational, constrained, global).

% Administer and interpret the UN Charter collective security framework through the Security Council, ICJ, and treaty bodies; derive institutional authority from being the designated legitimate channel for authorizing force and adjudicating self-defense claims.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Bear the constraint of having strategic and military options limited by a narrow legal interpretation; cannot lawfully invoke Article 51 against non-state actors or preventive threats without meeting strict state-attribution standards, though they sometimes act outside the rule and absorb diplomatic costs.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    powerful, biographical, mobile, global).

% Cannot trigger or directly invoke Article 51 self-defense rights under this reading unless their conduct is formally attributable to a host state; are treated as law enforcement or criminal threats rather than armed attack triggers, leaving them outside the protective legal framework.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_actors, excluded,
    organized, immediate, trapped, regional).

% Produce interpretive scholarship that sustains or contests the narrow reading; their analytical work shapes state rhetoric and ICJ reasoning without directly paying or benefiting from the constraint's operation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restricts unilateral use of force to cases of actual or imminent armed attack by a state, preserving the UN Security Council's monopoly on authorizing force and protecting weaker states from preventive war.
% TRANSFER_FUNCTION: Transfers strategic freedom from powerful states to the collective security system and weaker states; powerful states lose the option of preventive or non-state-targeting force, while weaker states gain legal protection and multilateral institutions retain authority over war powers.
% ABSENT_VOICES: Non-state actors facing existential threats from host states or external powers have no standing to trigger Article 51; populations in territories where unable-unwilling doctrines might apply are not directly represented in the legal framework; military strategists in powerful states advocating preventive self-defense are legally overridden.
% DISAPPEARANCE_RATIONALE: If the narrow reading disappeared, powerful states would expand unilateral force against non-state actors and preventive targets, the UN Security Council's centrality would erode, and weaker states would lose the legal shield against great-power intervention that has structured post-1945 international order.
% FOUNDING_PROBLEM: The interwar period demonstrated how expansive and self-judged claims of self-defense were used to justify aggression; the UN Charter was designed to centralize decisions on force and tightly cabin unilateral exceptions.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the UN Charter travaux prÃ©paratoires and the Kellogg-Briand Pact's failure corroborates the founding problem from outside the beneficiary set; however, revisionist legal and policy scholarship from powerful-state strategic studies institutions contests whether the narrow reading remains adequate for contemporary non-state threats.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.62) because the constraint significantly limits the strategic options of powerful states. Suppression is moderate (0.48) because expansive alternatives remain conceptually alive and are intermittently practiced, but are institutionally marginalized. Theater ratio is moderate (0.35): much legal discourse genuinely coordinates behavior, but a growing share of narrow-reading enforcement is performative defense of a text that powerful states increasingly treat as optional. Resistance is high (0.72) because major military powers actively contest the reading through practice and scholarship. The temporal series share a single grid spanning the post-9/11 era, when the armed-attack threshold came under sustained pressure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (powerful states) and the beneficiary seats (weaker states, multilateral institutions) should compute as different constraint types. From the powerful-state perspective, the same legal text functions as an institutionalized snare on strategic autonomy. From the weaker-state perspective, it functions as protective coordination. The divergence is structural and intended: the engine measures it from the authored beneficiary/victim data and divergent exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states are declared beneficiaries with constrained exit; they sit near the beneficiary pole (low d), receiving security coordination. Multilateral institutions are agenda setters with constrained exit; their authority is preserved by the constraint, yielding low-to-moderate d. Powerful states are declared victims with mobile exit; despite their global power, the legal framework extracts strategic freedom, placing them near the target pole (high d). The engine will compute effective extraction accordingly: amplified for powerful states, damped or inverted for weaker states and institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing aggressive war through self-judged defense claims â is contested as to whether it remains live. The R5 genealogy flags this: if the problem is dead but the arrangement persists, mandatrophy risk rises. Here, the narrow reading's defenders argue the problem is live (non-state threats do not justify abandoning the threshold), while critics argue the reading has outlived its fit. The authored metrics do not resolve this; the engine's mismatch consumer will flag it against the theater and extraction trajectories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_asymmetry,
    'Does the narrow reading effectively constrain powerful states, or is it primarily a legitimating discourse that powerful states ignore when strategically convenient?',
    'Comparative case study of powerful state military interventions and subsequent ICJ rulings or Security Council debates to measure actual compliance costs.',
    'If powerful states routinely exit at low cost, the constraint''s extractiveness is higher in theory than practice and the reading functions more as theater; if compliance costs are real, the asymmetric extraction is substantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether the constraint is enforced or theatrical').

omega_variable(
    attribution_standard_determinacy,
    'Is the ''attributable to a state'' standard under the narrow reading empirically determinable, or does it collapse into political judgment when applied to non-state actor threats?',
    'Systematic review of ICJ and state practice applying attribution standards to non-state actors.',
    'If attribution is inherently political, the narrow reading provides less coordination benefit than claimed and the constraint''s rope component is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_determinacy, conceptual, 'Whether state attribution is a legal standard or political cover').

omega_variable(
    non_state_actor_pressure,
    'Does the rise of transnational non-state actor threats structurally destabilize the narrow reading regardless of interpretive fidelity?',
    'Longitudinal analysis of state practice and legal scholarship pre- and post-2001 to measure erosion of the armed-attack threshold.',
    'If non-state threats have structurally degraded the reading, the constraint may be drifting toward mandatrophy or piton status as practice diverges from text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_pressure, empirical, 'Whether non-state threats undermine the reading''s stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t6, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(arti_tr_t18, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(arti_tr_t23, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 23, 0.35).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arti_be_t6, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(arti_be_t18, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(arti_be_t23, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 23, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t6, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(arti_su_t18, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(arti_su_t23, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 23, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
