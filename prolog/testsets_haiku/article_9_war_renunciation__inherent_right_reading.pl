% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation with Inherent Self-Defense Right
 *   domain: constitutional/security
 *
 * SUMMARY:
 *   Japan's Article 9 Constitution (1947) states: 'the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as means of settling international disputes' and 'land, sea,
 *   and air forces, as well as other war potential, will never be
 *   maintained.' Yet within five years, Japan established the Self-Defense
 *   Forces (SDF). The inherent-right reading reconciles this by arguing that
 *   Article 9 prohibits 'war' (aggressive use of force) but not the inherent
 *   right to self-defense recognized under international law, provided
 *   military forces remain 'minimum necessary' for territorial defense. This
 *   reading is one of three structurally distinct interpretations of the same
 *   kernel text, each with different empirical implications for what military
 *   capacity is legitimate.
 *
 * KEY AGENTS:
 *   - SDF operational authority: operates as the institutional implementation of the inherent-right reading; benefits from the reading's legitimation of military force while constrained by the 'minimum necessary' doctrine.
 *   - Conservative constitutional interpreters: scholars, judges, and policymakers who defend the inherent-right reading as the true exegesis of Article 9; benefit from a framing that preserves constitutional appearance while enabling military force.
 *   - Strict pacifist advocates: civil society and constitutional scholars who read Article 9 as an absolute prohibition; bear the cost of marginalization from official interpretation.
 *   - Regional security policymakers: defense strategists operating under the 'minimum necessary' constraint; bear costs of potential strategic insufficiency relative to assessed threats.
 *   - Collective-defense advocates: U.S. alliance partners and strategic actors interested in Japan's participation in collective security; structurally excluded from the scope the inherent-right reading permits.
 *   - Constitutional Court: the ultimate legal authority; currently passive, treating Article 9 as non-justiciable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.31).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.28).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation with Inherent Self-Defense Right").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional/security").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'a76bf6b8-f447-47c6-b78f-eeaac5ab8f56').
narrative_ontology:cs_kernel_codification('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', fixed_text).
narrative_ontology:cs_authority_grounding('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', lineage).
narrative_ontology:cs_interpretation_layer_present('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56').
narrative_ontology:cs_reading_relation('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', foundational, inherent_defense_right_recognized).
narrative_ontology:cs_axiom_status(inherent_defense_right_recognized, holdable).
narrative_ontology:cs_axiom_grounding('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', inherent_defense_right_recognized, deontological).
narrative_ontology:cs_axiom('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', foundational, war_vs_defense_distinction_holds).
narrative_ontology:cs_axiom_status(war_vs_defense_distinction_holds, holdable).
narrative_ontology:cs_axiom_grounding('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', war_vs_defense_distinction_holds, deontological).
narrative_ontology:cs_reference_frame('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', inherited_right_with_constitutional_renunciation).
narrative_ontology:cs_drift_state('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', contemporary_security_environment_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a76bf6b8-f447-47c6-b78f-eeaac5ab8f56', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, sdf_operational_authority).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, conservative_constitutional_interpreters).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_security_policymakers_constrained_by_minimum_necessary_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31) because the constraint operates primarily at the threshold level rather than through continuous rent collection. The 'minimum necessary' doctrine is proportionality-grounded and genuinely constrains military ambitions—it is not pure extraction like a monopoly pricing mechanism. However, it does transfer legitimacy from textual language to institutional definition: the SDF gets to define what counts as 'minimum,' and the court does not second-guess. Suppression (0.28) is moderate because strict pacifists are excluded from the official consensus without overt coercion—the suppression is structural (courts will not entertain pacifist readings) rather than violent. Theater ratio (0.42) reflects the ongoing performance of constitutional pacifism despite military modernization: speeches emphasize 'minimum necessary' and 'defensive' while capabilities grow. The measurement trajectory shows theater ratio declining from 1952–1990 as the reinterpretation normalized, then stabilizing as it became institutional consensus. Extractiveness rose modestly as the SDF's actual scope expanded, though 'minimum necessary' remains a real constraint relative to what an unrestricted military could be.
 *
 * PERSPECTIVAL GAP:
 *   The SDF and the conservative interpreters experience the reading as enabling (legitimating military force within bounds). Pacifists experience it as extractive (their reading is displaced). Security policymakers experience it as constraining. Collective-defense advocates experience it as insufficient. The engine's per-seat computation should reveal this: high d for pacifists (their alternative reading is suppressed), low d for SDF beneficiaries, moderate d for constrained security policymakers. The claim does not resolve this gap; the metrics describe it.
 *
 * DIRECTIONALITY LOGIC:
 *   The SDF and conservative interpreters are the primary beneficiaries: the reading legitimates military force that would be unconstitutional under pacifist reading. d for this group approaches 0 (beneficiaries). Strict pacifists are the primary targets: their reading is constitutionally excluded from official authority, and suppression is structural (courts will not hear pacifist arguments). d for this group approaches 1.0 (targets). Regional security policymakers are partially constrained: they have a real military but cannot use it at full scope; they are neither pure beneficiaries nor pure targets. d for this group sits around 0.5–0.6. The 'minimum necessary' doctrine is the distributional mechanism: it provides enough legitimacy to keep pacifists quiet (they maintain 'defensive' character) while giving SDF and conservatives enough operational space. Collective-defense advocates are excluded; their exit from the system is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits both tangled-rope features and piton risk. The coordination function (reconciling pacifism with defense necessity) is real, grounded in the constitutional text and the post-war security dilemma. The asymmetric extraction (SDF gets to define 'minimum necessary' while pacifists lose their reading) is also real. The theater ratio trajectory (declining from 0.65 to 0.42) suggests the constraint normalized over time: initially highly theatrical (everyone performing constitutional pacifism), it became institutionalized doctrine. This is not mandatrophy (function death + inertial persistence) but rather successful embedding. The risk is latent: if the founding problem (post-war remilitarization contradiction) is ever solved (e.g., through constitutional amendment or strategic realignment), the constraint could flip to piton (sustained for theatrical appearance of commitment without functional necessity). Currently, the problem is contested (regional powers rising, collective-defense expectations unmet), so the constraint remains tangled_rope. Mandatrophy is not present; the coordination function persists as live and contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_definition_ambiguity,
    'What counts as ''minimum necessary'' for territorial defense under Article 9? Is this a fixed doctrine, a sliding scale relative to threats, or inherently contestable?',
    'Review of SDF doctrine statements, defense white papers, and budget justifications over time; comparison with defense spending and military capability assessments from independent analysts; hypothetical: if regional threats receded (e.g., normalization with neighbors), would ''minimum necessary'' shrink, or would it remain at current levels?',
    'If ''minimum necessary'' is definitionally loose, the constraint has higher extractiveness than measured (SDF can expand under the label). If it is strict and threat-responsive, the measured extractiveness is accurate. If it slides upward ratchet-like (threat drops but ''minimum necessary'' does not), the constraint is piton-risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_definition_ambiguity, empirical, 'Whether ''minimum necessary'' is a fixed, threat-responsive, or ratcheting standard.').

omega_variable(
    reading_stability_under_strategic_shock,
    'Would the inherent-right reading survive a major strategic shock (large regional war, alliance collapse, direct attack on Japan)? Does the reading''s legitimacy depend on a particular security environment?',
    'Historical case study: Japan''s security posture in 1950–1953 during Korean War (SDF was established under emergency); current counterfactual modeling: if China attacked Taiwan and the U.S. requested Japanese military involvement, how would the reading respond?',
    'If the reading is shock-fragile (becomes untenable under pressure), it is more piton-like than tangled_rope—it persists in equilibrium but lacks resilience. If it is robust, it is a true structural reconciliation. Finding: if shock causes flip to collective-defense reading rather than strict pacifism, the constraint remains within the kernel family (no mandatrophy). If shock causes pacifist reading to prevail, the constraint does experience mandatrophy (death of function, inertial persistence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_under_strategic_shock, conceptual, 'Whether the inherent-right reading''s legitimacy is stable across security environments or environmentally contingent.').

omega_variable(
    constitutional_court_non_justiciability_fiction,
    'Is the Constitutional Court''s treatment of Article 9 as non-justiciable a genuine feature of the constraint, or a performance that masks the Court''s choice to defer to the political branches?',
    'If a challenge to the SDF''s constitutionality reached the Court with strong political backing (e.g., after a pacifist electoral sweep), would the Court maintain non-justiciability or rule? Does non-justiciability serve a real coordination function (keeping the question out of courts) or is it theater (deferring an uncomfortable question)?',
    'If non-justiciability is functional (prevents court-driven destabilization), it supports the constraint''s tangled-rope classification (genuine coordination need). If it is theater (courts are passively supporting the political reading), it raises suppression and theater ratio, moving toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_court_non_justiciability_fiction, conceptual, 'Whether constitutional non-justiciability is a coordination mechanism or a suppression device.').

omega_variable(
    kernel_reading_distinctness_stability,
    'Are the three readings (strict pacifist, inherent-right, collective-defense) truly structurally distinct, or do they blur into a continuum under pressure?',
    'Test: can a reading coherently hold both ''inherent right to defense'' AND ''no collective-defense obligation'' (this reading''s position)? Can strict pacifism coherently permit any armed force? The test reveals whether the boundaries are hard or soft.',
    'Hard boundaries mean the readings are three distinct constraints (three JSON files, linked by network). Soft boundaries mean the kernel itself is under-specified and readings shade into each other (would require different modeling). Finding soft boundaries would change the story structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinctness_stability, conceptual, 'Whether the three readings of Article 9 are structurally discrete or a continuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1952, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1952, 0.65).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1970, 0.58).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1952, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1952, 0.18).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1952, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1952, 0.15).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1990, 0.26).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2010, 0.27).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Article 9 is a contested kernel with three structurally distinct readings, each instantiating a different constraint. This story (inherent-right reading) coexists with strict-pacifist and collective-defense readings as a constraint family. The three readings share the same constitutional text but have different ε values, beneficiary/victim structures, and legitimacy bases. Links represent textual/conceptual kinship, not causal influence: which reading prevails in courts/policy affects the binding constraint, but all three are live interpretations of the same kernel. Each reading's ε is independent; a shift from one reading to another is a constraint-replacement event, not a dial adjustment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
