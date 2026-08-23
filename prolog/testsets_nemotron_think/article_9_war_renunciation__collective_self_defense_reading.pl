% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Reading
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   The collective_self_defense_reading is one of three live readings of the
 *   Article 9 war renunciation kernel. It holds that Japan's inherent right
 *   of self-defense extends to collective self-defense when Japan's survival
 *   is threatened, permitting military action to defend allies (principally
 *   the US) without a direct attack on Japan. This reading was
 *   authoritatively instantiated by the Abe Cabinet's 2014 constitutional
 *   reinterpretation and codified in the 2015 security legislation. The
 *   reading's structural signature is mission-scope expansion: the
 *   'survival-threatening situation' trigger is elastically defined,
 *   absorbing incremental expansion of SDF overseas roles from logistics to
 *   combat-adjacent support. The victim set includes those who relied on the
 *   narrower inherent_right_reading's stability — pacifist
 *   constitutionalists, regional neighbors, and SDF personnel deployed under
 *   ambiguous legal cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.75).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.65).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '9f9f0cf3-6f87-439b-9345-4f39b13daf79').
narrative_ontology:cs_kernel_codification('9f9f0cf3-6f87-439b-9345-4f39b13daf79', fixed_text).
narrative_ontology:cs_authority_grounding('9f9f0cf3-6f87-439b-9345-4f39b13daf79', extraction).
narrative_ontology:cs_interpretation_layer_present('9f9f0cf3-6f87-439b-9345-4f39b13daf79').
narrative_ontology:cs_reading_relation('9f9f0cf3-6f87-439b-9345-4f39b13daf79', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('9f9f0cf3-6f87-439b-9345-4f39b13daf79', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('9f9f0cf3-6f87-439b-9345-4f39b13daf79', foundational, collective_self_defense_permissible_under_survival_threat).
narrative_ontology:cs_axiom_status(collective_self_defense_permissible_under_survival_threat, holdable).
narrative_ontology:cs_axiom_grounding('9f9f0cf3-6f87-439b-9345-4f39b13daf79', collective_self_defense_permissible_under_survival_threat, conventional).
narrative_ontology:cs_axiom('9f9f0cf3-6f87-439b-9345-4f39b13daf79', secondary, survival_threat_trigger_elastic).
narrative_ontology:cs_axiom_status(survival_threat_trigger_elastic, holdable).
narrative_ontology:cs_axiom_grounding('9f9f0cf3-6f87-439b-9345-4f39b13daf79', survival_threat_trigger_elastic, instrumental).
narrative_ontology:cs_reference_frame('9f9f0cf3-6f87-439b-9345-4f39b13daf79', postwar_pacifist_constitutional_order).
narrative_ontology:cs_drift_state('9f9f0cf3-6f87-439b-9345-4f39b13daf79', contemporary_security_environment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f9f0cf3-6f87-439b-9345-4f39b13daf79', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_government_executive).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_alliance_structure).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, japanese_pacifist_constitutionalists).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, alliance_credibility_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, collective_security_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls constitutional interpretation through Cabinet Legislation Bureau and legislative majorities. Reinterpreted Article 9 in 2014 to permit collective self-defense, then enacted 2015 security legislation enabling SDF overseas deployments. Collects expanded security authority and alliance leverage; bears political cost of domestic opposition.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_government_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives binding Japanese commitment to collective defense, enabling integrated operational planning and burden-sharing. The reading's elastic 'survival threat' trigger provides strategic flexibility for joint operations. Does not bear Japanese domestic political costs.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_alliance_structure, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on Article 9's textual prohibition as constitutional commitment to pacifism. The collective self-defense reading erodes this constraint through executive reinterpretation without formal amendment. Their exit is constrained: constitutional amendment requires supermajority they cannot block indefinitely; emigration is only individual exit.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_pacifist_constitutionalists, payer,
    organized, biographical, constrained, national).

% Face security dilemma from Japanese military normalization: elastic 'survival threat' trigger makes threat perception unpredictable. Historical memories of Japanese militarization amplify perceived threat. Cannot exit the regional security environment; diplomatic and military countermeasures are costly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors, payer,
    powerful, generational, constrained, regional).

% Deployed overseas in non-combat roles that edge toward combat support under collective self-defense. Legal status ambiguous: not combatants under international law, but operating in conflict zones. Career structure binds them; resignation is possible but costly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_personnel, payer,
    moderate, biographical, constrained, national).

% Analyze the interpretive trajectory from inherent-right reading to collective-self-defense reading. Document the elastic trigger's expansionary logic. No material stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional-legal pathway for Japan to participate in collective security arrangements with the US and other allies, solving the coordination problem of alliance credibility without formal constitutional amendment.
% TRANSFER_FUNCTION: Moves interpretive authority and operational risk from the constitutional text (which categorically renounces war and maintenance of armed forces) to the executive branch, which defines 'survival-threatening situation' elastically. Transfers security burden to SDF personnel and regional stability costs to neighbors.
% ABSENT_VOICES: Okinawan communities hosting US bases bear disproportionate operational risk from expanded joint operations but lack veto power over security legislation. Future generations inherit the normalized military posture without having consented to the reinterpretation. Article 9's original drafters' intent (categorical pacifism) is excluded from the operative interpretive framework.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading vanished overnight, the 2015 security legislation would lose its constitutional basis, SDF overseas deployments for collective defense would become legally untenable, US-Japan alliance operational planning would revert to individual-self-defense-only framework, and the constitutional amendment debate would reopen with renewed urgency.
% FOUNDING_PROBLEM: The Cold War and post-Cold War security environment created a mismatch: Article 9's textual pacifism constrained Japan from contributing to collective security arrangements that the US-Japan alliance required for credibility, while the Soviet/Chinese/North Korean threat environment made pure pacifism strategically untenable for Japanese elites.
% FOUNDING_PROBLEM_CORROBORATION: The government attests the founding problem is live (evolving threat environment requires flexible response). Pacifist constitutionalists and opposition parties attest the founding problem was manufactured: the inherent-right reading already permitted minimum necessary defense; collective self-defense was a choice, not a necessity. Independent scholars (e.g., Sasada, Hughes) document that the 2014 reinterpretation was driven by alliance management preferences, not strategic necessity.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the reading transfers core constitutional constraint-setting authority from the fixed text to executive interpretation, enabling open-ended mission expansion. Suppression is moderate-high (0.65) because the reading's persistence depends on active maintenance: Cabinet Legislation Bureau opinion control, legislative majorities, and judicial non-reviewability of 'political questions.' Theater ratio is moderate (0.45): genuine alliance coordination exists, but the elastic trigger's expansionary logic means a growing share of enforcement activity serves mission-creep rather than the coordination function. Accessibility collapse is moderate (0.55): the inherent_right_reading remains a live alternative, but the collective_self_defense_reading has captured the operative interpretive channel. Resistance is high (0.70): sustained public opposition, scholarly consensus against constitutionality, and regional pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (japanese_government_executive), the reading is a necessary adaptation: the constitutional text is made workable for real-world alliance demands. From the payer seats, the same structure operates as executive aggrandizement: the 'survival threat' trigger is a blank check written by the executive on the constitution's account. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The japanese_government_executive is the primary beneficiary (collects expanded authority, alliance leverage — d near beneficiary end). The us_alliance_structure is a pure beneficiary (receives commitment without domestic cost — d ≈ 0.0). Japanese_pacifist_constitutionalists, regional_neighbors, and sdf_personnel are payers: they bear the costs of eroded constraint, security dilemma, and deployment risk respectively, with constrained exit (d near target end). The constitutional_scholars_observers sit at d=0.5 (analytical seat). The elastic 'survival threat' trigger means directionality for payers worsens over time as the trigger expands.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (categorical war renunciation) is dead; the collective_self_defense_reading is a successor arrangement that claims the original mandate's legitimacy while inverting its substance. The founding problem (Cold War alliance credibility) is contested as still live — but the reading's elastic trigger expands far beyond that problem. This is mandatrophy: the arrangement persists by absorbing the original mandate's authority while serving a different function (alliance integration via executive reinterpretation). The classification as tangled_rope (not snare) reflects that genuine coordination (alliance operability) coexists with extraction (executive authority expansion, constraint erosion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threat_trigger_objectivity,
    'Does the ''survival-threatening situation'' trigger have objective legal criteria, or is it a purely political determination by the executive?',
    'Judicial review of a concrete deployment decision (currently barred as political question), or legislative codification of trigger criteria with binding effect.',
    'If purely political, the trigger is an open-ended delegation enabling unlimited mission creep (extraction dominant). If objectively constrained, the coordination function has a genuine structural boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_threat_trigger_objectivity, conceptual, 'Whether the reading''s central trigger condition is legally bounded or politically elastic.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the collective self-defense permission genuinely necessary for alliance coordination, or does the alliance coordination story cover executive authority expansion?',
    'Counterfactual analysis: could the US-Japan alliance maintain credibility and operational integration under the inherent_right_reading (individual self-defense only) with revised operational plans?',
    'If alliance coordination is achievable under the narrower reading, the collective_self_defense_reading''s marginal coordination value is near zero and its extractiveness is dominant (snare classification). If the narrower reading genuinely fails coordination, the reading is tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the reading''s coordination function is necessary or pretextual.').

omega_variable(
    kernel_framing_ambiguity,
    'Is Article 9 a single constraint with contested readings, or are the three readings structurally distinct constraints sharing only a label?',
    'Apply the ε-invariance test: if measuring ''Article 9''s constraint'' via the strict_pacifist_reading yields ε≈0.1 while the collective_self_defense_reading yields ε≈0.75, the label covers multiple constraints. Decompose into separate constraint stories linked by network.affects_constraints.',
    'If the kernel decomposes, each reading gets its own ε, stakeholders, and classification. The ''contested kernel'' framing would be a linguistic confusion, not a structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel/reading frame correctly captures structural reality or obscures constraint identity.').

omega_variable(
    inherent_right_reading_stability_erosion,
    'Does the collective_self_defense_reading''s expansion destabilize the inherent_right_reading''s position, making the narrower reading politically untenable over time?',
    'Track scholarly and political discourse: if inherent_right_reading proponents are forced to either adopt collective_self_defense_logic or retreat to strict_pacifist_reading, the middle position is being squeezed out.',
    'If the inherent_right_reading is structurally destabilized, the victim set includes its adherents — their reliance on a stable intermediate position was extracted. This strengthens the extraction asymmetry claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_right_reading_stability_erosion, empirical, 'Whether the reading''s expansion creates a ratchet that eliminates the intermediate position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1954, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(arti_tr_t2004, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(arti_tr_t2016, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2016, 0.42).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t1954, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(arti_be_t2004, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(arti_be_t2016, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1954, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(arti_su_t2004, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(arti_su_t2016, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_operational_integration).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, sdf_overseas_deployment_legal_framework).

% DUAL FORMULATION NOTE:
% This reading is one of three in the Article 9 constraint family. The strict_pacifist_reading (ε≈0.1, mountain-claim) and inherent_right_reading (ε≈0.35, rope-claim) have lower extractiveness because they maintain tighter interpretive boundaries. This reading's ε≈0.75 reflects the executive's capture of interpretive authority via the elastic 'survival threat' trigger. All three stories share the kernel_id article_9_war_renunciation and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, institutional, 0.1).
constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
