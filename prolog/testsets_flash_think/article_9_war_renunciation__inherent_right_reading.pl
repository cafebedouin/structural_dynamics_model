% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation (Inherent Self-Defense Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'inherent right to self-defense'
 *   reading of Japan's Article 9, which renounces 'war' but permits
 *   maintaining a 'minimum necessary' defensive capacity. This interpretation
 *   legitimizes the Self-Defense Forces (SDF) while imposing a
 *   proportionality constraint on their size and mission. It stands in
 *   contrast to both a strict pacifist reading (no forces at all) and a
 *   collective self-defense reading (permitting action to defend allies). The
 *   classification as a Tangled Rope reflects the genuine coordination
 *   function of national security balanced against the extraction of
 *   resources and the ongoing contestation over the interpretation's scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.65).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.55).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation (Inherent Self-Defense Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'c52df2a2-07f2-4310-bf00-ed5fca2b4168').
narrative_ontology:cs_kernel_codification('c52df2a2-07f2-4310-bf00-ed5fca2b4168', fixed_text).
narrative_ontology:cs_authority_grounding('c52df2a2-07f2-4310-bf00-ed5fca2b4168', lineage).
narrative_ontology:cs_interpretation_layer_present('c52df2a2-07f2-4310-bf00-ed5fca2b4168').
narrative_ontology:cs_reading_relation('c52df2a2-07f2-4310-bf00-ed5fca2b4168', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('c52df2a2-07f2-4310-bf00-ed5fca2b4168', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('c52df2a2-07f2-4310-bf00-ed5fca2b4168', foundational, sovereign_right_to_self_defense_is_inherent).
narrative_ontology:cs_axiom_status(sovereign_right_to_self_defense_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('c52df2a2-07f2-4310-bf00-ed5fca2b4168', sovereign_right_to_self_defense_is_inherent, deontological).
narrative_ontology:cs_axiom('c52df2a2-07f2-4310-bf00-ed5fca2b4168', foundational, article_9_prohibits_aggressive_war_only).
narrative_ontology:cs_axiom_status(article_9_prohibits_aggressive_war_only, holdable).
narrative_ontology:cs_axiom_grounding('c52df2a2-07f2-4310-bf00-ed5fca2b4168', article_9_prohibits_aggressive_war_only, conventional).
narrative_ontology:cs_reference_frame('c52df2a2-07f2-4310-bf00-ed5fca2b4168', post_wwii_sovereignty_framework).
narrative_ontology:cs_drift_state('c52df2a2-07f2-4310-bf00-ed5fca2b4168', contemporary_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c52df2a2-07f2-4310-bf00-ed5fca2b4168', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, japanese_citizens).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_right_to_self_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit a 'minimum necessary' self-defense capacity, maintaining the Self-Defense Forces (SDF). Benefits from national security and international standing, but is constrained by the constitutional text and domestic political opposition to re-militarization.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from national security provided by the SDF, avoiding direct military conscription and the costs of offensive war. Indirectly pay for the SDF through taxes. Their political will influences the interpretation's boundaries.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_citizens, payer).

% Bear the financial cost of maintaining the Self-Defense Forces through taxation. While benefiting from security, they experience the direct extraction of resources for defense spending.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_taxpayers, payer,
    moderate, immediate, constrained, national).

% Serve in a military force whose constitutional legitimacy and operational scope are perpetually debated. They bear the personal risks of military service while operating under strict legal and political constraints on their mission, often facing identity ambiguity regarding their role as a 'military'.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, self_defense_forces_personnel, payer,
    moderate, biographical, identity_locked, national).

% Analyze and debate the legal interpretations of Article 9, influencing public discourse and judicial opinions. They provide critical commentary on the evolving scope of self-defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Advocate for a strict interpretation of Article 9 that prohibits any military forces. Their preferred reading is actively suppressed by the dominant 'inherent right' interpretation, though their activism continues to shape the political debate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national security by providing a framework for maintaining defensive capabilities while renouncing aggressive war, balancing security needs with constitutional principles.
% TRANSFER_FUNCTION: Transfers financial resources from Japanese taxpayers to the Self-Defense Forces for national defense, and transfers the burden of military service to SDF personnel, in exchange for national security.
% ABSENT_VOICES: Strict pacifist advocates, who interpret Article 9 as an absolute prohibition on any military force, are structurally excluded from the dominant discourse that legitimizes the SDF. Their arguments are marginalized in official interpretations.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, Japan would either face immediate pressure to fully re-militarize (adopting a more conventional military posture) or would be left without a constitutionally sanctioned defense force, fundamentally altering its security policy and international relations.
% FOUNDING_PROBLEM: To establish a post-war national identity that renounced war as a sovereign right, while still allowing for the defense of national territory and sovereignty in a volatile geopolitical environment.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and a majority of citizens attest that the need for self-defense remains live due to regional security threats. International allies also corroborate the need for Japan to maintain defensive capabilities for regional stability. Pacifist groups contest the 'live' status of the problem, arguing for diplomatic solutions over military ones.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) due to the significant financial cost of maintaining the SDF and the personal costs borne by personnel, coupled with the constitutional ambiguity that limits their full integration as a conventional military. Suppression (0.55) is moderate, as the interpretation actively suppresses calls for full re-militarization while also marginalizing strict pacifist views. Theater ratio (0.25) is low-moderate; the SDF is a functional defense force, but the 'defensive only' rhetoric and the constitutional debate introduce some performative elements to manage domestic and international expectations. Resistance (0.6) is high due to ongoing political and legal challenges from both pacifist groups and those advocating for a more robust military.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Japanese state, this reading is a necessary and legitimate balance for national security. From the perspective of SDF personnel, it can be a source of identity ambiguity and operational constraint. Pacifist advocates view it as a betrayal of the constitutional ideal. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese state and its citizens are primary beneficiaries of the security provided by the SDF, but also bear costs. Taxpayers are direct payers. SDF personnel are significant payers, bearing personal risks and operating under identity-locked constraints. Pacifist advocates are excluded, as their interpretation is not the one being enacted. Constitutional scholars act as observers, analyzing the evolving legal landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring extraction and suppression) or a Snare (ignoring the genuine coordination function of national defense). The 'minimum necessary' clause provides a genuine coordination function for security, but the ongoing debate and resource allocation reveal the extractive and suppressive elements inherent in its enforcement and interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_ambiguity,
    'What constitutes ''minimum necessary defensive capacity'' in a rapidly evolving geopolitical landscape?',
    'Ongoing legislative debate, judicial review, and shifts in international security norms. Empirical analysis of regional threats and comparative defense spending.',
    'A broader interpretation of ''minimum necessary'' would increase extractiveness and suppression, potentially shifting the constraint closer to a Snare. A narrower interpretation would reduce these, moving it closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_ambiguity, conceptual, 'Ambiguity in the scope of permissible defensive capacity.').

omega_variable(
    identity_of_sdf,
    'Is the Self-Defense Forces'' identity as a ''non-military'' defense organization genuinely held, or is it a theatrical performance to circumvent constitutional limitations?',
    'Analysis of SDF training, equipment, and operational deployments compared to conventional militaries. Public opinion surveys on SDF identity. Legal challenges to SDF status.',
    'If largely theatrical, the theater_ratio would be higher, and the constraint''s effective suppression of alternative military postures would be more pronounced. If genuinely non-military, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_of_sdf, empirical, 'Theatricality vs. genuine non-military identity of the SDF.').

omega_variable(
    constitutional_revision_pressure,
    'To what extent is the current interpretation of Article 9 a stable equilibrium, versus a temporary compromise under pressure for formal revision?',
    'Tracking public support for constitutional revision, legislative initiatives, and the outcomes of national elections. Analysis of geopolitical shifts that increase pressure for revision.',
    'If revision pressure is high and acknowledged, the constraint''s stability is lower, and its persistence relies more on active political management than settled interpretation. If stable, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_revision_pressure, empirical, 'Stability of Article 9 interpretation against revision pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(arti_tr_t2010, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(arti_be_t2010, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(arti_su_t2010, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_us_security_alliance).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, regional_security_treaties).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
