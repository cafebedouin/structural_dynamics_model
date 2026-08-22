% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Reading
 *   domain: constitutional/law/security_policy
 *
 * SUMMARY:
 *   This constraint is the collective_self_defense_reading of the contested
 *   kernel article_9_war_renunciation. It holds that Article 9 of the
 *   Japanese Constitution permits military action to defend allies without a
 *   direct attack on Japan, provided Japan's survival is threatened.
 *   Crystallized in the 2014 Cabinet decision and 2015 security legislation,
 *   the reading expands mission scope beyond the narrower
 *   inherent_right_reading while avoiding the strict_pacifist_reading's
 *   categorical prohibition. The interpretive constraint is elastic: the
 *   'survival-threatening' trigger absorbs incremental expansion
 *   (counter-strike capabilities, Middle East deployments) without textual
 *   revision. The authored metrics and claimed type are independent: the
 *   claim is tangled_rope because the arrangement simultaneously coordinates
 *   alliance security and extracts interpretive authority from constitutional
 *   traditionalists.
 *
 * KEY AGENTS:
 *   - Japanese executive (agenda_setter, institutional/arbitrage): Administers the reinterpretation, collects expanded policy autonomy.
 *   - SDF institution (beneficiary, organized/constrained): Gains missions and budget under the elastic interpretation.
 *   - Article 9 traditionalists (payer, organized/constrained): Bear the loss of interpretive stability and increased entanglement risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.65).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.55).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional/law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'f1264307-d8ea-4a08-b341-292b80e63d6e').
narrative_ontology:cs_kernel_codification('f1264307-d8ea-4a08-b341-292b80e63d6e', fixed_text).
narrative_ontology:cs_authority_grounding('f1264307-d8ea-4a08-b341-292b80e63d6e', lineage).
narrative_ontology:cs_interpretation_layer_present('f1264307-d8ea-4a08-b341-292b80e63d6e').
narrative_ontology:cs_reading_relation('f1264307-d8ea-4a08-b341-292b80e63d6e', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('f1264307-d8ea-4a08-b341-292b80e63d6e', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('f1264307-d8ea-4a08-b341-292b80e63d6e', foundational, collective_self_defense_inherent_to_article_9).
narrative_ontology:cs_axiom_status(collective_self_defense_inherent_to_article_9, holdable).
narrative_ontology:cs_axiom_grounding('f1264307-d8ea-4a08-b341-292b80e63d6e', collective_self_defense_inherent_to_article_9, conventional).
narrative_ontology:cs_axiom('f1264307-d8ea-4a08-b341-292b80e63d6e', foundational, alliance_defense_as_self_preservation).
narrative_ontology:cs_axiom_status(alliance_defense_as_self_preservation, holdable).
narrative_ontology:cs_axiom_grounding('f1264307-d8ea-4a08-b341-292b80e63d6e', alliance_defense_as_self_preservation, instrumental).
narrative_ontology:cs_reference_frame('f1264307-d8ea-4a08-b341-292b80e63d6e', collective_self_defense_interpretive_framework).
narrative_ontology:cs_drift_state('f1264307-d8ea-4a08-b341-292b80e63d6e', contemporary_security_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1264307-d8ea-4a08-b341-292b80e63d6e', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_executive).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, sdf_institution).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, article_9_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit collective self-defense through Cabinet decision and security legislation, avoiding constitutional amendment. Collects expanded security policy autonomy and alliance bargaining leverage. Could reverse the interpretation but bears prohibitive political and diplomatic cost.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains legal authorization for overseas missions, expanded operational scope, and budget under the reinterpretation. Dependent on the executive and Diet for mandate and funding; cannot unilaterally expand or contract the interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_institution, beneficiary,
    organized, biographical, constrained, national).

% Includes opposition parties, pacifist civic groups, and constitutional scholars who relied on the narrower inherent-right or strict-pacifist readings. Bear the loss of interpretive stability and face heightened military entanglement risk. Structurally excluded from interpretive authority; their exit is limited to political protest or electoral reversal.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, article_9_traditionalists, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, japanese_executive).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Japan's security policy with US alliance commitments, enabling integrated deterrence and burden-sharing without the political friction of formal constitutional amendment.
% TRANSFER_FUNCTION: Moves the authority to deploy military force overseas from the constitutional text and its traditional interpreters to the executive branch, under an elastic 'survival-threatening' trigger; transfers risk of military entanglement from the alliance framework to the Japanese public and SDF personnel.
% ABSENT_VOICES: Strict pacifist constitutional scholars and opposition parties who view any collective military action as unconstitutional; they are present in academia and the Diet minority but excluded from interpretive authority by the executive's monopoly on constitutional interpretation in practice.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading vanished overnight, Japan-US alliance operational planning would lose legal basis for integrated defense; SDF overseas missions would halt; the executive would revert to narrower inherent-right or strict-pacifist constraints; constitutional politics would destabilize as amendment debates resurface.
% FOUNDING_PROBLEM: Post-WWII security vacuum and US alliance pressure required Japan to contribute to collective security while domestic pacifist sentiment prohibited constitutional revision.
% FOUNDING_PROBLEM_CORROBORATION: US diplomatic historians and alliance managers attest to the original security vacuum; Japanese constitutional scholars outside the executive attest the problem is now superseded by changed strategic context and that the arrangement persists as executive aggrandizement.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is moderate-high because the reinterpretation transfers constitutional war-powers authority to the executive without formal amendment, generating policy flexibility that is decoupled from the text. Suppression (0.55) is moderate: opposition parties and scholars can voice dissent but are shut out of interpretive authority by legislative majority and executive control. Theater_ratio (0.62) is high because the government insists Article 9 is 'unchanged' while functionally permitting what the text appears to prohibit. Accessibility_collapse (0.48) reflects that strict-pacifist alternatives remain culturally vivid but are legally marginal. Resistance (0.52) captures sustained scholarly critique, opposition filibusters, and periodic mass protests. The measurement series share a single time grid (2014â2024) to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the constraint is necessary security coordination that preserves constitutional form while adapting to threats. From the traditionalist seat, the same text is being stretched to legitimize executive extraction of war-powers authority that the constitution was designed to withhold. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Japanese executive is both agenda_setter and beneficiary: its directionality is near the full-beneficiary end (low d), because it subsidizes this agent with expanded authority. The SDF is a beneficiary with constrained exit, also low d. Article 9 traditionalists are payers with constrained exit, placing them near the full-target end (high d). No override is needed because the structural derivation chain already captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpostwar security vacuum and alliance pressureâis contested. The coordination function (alliance integration) remains live, which prevents pure snare classification. However, the reading's elasticity allows mission expansion beyond the original coordination need, indicating that extraction has accumulated atop coordination. This is why the metrics (theater_ratio rising, extractiveness accumulating) support tangled_rope rather than scaffold (no sunset) or piton (still functional, not purely theatrical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elastic_trigger_boundary,
    'Does the ''survival-threatening situation'' standard retain any legally enforceable limit, or has it become a rubber stamp for executive discretion?',
    'Supreme Court ruling or legislative oversight inquiry testing the trigger''s boundary against concrete deployment decisions.',
    'If the trigger is effectively standardless, the constraint tilts toward snare (executive extraction without coordination); if bounded, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elastic_trigger_boundary, conceptual, 'Ambiguity of the survival-threatening trigger''s legal limit').

omega_variable(
    amendment_avoidance_motive,
    'Is the interpretive expansion driven by genuine security necessity, or primarily by the political benefit of avoiding Article 96 amendment procedures?',
    'Historical analysis of cabinet deliberations and comparative constitutional practice on amendment difficulty.',
    'If motive is amendment avoidance, the reading''s legitimacy is conventional extraction rather than evolutionary interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_avoidance_motive, empirical, 'Whether expansion is security-driven or procedurally opportunistic').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist opposition structural (executive monopoly on interpretation, legislative majority) or internalized (public acquiescence to security framing)?',
    'Polling and protest data tracking public opposition intensity relative to institutional access.',
    'If internalized, effective suppression exceeds the structural measure; resistance may be latent rather than expressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of opposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arti_tr_t2, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2, 0.42).
narrative_ontology:measurement(arti_tr_t4, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 4, 0.5).
narrative_ontology:measurement(arti_tr_t6, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 6, 0.56).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t2, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(arti_be_t4, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(arti_be_t6, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(arti_su_t2, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(arti_su_t4, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(arti_su_t6, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
