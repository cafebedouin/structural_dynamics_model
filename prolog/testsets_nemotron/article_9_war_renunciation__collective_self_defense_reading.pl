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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Collective Self-Defense Reading of Article 9 War Renunciation
 *   domain: constitutional/law/security/policy/institutional/legitimacy
 *
 * SUMMARY:
 *   The collective self-defense reading of Article 9, crystallized in the
 *   2014 Cabinet Decision and 2015 security legislation, reinterprets Japan's
 *   constitutional war renunciation to permit military action defending
 *   allies (primarily the United States) without a direct attack on Japan.
 *   The 'survival-threatening' trigger is defined by the executive branch,
 *   creating an elastic interpretive constraint that has absorbed incremental
 *   mission expansion: minesweeping in the Persian Gulf, refueling in the
 *   Indian Ocean, UN peacekeeping with use-of-force mandates, and now
 *   collective defense operations integrated with US forces. The reading
 *   claims coordination function (alliance interoperability) but operates as
 *   extraction from the constitutional text's categorical language and from
 *   the pacifist constituency's constitutional consent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.68).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Collective Self-Defense Reading of Article 9 War Renunciation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional/law/security/policy/institutional/legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '59f4091e-a1ea-4859-9fc5-0a59b6f307cf').
narrative_ontology:cs_kernel_codification('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', fixed_text).
narrative_ontology:cs_authority_grounding('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', extraction).
narrative_ontology:cs_interpretation_layer_present('59f4091e-a1ea-4859-9fc5-0a59b6f307cf').
narrative_ontology:cs_reading_relation('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', foundational, collective_self_defense_as_inherent_right).
narrative_ontology:cs_axiom_status(collective_self_defense_as_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', collective_self_defense_as_inherent_right, instrumental).
narrative_ontology:cs_axiom('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', foundational, survival_threatening_trigger_executive_discretion).
narrative_ontology:cs_axiom_status(survival_threatening_trigger_executive_discretion, holdable).
narrative_ontology:cs_axiom_grounding('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', survival_threatening_trigger_executive_discretion, conventional).
narrative_ontology:cs_reference_frame('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', postwar_constitutional_pacifism).
narrative_ontology:cs_drift_state('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', post_2014_cabinet_decision, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59f4091e-a1ea-4859-9fc5-0a59b6f307cf', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japan_self_defense_forces).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_infrastructure).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, security_policy_establishment).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, article_9_textual_integrity).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors_security_perception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional actor that administers and executes the collective self-defense interpretation. Gained legal basis for overseas deployments, joint operations with US forces, and expanded mission scope under 'survival-threatening' trigger. Their organizational mandate and budget expanded; career paths now include collective defense roles. Exit from this interpretation would mean retraction of acquired capabilities and legal authority.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japan_self_defense_forces, agenda_setter,
    institutional, generational, constrained, national).

% Bureaucratic and political actors (Cabinet Legislation Bureau alumni, LDP security faction, Ministry of Defense) who authored and maintain the collective self-defense interpretation. Benefit from expanded policy latitude, institutional relevance, and alliance management role. Their professional identity is fused with the post-2014 security legislation framework. Exit would require repudiating their own legal work and political capital.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, security_policy_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, security_policy_establishment, beneficiary).

% The network of treaties, joint commands, basing arrangements, and interoperability programs that constitute the US-Japan alliance. Receives a more capable and legally unconstrained Japanese partner for regional contingency planning, burden-sharing, and integrated deterrence. Does not bear the domestic constitutional cost; can shift alliance posture if Japan's interpretation changes. Exit is low-cost for the US side — the alliance persists under narrower readings.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_infrastructure, beneficiary,
    institutional, generational, arbitrage, global).

% Citizens, scholars, opposition parties, and civil society groups who understand Article 9 as a categorical war renunciation. Bear the cost of seeing their constitutional commitment reinterpreted away; their identity is constituted through the pacifist reading. Exit from this identity is existentially costly — it would mean abandoning the moral framework through which they understand Japan's postwar legitimacy. They are structurally trapped in the dispute; the constraint extracts their constitutional consent without their participation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_pacifist_constituency, payer,
    organized, biographical, identity_locked, national).

% The textual and normative coherence of Article 9 itself — 'never be maintained' and 'forever renounce war' — as a stable legal object. The collective self-defense reading stretches 'inherent right' and 'survival-threatening' to cover overseas collective operations, making the text mean something its framers explicitly rejected. This is not an agent but a structural casualty: the constraint's operation degrades the very object it claims to interpret. No exit exists for a text; it can only be amended or abandoned.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, article_9_textual_integrity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__collective_self_defense_reading, article_9_textual_integrity).

% Northeast Asian states (ROK, PRC, DPRK, Russia) whose threat perception shifts when Japan's constitutional constraint becomes elastic. They bear the cost of strategic uncertainty: a Japan that can deploy forces collectively under a self-defined 'survival' threshold is less predictable and more capable of power projection. Their exit options are constrained — they cannot leave the region, and diplomatic protest has limited effect on Japan's domestic constitutional interpretation. They are involuntary parties to the constraint's expansion.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighbors_security_perception, payer,
    moderate, generational, constrained, regional).

% Academic observers who track the interpretive trajectory: the 2014 Cabinet Decision, the 2015 security legislation, subsequent operational increments. They see the full structure — how the reading absorbs incremental expansion while maintaining the formal shell of Article 9. They neither collect nor pay; their situation is analytical clarity about the constraint's actual operation versus its claimed coordination function.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Japan's security relationship with the United States under conditions of rising regional threat: provides a constitutional-legal basis for joint operations, burden-sharing, and integrated deterrence that the strict pacifist reading would forbid and the inherent-right reading would leave legally ambiguous.
% TRANSFER_FUNCTION: Moves constitutional authority and operational latitude from the textual constraint (Article 9's categorical language) to the executive branch and SDF, enabling overseas deployments and collective defense actions. The transfer is legitimated by the 'survival-threatening' trigger, which the executive branch defines. Costs fall on the pacifist constituency (eroded constitutional commitment) and regional neighbors (increased strategic uncertainty).
% ABSENT_VOICES: The Article 9 framers (1946-47) — their explicit intent was to foreclose exactly the collective self-defense logic now read into the text. Okinawan residents who host the alliance infrastructure and bear disproportionate basing costs without meaningful voice in the constitutional interpretation. Future generations who inherit an elastic constraint whose trigger ('survival-threatening') has no judicial review and no legislative veto.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading vanished overnight, the 2015 security legislation would lose its constitutional basis; SDF overseas deployments for collective defense would become legally unauthorized; US-Japan alliance operational plans would revert to stricter self-defense-only parameters; the security policy establishment would lose its primary post-2014 legal achievement. The constitutional order would revert to the inherent-right reading's narrower scope or the strict pacifist reading's categorical prohibition — either way, a major rearrangement.
% FOUNDING_PROBLEM: The Cold War-era 'inherent right' reading became insufficient when post-Cold War regional threats (North Korean missiles, Chinese maritime expansion) required Japan to operate beyond strict individual self-defense — specifically, to defend US forces defending Japan, and to participate in collective security operations that the US-Japan alliance increasingly demanded.
% FOUNDING_PROBLEM_CORROBORATION: The security policy establishment and US-Japan alliance infrastructure attest the problem is live and growing. Constitutional pacifist constituency and major opposition parties attest the problem is manufactured — that the 'inherent right' reading was sufficient and the collective self-defense reading was a solution in search of a problem, driven by alliance management rather than existential threat. The Cabinet Legislation Bureau's own pre-2014 opinion (that collective self-defense was unconstitutional) is corroboration from within the state apparatus that the problem was not previously recognized as requiring this solution.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the reading transfers constitutional authority from a stable textual constraint to executive discretion, enabling capabilities (overseas collective operations) the text explicitly renounced. Suppression (0.72) is high because the reading's persistence depends on active maintenance: Cabinet Legislation Bureau reinterpretation, legislative majorities, SDF operational doctrine, and alliance pressure — all suppressing the textual and pacifist alternatives. Theater ratio (0.45) is moderate-high: the coordination function (alliance deterrence) is real but a growing share of enforcement activity defends the interpretation's elasticity rather than operational necessity. The measurement series captures the 2014-2015 inflection point when the reading became positive law.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (JSDF, security establishment), the constraint is genuine coordination — it solves the alliance interoperability problem the inherent-right reading left ambiguous. From the identity-locked payer seat (pacifist constituency), the same structure is extraction — their constitutional consent is harvested without participation. From the trapped non-agent seat (textual integrity), it is degradation — the constraint consumes the object it interprets. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   JSDF and security policy establishment are structural beneficiaries (institutional power, constrained exit — they built and maintain the reading). US-Japan alliance infrastructure is a beneficiary with arbitrage exit (gains capability, low cost to adapt). Constitutional pacifist constituency is identity-locked payer (their constitutional identity is constituted through the narrow reading; exit is existential). Article 9 textual integrity is a trapped non-agent victim (the text cannot exit its own deformation). Regional neighbors are constrained payers (involuntary strategic exposure). The analytical observer sees the full extraction-coordination hybrid.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by exposing the coordination-extraction hybrid: the alliance coordination function is real (rope element) but the elastic trigger and executive-defined 'survival' make it extractive (snare element). The founding problem (Cold War reading insufficiency) is attested as live by beneficiaries but contested by payers — the mandate has not atrophied but has expanded beyond its claimed justification. The reading's persistence depends on active enforcement (Cabinet Decision, legislation, operational doctrine), not inertia — distinguishing it from piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threatening_trigger_boundary,
    'Where is the actual boundary of ''survival-threatening'' — is it a judiciable standard or an executive political determination?',
    'Supreme Court review of a collective self-defense deployment challenged as exceeding the trigger; or Diet legislation codifying the trigger with judicial review.',
    'If judiciable, the reading''s elasticity is constrained and extraction diminishes toward rope. If purely executive, the reading remains an open-ended authorization — extraction and suppression sustain at current levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_threatening_trigger_boundary, conceptual, 'Whether the trigger''s elasticity is a structural feature or a constitutional defect.').

omega_variable(
    coordination_extraction_separability,
    'Can the alliance coordination function (interoperability, deterrence) be maintained under the inherent-right reading''s narrower scope, or does it REQUIRE the collective self-defense reading''s elasticity?',
    'Counterfactual analysis: would US-Japan alliance deterrence degrade if Japan reverted to individual self-defense only? US defense planning documents, joint exercise outcomes, allied burden-sharing assessments.',
    'If coordination is separable, the collective self-defense reading''s extraction is gratuitous — the same coordination could be achieved with lower extraction. If inseparable, part of the measured extraction is the price of the coordination function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the reading''s coordination and extraction components are structurally separable.').

omega_variable(
    pacifist_constituency_irreversibility,
    'Is the pacifist constituency''s identity-locked exit structurally reversible, or has the reading permanently altered the constitutional subject?',
    'Longitudinal study of constitutional consciousness: does the pacifist constituency shrink, adapt, or persist across generations? Comparative cases (German Bundeswehr constitutionalization, Italian Article 11 reinterpretation).',
    'If irreversible, the extraction from the pacifist constituency is a one-time constitutional transformation, not ongoing extraction — the constraint''s type may shift toward mountain (new constitutional settlement). If reversible, the extraction continues as long as the constituency persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pacifist_constituency_irreversibility, preference, 'Whether the victim set''s identity-lock is a permanent constitutional mutation or a contestable political condition.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the collective self-defense reading logically foreclose the strict pacifist reading within a single constitutional framework, or do they coexist as competing interpretations?',
    'Constitutional theory analysis: can a single legal system simultaneously hold ''war is forever renounced'' and ''collective self-defense is permitted under survival threat'' without contradiction? The 2014 Cabinet Decision''s reasoning versus the 1972 Cabinet Legislation Bureau opinion.',
    'If forecloses, the kernel has a genuine structural fork — the readings cannot coexist in one framework, and the constraint family models a constitutional schism. If coexists_with, the kernel tolerates pluralism and the constraint is one live position among others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the strict pacifist sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(arti_tr_t2001, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(arti_tr_t2025, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(arti_be_t2001, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(arti_be_t2025, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1947, 0.15).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(arti_su_t2001, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(arti_su_t2025, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_operational_integration).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, japan_security_legislation_2015).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, sd_force_operational_doctrine).

% DUAL FORMULATION NOTE:
% Kernel article_9_war_renunciation decomposes into three constraint stories: strict_pacifist_reading (categorical prohibition, mountain-claimed, near-zero extraction), inherent_right_reading (individual self-defense only, rope/tangled_rope boundary, low-moderate extraction), collective_self_defense_reading (this story — elastic collective defense, tangled_rope, high extraction). The epsilon values diverge because each reading instantiates a different constraint with different beneficiary/victim structures and enforcement requirements. The collective self-defense reading extracts from the textual integrity and pacifist constituency that the other readings protect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, organized, 0.85).
constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, moderate, 0.7).
constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
