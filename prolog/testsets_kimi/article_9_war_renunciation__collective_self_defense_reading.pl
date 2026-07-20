% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: constitutional law/security policy/institutional legitimacy
 *
 * SUMMARY:
 *   The Japanese Constitution's Article 9 renounces war and the maintenance
 *   of war potential. The collective_self_defense_reading holds that this
 *   text nonetheless permits military action to defend allies when Japan's
 *   survival is threatened, even absent a direct attack on Japan. This
 *   reading was operationalized by the 2014 Cabinet Decision and 2015
 *   security legislation, expanding JSDF mission scope without formal
 *   constitutional amendment under Article 96. It coordinates Japan within
 *   collective security frameworks while extracting interpretive stability
 *   from constitutional textualists and pacifist constituencies who relied on
 *   a narrower inherent-right reading.
 *
 * KEY AGENTS:
 *   - cabinet_legislative_majority: Agenda setter (institutional/constrained) â administers the reinterpretation and controls legislative enforcement.
 *   - security_establishment: Primary beneficiary (organized/constrained) â gains missions, budget, and operational authority.
 *   - alliance_policy_managers: Secondary beneficiary (institutional/constrained) â gains alliance operational flexibility.
 *   - us_alliance_partner: External beneficiary (institutional/constrained) â gains strategic burden-sharing.
 *   - pacifist_constituencies: Primary target (organized/constrained) â bears normative loss and entanglement risk.
 *   - constitutional_textualists: Target (moderate/constrained) â bears interpretive stability loss.
 *   - narrow_reading_reliants: Target (moderate/constrained) â relied on doctrinal stability for base and deployment limits.
 *   - opposition_parties: Excluded (organized/constrained) â structurally bypassed in executive interpretive acts.
 *   - constitutional_scholars: Observer (moderate/analytical) â evaluates legitimacy from outside beneficiary set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.65).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.6).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional law/security policy/institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '349d7f12-2f2b-4946-8631-aa8cd2b1105a').
narrative_ontology:cs_kernel_codification('349d7f12-2f2b-4946-8631-aa8cd2b1105a', fixed_text).
narrative_ontology:cs_authority_grounding('349d7f12-2f2b-4946-8631-aa8cd2b1105a', lineage).
narrative_ontology:cs_interpretation_layer_present('349d7f12-2f2b-4946-8631-aa8cd2b1105a').
narrative_ontology:cs_reading_relation('349d7f12-2f2b-4946-8631-aa8cd2b1105a', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('349d7f12-2f2b-4946-8631-aa8cd2b1105a', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('349d7f12-2f2b-4946-8631-aa8cd2b1105a', foundational, collective_self_defense_as_inherent_right).
narrative_ontology:cs_axiom_status(collective_self_defense_as_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('349d7f12-2f2b-4946-8631-aa8cd2b1105a', collective_self_defense_as_inherent_right, conventional).
narrative_ontology:cs_axiom('349d7f12-2f2b-4946-8631-aa8cd2b1105a', foundational, allied_attack_as_survival_threat).
narrative_ontology:cs_axiom_status(allied_attack_as_survival_threat, holdable).
narrative_ontology:cs_axiom_grounding('349d7f12-2f2b-4946-8631-aa8cd2b1105a', allied_attack_as_survival_threat, conventional).
narrative_ontology:cs_reference_frame('349d7f12-2f2b-4946-8631-aa8cd2b1105a', postwar_pacifist_constitution).
narrative_ontology:cs_drift_state('349d7f12-2f2b-4946-8631-aa8cd2b1105a', contemporary_security_environment, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('349d7f12-2f2b-4946-8631-aa8cd2b1105a', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, security_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, alliance_policy_managers).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_alliance_partner).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_textualists).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, narrow_reading_reliants).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, inherent_right_expansion_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional reinterpretation through cabinet decisions and security legislation. Controls the interpretive apparatus and gains policy flexibility to deploy forces without pursuing Article 96 amendment. Reversal would entail severe political and diplomatic costs with alliance partners.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, cabinet_legislative_majority, agenda_setter,
    institutional, biographical, constrained, national).

% Gains expanded mission scope, budget authorization, and operational rules of engagement through the reinterpretation. Dependent on legislative authorization and cabinet direction; cannot unilaterally expand or contract the constitutional constraint.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, security_establishment, beneficiary,
    organized, generational, constrained, national).

% Japanese foreign and defense officials who manage alliance strategy. Gain operational credibility and burden-sharing capacity with the United States through expanded legal authorities. Their professional success is tied to alliance deepening.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, alliance_policy_managers, beneficiary,
    institutional, generational, constrained, national).

% Benefits from expanded Japanese operational support in collective defense scenarios, gaining strategic depth and burden-sharing in the Indo-Pacific. Does not control the Japanese constitutional interpretation but exerts diplomatic pressure to maintain the expanded reading.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_alliance_partner, beneficiary,
    institutional, generational, constrained, global).

% Citizens, civic groups, and religious organizations who structured political identity and social practice around Article 9 as a pacifist constraint. Bear normative loss, expanded tax burden, and entanglement risk in foreign conflicts without having supported the reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies, payer,
    organized, generational, constrained, national).

% Legal scholars and jurists who maintain that the plain text of Article 9 renounces the maintenance of all war potential. Bear a loss of interpretive stability and constitutional fidelity as the text is stretched to authorize activities its language prohibits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_textualists, payer,
    moderate, civilizational, constrained, national).

% Local governments, businesses near bases, and citizens who relied on the narrower inherent-right reading to limit base expansion and overseas deployment. Face expanded operational footprints and risk exposure as mission scope elasticizes.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, narrow_reading_reliants, payer,
    moderate, biographical, constrained, national).

% Present in the Diet but structurally excluded from the operative interpretive authority. The 2014 Cabinet Decision was an executive act that bypassed their consent; they contest the reading but lack institutional veto over reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, opposition_parties, excluded,
    organized, biographical, constrained, national).

% Analyze the legitimacy and coherence of the reinterpretation from outside the benefiting parties. Provide corroboration and critique of the founding problem narrative without capturing gains or bearing operational costs.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_scholars, observer,
    moderate, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for Japan to engage in collective self-defense with allies when Japan's survival is threatened, enabling alliance operational planning and regional deterrence without the political friction of formal constitutional amendment.
% TRANSFER_FUNCTION: Transfers constitutional interpretive authority from the Article 96 amendment process to the Cabinet and legislative majority; transfers operational risk and tax burden from the state abstraction to military personnel and the public; transfers interpretive stability from pacifist and textualist constituencies to an elastic security doctrine.
% ABSENT_VOICES: Strict-pacifist constitutional scholars and opposition parties are present in public discourse but excluded from operative interpretive authority. Local communities facing base expansion are not consulted on security doctrine shifts. Their exclusion is structural: the Cabinet Decision mechanism does not require their assent.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal basis for collective defense operations would collapse; US-Japan alliance operational planning would require treaty renegotiation or constitutional amendment; the security establishment would lose overseas mission authority and budget justification; and Japanese security policy would revert to a narrower inherent-defense framework.
% FOUNDING_PROBLEM: The perceived legal incapacity of Japan to assist allies under attack and the operational friction this created within the US-Japan alliance during the post-Cold War security environment.
% FOUNDING_PROBLEM_CORROBORATION: US defense and diplomatic officials attest the alliance credibility gap from outside the Japanese beneficiary set. Japanese constitutional scholars outside the ruling coalition corroborate the existence of security demands but dispute that executive reinterpretation is the legitimate solution; they attest the problem is addressable through Article 96 amendment rather than interpretive expansion.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is substantial because the reading transfers constitutional interpretive authority from the amendment process to the executive/legislative majority, and transfers operational risk to the public without commensurate democratic authorization. Suppression (0.60) is moderate-to-high because the constraint's persistence depends on legislative majority enforcement and the marginalization of strict-pacifist readings from operative legal status. Theater ratio (0.60) is high because the reading maintains the appearance of constitutional continuity while functionally authorizing activities the text's plain language renounces; the 'survival threatened' trigger performs constraint without offering a falsifiable operational standard. Accessibility collapse (0.45) is moderate because alternatives (strict reading, formal amendment) are visible but politically blocked by the governing majority. Resistance (0.55) is moderate because significant scholarly, civic, and opposition resistance persists but is structurally marginalized by the cabinet-legislative axis.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the reading is a necessary evolutionary interpretation responding to contemporary security threats and alliance obligations; from the payer seats, it is an unauthorized constitutional revision that destabilizes the postwar order and imposes military risk without popular mandate. The engine computes this divergence from structural positions, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Cabinet and legislative majority sit near the beneficiary end (low d): they control the interpretive apparatus and gain policy flexibility. Security establishment and alliance managers sit near the beneficiary end (low d): they receive expanded missions and alliance credibility. The US alliance partner sits near the beneficiary end (low d): it receives strategic support. Pacifist constituencies, textualists, and narrow-reading reliants sit near the target end (high d): they bear the costs of expanded military role and eroded constitutional constraints without corresponding agency. Opposition parties sit near the target end (high d) by virtue of exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The original founding problem of Article 9 (unconditional demilitarization under occupation) is dead, but this reading addresses a live successor problem (alliance credibility, regional deterrence). It is not mandatrophied because the coordination function is currently invoked and actively maintained by beneficiaries. However, the reliance on executive reinterpretation rather than formal amendment creates a drift vector that could normalize extra-constitutional governance if the interpretive elasticity is not bounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_vs_inherent_boundary,
    'Is the collective self-defense reading structurally distinct from the inherent right reading, or merely its elastic extension under alliance pressure?',
    'Trace whether the ''survival threatened'' trigger has independent constraining force or is epiphenomenal to alliance demands and executive discretion.',
    'If epiphenomenal, the constraint is a cover for alliance-driven mission expansion and extraction is higher than the coordination story suggests; classification may shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_inherent_boundary, conceptual, 'Boundary between collective self-defense reading and inherent right reading').

omega_variable(
    reinterpretation_vs_amendment,
    'Does the 2014 Cabinet Decision and subsequent legislation constitute legitimate constitutional interpretation or a de facto amendment bypassing Article 96?',
    'Judicial review by the Supreme Court of Japan on the formal amendment question, or comparative constitutional analysis of interpretive limits.',
    'If the latter, the constraint''s legitimacy rests on raw executive power rather than legal continuity, raising extraction and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinterpretation_vs_amendment, conceptual, 'Whether the reading is interpretation or extra-constitutional amendment').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legislative majority blocking reversal) or internalized (public acquiescence to expanded security role)?',
    'Track public opinion and electoral behavior following a hypothetical opposition victory committed to reversing the reading.',
    'If internalized, effective suppression exceeds the structural measure because the public carries the constraint even after a change in legislative majority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(arti_tr_t16, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(arti_tr_t24, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(arti_tr_t34, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 34, 0.6).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(arti_be_t16, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(arti_be_t24, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(arti_be_t34, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 34, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(arti_su_t16, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(arti_su_t24, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(arti_su_t34, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 34, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, inherent_right_reading).

% DUAL FORMULATION NOTE:
% The article_9_war_renunciation kernel decomposes into three structurally distinct constraints: strict_pacifist_reading (high extraction from security establishment, near-total suppression of military capacity), inherent_right_reading (moderate coordination of minimum individual defense), and collective_self_defense_reading (expanded coordination with asymmetric extraction through elastic interpretation). Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
