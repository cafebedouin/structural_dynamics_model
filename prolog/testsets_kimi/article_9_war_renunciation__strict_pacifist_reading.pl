% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading: Absolute War Renunciation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution contains the clause that 'land,
 *   sea, and air forces, as well as other war potential, will never be
 *   maintained.' The strict pacifist reading treats this as a categorical
 *   prohibition on any organized military force, including defensive
 *   capacity. This reading is one of three structurally distinct
 *   interpretations of the same constitutional kernel; it instantiates a
 *   constraint that extracts heavily from Japanese state security autonomy
 *   while coordinating regional trust and domestic pacifist identity. The
 *   Self-Defense Forces exist in a constitutional grey zone under this
 *   reading, which requires active interpretive enforcement to maintain
 *   coherence between absolute text and practiced military capacity. This
 *   story models the strict pacifist reading as an Îµ-invariant constraint
 *   independent of its sibling readings.
 *
 * KEY AGENTS:
 *   - pacifist_interpretive_authority: Primary agenda-setter (institutional/constrained/national) â administers the strict reading through constitutional scholarship, judicial interpretation, and political advocacy
 *   - pacifist_civil_society: Primary domestic beneficiary (organized/constrained/national) â maintains pacifist national identity and democratically mobilizes against remilitarization
 *   - state_security_apparatus: Primary target (institutional/constrained/national) â operates the Self-Defense Forces in constitutional limbo under capability and doctrine constraints
 *   - security_policy_reformers: Secondary target (organized/constrained/national) â advocates for constitutional amendment and normalized defense policy, blocked by the reading's interpretive hegemony
 *   - regional_neighbors: External beneficiary (organized/constrained/regional) â collects security-trust dividends from Japan's constrained military posture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.78).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.75).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading: Absolute War Renunciation").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'bd2792d5-7d2b-4617-82ed-635c21511d12').
narrative_ontology:cs_kernel_codification('bd2792d5-7d2b-4617-82ed-635c21511d12', fixed_text).
narrative_ontology:cs_authority_grounding('bd2792d5-7d2b-4617-82ed-635c21511d12', lineage).
narrative_ontology:cs_interpretation_layer_present('bd2792d5-7d2b-4617-82ed-635c21511d12').
narrative_ontology:cs_reading_relation('bd2792d5-7d2b-4617-82ed-635c21511d12', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('bd2792d5-7d2b-4617-82ed-635c21511d12', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('bd2792d5-7d2b-4617-82ed-635c21511d12', foundational, organized_military_forces_categorically_impermissible).
narrative_ontology:cs_axiom_status(organized_military_forces_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('bd2792d5-7d2b-4617-82ed-635c21511d12', organized_military_forces_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('bd2792d5-7d2b-4617-82ed-635c21511d12', secondary, self_defense_excludes_organized_force).
narrative_ontology:cs_axiom_status(self_defense_excludes_organized_force, holdable).
narrative_ontology:cs_axiom_grounding('bd2792d5-7d2b-4617-82ed-635c21511d12', self_defense_excludes_organized_force, deontological).
narrative_ontology:cs_reference_frame('bd2792d5-7d2b-4617-82ed-635c21511d12', constitutional_pacifist_origin).
narrative_ontology:cs_drift_state('bd2792d5-7d2b-4617-82ed-635c21511d12', post_2015_security_legislation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bd2792d5-7d2b-4617-82ed-635c21511d12', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, regional_neighbors).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, security_policy_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the strict pacifist reading through constitutional scholarship, judicial interpretation, and political advocacy. Maintains that 'never be maintained' categorically prohibits all organized military force regardless of defensive intent. Controls the interpretive framework that legitimizes or delegitimizes security policy, and gains authority and political relevance from the reading's persistence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, pacifist_interpretive_authority, beneficiary).

% Benefits from the constraint's operation through maintenance of Japan's pacifist identity, prevention of remilitarization, and preservation of postwar constitutional values. Mobilizes politically to defend the strict reading against revisionist challenges, with democratic participation as their primary channel of influence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society, beneficiary,
    organized, generational, constrained, national).

% Benefit from Japan's constitutionally constrained military posture through reduced regional arms-race incentives and lower threat perception. They do not administer the constraint but collect security-trust dividends from its persistence, while having no direct voice in Japanese constitutional interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_neighbors, beneficiary,
    organized, generational, constrained, regional).

% Bears the costs of operating the Self-Defense Forces in a constitutional grey zone under a reading that categorically denies their legitimacy. Subject to legal and budgetary constraints that prevent full-spectrum defense development, power projection, and alliance burden-sharing, forcing doctrinal contortions to maintain constitutional veneer.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% Bear the costs of suppressed policy space. Advocate for constitutional amendment or reinterpretation to permit normalized defense capacity, but are blocked by the strict reading's interpretive hegemony. Their agenda requires overcoming the categorical prohibition through supermajorities that the reading's political defenders prevent.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, security_policy_reformers, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional prohibition on organized military force that coordinates regional trust in Japan's non-militarized posture and domestic political consensus around pacifist national identity, eliminating security-dilemma pressures that would arise from unconstrained Japanese rearmament.
% TRANSFER_FUNCTION: Transfers security policy autonomy and full-spectrum defense capacity from the state security apparatus and security policy reformers to the pacifist interpretive framework and civil society, constraining the state's monopoly on legitimate organized violence to non-military means.
% ABSENT_VOICES: Security policy reformers who would advocate for full constitutional normalization are partially excluded from the interpretive conversation when the strict reading is treated as settled; alliance military planners who bear asymmetric defense burdens under the constraint have no seat in Japanese constitutional interpretation; Self-Defense Forces personnel whose service is delegitimized by the reading have no voice in its maintenance.
% DISAPPEARANCE_RATIONALE: If the strict pacifist reading disappeared overnight, the constitutional barrier to full-spectrum defense development, collective self-defense, and alliance burden-sharing would collapse. The Self-Defense Forces would be normalized, security policy reformers would gain legislative and budgetary space, regional neighbors would face a remilitarized Japan, and the pacifist interpretive authority would lose its core constitutional mandate.
% FOUNDING_PROBLEM: Prevention of Japanese remilitarization and recurrence of aggressive warfare following World War II; establishment of a constitutional break with imperial military tradition.
% FOUNDING_PROBLEM_CORROBORATION: Postwar occupation authorities and early postwar Japanese governments attested the need for demilitarization. Contemporary security policy reformers and alliance partners attest the founding problem has evolved and absolute prohibition is now maladapted; international relations scholars outside the pacifist beneficiary set corroborate that the security environment has transformed since 1947. Pacifist civil society attests the problem remains live.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the categorical prohibition blocks full-spectrum defense development, power-projection capability, and alliance burden-sharing normalization. Suppression is high (0.75) because the reading must actively suppress constitutional revision, SDF normalization, and the inherent-right and collective-self-defense sibling readings. Theater is substantial (0.50) because the SDF's mere existence under an 'absolute' prohibition creates a growing performative gap between text and practice. Accessibility collapse is high (0.80) because once the strict reading is accepted as the only legitimate framework, organized military alternatives become cognitively unavailable. Resistance is moderate-high (0.60) due to persistent revisionist movements, security-establishment pressure, and alliance demands. The temporal series show rising extraction and theater post-Cold War as the security environment diverges from the reading's assumptions, while suppression ratchets upward to maintain the constraint against mounting pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (pacifist interpretive authority, civil society) experience the constraint as legitimate constitutional identity and necessary trust-building coordination. The payer seats (security apparatus, reformers) experience the same text as actively extractive â a legal fiction that constrains democratic security choice and forces alliance dependence. The engine computes this divergence from structural position: low directionality for beneficiaries who gain identity and security-trust goods, high directionality for institutional targets trapped in legal limbo.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pacifist_civil_society, regional_neighbors) are positioned at low d because the constraint subsidizes their preferred outcomes: pacifist identity maintenance and regional stability. The agenda_setter (pacifist_interpretive_authority) is d-near-beneficiary because the constraint is the source of their interpretive authority and political relevance. Victims (state_security_apparatus, security_policy_reformers) are at high d because the constraint directly extracts policy autonomy and constitutional legitimacy from them; their exit is constrained by institutional identity and democratic-political structure. The national-to-regional scope amplifies extraction for the state apparatus because the reading operates as a universal-scope constitutional norm that is harder to evade than a ordinary policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misidentifying the constraint as pure extraction (snare) because the reading solves a genuine coordination problem: regional trust in Japan's non-militarized posture and domestic prevention of remilitarization spiral. It prevents misidentifying it as pure coordination (rope) because the categorical form extracts asymmetrically from security autonomy and forces alliance dependence. The mandate â preventing Japanese remilitarization and recurrence of aggressive warfare â is contested whether it still requires absolute prohibition; the R5 genealogy shows corroboration from postwar occupation-era actors but contemporary contestation from security policy experts and alliance planners outside the beneficiary set, indicating potential mandatrophy without decisive resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_practice_gap_nature,
    'Does the strict pacifist reading function as an operative legal constraint that still materially shapes policy, or as a performative interpretive fiction concealing practiced military normalization?',
    'Compare the reading''s material policy effects â budget caps, doctrine constraints, alliance limitations, legal restrictions on collective self-defense â against the size, capability, and global operational footprint of the Self-Defense Forces.',
    'If the gap is purely theatrical with no material policy effect, the reading is a piton; if it still constrains procurement, doctrine, and alliance integration, it remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_practice_gap_nature, conceptual, 'Whether the strict reading has material force or is pure interpretive theater').

omega_variable(
    constitutional_suppression_mechanism,
    'Does the constraint''s persistence depend primarily on structural constitutional barriers or on internalized pacifist identity that would persist even if the text were amended?',
    'Observe constitutional-amendment simulation or compare with states that have formally similar prohibitions but different political cultures (e.g., Costa Rica) to see if military capacity remains constrained by culture alone.',
    'If internalized, the constraint operates more like identity_coordination with lower effective extraction; if structural, it is enforced extraction via legal suppression independent of belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_textual_ambiguity,
    'Is the strict pacifist reading the only grammatically and historically defensible interpretation of ''never be maintained,'' or does the constitutional text contain latent ambiguity that permits the inherent right reading?',
    'Linguistic and historical constitutional analysis of the original English and Japanese texts, plus drafting-history examination of the Imperial Diet and occupation authorities.',
    'If the text is genuinely unambiguous, the sibling readings are interpretive inventions and the strict reading''s foreclosure is textually grounded; if ambiguous, the foreclosure is a political-interpretive choice rather than a logical necessity, and the extraction is layered onto a more contingent coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_textual_ambiguity, conceptual, 'Whether the kernel text permits alternative readings or strictly entails the pacifist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9_strict_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(a9_strict_tr_t15, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(a9_strict_tr_t30, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(a9_strict_tr_t45, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 45, 0.5).
narrative_ontology:measurement(a9_strict_tr_t60, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(a9_strict_tr_t75, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 75, 0.6).

% Extraction over time
narrative_ontology:measurement(a9_strict_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(a9_strict_be_t15, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(a9_strict_be_t30, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(a9_strict_be_t45, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(a9_strict_be_t60, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(a9_strict_be_t75, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(a9_strict_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(a9_strict_su_t15, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(a9_strict_su_t30, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(a9_strict_su_t45, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(a9_strict_su_t60, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(a9_strict_su_t75, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is the strict pacifist reading of Article 9, one of three structurally distinct readings of the same kernel. The inherent right reading and collective self-defense reading instantiate different constraints with different epsilon values, beneficiary/victim structures, and classification profiles. See those stories for the sibling formulations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
