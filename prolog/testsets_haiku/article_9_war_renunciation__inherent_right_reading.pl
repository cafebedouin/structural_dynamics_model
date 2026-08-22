% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right to Self-Defense Reading
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This constraint instantiates the inherent-right reading of Article 9 of
 *   Japan's Constitution. Article 9 contains categorical language renouncing
 *   war and military armament, yet Japan has maintained the Self-Defense
 *   Forces for over 70 years through a constitutional interpretation that
 *   distinguishes aggressive war (prohibited) from minimum-necessary
 *   self-defense (permissible). This reading asserts that sovereign states
 *   retain an inherent right to self-defense that cannot be waived by
 *   constitutional text alone; Article 9 renounces the use of force for
 *   aggressive purposes but permits defensive military capacity. The reading
 *   is contested: strict pacifist interpreters read Article 9 as
 *   categorically prohibiting any armed forces; collective-defense adherents
 *   read the inherent right as extending to collective action. This JSON
 *   generates only the inherent-right reading as a structurally coherent
 *   constraint.
 *
 * KEY AGENTS:
 *   - Japan state (institutional): Agenda-setter; interprets Article 9, authorizes SDF, defines 'minimum necessary' threshold
 *   - Executive branch authority (powerful): Primary beneficiary; consolidates discretionary interpretation power
 *   - Pacifist constituencies (organized): Payers; their preferred reading is institutionally defeated without amendment
 *   - Neighboring states (powerful): Secondary payers; experience uncertainty about scope of 'minimum necessary'
 *   - Constitutional court (institutional): Observer; formally reviews compliance but historically defers to executive judgment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.62).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right to Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '7e6cb8f3-8e71-40b5-b47f-9904570a187d').
narrative_ontology:cs_kernel_codification('7e6cb8f3-8e71-40b5-b47f-9904570a187d', fixed_text).
narrative_ontology:cs_authority_grounding('7e6cb8f3-8e71-40b5-b47f-9904570a187d', lineage).
narrative_ontology:cs_interpretation_layer_present('7e6cb8f3-8e71-40b5-b47f-9904570a187d').
narrative_ontology:cs_reading_relation('7e6cb8f3-8e71-40b5-b47f-9904570a187d', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e6cb8f3-8e71-40b5-b47f-9904570a187d', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('7e6cb8f3-8e71-40b5-b47f-9904570a187d', foundational, sovereign_inherent_self_defense_right).
narrative_ontology:cs_axiom_status(sovereign_inherent_self_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('7e6cb8f3-8e71-40b5-b47f-9904570a187d', sovereign_inherent_self_defense_right, deontological).
narrative_ontology:cs_axiom('7e6cb8f3-8e71-40b5-b47f-9904570a187d', foundational, war_aggression_distinction_tenable).
narrative_ontology:cs_axiom_status(war_aggression_distinction_tenable, holdable).
narrative_ontology:cs_axiom_grounding('7e6cb8f3-8e71-40b5-b47f-9904570a187d', war_aggression_distinction_tenable, empirically_contingent).
narrative_ontology:cs_reference_frame('7e6cb8f3-8e71-40b5-b47f-9904570a187d', sovereign_inherent_self_defense_right_preserved).
narrative_ontology:cs_drift_state('7e6cb8f3-8e71-40b5-b47f-9904570a187d', contemporary_regional_threat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e6cb8f3-8e71-40b5-b47f-9904570a187d', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japan_state).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, executive_branch_authority).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, neighboring_states_threat_perception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the Self-Defense Forces under a constitutional reading that distinguishes war (prohibited) from minimum-necessary defense (permissible). Sets policy on what constitutes the 'minimum necessary' threshold and enforces that boundary through military doctrine and legal review. Collects legitimacy for maintaining military capacity despite Article 9's apparent categorical prohibition.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japan_state, agenda_setter,
    institutional, civilizational, analytical, national).

% Gains discretionary authority to interpret and implement the 'minimum necessary' threshold. This reading consolidates executive power to make security judgments without requiring constitutional amendment or explicit legislative reauthorization each time military posture shifts. Benefits from the ambiguity between the prohibition and the permissible exception.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, executive_branch_authority, beneficiary,
    powerful, biographical, constrained, national).

% Advocated for and interpreted Article 9 as a categorical prohibition on any armed forces. This reading treats that advocacy as partially defeated: the constitutional text they cite is reread to permit precisely what they sought to prohibit. They bear the cost of living under a constitutional interpretation that erodes their preferred reading without formal amendment. Exit would require constitutional change, which requires broad consensus they lack.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constituencies, payer,
    organized, biographical, constrained, national).

% Experience uncertainty about whether Japan's military posture is genuinely limited to 'minimum necessary' defense or whether the reading serves as legal cover for capacity expansion. The 'minimum necessary' standard is internally defined by Japan, creating asymmetric information. They bear reputational and security costs if the interpretation expands beyond regional consensus about what counts as defensive.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, neighboring_states_threat_perception, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, neighboring_states_threat_perception, observer).

% Reviews whether military actions comply with the 'minimum necessary' standard and whether the state's interpretation of this threshold remains within the boundaries of Article 9. Holds formal veto power but has historically deferred to executive judgment on what minimum-necessary entails, concentrating effective authority in the executive.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Would argue that Article 9's language 'never be maintained' is categorical and admits no exception for defense. They are excluded from the operative constitutional interpretation despite having a coherent alternative reading grounded in the same text. Their exclusion reflects institutional power to fix the reading, not textual unambiguity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading_adherents, excluded,
    moderate, biographical, identity_locked, national).

% Documents and debates whether Japan's interpretation aligns with general international law principles (which do recognize inherent self-defense) or violates the specific textual language of Article 9. Provides external corroboration for the reading's coherence but holds no formal enforcement authority within Japan's constitutional system.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, scholarly_international_law_community, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, japan_state).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of reconciling Japan's need for territorial defense with post-war constitutional constraints on military power: permits minimum-necessary military capacity while restricting expansionary military projects through the proportionality frame. Provides a stable legal basis for the Self-Defense Forces' operation without requiring constitutional amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from the text's apparent plain meaning (categorical prohibition) to the executive branch's practical judgment about what 'minimum necessary' entails. Moves legitimacy from pacifist constituencies (who anchored on the text's categorical language) to state security institutions. Moves resources: sustained military expenditure justified under a constitutional reading that might otherwise prohibit it.
% ABSENT_VOICES: Strict pacifist reading adherents, who hold a coherent textual interpretation but are excluded from the operative constitutional framework by institutional power to fix readings. International actors (neighboring states, treaty partners) who experience Japan's military posture but cannot formally contest the constitutional interpretation. Future generations, whose security constraints are set by current decisions about the 'minimum necessary' threshold.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the strict pacifist reading took institutional effect, Japan would be required to disband the Self-Defense Forces and reconstitute its security strategy entirely around alliances and non-military defense. Regional security architecture would shift as other states recalibrated for Japanese non-militarism. If it vanished and the collective self-defense reading took effect, military scope would expand to include offensive-capable operations beyond Japan's borders, restructuring regional alliance commitments. The world does not survive unchanged.
% FOUNDING_PROBLEM: Postwar Japan adopted Article 9 as a peace commitment and constraint on remilitarization after Imperial military aggression. The founding problem was preventing return to the imperial military state while maintaining international legitimacy and internal political consensus for a pacifist constitution.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist constituencies attest the founding problem is still live and Article 9 remains necessary to prevent remilitarization. Security-focused policymakers and regional neighbors attest the founding problem is partially obsolete: regional threats (North Korea, China's rise) now demand defensive capacity that a categorical reading would prohibit. International law scholars document that inherent self-defense rights are recognized globally; the debate centers whether Article 9 was meant to waive Japan's inherent right or merely to renounce aggressive use. Historical scholarship shows postwar Japanese drafters disagreed on this interpretation; the operative reading was fixed by executive practice, not by unanimous consensus.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) because the reading provides both genuine coordination (stable military capacity for territorial defense) and asymmetric authority (executive power to define 'minimum necessary' without supermajority amendment). The temporal trajectory shows extraction rising from 0.25 to 0.42 over the first 42 years (as the 'minimum necessary' threshold expanded through successive reinterpretations) and then plateauing at 0.38 as regional threat-perception stabilized post-Cold War. Suppression is substantial (0.62) because pacifist constituencies are institutionally barred from revising the reading without constitutional amendment (supermajority requirement creates a high activation threshold). Theater ratio rises over time (0.25 to 0.48) as the gap widens between the original constitutional text's apparent categorical prohibition and the operative security practice, requiring increasing public legitimation effort. Accessibility collapse is high (0.71) because the 'minimum necessary' threshold is internally defined by the state, limiting how far alternative readings can gain purchase: pacifist constituencies cannot credibly argue the SDF is unconstitutional given the institutional reading's dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the state's institutional position, this reading is genuine coordination that solves the founding problem (territorial defense without remilitarization) and is anchored in universally recognized international law (inherent self-defense right). From the pacifist constituency position, the reading is cover story that defeats their preferred interpretation without formal amendment, using ambiguity in 'war' to justify what the text appears to prohibit. The Constitutional Court sits between: it acknowledges both readings' coherence but defers to executive judgment on scope, effectively endorsing the state's reading through inaction. The engine should compute divergent types across seats: the state and court seats should trend toward rope (genuine coordination) while pacifist seats compute as snare (their position is suppressed, not coordinated). This structural asymmetry is the signature of a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Japan state and executive branch are beneficiaries: they collect the benefit of unambiguous military legitimacy and avoid the supermajority amendment requirement that constitutional revision would impose. Pacifist constituencies are victims: their preferred reading is institutionally defeated, suppression is structural (supermajority amendment requirement), and exit requires constitutional change with zero institutional support. Neighboring states are secondary victims: they experience unilateral interpretation of 'minimum necessary' without formal voice. Directionality for the state should approach 0.0 (beneficiary): low cost, high benefit, control of interpretation. Directionality for pacifist constituencies should approach 1.0 (target): high cost (excluded from operative reading), high exit barrier (trapped in institutional structure), no benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent remilitarization, maintain pacifist constitution) is contested between readings but not dead: pacifist constituencies attesting that the problem is still live and Article 9 remains essential. However, the constraint-as-operative (the inherent-right reading institutionally fixed in policy) treats the founding problem as substantially obsolete: executive security doctrine treats Article 9 as no longer constraining, merely channeling military capacity. This is a mandate-atrophy candidate: the operative reading preserves the text while inverting its intention, and the founding problem's status is captured by the reading-contest rather than by the world's actual security needs. The disappearance_verdict (world_rearranges) and founding_problem_status (contested) together suggest mandatrophy: the arrangement persists because the institutional reading won the right to reinterpret the founding commitment, not because the founding problem is alive in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_war_definition,
    'Is Article 9''s prohibition on ''war'' (senso) categorically absolute, or does it permit a distinction between aggressive war (prohibited) and defensive military action (permissible)?',
    'Textual analysis of the original Japanese language and contemporaneous legislative intent; comparison with how other pacifist constitutions (Germany, Austria) define their prohibitions; examination of whether ''war'' in postwar international law was understood to exclude defensive action.',
    'If the text permits a war/defense distinction, the inherent-right reading becomes structurally coherent and extraction drops (genuine coordination). If the text is categorically absolute, the reading becomes a cover story for executive authority expansion and extraction rises (tangled rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_war_definition, empirical, 'Whether Article 9''s language prohibits all military forces or only aggressive war.').

omega_variable(
    minimum_necessary_threshold_drift,
    'Has the ''minimum necessary'' threshold for self-defense remained stable over the reading''s 70-year operative history, or has it expanded through successive reinterpretations?',
    'Temporal tracking of Constitutional Court rulings, executive defense white papers, and military doctrine across 5-year intervals. Compare authorized SDF capacity in 1954, 1978, 2000, and 2024.',
    'Stable threshold = genuine proportionality constraint with moderate extraction (tangled rope). Expanding threshold = the ''minimum necessary'' is a rhetorical placeholder for whatever military capacity the executive chooses, and extraction rises toward snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_threshold_drift, empirical, 'Whether the minimum-necessary standard functions as a durable constraint or drifts toward executive prerogative.').

omega_variable(
    suppression_identity_lock_mechanism,
    'Is the measured suppression (0.62) structural (institutional barriers to pacifist revival: supermajority amendment requirement) or internalized (pacifist constituencies have accepted the inherent-right reading into their own frameworks)?',
    'Post-suppression trajectory: if pacifist constituencies regain institutional voice (e.g., through electoral shifts), do they attempt constitutional amendment, or have they internalized the inherent-right reading as inevitable? Do pacifist movements frame their demands as ''strengthen SDF oversight'' vs. ''dismantle SDF''?',
    'If structural only, suppression would decrease if institutions open. If internalized, pacifist constituencies carry suppression with them even after institutional opportunity arises, and effective suppression is higher than the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_identity_lock_mechanism, empirical, 'Whether suppression of pacifist positions is structural or internalized.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the inherent-right reading logically foreclose the strict-pacifist reading within a single institutional framework, or do the readings coexist as live contested positions?',
    'Test whether a state actor can simultaneously hold both readings without internal contradiction. The inherent-right reading asserts the right exists; the strict-pacifist reading asserts it was waived. These are contradictory claims about the same state''s rights. However, the contradiction is resolvable: ''Japan retained the right but chose to exercise it only minimally.'' This makes them compatible within a single framework if one interprets the choice (exercise) differently.',
    'If readings foreclose each other logically, the reading-contest is a zero-sum institutional struggle and one must eventually disappear. If they coexist, the stable state is contested legitimacy, and the classification remains tangled rope (coordination need + enforcement against rivals).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether the inherent-right reading logically eliminates its sibling readings or coexists with them.').

omega_variable(
    collective_defense_scope_boundary,
    'Where does the ''minimum necessary'' threshold for inherent self-defense end, and collective self-defense (the sibling reading) begin? Is the boundary between readings stable or does inherent-right reasoning naturally extend to collective defense?',
    'Constitutional law trajectory: does the court and executive treat collective defense as a separate, second authorizing principle? Or do they justify collective defense using inherent-right language, implying the boundary is permeable?',
    'If the boundary is stable, the two readings are genuinely distinct and the reading-family is coherent. If inherent-right reasoning extends naturally to collective defense, this reading functions as a stepping stone toward the collective-defense reading, and the classification within this constraint moves toward piton (staged expansion defended by the prior reading''s legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_defense_scope_boundary, empirical, 'Whether the inherent-right reading''s logical principles extend to justify collective self-defense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t14, article_9_war_renunciation__inherent_right_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement_basis(arti_tr_t14, observed).
narrative_ontology:measurement(arti_tr_t28, article_9_war_renunciation__inherent_right_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(arti_tr_t28, observed).
narrative_ontology:measurement(arti_tr_t42, article_9_war_renunciation__inherent_right_reading, theater_ratio, 42, 0.47).
narrative_ontology:measurement_basis(arti_tr_t42, observed).
narrative_ontology:measurement(arti_tr_t56, article_9_war_renunciation__inherent_right_reading, theater_ratio, 56, 0.48).
narrative_ontology:measurement_basis(arti_tr_t56, observed).
narrative_ontology:measurement(arti_tr_t70, article_9_war_renunciation__inherent_right_reading, theater_ratio, 70, 0.48).
narrative_ontology:measurement_basis(arti_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t14, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement_basis(arti_be_t14, observed).
narrative_ontology:measurement(arti_be_t28, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 28, 0.38).
narrative_ontology:measurement_basis(arti_be_t28, observed).
narrative_ontology:measurement(arti_be_t42, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 42, 0.42).
narrative_ontology:measurement_basis(arti_be_t42, observed).
narrative_ontology:measurement(arti_be_t56, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 56, 0.38).
narrative_ontology:measurement_basis(arti_be_t56, observed).
narrative_ontology:measurement(arti_be_t70, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 70, 0.38).
narrative_ontology:measurement_basis(arti_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t14, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement_basis(arti_su_t14, observed).
narrative_ontology:measurement(arti_su_t28, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement_basis(arti_su_t28, observed).
narrative_ontology:measurement(arti_su_t42, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 42, 0.63).
narrative_ontology:measurement_basis(arti_su_t42, observed).
narrative_ontology:measurement(arti_su_t56, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement_basis(arti_su_t56, observed).
narrative_ontology:measurement(arti_su_t70, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 70, 0.62).
narrative_ontology:measurement_basis(arti_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% Article 9 kernel decomposes into three structurally distinct constraints with different ε values, institutional effects, and contested status. The inherent-right reading (this story) treats the constitutional text as permitting defensive force and interprets 'war' as aggressive action only. The strict-pacifist reading (sibling) treats the text as categorically prohibiting any armed forces. The collective-self-defense reading (sibling) extends inherent right to collective action. All three readings operate within the same fixed constitutional text (the kernel); their divergence is interpretive, not textual. Sibling relationships: inherent-right coexists-with strict-pacifist (both remain live contested positions in Japanese politics); inherent-right influences collective-self-defense (the inherent-right reasoning provides linguistic and conceptual resources for the collective-defense extension). See network.affects_constraints for bidirectional links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
