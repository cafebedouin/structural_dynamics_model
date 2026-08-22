% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading — Categorical Prohibition on Armed Forces
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution states: 'Aspiring sincerely to an
 *   international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as means of settling international disputes. In order to
 *   accomplish the aim of the preceding paragraph, land, sea, and air forces,
 *   as well as other war potential, will never be maintained. The right of
 *   belligerency of the state will not be recognized.' The strict pacifist
 *   reading takes the textual language 'never be maintained' as a categorical
 *   prohibition on any organized armed forces, including those configured for
 *   self-defense. War renunciation is absolute. This reading was dominant in
 *   the early postwar period but has been progressively hollowed by the
 *   creation of the Self-Defense Forces (1954), the gradual expansion of
 *   their missions, the 2014 cabinet reinterpretation permitting collective
 *   self-defense, and the 2015 security legislation. The constraint persists
 *   as a textual absolute while the reality it governs has diverged
 *   dramatically — the central tension this story captures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.38).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.42).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading — Categorical Prohibition on Armed Forces").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'bfb34516-5734-4c81-b8a6-e24a61cada44').
narrative_ontology:cs_kernel_codification('bfb34516-5734-4c81-b8a6-e24a61cada44', fixed_text).
narrative_ontology:cs_authority_grounding('bfb34516-5734-4c81-b8a6-e24a61cada44', lineage).
narrative_ontology:cs_interpretation_layer_present('bfb34516-5734-4c81-b8a6-e24a61cada44').
narrative_ontology:cs_reading_relation('bfb34516-5734-4c81-b8a6-e24a61cada44', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('bfb34516-5734-4c81-b8a6-e24a61cada44', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('bfb34516-5734-4c81-b8a6-e24a61cada44', foundational, categorical_prohibition_on_armed_forces).
narrative_ontology:cs_axiom_status(categorical_prohibition_on_armed_forces, holdable).
narrative_ontology:cs_axiom_grounding('bfb34516-5734-4c81-b8a6-e24a61cada44', categorical_prohibition_on_armed_forces, deontological).
narrative_ontology:cs_axiom('bfb34516-5734-4c81-b8a6-e24a61cada44', foundational, war_renunciation_absolute_no_self_defense_exception).
narrative_ontology:cs_axiom_status(war_renunciation_absolute_no_self_defense_exception, holdable).
narrative_ontology:cs_axiom_grounding('bfb34516-5734-4c81-b8a6-e24a61cada44', war_renunciation_absolute_no_self_defense_exception, deontological).
narrative_ontology:cs_axiom('bfb34516-5734-4c81-b8a6-e24a61cada44', secondary, non_military_security_paradigm_viable).
narrative_ontology:cs_axiom_status(non_military_security_paradigm_viable, holdable).
narrative_ontology:cs_axiom_grounding('bfb34516-5734-4c81-b8a6-e24a61cada44', non_military_security_paradigm_viable, instrumental).
narrative_ontology:cs_reference_frame('bfb34516-5734-4c81-b8a6-e24a61cada44', postwar_constitutional_pacifism).
narrative_ontology:cs_drift_state('bfb34516-5734-4c81-b8a6-e24a61cada44', contemporary_security_environment, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bfb34516-5734-4c81-b8a6-e24a61cada44', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars_strict_constructionist).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, opposition_parties_constitutionalist).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_ngos).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_bureaucracy).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, constitutional_pacifism).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, war_renunciation_absolute).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, non_military_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilized around Article 9 as the defining constitutional commitment. Views any military normalization as existential betrayal of Japan's postwar identity. Organizes mass protests, constitutional defense networks, and electoral pressure. Exit from this commitment would mean abandoning the core of their political identity and the moral framework that legitimizes their activism.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society, beneficiary,
    organized, generational, identity_locked, national).

% Provide the authoritative textual interpretation that 'never be maintained' categorically prohibits any armed forces. Their professional standing and interpretive tradition are built on this reading. They advise courts, legislators, and civil society. An alternative reading would not merely change their view — it would dissolve the interpretive tradition their careers instantiate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars_strict_constructionist, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars_strict_constructionist, observer).

% Electoral viability depends on defending the strict pacifist reading against revisionist pressures. They use Article 9 as a rallying point for their base and a constraint on the ruling party. Abandoning this position would fracture their coalition; adopting a more flexible reading would hand the initiative to revisionists. Their exit is constrained by the electoral market they operate in.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, opposition_parties_constitutionalist, beneficiary,
    organized, biographical, constrained, national).

% Cite Article 9 as a model for constitutional pacifism worldwide. Their advocacy campaigns, funding appeals, and moral authority leverage Japan's 'peace constitution.' They can redirect to other cases if this reading collapses, but the loss of the flagship example would weaken their global framework.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_peace_ngos, beneficiary,
    moderate, generational, mobile, global).

% The abstract capacity of the Japanese state to make independent security decisions without alliance permission. The strict reading forecloses autonomous military action even in defense, forcing reliance on the US alliance for any credible deterrence. This is not an agent with preferences — it is a structural capacity that the constraint extracts from. The 'cost' is paid in strategic dependence and decisional incapacity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy, payer,
    institutional, civilizational, trapped, national).

% Serve in an organization whose constitutional legitimacy is perpetually contested under the strict reading. Their professional identity, career structure, and institutional mission exist in a zone of textual ambiguity — the SDF is maintained *despite* the strict reading's categorical prohibition. They bear the professional stigma of 'unconstitutional' service and the operational constraints of a force that cannot officially exist for its stated purpose. Exit means leaving a career and professional identity built around this ambiguous status.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel, payer,
    organized, biographical, constrained, national).

% The Ministry of Defense and associated agencies that administer the SDF and manage the US alliance. They must operate the security apparatus while formally adhering to a constitutional text that, under the strict reading, forbids their core function. This produces elaborate legal fictions (the SDF is not 'armed forces,' collective self-defense is not 'war') and a permanent campaign for interpretive space. They are both the administrators of the constraint's workaround and its primary institutional victims.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, defense_bureaucracy, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, defense_bureaucracy, agenda_setter).

% Advocate explicit constitutional amendment to recognize the SDF and permit collective self-defense. They are structurally excluded from the constitutional order the strict reading defends — their project requires breaking the very constraint this story describes. They would object to the strict reading's dominance but are kept out of the 'legitimate interpreter' circle by the same textual absolutism.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_revisionists, excluded,
    powerful, generational, mobile, national).

% Manage the US-Japan security treaty that fills the deterrence gap the strict reading creates. They need Japan to bear more burden but cannot say so openly; they need the SDF to be more capable but must respect the constitutional fiction. They watch the interpretive struggle from outside, calculating how each shift affects alliance credibility and burden-sharing.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, us_alliance_managers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide commitment to renunciation of war as national policy, providing a stable identity anchor for postwar Japan and a credible signal to neighbors that Japanese militarism is permanently foreclosed. Solves the coordination problem of collective trauma processing and international reassurance through a single textual commitment.
% TRANSFER_FUNCTION: Transfers security autonomy and independent defense capacity from the Japanese state to the US alliance structure. The state pays in strategic dependence; the alliance partner receives a protected market and a forward basing architecture. Domestic pacifist constituencies receive moral ownership of the constitutional order; the defense bureaucracy receives an impossible mandate it must creatively circumvent.
% ABSENT_VOICES: The constitutional revisionists (excluded stakeholders) would object to the strict reading's dominance but are structurally barred from the 'legitimate interpreter' circle. Okinawan communities bearing disproportionate base burden are partially excluded — their security concerns are mediated through mainland pacifist frameworks that prioritize constitutional purity over their lived exposure. Future generations who will inherit the strategic consequences of current interpretive choices have no voice.
% DISAPPEARANCE_RATIONALE: If the strict reading vanished overnight, the SDF would gain explicit constitutional recognition within months, collective self-defense would be formally codified, the US alliance would shift toward more equal burden-sharing, and Japan's regional security posture would visibly normalize. The postwar identity settlement would fracture, triggering a constitutional crisis and realignment of party systems. The world rearranges because the constraint is the keystone of the entire postwar order.
% FOUNDING_PROBLEM: How to constitutionally foreclose the return of Japanese militarism after 1945, while credibly signaling to Asia-Pacific neighbors that the wartime state was permanently dismantled. The founding problem was not abstract pacifism but the specific historical need to make renunciation irreversible and verifiable.
% FOUNDING_PROBLEM_CORROBORATION: The strict reading's beneficiaries (pacifist civil society, strict constructionist scholars) attest the founding problem remains live — regional tensions and revisionist politics make the foreclosure function essential. The defense bureaucracy and revisionist politicians attest the founding problem is substantially solved — Japan has been a peaceful democracy for 80 years, the SDF exists de facto, and the constraint now obstructs necessary adaptation. US diplomatic archives from 1950-51 (declassified) corroborate the 'irreversible and verifiable' intent — the US initially opposed Article 9 but accepted it as the price of a peace treaty that locked in the Yoshida government.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).
:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the real but not total extraction of security autonomy — the state retains the US alliance and a de facto military (SDF), so the constraint does not leave the state defenseless. Suppression (0.42) is moderate: the constraint requires active maintenance of legal fictions (SDF ≠ armed forces) and interpretive discipline, but does not rely on overt coercion against dissenters. Theater ratio (0.28) captures the growing gap between the constitutional text and the security reality — the strict reading's maintenance increasingly performs constitutional fidelity while the substance has migrated. Accessibility collapse (0.71) is high: once the strict reading is understood, alternative security arrangements (explicit amendment, normalized military) appear as constitutional betrayals, not policy options. Resistance (0.34) is moderate: revisionist pressures exist but have not achieved Article 9 amendment despite decades of effort, indicating the constraint still commands significant loyalty.
 *
 * PERSPECTIVAL GAP:
 *   The pacifist civil society and strict constructionist scholar seats experience this as a genuine coordination constraint (rope-like) that protects their identity and interpretive tradition. The state_security_autonomy and defense_bureaucracy seats experience it as extraction with active suppression (snare-like) — they must operate in the gap between text and reality. The SDF personnel seat sits in the tangled middle: they benefit from the constraint's legitimation of Japan's peaceful identity but pay in professional ambiguity and operational constraint. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the aggregate structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pacifist civil society, strict constructionist scholars, opposition parties, peace NGOs) collect identity, moral authority, electoral coherence, and global exemplary status from the constraint. Their directionality is toward the beneficiary end (d ~ 0.15-0.3). Victims (state_security_autonomy as structural capacity, SDF personnel, defense bureaucracy) bear the costs of strategic dependence, professional ambiguity, and institutional impossibility. Their directionality is toward the target end (d ~ 0.7-0.85). Constitutional revisionists are excluded — their exit is blocked by the constraint's textual absolutism. US alliance managers observe from outside with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreclosing militarism's return) is contested as live or solved. If live, the constraint remains functional coordination. If solved, the constraint has become a piton — persisting by inertia and identity-lock after its function atrophied. The strict reading's persistence despite the SDF's 70-year existence suggests mandatrophy: the constraint's original coordination function (making renunciation credible) has been achieved by 80 years of peaceful democracy, but the textual absolute prevents the constitutional order from acknowledging the SDF's reality. The constraint now extracts from the state's security autonomy without delivering additional coordination value — the hallmark of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the strict pacifist reading a distinct constraint from the inherent_right_reading and collective_self_defense_reading, or are they measurement perspectives on a single constraint?',
    'Apply the ε-invariance test: if the three readings produce structurally different ε values, beneficiary/victim sets, and classification outcomes when evaluated against the SAME standing arrangement (the postwar Japanese security order), they are distinct constraints. The kernel_id groups them; the reading_ids instantiate separate constraints.',
    'If distinct, each reading gets its own constraint story with independent classification. If not, the framework must model reading-variance within one constraint — which the current architecture forbids (DP-001). This omega documents the committer-frame boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are separate constraints or one constraint with observer variance.').

omega_variable(
    strict_reading_forecloses_inherent_right,
    'Does the strict reading''s core premise (categorical prohibition on any armed forces) logically foreclose the inherent_right_reading''s core premise (minimum defensive capacity permitted) within any single constitutional framework?',
    'Analyze whether a single constitutional interpreter could simultaneously hold both: that ''never be maintained'' categorically prohibits all armed forces AND that the constitution permits minimum defensive capacity. If logically impossible, the relation is ''forecloses''; if merely politically opposed, ''coexists_with''.',
    'Determines cs_structure.reading_relations entry for inherent_right_reading. A ''forecloses'' relation means the strict reading''s dominance structurally eliminates the inherent right reading as a live position within the same framework — relevant for drift_state computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reading_forecloses_inherent_right, conceptual, 'Logical foreclosure vs. political coexistence between strict pacifist and inherent right readings.').

omega_variable(
    security_autonomy_as_victim,
    'Is ''state_security_autonomy'' a legitimate victim stakeholder — a structural capacity that bears extraction — or a metaphorical projection that conflates the state''s interests with a non-agent entity?',
    'Test whether the extraction from state_security_autonomy has measurable consequences: does the strict reading''s dominance correlate with specific strategic dependence outcomes (alliance permission-seeking, deterrence gaps, crisis decision latency)? If yes, the structural capacity is a real extraction target. If no, it is a revisionist talking point.',
    'If legitimate, the victim declaration stands and feeds directionality computation for the institutional seat. If metaphorical, the victim set shrinks to SDF personnel and defense bureaucracy only, altering the constraint''s extraction profile and potentially its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_autonomy_as_victim, empirical, 'Whether state security autonomy is a real extraction target or a rhetorical construct.').

omega_variable(
    mandatrophy_tipping_point,
    'At what point does the strict reading''s persistence shift from functional coordination (credible renunciation signal) to mandatrophy (identity-locked extraction after the founding problem is solved)?',
    'Track the correlation between regional threat perception, domestic revisionist sentiment, and the strict reading''s mobilization capacity. If the reading''s enforcement energy correlates with identity politics rather than credible deterrence signaling, the shift has occurred.',
    'If mandatrophy has occurred, the constraint''s claimed_type should be piton, not tangled_rope. The theater_ratio trajectory (rising from 0.05 to 0.28) is evidence but not dispositive — the engine''s T17 abductive trigger would fire on extraction accumulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_tipping_point, preference, 'Whether the constraint has crossed from functional coordination to identity-locked extraction.').

omega_variable(
    sd_personnel_dual_role_validity,
    'Are SDF personnel genuinely dual-positioned (payer + beneficiary) or is the beneficiary assignment a category error — they benefit from the *peaceful identity* the constraint produces, not from the constraint itself?',
    'Disaggregate: would SDF personnel prefer a constitution that explicitly recognizes their service (ending professional ambiguity) even if it weakens Japan''s pacifist brand? If yes, they are net payers; the ''beneficiary'' assignment reflects the constraint''s externalities, not their structural position.',
    'If the secondary_role is invalid, the SDF personnel stakeholder should be role: payer only. This changes the beneficiary/payer balance and the constraint''s coordination function assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_personnel_dual_role_validity, empirical, 'Whether SDF personnel''s beneficiary status is structural or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9_spr_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(a9_spr_tr_t1954, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(a9_spr_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(a9_spr_tr_t1972, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1972, 0.18).
narrative_ontology:measurement(a9_spr_tr_t1991, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(a9_spr_tr_t2001, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(a9_spr_tr_t2014, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(a9_spr_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(a9_spr_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.12).
narrative_ontology:measurement(a9_spr_be_t1954, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(a9_spr_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(a9_spr_be_t1972, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1972, 0.26).
narrative_ontology:measurement(a9_spr_be_t1991, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1991, 0.31).
narrative_ontology:measurement(a9_spr_be_t2001, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2001, 0.33).
narrative_ontology:measurement(a9_spr_be_t2014, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2014, 0.36).
narrative_ontology:measurement(a9_spr_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(a9_spr_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.15).
narrative_ontology:measurement(a9_spr_su_t1954, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1954, 0.28).
narrative_ontology:measurement(a9_spr_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(a9_spr_su_t1972, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1972, 0.38).
narrative_ontology:measurement(a9_spr_su_t1991, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(a9_spr_su_t2001, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2001, 0.41).
narrative_ontology:measurement(a9_spr_su_t2014, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement(a9_spr_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.1).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty_operation).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, sd_f_legal_status).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, japan_defense_budget_trajectory).

% DUAL FORMULATION NOTE:
% This constraint is the strict_pacifist_reading of the article_9_war_renunciation kernel. The inherent_right_reading and collective_self_defense_reading are sibling constraints with different ε values, beneficiary/victim structures, and claimed types. The ε divergence: strict reading (ε=0.38, extractive coordination), inherent right reading (ε~0.15, functional coordination with residual extraction), collective self-defense reading (ε~0.25, coordination with alliance-mediated extraction). They are linked as a constraint family via affects_constraints. The upstream constraint (strict reading) influences the downstream readings by setting the textual baseline they must interpret around.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, institutional, 0.78).
constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
