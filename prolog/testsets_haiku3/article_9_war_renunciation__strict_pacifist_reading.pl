% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Article 9 Strict Pacifist Reading: Categorical Ban on Organized Military Forces
 *   domain: constitutional/security
 *
 * SUMMARY:
 *   Article 9 of Japan's Constitution is a kernel text around which multiple
 *   incompatible readings cluster. This constraint story instantiates the
 *   strict pacifist reading: the textual language 'war ... shall never be
 *   maintained' is interpreted as categorical prohibition on any organized
 *   military forces whatsoever, including those framed as purely defensive.
 *   Under this reading, Japan has renounced not merely aggressive war but the
 *   capacity to wage war in any form, making strategic self-defense
 *   achievable only through alliance dependence or non-military means. The
 *   reading treats the constraint as if it were a natural law — textually
 *   determinate, morally imperative, not negotiable — though this claim
 *   itself is contested by the other readings (inherent_right and
 *   collective_self_defense), which argue the text permits defensive capacity
 *   or alliance-based collective action. This story models the strict reading
 *   as a constraint structure: what extraction, suppression, and resistance
 *   does the strict interpretation actually produce? The authored claim is
 *   mountain (the reading treats the constraint as textual/natural law); the
 *   authored metrics show substantial extraction (strategic autonomy is
 *   sacrificed, state bears the cost of alliance dependence) and persistent
 *   suppression (the reading forecloses certain arguments from constitutional
 *   debate). The gap between claimed type and measured metrics is exactly the
 *   diagnosis the engine performs: does a 'natural law' reading actually
 *   describe how the constraint operates, or does it mask constructed choices
 *   benefiting particular parties?
 *
 * KEY AGENTS:
 *   - Strict pacifist reading: treats Article 9's text as categorical, non-negotiable prohibition (textual authority, institutional power) — operates as agenda-setter for constitutional interpretation, though courts have informally rejected this reading in practice
 *   - Security establishment: bears the cost of operating under categorical prohibition, constrained by inability to legally maintain autonomous military capacity (institutional power, high constraint, low exit)
 *   - Pacifist political movement: benefits from the reading's vindication of their moral position and from the formal barrier it creates against remilitarization (organized power, beneficiary)
 *   - US-Japan alliance: benefits from Japan's constitutional inability to free-ride or defect; the constraint enforces Japan's dependence (institutional power, beneficiary, trapped in the structure)
 *   - Strategic autonomy interest: bears the cost of constitutional prohibition on independent military capacity (abstract agent, structural victim)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.82).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.71).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, mountain).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading: Categorical Ban on Organized Military Forces").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional/security").

domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'b327234c-0056-4622-ac66-7c703be8604c').
narrative_ontology:cs_kernel_codification('b327234c-0056-4622-ac66-7c703be8604c', fixed_text).
narrative_ontology:cs_authority_grounding('b327234c-0056-4622-ac66-7c703be8604c', lineage).
narrative_ontology:cs_interpretation_layer_present('b327234c-0056-4622-ac66-7c703be8604c').
narrative_ontology:cs_reading_relation('b327234c-0056-4622-ac66-7c703be8604c', article_9_war_renunciation__inherent_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('b327234c-0056-4622-ac66-7c703be8604c', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('b327234c-0056-4622-ac66-7c703be8604c', foundational, absolute_war_prohibition_no_exceptions).
narrative_ontology:cs_axiom_status(absolute_war_prohibition_no_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('b327234c-0056-4622-ac66-7c703be8604c', absolute_war_prohibition_no_exceptions, deontological).
narrative_ontology:cs_axiom('b327234c-0056-4622-ac66-7c703be8604c', foundational, military_forces_categorically_impermissible).
narrative_ontology:cs_axiom_status(military_forces_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('b327234c-0056-4622-ac66-7c703be8604c', military_forces_categorically_impermissible, conventional).
narrative_ontology:cs_axiom('b327234c-0056-4622-ac66-7c703be8604c', secondary, security_via_demilitarization_not_armament).
narrative_ontology:cs_axiom_status(security_via_demilitarization_not_armament, holdable).
narrative_ontology:cs_axiom_grounding('b327234c-0056-4622-ac66-7c703be8604c', security_via_demilitarization_not_armament, instrumental).
narrative_ontology:cs_reference_frame('b327234c-0056-4622-ac66-7c703be8604c', postwar_pacifist_constitution).
narrative_ontology:cs_drift_state('b327234c-0056-4622-ac66-7c703be8604c', contemporary_alliance_dependence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b327234c-0056-4622-ac66-7c703be8604c', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_advocates).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_defensive_capacity).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, strategic_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_political_movement).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, us_japan_alliance).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_system).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, security_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The written text of Article 9 and its plain-language interpretation. The reading treats the constraint as textually self-evident: 'never be maintained' admits no exception for defensive forces. The authority to read the text is distributed among courts, constitutional scholars, and legislatures, but the text's meaning under this reading is claimed as determinate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_text_authority, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__strict_pacifist_reading, constitutional_text_authority).

% Advocates for absolute non-militarization as moral imperative and constitutional mandate. They benefit from the constraint's interpretation because it vindicates their worldview and creates a formal barrier against remilitarization. They argue that security through alliance and international law is superior to national military capacity and that maintaining the constraint reinforces Japan's peace identity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_political_movement, beneficiary,
    organized, generational, mobile, national).

% Military and defense officials tasked with protecting state security. Under the strict reading, they operate under categorical prohibition: they cannot maintain organized armed forces, only self-defense forces framed as non-military. They bear the cost of operating under legal fiction and strategic constraint. Their exit option is constitutional amendment, which is procedurally prohibitive (supermajority required, political energy extremely high).
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, security_establishment, payer,
    institutional, generational, constrained, national).

% Japan's ability to defend itself without external dependence on allies for security. The strict reading forecloses independent military capacity, making strategic autonomy hostage to alliance relationships (primarily the US-Japan alliance). A state cannot simultaneously renounce all military capacity and retain genuine strategic autonomy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, strategic_autonomy_interest, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__strict_pacifist_reading, strategic_autonomy_interest).

% The US-Japan security alliance benefits from Japan's constitutional inability to free-ride or defect: because Japan cannot militarily defend itself independently, it must remain bound to the alliance. The constraint creates structural dependence that enforces alliance loyalty.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, us_japan_alliance, beneficiary,
    institutional, generational, trapped, global).

% Interpret Article 9 in actual cases. Under the strict reading, they enforce the categorical prohibition. In practice, Japanese courts have upheld the Self-Defense Forces as constitutional by interpreting them as non-military, which creates a performative gap: the text says 'never,' the courts allow SDF by linguistic reframing. The reading the courts formally endorse is closer to 'inherent_right' than 'strict_pacifist,' but this story models the strict reading as a constraint on what the courts could legitimately authorize.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, constitutional_courts, observer).

% International organizations, treaty bodies, and multilateral frameworks grounded in collective security and non-militarization norms benefit from the strict reading as a vindication of the principle that security can be achieved through institutional frameworks rather than unilateral military capacity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_system, beneficiary,
    institutional, generational, mobile, global).

% Security professionals and constitutional scholars who argue for flexible interpretation (Japan's defensive capacity is minimal and proportionate; Japan's survival requires military autonomy; inherent right to self-defense is inalienable) are structurally excluded from the strict reading's interpretive frame. Under this reading, their arguments are ruled out a priori by the text's categorical language.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, strategic_realist_excluded_voices, excluded,
    organized, generational, constrained, national).

% Structural analyst examining how the constraint operates and what trade-offs it embeds, independent of endorsement.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, pacifist_political_movement).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Renounces participation in international war as a means of national policy; coordinates Japanese state behavior with a global non-militarization norm and collective security framework. Structures Japan's security commitment around alliance relationships and international law rather than unilateral military deterrence.
% TRANSFER_FUNCTION: Transfers security decision-making authority from Japan to: (1) the US-Japan alliance framework, requiring US security commitment to protect Japan; (2) international institutions and collective security arrangements that assume member states have capacity to contribute only non-military resources. Also transfers the psychological/identity benefit of non-militarization to pacifist constituencies and the global peace movement.
% ABSENT_VOICES: Strategic realists and national-defense specialists who would argue for flexible interpretation (inherent right reading) are structurally excluded from this reading's framing. Likewise, military strategists who view autonomous defensive capacity as essential to survival. These voices exist in Japanese politics and constitutional scholarship but the strict reading treats their premises as ruled out by text.
% DISAPPEARANCE_RATIONALE: If the strict reading and its categorical prohibition disappeared, replaced by the 'inherent_right' reading or collective defense reading, Japan could legally maintain autonomous military forces and could debate their scope. Strategic calculations would shift, alliance relationships would reconfigure (the US could no longer rely on Japan's constitutional dependence), and geopolitical competition in East Asia would change shape. The constraint is not natural law — it is a human institution whose disappearance would cause reorganization.
% FOUNDING_PROBLEM: Japan emerged from WWII with devastating defeat and moral crisis; the victorious occupiers and reformist Japanese constituencies sought to constitute a state that could never again wage aggressive war and would bind itself to international law and peace norms. Article 9 was drafted to make militarism constitutionally impossible and to anchor Japan's identity in peace commitment.
% FOUNDING_PROBLEM_CORROBORATION: The strict pacifist reading affirms the founding problem remains live: Japan must never again be a military threat to itself or others, and the categorical ban ensures this. However, security professionals and the inherent-right reading contest this: they argue the founding problem (preventing imperial aggression) is solved, the threat environment has changed, and Japan now faces different security challenges. International relations scholars outside the Japanese government attest that the founding problem was historically real but is contested whether it remains live in contemporary form.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_9_war_renunciation__strict_pacifist_reading),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82 at interval end) because the constraint costs Japan genuine strategic autonomy — it forecloses the option to defend itself without external dependence. Suppression is also substantial (0.71) because the reading suppresses certain constitutional arguments: under strict pacifism, the 'inherent right' frame is ruled out a priori as inconsistent with categorical text. Theater_ratio rises over time (0.28 to 0.44) because the enforcement of the constraint increasingly depends on legal and rhetorical performance — the Self-Defense Forces exist in de facto form, but the strict reading requires them to be reframed as not-military, which is a growing divergence between text-as-read and operational reality. Accessibility_collapse is very high (0.89) because once the reading's categorical interpretation is accepted, alternatives (armed self-defense, military reform) are logically unavailable — the text is read as admitting no exception. Resistance is substantial (0.68) because security professionals, strategic realists, and the inherent-right constituency actively resist the strict reading, both in constitutional scholarship and in de facto expansion of SDF capabilities. The temporal series reflects how the constraint's extractiveness increases as the threat environment changes post-Cold War (Japan faces evolving security challenges while the constraint's scope does not adapt), and theater_ratio increases as the performative gap between text and operation widens.
 *
 * PERSPECTIVAL GAP:
 *   From the pacifist reading's seat, the constraint is a textually-mandated moral imperative — a mountain, natural law. From the security establishment's seat, it is an imposed strategic handicap whose enforcement depends on legal reinterpretation (the SDF is technically not military, which is performative theater). From an outside analytical seat, the gap is structural: the reading claims its constraint emerges naturally from text, but the measured extraction (loss of autonomy) and suppression (foreclosure of alternative readings) and rising theater_ratio (growing divergence between interpretation and practice) suggest the constraint's persistence depends on active institutional work, not textual determinacy. The engine computes this per-seat divergence from the structural data: beneficiary (pacifist movement) sees vindication; payer (security establishment) sees imposed cost; analytical observer sees the gap between the claimed naturalness and the measured artificiality.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist constituencies have d near 0.0 (beneficiary: the reading vindicates their worldview, creates a formal barrier against remilitarization, and structures Japan's identity around peace commitment). Security establishment has d near 1.0 (target: they cannot legally pursue autonomous military policy, they are constrained by the reading's categorical prohibition, and they must operate under legal fiction). The US-Japan alliance is a structural beneficiary (d near 0.1): the constraint enforces Japan's dependence, which ensures alliance loyalty. Strategic autonomy as an interest has d=1.0 (it is the primary cost bearer). Constitutional courts sit near d=0.5 because they must interpret the text but are caught between the strict reading's categorical mandate and the security establishment's operational needs — they have split the difference by reinterpreting SDF as non-military, which is symmetric cost/benefit (neither fully satisfies either pole). The directionality derivation flows from beneficiary/victim declarations: pacifist movement is a beneficiary (benefits without paying); security establishment and strategic autonomy are victims (pay without benefiting); alliance relationship is a beneficiary (receives structural enforcement of dependence).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is CONTESTED. The strict pacifist reading affirms the founding problem (Japanese militarism must never return) as live and perpetual. However, the security establishment and inherent-right reading contest it: they argue the problem (imperial aggression) was solved by 1947 defeat and constitutional reform; the current security environment is different (Russia, China, North Korea, terrorism, maritime challenges); and the constraint now persists as inertia, not because the founding problem is live. The R5 grid: founding_problem_status=contested, disappearance_verdict=world_rearranges. The mismatch alert fires: if the founding problem is contested and the constraint persists because the world would rearrange without it, the constraint may be a ZOMBIE (mandatrophy: function dead but structure persists). The theater_ratio trajectory (0.28→0.44) supports this: the ratio rises because the functional gap widens (SDF exists de facto; strict reading exists de jure; the gap requires increasing performance/reinterpretation to hold). The TSA alignment is close (theater rising with extractiveness rising with suppression rising) — this is the Piton profile (extractiveness for other reasons, theater because functional rationale is weakening). However, the beneficiary presence (pacifist movement, international peace norms) means this is not a pure piton — someone is collecting from the constraint's maintenance, and someone believes the founding problem is live. The resolution: the constraint is a CONTESTED-FUNCTION structure, not a zombie yet, but at risk of mandatrophy if the founding problem status shifts decisively to 'dead.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determinacy_vs_construction,
    'Is the strict reading''s categorical prohibition a determinate, natural reading of the Japanese text, or is it one interpretive choice among several that the text grammatically permits?',
    'Linguistic and comparative constitutional analysis: does the Japanese text admit grammatical readings that permit some defensive capacity, or is the categorical reading truly obligatory? Examine how other languages and constitutional traditions express prohibition vs. permission.',
    'If the text is truly determinate, the reading is a mountain (natural law of language/grammar). If the text is ambiguous and the strict reading is one choice among several, the reading is a constructed constraint (rope, tangled rope, or snare depending on beneficiary structure) and the ''natural law'' framing is false-summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_determinacy_vs_construction, empirical, 'Whether strict-pacifist interpretation is textually mandatory or one choice among permissible readings.').

omega_variable(
    founding_problem_status_shift,
    'Is the founding problem — preventing Japanese militarism and aggressive war — still live, or has it been solved and the constraint now persists from inertia?',
    'Track security environment evolution, threat assessments from multiple parties (pacifist movement, security establishment, foreign powers, international institutions), and whether the constraint''s persistence is still justified by the original problem or by other reasons (identity, alliance structure, international norm-setting).',
    'If founding problem is live: constraint is justified and mandatrophy has not occurred. If dead: constraint is a zombie (maintenance theater with no functional purpose) and mandatrophy should trigger. If contested: the constraint is in mismatch state and vulnerable to renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_shift, empirical, 'Whether the 1947 founding problem (preventing imperial militarism) remains live or has been superseded.').

omega_variable(
    performance_gap_sustainability,
    'How long can the rising theater_ratio (0.28→0.44) be sustained? At what point does the gap between strict textual reading and operational SDF reality become unsustainable politically?',
    'Monitor constitutional amendment proposals, court decisions, political shifts, and SDF operational expansion. A critical threshold is when the reinterpretive gap becomes so wide that the theater of ''non-military'' forces becomes implausible (SDF acquiring offensive weapons, overseas deployment, etc.).',
    'If gap becomes unsustainable, the constraint will either collapse (shift to inherent_right or collective_defense reading) or be formally amended. This would resolve the mandatrophy ambiguity: either the founding problem is affirmed as live (constraint is renegotiated) or abandoned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_gap_sustainability, empirical, 'Sustainability of the interpretive performance gap between text and operation.').

omega_variable(
    beneficiary_concentration,
    'Who is the primary beneficiary capturing the constraint''s operation? Is it the pacifist movement (political/ideological benefit), the international peace norm system (norm vindication), or the US-Japan alliance (structural dependence enforcement)?',
    'Examine who actively defends the constraint against amendment efforts, who frames its maintenance as a priority, and whose interests would suffer if it were reformed. Track resource flows and political coalitions.',
    'If pacifist movement is the primary beneficiary, the constraint is a Rope or Tangled Rope (coordination with extraction). If the alliance is primary, the constraint is more Snare-like (structural extraction masquerading as moral commitment). If international norms are primary, the constraint is Rope. Beneficiary concentration determines whether mandatrophy triggers — a concentrated beneficiary can maintain it indefinitely; diffuse benefits suggest eventual decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Identification of the primary beneficiary from the constraint''s operation.').

omega_variable(
    reading_foreclosure_mechanism,
    'Does the strict reading logically foreclose the inherent_right and collective_defense readings within a single constitutional framework, or do they coexist as live alternatives held by different parties?',
    'Examine whether any party or court has claimed the readings are logically incompatible (foreclosure) or whether they coexist as competing constitutional interpretations (coexistence). Check whether constitutional amendment would be required to shift readings or whether reinterpretation suffices.',
    'If readings foreclose each other, they are mutually exclusive constraints (only one can be true). If they coexist, they are different structural readings of the same ambiguous kernel, and the constraint landscape includes three live alternatives. This affects whether the engine should compute per-reading classifications or whether the readings should be consolidated under single-framework analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Logical exclusivity vs. coexistence of the strict-pacifist reading with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(arti_tr_t16, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(arti_tr_t24, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(arti_tr_t32, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(arti_tr_t40, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(arti_be_t16, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(arti_be_t24, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(arti_be_t32, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(arti_be_t40, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(arti_su_t16, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(arti_su_t24, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(arti_su_t32, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(arti_su_t40, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.35).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The article_9_war_renunciation kernel decomposes into three constraint stories corresponding to three interpretive readings: strict_pacifist_reading (categorical ban on military forces), inherent_right_reading (sovereign right to minimum defensive capacity), and collective_self_defense_reading (right to collective defense within alliances). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classifications. The readings coexist in Japanese constitutional politics — they are not sequential or nested, but held simultaneously by different parties. This story models the strict-pacifist reading as one constraint structure; the siblings are separate files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
