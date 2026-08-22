% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense: Expansive Preventive Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates the expansive preventive reading of the Article
 *   51 self-defense kernel: the claim that self-defense lawfully extends to
 *   preemptive or preventive force against non-state actors or emerging
 *   threats whenever the acting state itself determines necessity is
 *   demonstrated. Two sibling readings of the same kernel text — a narrow
 *   armed-attack-only reading and an unable/unwilling hybrid doctrine — are
 *   separate constraints with their own ε and stakeholder structures, not
 *   alternative measurements of this one. This reading is authored on its own
 *   terms: self-certified necessity, low external constraint, and a
 *   beneficiary set concentrated among militarily capable states and their
 *   defense sectors, with costs falling on target-region populations, weaker
 *   host states, and the Security Council's institutional authority.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: primary agenda-setter and beneficiary — self-certifies necessity, bears no binding external check
 *   - domestic_defense_sectors: secondary beneficiary — procurement and budget gains from standing preventive capability
 *   - executive_war_powers_offices: agenda-setter — expanded unilateral discretion over force determinations
 *   - target_region_civilian_populations: primary victim — absorbs strikes and displacement with no forum for contest
 *   - un_security_council_authority: institutional victim — Chapter VII gatekeeping function eroded by unrebutted unilateral practice
 *   - weaker_states_bordering_threat_actors: victim — sovereignty violated without reciprocal capacity
 *   - international_law_scholars: analytical observer — tracks whether state practice is congealing into custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.71).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense: Expansive Preventive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '31dfde2c-b094-48eb-9fa9-d8e2fb0d721a').
narrative_ontology:cs_kernel_codification('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', fixed_text).
narrative_ontology:cs_authority_grounding('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', distributed).
narrative_ontology:cs_reading_relation('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', foundational, state_self_judged_necessity_sufficient).
narrative_ontology:cs_axiom_status(state_self_judged_necessity_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', state_self_judged_necessity_sufficient, conventional).
narrative_ontology:cs_axiom('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', foundational, anticipatory_defense_extends_to_non_state_actors).
narrative_ontology:cs_axiom_status(anticipatory_defense_extends_to_non_state_actors, holdable).
narrative_ontology:cs_axiom_grounding('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', anticipatory_defense_extends_to_non_state_actors, instrumental).
narrative_ontology:cs_reference_frame('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', un_charter_collective_security_framework).
narrative_ontology:cs_drift_state('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', post_9_11_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('31dfde2c-b094-48eb-9fa9-d8e2fb0d721a', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, domestic_defense_sectors).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, executive_war_powers_offices).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, weaker_states_bordering_threat_actors).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, state_sovereignty_of_self_preservation).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, anticipatory_self_defense_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 51 to justify strikes against non-state actors or emerging threats without waiting for an armed attack to materialize, self-certifying the necessity and imminence determinations. They control the intelligence assessments underlying the claim, face no binding external check on the invocation, and can act unilaterally or through ad hoc coalitions when Security Council authorization is unlikely.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Supply the platforms, munitions, and surveillance infrastructure that preventive operations consume at a faster rate than reactive defense postures. Budget authorizations and procurement cycles benefit directly from a legal doctrine that normalizes standing preventive capability rather than crisis-triggered response.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, domestic_defense_sectors, beneficiary,
    organized, biographical, arbitrage, national).

% Hold the authority to determine when a threat is sufficiently 'emerging' to warrant preventive force, largely insulated from legislative or judicial review because the necessity determination is framed as a national security judgment. The doctrine expands executive discretion relative to any framework requiring multilateral or after-the-fact accountability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, executive_war_powers_offices, agenda_setter,
    institutional, biographical, arbitrage, national).

% Live in the areas where preventive strikes occur, absorbing the casualties, displacement, and infrastructure damage from operations premised on threats that have not yet crystallized into an attack. They have no standing to contest the necessity determination and no forum in which the acting state's self-judgment can be tested before harm occurs.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations, payer,
    powerless, immediate, trapped, regional).

% Its Chapter VII monopoly on authorizing force outside clear self-defense is structurally bypassed whenever a state's preventive invocation stands unchallenged; each unrebutted invocation further normalizes unilateral preventive force as lawful custom, eroding the institution's gatekeeping function without any formal amendment to the Charter.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_authority, payer,
    institutional, generational, constrained, global).

% Host or border the non-state actors cited as emerging threats and bear strikes on their territory without having attacked anyone themselves. Their objections to sovereignty violation carry little practical weight against a militarily capable state's self-certified necessity claim, and they lack reciprocal capacity to invoke the same doctrine against more powerful states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, weaker_states_bordering_threat_actors, payer,
    moderate, biographical, constrained, regional).

% Document and debate whether repeated state practice under this reading is congealing into new customary international law or remains a persistent violation tolerated only because enforcement mechanisms are weak. Their assessments shape scholarly and diplomatic discourse but do not bind state behavior.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a state to act against a materializing threat from a non-state actor before that threat produces mass casualties, addressing the genuine problem that traditional armed-attack thresholds can be too late against actors who strike without warning and without a state's formal declaration.
% TRANSFER_FUNCTION: Moves the burden of proof for the use of force from the acting state (which would otherwise need to demonstrate an actual or imminent attack under stricter tests) onto the target region and the international community, who must absorb the consequences of a self-certified necessity judgment they cannot contest in advance.
% ABSENT_VOICES: The civilian populations in strike zones and the weaker host states have no forum comparable to the acting state's own intelligence and legal apparatus; the Security Council's collective judgment is structurally sidelined precisely where its authorization function is most needed. Their objections surface after the fact, in UN debates and human rights reporting, well after any harm has occurred.
% DISAPPEARANCE_RATIONALE: If this expansive reading were repudiated and states reverted strictly to the narrow armed-attack standard, militarily capable states would lose their primary legal cover for unilateral preventive strikes against non-state actors, defense procurement tied to standing preventive capability would face renewed political scrutiny, and the Security Council's authorization role would regain practical significance as the default legal pathway.
% FOUNDING_PROBLEM: The classical Article 51 framework, drafted for interstate warfare, appeared to leave states unable to lawfully respond to fast-moving, non-state, or unconventional threats until after an attack had already occurred, particularly after mass-casualty terrorism demonstrated that waiting for an 'armed attack' could mean waiting for catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and allied legal scholars attest the problem remains live, citing continued terrorist and proxy threats. Independent international law scholars, UN special rapporteurs, and target-state governments outside the beneficiary set argue the doctrine has been stretched well past any genuine imminence requirement into a general license for preventive war, corroborated by documented strikes against speculative or long-horizon threats rather than materializing attacks.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 at interval end) because the doctrine transfers the practical cost of the necessity determination onto parties who cannot contest it beforehand, and that transfer has widened over the interval as more states have invoked preventive rationales with less specificity about imminence. Suppression is substantial (0.62) but lower than extraction because the doctrine operates more through self-certification and asymmetric power than through direct coercive silencing of objectors — though the erosion of Security Council review functions as a structural suppression of the collective check. Theater ratio (0.40) reflects that a meaningful share of legal justification exercises (invoking 'imminence' language, citing intelligence assessments) function as post hoc legitimation rather than genuine constraint on the decision already made. Accessibility collapse (0.58) is moderate: the narrow reading and the unable/unwilling doctrine remain live legal alternatives argued by other states and scholars, so alternatives have not fully collapsed, but repeated unrebutted practice narrows the practical space for contesting the expansive reading. Resistance (0.60) is substantial and comes from scholarly criticism, UN special rapporteur findings, and diplomatic protest, though it has proven insufficient to reverse the trend given the acting states' military and institutional leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and their defense sectors sit at the beneficiary end: they gain expanded legal cover, budget justification, and reduced external constraint, with arbitrage-grade exit (they can act unilaterally when multilateral approval is unavailable). Target-region civilian populations sit at the full-target end: trapped, immediate horizon, no participation in the necessity determination that produces the harm they bear. The UN Security Council is a distinctive institutional victim — not physically harmed but structurally displaced as the gatekeeping authority the Charter assigns it, which is why it is authored as institutional-power/constrained-exit rather than powerless: it retains formal authority it cannot practically exercise against a determined unilateral actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inability to respond lawfully to fast-moving non-state threats before catastrophe — was genuinely live in specific historical moments (post-9/11 threat environment). Whether it remains the operative justification for each subsequent invocation, versus having become a durable license reused for threats far more speculative and long-horizon than the founding cases, is exactly the contested status this story authors as 'contested' rather than resolving it. The classification as tangled_rope (not pure snare) reflects that a genuine coordination problem — legitimate anticipatory defense against genuinely imminent non-state threats — is real and would need addressing under any reading; what the metrics register is that this reading's implementation has drifted from that genuine core toward self-serving extraction, without erasing the coordination function entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_self_judgment_verifiability,
    'Can a state''s self-certified necessity determination for preventive force ever be verified by an external party before the force is used, or is ex ante self-judgment structurally unfalsifiable?',
    'Comparative study of instances where preventive strike intelligence was later disclosed or challenged in international fora, assessing whether independent verification mechanisms (UN investigation, allied intelligence sharing, post hoc judicial review) meaningfully constrained the initial determination.',
    'If self-judgment is structurally unfalsifiable in practice, the expansive reading functions as a near-unconstrained license regardless of its formal ''necessity'' requirement, supporting the high extraction and suppression scores; if meaningful external verification does occur in practice, the doctrine is closer to a genuinely constrained coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_verifiability, empirical, 'Whether self-certified necessity is verifiable in practice or only in form.').

omega_variable(
    customary_law_congealment,
    'Has repeated, largely unrebutted state practice under this expansive reading crystallized into new customary international law, or does it remain a persistent violation tolerated due to weak enforcement rather than accepted as lawful?',
    'Systematic review of opinio juris — formal state statements, UN General Assembly and Security Council debate records, ICJ commentary — to determine whether states invoking or tolerating the doctrine do so believing it lawful, versus merely lacking power to object.',
    'If congealed into custom, the expansive reading''s classification shifts toward a more durable, less contestable coordination structure; if it remains tolerated violation, the tangled_rope classification''s extraction component is better understood as unlawful practice rather than settled doctrine, strengthening the case for the narrow reading''s continued validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_congealment, conceptual, 'Whether the reading is congealing into binding custom or persists only as tolerated violation.').

omega_variable(
    beneficiary_capture_of_interpretation,
    'Given that the states most capable of exercising preventive force are also the states with the greatest influence over international legal scholarship, diplomatic fora, and Security Council permanent membership, to what extent is the expansive reading''s growing acceptance a function of genuine legal evolution versus interpretive capture by its own beneficiaries?',
    'Track voting patterns, veto usage, and scholarly output by institutional affiliation and funding source when assessing which states and institutions most actively advance the expansive reading versus the narrow or hybrid readings.',
    'High capture would support treating the doctrine''s apparent international acceptance as manufactured consensus rather than principled convergence, reinforcing the tangled_rope (not rope) classification and the high suppression score attributed to Security Council authority erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_interpretation, conceptual, 'Whether apparent doctrinal acceptance reflects genuine consensus or beneficiary-driven interpretive capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__expansive_preventive_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__expansive_preventive_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(arti_tr_t24, article_51_self_defense__expansive_preventive_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(arti_tr_t32, article_51_self_defense__expansive_preventive_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(arti_tr_t40, article_51_self_defense__expansive_preventive_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(arti_be_t24, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(arti_be_t32, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(arti_be_t40, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(arti_su_t24, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(arti_su_t32, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(arti_su_t40, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.1).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article_51_self_defense kernel. narrow_armed_attack_reading authors a much lower ε (tighter constraint on unilateral force, Security Council authority preserved). unable_unwilling_doctrine_reading authors an intermediate ε (hybrid trigger requiring host-state unwillingness/inability as a threshold before non-state-actor-directed force is lawful). This file's high ε (0.71) reflects the specific structural claim that necessity is self-judged with minimal external constraint — a materially different claim from either sibling, not a different measurement of the same claim. All three should be read together as a decomposed kernel family, linked bidirectionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
