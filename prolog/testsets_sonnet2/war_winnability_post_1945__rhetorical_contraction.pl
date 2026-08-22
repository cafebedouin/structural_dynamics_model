% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Rhetorical Contraction of War-Winnability Discourse Post-1945
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   After 1945, and especially after thermonuclear weapons made mutual
 *   annihilation credible, public and much elite discourse converged on
 *   treating 'winning' a nuclear war as a taboo, almost unsayable proposition
 *   — a position the deterrence_unthinkable reading takes at face value. But
 *   declassified planning records (SIOP iterations, flexible response
 *   doctrine, countervailing strategy documents, damage-limitation studies)
 *   show that operational planning throughout the Cold War and beyond
 *   continued to specify targeting sequences, escalation control measures,
 *   and war-termination objectives that presuppose some graduated,
 *   survivable, terminable conflict — a warfighting logic inconsistent with
 *   pure unwinnability. This story is about the STRUCTURAL GAP itself: the
 *   rhetorical space contracted (winnability became unsayable in public,
 *   academic, and much policy discourse) while the operational space did not
 *   contract correspondingly (planning retained constrained winnability as a
 *   live category). The gap is not incidental — it is maintained by
 *   classification regimes, career incentives inside the planning
 *   establishment, and a public vocabulary that punishes anyone who raises
 *   operational winnability as reckless or destabilizing.
 *
 * KEY AGENTS:
 *   - strategic_planning_establishment: Primary agenda-setter and beneficiary (institutional/arbitrage) — drafts classified plans, controls classification, speaks only in deterrence register publicly
 *   - nuclear_weapons_laboratories: Secondary beneficiary (institutional/arbitrage) — builds hardware whose rationale requires winnability logic while budget-justifying in deterrence language
 *   - war_college_targeteers: Beneficiary/agenda-setter (organized/identity_locked) — career-bound to operational winnability planning, professionally forbidden from public acknowledgment
 *   - legislative_oversight_committees: Primary payer (powerful/constrained) — nominal overseers disarmed by the taboo's chilling effect on their own inquiries
 *   - general_public: Diffuse payer (powerless/trapped) — bears ultimate risk, holds least accurate model of actual doctrine
 *   - declassification_historians: Analytical observer (analytical) — the vantage from which this gap becomes visible, decades after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Rhetorical Contraction of War-Winnability Discourse Post-1945").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '971007bd-d736-4946-8b38-ca8a70fa1976').
narrative_ontology:cs_kernel_codification('971007bd-d736-4946-8b38-ca8a70fa1976', distributed).
narrative_ontology:cs_authority_grounding('971007bd-d736-4946-8b38-ca8a70fa1976', extraction).
narrative_ontology:cs_interpretation_layer_present('971007bd-d736-4946-8b38-ca8a70fa1976').
narrative_ontology:cs_reading_relation('971007bd-d736-4946-8b38-ca8a70fa1976', war_winnability_post_1945__deterrence_unthinkable, influences).
narrative_ontology:cs_reading_relation('971007bd-d736-4946-8b38-ca8a70fa1976', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('971007bd-d736-4946-8b38-ca8a70fa1976', foundational, discourse_and_doctrine_can_diverge_stably).
narrative_ontology:cs_axiom_status(discourse_and_doctrine_can_diverge_stably, holdable).
narrative_ontology:cs_axiom_grounding('971007bd-d736-4946-8b38-ca8a70fa1976', discourse_and_doctrine_can_diverge_stably, empirically_contingent).
narrative_ontology:cs_axiom('971007bd-d736-4946-8b38-ca8a70fa1976', foundational, accountability_gap_is_the_relevant_object_of_analysis).
narrative_ontology:cs_axiom_status(accountability_gap_is_the_relevant_object_of_analysis, holdable).
narrative_ontology:cs_axiom_grounding('971007bd-d736-4946-8b38-ca8a70fa1976', accountability_gap_is_the_relevant_object_of_analysis, conventional).
narrative_ontology:cs_reference_frame('971007bd-d736-4946-8b38-ca8a70fa1976', early_cold_war_stabilization_compromise).
narrative_ontology:cs_drift_state('971007bd-d736-4946-8b38-ca8a70fa1976', post_cold_war_prompt_global_strike_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('971007bd-d736-4946-8b38-ca8a70fa1976', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, war_college_targeteers).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, general_public).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, allied_governments_outside_planning_loop).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_taboo_norm).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, civilian_control_of_military_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and maintains classified nuclear employment plans (SIOP and successors) that specify counterforce targeting, escalation ladders, and damage-limitation objectives premised on some form of survivable, terminable conflict. Speaks publicly only in the register of pure deterrence and mutual destruction. Controls classification decisions that determine what oversight bodies are permitted to see, and benefits from a public vocabulary that forecloses the very questions that would expose the gap between doctrine and rhetoric.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, beneficiary).

% Design and refine warheads and delivery systems whose technical rationale (accuracy improvements, yield flexibility, penetration aids) only makes sense against a winnability logic — damage limitation, counterforce precision, warfighting reserve. Their budget justifications must speak in deterrence language even as the hardware they build serves warfighting requirements.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories, beneficiary,
    institutional, generational, arbitrage, national).

% Career military and civilian analysts who build and refine targeting packages, escalation scenarios, and war termination studies. Their professional identity and promotion track depend on treating winnability as a live planning problem; publicly they must describe their own work as merely deterrent, never operational, producing a chronic gap between what they do and what they are permitted to say they do.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, war_college_targeteers, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, war_college_targeteers, agenda_setter).

% Nominally responsible for authorizing and reviewing nuclear posture and budgets, but operate with compartmented, delayed, or sanitized briefings. Because the public and even much elite discourse treats winnability as an unthinkable, discredited category, committee members who probe the operational planning risk being cast as reckless warmongers rather than diligent overseers — the taboo itself disarms the oversight function it is supposed to preserve.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees, payer,
    powerful, biographical, constrained, national).

% Consumes a public narrative in which nuclear war is uniformly apocalyptic and unwinnable, which shapes electoral preferences, protest movements, and risk tolerance. Has no access to the classified planning assumptions that actually govern targeting and escalation policy, and bears the ultimate risk if the operational plans the rhetoric hides are ever executed.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, general_public, payer,
    powerless, generational, trapped, national).

% Rely on extended deterrence guarantees and must calibrate their own defense postures, alliance commitments, and public reassurances to the patron state's declared doctrine, but are typically not read into the classified operational planning that determines actual employment thresholds. They pay the strategic cost of miscalibration if the hidden plans diverge from the doctrine they were told to trust.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, allied_governments_outside_planning_loop, payer,
    moderate, biographical, constrained, continental).

% Academic and think-tank analysts who study declared doctrine and treaty texts but are structurally denied access to classified targeting and employment planning. They can critique the public rhetoric of unwinnability but cannot test it against the operational reality, so their advocacy for stability measures is built on an incomplete picture they did not choose and cannot correct.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_epistemic_community, excluded,
    organized, generational, constrained, global).

% Decades after the fact, gain access to formerly classified planning documents (SIOP records, NSC memoranda) and can compare the historical public rhetoric against the contemporaneous operational plans, documenting the gap this constraint describes long after it has shaped multiple generations of policy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, declassification_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining a public taboo against discussing nuclear war as winnable coordinates crisis stability (reduces the chance that adversaries or the public treat a first strike as attractive) and coordinates alliance reassurance (a stated posture of mutual vulnerability is simpler to communicate and rally around than a graduated warfighting doctrine).
% TRANSFER_FUNCTION: Moves accountability and informed consent away from legislatures, allied publics, and the general citizenry and toward a closed community of planners and technical specialists; what is transferred is not money but epistemic control over the actual scope of national risk exposure, concentrated in agencies that can operate free of the scrutiny the taboo would otherwise invite.
% ABSENT_VOICES: The arms control epistemic community and much of the legislative oversight apparatus would object that operational planning premised on limited nuclear war undermines the very stability rationale used to justify secrecy, but they lack the clearances to make that argument with operational specificity — they are structurally excluded from the room where the actual plans are debated.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo dissolved and winnability planning became openly discussable, public debate over nuclear posture, arms control negotiating positions, extended deterrence commitments, and defense budgets would shift substantially — allies would recalibrate trust in reassurance guarantees, oversight committees would demand different briefing structures, and the planning establishment would lose the insulation the taboo currently provides. The operational plans themselves would not necessarily change, but who gets to contest them would.
% FOUNDING_PROBLEM: Early Cold War strategists needed a way to signal restraint and stabilize expectations after Hiroshima and Nagasaki demonstrated the human costs of nuclear use, while military planners still needed workable options short of civilizational suicide in case deterrence failed — the taboo solved the signaling and legitimacy problem without requiring planners to give up operational flexibility.
% FOUNDING_PROBLEM_CORROBORATION: Declassified planning documents and historians of the SIOP process (outside the benefiting planning community) attest that operational winnability planning persisted continuously from the 1950s through the post-Cold War era even as public doctrine shifted toward pure deterrence language; some retired strategists and a minority of political scientists corroborate that the founding stabilization problem is still partially live, while arms control scholars argue the taboo now functions primarily to shield planning from accountability rather than to preserve stability.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by 2025) reflects the accumulating gap between what oversight bodies and the public believe governs nuclear posture and what actually governs it — a gap that widened as flexible response, countervailing strategy, and post-Cold War prompt global strike doctrines layered increasingly explicit warfighting planning underneath an unchanged public taboo. Theater ratio (0.61) is high because a large and growing share of public deterrence rhetoric functions performatively — reassuring allies and publics — while the substantive planning work happens elsewhere; the theater is not incidental noise, it is the mechanism that makes the extraction possible. Suppression (0.72) captures the active classification and professional-incentive machinery required to keep the operational layer from surfacing into the discourse layer; this is not the passive suppression of a settled taboo but the maintained effort of keeping two contradictory postures stable simultaneously. Accessibility collapse (0.58) is moderate: the rhetorical taboo has become nearly total in polite public discourse, but recurring declassification cycles and occasional insider leaks (Ellsberg, the Pentagon Papers-adjacent nuclear planning disclosures, post-Cold War archival releases) mean the alternative framing periodically resurfaces rather than vanishing completely. Resistance (0.48) is moderate: arms control scholars, some legislators, and declassification historians persistently push against the taboo's epistemic closure, but without clearances they cannot fully close the gap they identify.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners, weapons labs, and targeteers sit near the beneficiary end of directionality: the taboo grants them operational latitude (arbitrage-grade exit from public accountability) without correspondingly constraining what they may plan, and their institutional and career structures actively reward maintaining the gap. Oversight committees, the general public, and allied governments sit near the target end: they bear the transferred cost (loss of informed consent, miscalibrated trust in declared doctrine) without the exit options planners enjoy — a legislator cannot simply route around classification, and a citizen cannot demand documents that do not officially exist in discussable form. The arms control epistemic community is treated as excluded rather than as a straightforward payer, because their harm is specifically the denial of the voice/critique function rather than a resource transfer — they are the excluded seat whose absence from the planning room is why the taboo persists uncontested for so long.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stabilizing expectations and signaling restraint after 1945 while preserving some contingency planning against deterrence failure — retains partial vitality (contested, not dead): crisis stability arguments for avoiding public winnability talk are not pure cover stories. But the classification below the founding problem has drifted: what began as a stabilization measure has become, over eight decades, an accountability-avoidance structure whose main beneficiaries are the planners it insulates rather than the publics it was meant to reassure. Classifying this as tangled_rope rather than pure snare or pure rope honors both halves: there IS a genuine coordination function (crisis stability, alliance reassurance) that a pure-snare reading would miss, and there IS asymmetric extraction (oversight and public accountability transferred to insulated planners) that a pure-rope reading would launder away. The tangled_rope frame prevents both mislabelings: it neither treats the taboo as simple public-interest coordination (ignoring the accountability cost) nor treats it as naked extraction with no coordination logic at all (ignoring the genuine crisis-stability function the founding problem still partially serves).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s account of the winnability kernel diverge from the deterrence_unthinkable and countervailing_thinkable siblings — is the disagreement about the FACTS of what was planned, or about which layer (rhetoric vs. operations) is the authoritative referent for ''is war winnable''?',
    'Comparative analysis of declassified planning documents against contemporaneous public/elite rhetoric across multiple administrations; if the documented planning consistently specifies graduated, terminable conflict scenarios while public doctrine consistently denies this is possible, the rhetorical_contraction reading is empirically distinguishable from both siblings rather than a mere terminological variant.',
    'If the gap between rhetoric and operations is confirmed as structurally persistent and not just an artifact of selective declassification, this reading is validated as capturing a real third structure rather than being reducible to either sibling; if declassified materials instead show planning converging toward genuine unwinnability over time, this reading''s ε should decay toward the deterrence_unthinkable sibling''s low-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, empirical, 'Whether the rhetoric/operations gap this reading describes is a persistent structural feature or a historically contingent, closing one.').

omega_variable(
    planner_beneficiary_versus_stability_beneficiary,
    'Are strategic planners genuinely the primary beneficiaries of the taboo (accountability-avoidance framing), or is the beneficiary better described as ''crisis stability'' itself, an impersonal collective good, with planners merely its administrators?',
    'Examine whether planners'' institutional and career incentives track accountability-avoidance specifically (e.g., resistance to declassification even when declassification would not compromise operational security) versus tracking genuine stability concerns (e.g., support for declassification once operational relevance has expired).',
    'If planner resistance to disclosure tracks operational security concerns rather than accountability avoidance, the beneficiary structure shifts toward a genuine (if still imperfect) coordination story, pulling this constraint toward rope; if resistance persists well past the point of operational relevance, the tangled_rope/beneficiary framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planner_beneficiary_versus_stability_beneficiary, conceptual, 'Whether the named beneficiary (planners) is capturing rents beyond what stability coordination requires.').

omega_variable(
    natural_versus_constructed_taboo,
    'Is the unsayability of winnability a natural consequence of the horror of nuclear weapons (an emergent moral/psychological limit) or a constructed rhetorical regime actively maintained by institutional actors for strategic purposes?',
    'Track whether the taboo''s boundaries shift in ways that track institutional interest (e.g., loosening around administrations that want more room for warfighting rhetoric, tightening around arms control negotiations) versus tracking purely the horror/salience of nuclear weapons themselves (which would predict a stable or monotonically strengthening taboo independent of institutional interest).',
    'If taboo boundaries move with institutional interest, the taboo is substantially constructed and the beneficiary/victim structure authored here is well-grounded; if boundaries move independent of institutional interest, an emergent-mountain-like component exists alongside the constructed extraction, complicating the tangled_rope claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_versus_constructed_taboo, conceptual, 'Whether the discourse-level taboo is emergent moral consensus or actively engineered and maintained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.38).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1975, 0.47).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1990, 0.53).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2005, 0.58).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.61).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(war__su_t1975, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the war_winnability_post_1945 kernel, each capturing a structurally distinct claim about the same contested proposition ('is great-power nuclear war winnable?'). deterrence_unthinkable claims categorical unwinnability with negligible extraction (near-mountain). countervailing_thinkable claims winnability remains substantively achievable through counterforce targeting, with its own beneficiary/victim structure centered on the targeting establishment versus arms-control advocates. rhetorical_contraction (this story) claims a dual-layer split: unthinkability at the discourse layer, constrained-winnability at the operational layer, with extraction running through the accountability gap between the two layers. The three stories share no ε value by design — per the ε-invariance principle, decomposition into three files is required precisely because a single observable ('is winnability sayable/plannable?') yields three different answers depending on which layer is measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
