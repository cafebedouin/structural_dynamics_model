% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Winnability: Limited Victory Through Counterforce (post-1945)
 *   domain: strategic/military-political
 *
 * SUMMARY:
 *   After 1945, nuclear weapons imposed a real constraint on great-power war;
 *   this story instantiates the countervailing_thinkable reading of the
 *   kernel war_winnability_post_1945, on which that constraint is real but
 *   partial: limited victory remains reachable through counterforce
 *   targeting, and the planning state that maintains that option persists
 *   across administrations and doctrine cycles. The ε referent is the
 *   standing arrangement under contest — the post-1945 order of continued
 *   counterforce planning, force sizing, and modernization under the nuclear
 *   constraint — assessed by this reading's own lights: the coordination
 *   function (channeling great-power rivalry into bounded forms; no direct
 *   nuclear-power total war since 1945) is genuine, and the extraction
 *   (mission-continuity rents to the planning-industrial complex, erosion of
 *   arms control regimes, unconsented risk to populations under the target
 *   sets) is real but rides on the coordination rather than replacing it. The
 *   sibling readings are separate constraints with their own ε:
 *   deterrence_unthinkable (which withholds the coordination credit and reads
 *   the apparatus as incoherent waste and instability) and
 *   rhetorical_contraction (which locates the constraint in the gap between
 *   public taboo and operational planning). KEY AGENTS (by structural
 *   relationship): counterforce_planning_establishment — agenda-setter and
 *   beneficiary (institutional/identity_locked); military_industrial_complex
 *   — primary beneficiary and receipt seat (institutional/constrained);
 *   extended_deterrence_allies — secondary beneficiary
 *   (organized/constrained); arms_control_regimes — primary payer
 *   (institutional/constrained); npt_non_nuclear_states — excluded
 *   (organized/constrained); populations_under_escalation_paths — payer
 *   (powerless/trapped); strategic_studies_community — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.6).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Winnability: Limited Victory Through Counterforce (post-1945)").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic/military-political").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'b7154f61-b13a-4066-8044-e9defe711016').
narrative_ontology:cs_kernel_codification('b7154f61-b13a-4066-8044-e9defe711016', distributed).
narrative_ontology:cs_authority_grounding('b7154f61-b13a-4066-8044-e9defe711016', practice).
narrative_ontology:cs_interpretation_layer_present('b7154f61-b13a-4066-8044-e9defe711016').
narrative_ontology:cs_reading_relation('b7154f61-b13a-4066-8044-e9defe711016', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('b7154f61-b13a-4066-8044-e9defe711016', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('b7154f61-b13a-4066-8044-e9defe711016', foundational, limited_victory_remains_achievable).
narrative_ontology:cs_axiom_status(limited_victory_remains_achievable, holdable).
narrative_ontology:cs_axiom_grounding('b7154f61-b13a-4066-8044-e9defe711016', limited_victory_remains_achievable, empirically_contingent).
narrative_ontology:cs_axiom('b7154f61-b13a-4066-8044-e9defe711016', secondary, escalation_control_suffices_for_limitation).
narrative_ontology:cs_axiom_status(escalation_control_suffices_for_limitation, holdable).
narrative_ontology:cs_axiom_grounding('b7154f61-b13a-4066-8044-e9defe711016', escalation_control_suffices_for_limitation, instrumental).
narrative_ontology:cs_reference_frame('b7154f61-b13a-4066-8044-e9defe711016', limited_war_continuity_framework).
narrative_ontology:cs_drift_state('b7154f61-b13a-4066-8044-e9defe711016', contemporary_post_new_start_expiry, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7154f61-b13a-4066-8044-e9defe711016', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, npt_non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, populations_under_escalation_paths).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staffs the strategic commands and defense secretariats that write targeting policy, run the wargames, and size the force. It maintains the option sets that keep limited victory inside the planning envelope, and its institutional identity is fused with that mission: an establishment that stopped planning for victory would have no reason to exist at anything like its current scale. Exit would mean dismantling career structures, classification regimes, and organizational purpose built up over decades.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, counterforce_planning_establishment, beneficiary).

% Builds and sustains the delivery systems, warheads, command-and-control networks, and the industrial base that counterforce options require. Winnable-war planning sustains mission continuity and modernization budgets across administrations of both parties; the specialized workforce, facilities, and design knowledge have no equivalent civilian market, so the firms can pivot to conventional programs only at substantial cost.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, constrained, global).

% Treaty-allied states that shelter under a protector's nuclear umbrella without fielding their own arsenals. The umbrella's credibility is argued partly through usable, limited options rather than pure massive retaliation; these states gain security cover and pay basing, integration, and entanglement costs. Exiting would require rebuilding independent deterrence or accepting exposure, so they stay and lobby for the options that keep the umbrella credible.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies, beneficiary,
    organized, generational, constrained, continental).

% The treaty frameworks, verification machinery, and negotiating channels of the SALT/START lineage, the INF regime, and New START. Each generation of counterforce capability and each planning cycle that treats limited nuclear exchange as workable gives arsenal states reasons to refuse deeper cuts and to resist verifying the very systems counterforce requires. The regime's baselines erode with every modernization round, and when a treaty collapses (INF 2019, New START expiry 2026) there is no successor framework to retreat into.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    institutional, generational, constrained, global).

% Non-weapon states party to the NPT bargain, who forswore arsenals in exchange for disarmament progress and security assurances. They would object that planning for nuclear victory entrenches exactly the arsenals Article VI obliges the weapon states to reduce; they voice this in review conferences but are structurally outside the targeting, force-sizing, and modernization decisions the constraint actually turns on.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, npt_non_nuclear_states, excluded,
    organized, generational, constrained, global).

% Civilian populations living near counterforce target sets, downwind of projected fallout corridors, and inside the escalation pathways that any limited-exchange plan must traverse. They bear the tail risk that limitation fails and the exchange goes general, have no seat in the planning, and cannot exit the geography the target sets are drawn on.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, populations_under_escalation_paths, payer,
    powerless, generational, trapped, global).

% Analysts inside and outside academies who map the winnability debate, build escalation models, publish net assessments, and adjudicate nothing. They can see the full structure at once: the doctrine, the capability programs, the treaty erosion, and the opposition, without collecting from or paying into any of it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels great-power conflict into limited forms: the nuclear constraint raises the cost of total war to prohibitive levels, while the maintained counterforce option set keeps sub-total conflict thinkable and bounded, so that interstate rivalry persists without escalating to civilizational exchange.
% TRANSFER_FUNCTION: Moves budget share, industrial capacity, and technical talent from national treasuries and taxpayers into the strategic weapons enterprise; moves escalation risk onto non-consenting populations and onto the treaty frameworks; moves strategic autonomy from arms-control-bound constraints into the hands of the planning establishment.
% ABSENT_VOICES: The NPT non-nuclear states and the disarmament community would object that winnable-war planning violates the Article VI bargain they accepted; they are present in review conferences but absent from the targeting and modernization rooms where the constraint is actually set. Populations under the target sets are absent entirely.
% DISAPPEARANCE_RATIONALE: If the winnability window and the planning apparatus that maintains it vanished overnight, every nuclear state's posture would rearrange: forces would be sized for deterrence-only rather than war-fighting, arms control would deepen in the absence of counterforce demand, the strategic industrial base would contract or convert, and alliance bargains built on usable-option credibility would need renegotiation.
% FOUNDING_PROBLEM: How to preserve war as an instrument of policy — the state's capacity to fight and win — once nuclear weapons made unlimited great-power war suicidal. The countervailing answer: keep victory reachable in limited form through counterforce targeting and escalation management.
% FOUNDING_PROBLEM_CORROBORATION: Declassified planning records (NSDM-242, PD-59 lineage) and the strategic-studies literature attest the founding problem is live; corroboration comes substantially from outside the benefiting parties — academic escalation modelers who dispute the premise, arms-control negotiators on both delegations who describe the pressure winnable-war planning places on treaty talks, and former officials who crossed from planning into criticism.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high and rising across the interval (0.45 at t0 to 0.65 at t66, time points = years since 1960) because each counterforce generation converts doctrine into budget lines and each modernization round erodes the treaty baselines that would cap the enterprise — but the series is not monotonic: the post-Cold War drawdown (t33) genuinely reduced extraction when force-sizing rationales briefly shifted toward deterrence-only, which is evidence the extraction tracks the winnability mission rather than being a constant of the arsenal. Suppression (0.60 at interval end) reflects the coercive machinery the constraint requires: arsenals, hardened command-and-control, and doctrine policing inside the planning institutions that marginalizes minimal-deterrence and no-first-use alternatives. Suppression_requirement is tracked as a series because enforcement capacity is a central dynamic of this story — built up through the 1960s-80s (peak 0.65 at t22), decayed in the 1990s drawdown (0.50 at t33), and rebuilt with the modernization programs of the 2000s-2020s (0.60 at t66). Theater_ratio ends at 0.48: wargaming and net assessment retain real signaling and planning function, but a growing share of modernization justification rests on limited-victory scenarios that escalation modeling treats as implausible, and the share rises as peer arsenals harden and disperse. Accessibility_collapse is 0.5: deep-cuts, minimal-deterrence, and abolition alternatives persist as live positions in public discourse but are institutionally foreclosed inside the planning establishment. Resistance is 0.55: the freeze movement, NPT Article VI pressure, and the arms-control community mount sustained opposition without displacing the planning core. All series share one grid (t = 0, 11, 22, 33, 44, 55, 66).
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute different types from the same structure. From the counterforce_planning_establishment seat, the constraint is doctrine and mission: planning for limited victory is the professional practice that gives the institution its purpose, and its identity lock makes the winnability frame constitutive rather than chosen. From the military_industrial_complex seat, it is a revenue stream with mission continuity. From the arms_control_regimes seat, the same structure is baseline erosion: every counterforce program is a reason the next round of cuts fails. From the npt_non_nuclear_states seat, it is a violated bargain; from populations_under_escalation_paths, it is unconsented tail risk. The strategic_studies_community seat sees all of these at once. The engine computes this divergence from power, exit options, and role; the divergence is the finding, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (planning establishment, military-industrial complex, extended-deterrence allies) place those seats near the beneficiary end of d — the constraint subsidizes them with mission, budget, and security cover. Victim declarations (arms control regimes, non-nuclear NPT parties, populations under escalation paths) place those seats near the target end — they pay in eroded treaty baselines, devalued security assurances, and imposed risk. Exit structure modulates within each side: the planning establishment's identity lock stabilizes its beneficiary position (it cannot leave even if it wanted to); the military-industrial complex is constrained rather than locked (conversion is costly but not unthinkable); arms control regimes are constrained with no successor framework, which pushes them toward the trapped end of the target side; populations are fully trapped. Extended-deterrence allies sit low-mid: genuine beneficiaries who also pay integration and entanglement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling victory-seeking with the nuclear revolution — is live, not dead: states still demand options below total war, and the arrangement has not outlived its function, so mandatrophy is not resolved and the classification should not drift toward piton on this record. The tangled_rope classification does distinct work against both mislabelings: a pure-rope reading would miss the asymmetric extraction (the receipt seat, the treaty erosion, the unconsented risk); a pure-snare reading would miss the genuine coordination (the absence of great-power total war since 1945 is partly attributable to the bounded-conflict structure this arrangement maintains, and the post-Cold War extraction dip shows the extraction is contingent on the mission, not intrinsic to the arsenal). The R5 mismatch check runs clean here: founding_problem_status=live with disappearance_verdict=world_rearranges is the consistent cell — no zombie flag — because the world genuinely still rearranges around this constraint and the problem it was built for still exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_countervailing,
    'This constraint instantiates the countervailing_thinkable reading of the kernel war_winnability_post_1945; what changes structurally if a sibling reading is adopted instead, and where exactly is the disagreement located?',
    'Compare against the sibling files: war_winnability_post_1945__deterrence_unthinkable authors the same standing arrangement with no coordination credit (trending the apparatus toward snare on waste-and-instability grounds); war_winnability_post_1945__rhetorical_contraction locates the constraint in the discourse-strategy gap and weights theater_ratio as load-bearing. The disagreement is located at a single structural element: whether maintained counterforce capability constitutes a genuine limited-victory option or a planning artifact.',
    'Reading choice changes the classification of the same referent: this reading yields tangled_rope (genuine coordination plus asymmetric extraction); deterrence_unthinkable yields snare-flavored readings of the same budget flows; rhetorical_contraction yields piton-adjacent readings if the operational content is judged mostly performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_countervailing, conceptual, 'Committer structure: one reading of the war_winnability_post_1945 kernel, with sibling readings as separate constraints.').

omega_variable(
    counterforce_limited_victory_genuineness,
    'Does precision counterforce capability against a peer arsenal actually create a controllable limited-exchange option, or do escalation dynamics (entangled command-and-control, use-it-or-lose-it pressure, fog of a nuclear battlefield) make any nuclear exchange uncontrollable regardless of targeting?',
    'Escalation modeling, declassified wargame results (e.g., Proud Prophet 1983, which escalated to general exchange under countervailing assumptions), and incident analysis of nuclear-adjacent crises; convergence of independent models on controllability bounds.',
    'If the limited-victory option is genuine, the coordination function is real and the tangled_rope classification holds; if it is an artifact, the coordination story is cover, the theater_ratio is understated, and the constraint trends toward snare with the planning establishment as capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_limited_victory_genuineness, empirical, 'Whether counterforce targeting genuinely bounds escalation or merely theatricalizes it.').

omega_variable(
    arms_control_erosion_causality,
    'Is the erosion of arms control regimes caused by winnable-war planning (the declared victim relation), or by great-power political deterioration that would have collapsed the treaties regardless of counterforce doctrine?',
    'Comparative timeline analysis aligning counterforce program milestones with treaty collapse points across differently-configured dyads; negotiator testimony from both sides on which demands actually killed which rounds.',
    'Determines whether arms_control_regimes is a genuine victim of THIS constraint (supporting high d and high effective extraction) or a co-casualty of a separate great-power-decay constraint (weakening the victim declaration and reattributing the extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_control_erosion_causality, empirical, 'Causal attribution of arms control collapse between winnable-war planning and general geopolitical deterioration.').

omega_variable(
    extraction_share_of_enterprise,
    'What share of the strategic enterprise''s budget and staffing depends specifically on winnability remaining thinkable, as opposed to what a deterrence-only posture would require?',
    'Cross-administration budget analysis comparing force-sizing rationales and program lines under countervailing doctrine versus minimal-deterrence and no-first-use proposals (e.g., costed minimal-deterrence studies).',
    'Sizes the military_industrial_complex seat''s capture: a large winnability-dependent share confirms the receipt surface and the tangled_rope extraction; a small share would demote the beneficiary to incidental and push the classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_share_of_enterprise, empirical, 'How much of the strategic enterprise''s extraction is attributable to the winnability mission specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ww45_countervailing_tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ww45_countervailing_tr_t11, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 11, 0.38).
narrative_ontology:measurement(ww45_countervailing_tr_t22, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 22, 0.45).
narrative_ontology:measurement(ww45_countervailing_tr_t33, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 33, 0.38).
narrative_ontology:measurement(ww45_countervailing_tr_t44, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 44, 0.42).
narrative_ontology:measurement(ww45_countervailing_tr_t55, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 55, 0.45).
narrative_ontology:measurement(ww45_countervailing_tr_t66, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 66, 0.48).

% Extraction over time
narrative_ontology:measurement(ww45_countervailing_be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ww45_countervailing_be_t11, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 11, 0.5).
narrative_ontology:measurement(ww45_countervailing_be_t22, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(ww45_countervailing_be_t33, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 33, 0.48).
narrative_ontology:measurement(ww45_countervailing_be_t44, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 44, 0.55).
narrative_ontology:measurement(ww45_countervailing_be_t55, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 55, 0.6).
narrative_ontology:measurement(ww45_countervailing_be_t66, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 66, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ww45_countervailing_su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ww45_countervailing_su_t11, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 11, 0.6).
narrative_ontology:measurement(ww45_countervailing_su_t22, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 22, 0.65).
narrative_ontology:measurement(ww45_countervailing_su_t33, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 33, 0.5).
narrative_ontology:measurement(ww45_countervailing_su_t44, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 44, 0.55).
narrative_ontology:measurement(ww45_countervailing_su_t55, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 55, 0.6).
narrative_ontology:measurement(ww45_countervailing_su_t66, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 66, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'nuclear war winnability after 1945' decomposes into three structurally distinct readings of one kernel, each a separate story with its own ε, beneficiary structure, and classification. This file is the countervailing_thinkable reading (winnability reachable, planning coherent); war_winnability_post_1945__deterrence_unthinkable denies the coordination credit entirely; war_winnability_post_1945__rhetorical_contraction splits the constraint along the discourse/operations boundary. All three are linked via affects_constraints; ε divergence across the family is the measurement, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
