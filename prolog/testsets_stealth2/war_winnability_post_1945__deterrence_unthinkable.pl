% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Post-1945 Great-Power Total War Unwinnability (Deterrence-Unthinkable Reading)
 *   domain: strategic studies/international relations
 *
 * SUMMARY:
 *   After 1945 — decisively after thermonuclear weapons and survivable
 *   second-strike forces — general war between great powers ceased to be an
 *   event that could be fought to a decision, and strategic planning
 *   reorganized around preventing war rather than winning it. This story
 *   instantiates the deterrence_unthinkable reading of the
 *   war_winnability_post_1945 kernel: the contraction is OPERATIONAL, not
 *   merely rhetorical — winnability exited the reachable space, and planning
 *   for victory lost coherence as a professional activity. The epsilon
 *   referent is the standing post-1945 arrangement under contest (the
 *   deterrence regime premised on unwinnability), assessed by this reading's
 *   own lights; the reading's endorsed alternative plays no role in the
 *   value. Beneficiary structure: civilian populations of the great powers
 *   receive eight decades of great-power war-absence; political leaderships
 *   receive concentrated final authority over the weapon; the arms-control
 *   community receives the premise that justifies its enterprise. Victim:
 *   military establishments, whose traditional mission — fighting and winning
 *   the nation's wars — became formally unavailable at the top of the
 *   escalation ladder. The colloquial label 'the nuclear revolution'
 *   decomposes into three structurally distinct readings of the same kernel
 *   (this file, countervailing_thinkable, rhetorical_contraction), each a
 *   separate constraint story with its own epsilon over the same referent;
 *   the siblings are linked via network.affects_constraints and are not
 *   described further inside this constraint. The claimed type and the
 *   metrics are independent authored facts: tangled_rope is claimed from
 *   structure (genuine coordination output plus asymmetric costs plus active
 *   enforcement), while the metrics describe the arrangement's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - - civilian_populations_of_great_powers: Primary beneficiary (moderate/trapped) — receives the war-absence the arrangement produces; no exit from the balance exists anywhere on earth
 *   - - political_leaderships: Agenda-setter and receipt seat (institutional/constrained) — sets grand strategy and declaratory policy, collects planning supremacy and crisis leverage
 *   - - military_establishments: Primary target (institutional/identity_locked) — bears mission incoherence; maintains the forces whose use the arrangement forecloses
 *   - - counterforce_strategists: Excluded voice (moderate/identity_locked) — warfighting research program placed outside respectable strategy
 *   - - arms_control_diplomatic_community: Secondary beneficiary (institutional/mobile) — treaty architecture presupposes and reinforces the unwinnability premise
 *   - - nonaligned_rest_of_world: Excluded voice (moderate/constrained) — lives under the balance without consent and without a seat
 *   - - strategic_studies_academy: Analytical observer — sees the full structure, collects nothing, pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.45).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.45).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Post-1945 Great-Power Total War Unwinnability (Deterrence-Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic studies/international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '7476a95d-6edf-4421-9079-8a32de4e0598').
narrative_ontology:cs_kernel_codification('7476a95d-6edf-4421-9079-8a32de4e0598', distributed).
narrative_ontology:cs_authority_grounding('7476a95d-6edf-4421-9079-8a32de4e0598', expertise).
narrative_ontology:cs_interpretation_layer_present('7476a95d-6edf-4421-9079-8a32de4e0598').
narrative_ontology:cs_reading_relation('7476a95d-6edf-4421-9079-8a32de4e0598', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('7476a95d-6edf-4421-9079-8a32de4e0598', war_winnability_post_1945__rhetorical_contraction, forecloses).
narrative_ontology:cs_axiom('7476a95d-6edf-4421-9079-8a32de4e0598', foundational, total_war_categorically_unwinnable).
narrative_ontology:cs_axiom_status(total_war_categorically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('7476a95d-6edf-4421-9079-8a32de4e0598', total_war_categorically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('7476a95d-6edf-4421-9079-8a32de4e0598', secondary, victory_planning_is_incoherent).
narrative_ontology:cs_axiom_status(victory_planning_is_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('7476a95d-6edf-4421-9079-8a32de4e0598', victory_planning_is_incoherent, instrumental).
narrative_ontology:cs_reference_frame('7476a95d-6edf-4421-9079-8a32de4e0598', thermonuclear_revolution_baseline).
narrative_ontology:cs_drift_state('7476a95d-6edf-4421-9079-8a32de4e0598', contemporary_third_nuclear_age, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7476a95d-6edf-4421-9079-8a32de4e0598', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_of_great_powers).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, political_leaderships).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_diplomatic_community).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, counterforce_strategists).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, categorical_unwinnability_thesis).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutual_assured_destruction_logic).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, civilian_supremacy_over_war_planning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the post-1945 great-power order in which general war between nuclear-armed states has not occurred. They receive the war-absence the arrangement produces and bear its diffuse underside: peacetime defense burdens, accident and escalation risk they did not choose, and no individual ability to leave the system — there is nowhere on earth outside the balance. Their influence runs through electoral channels that rarely engage strategic doctrine directly.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_of_great_powers, beneficiary,
    moderate, generational, trapped, global).

% Set grand strategy and declaratory policy, decide force postures, and preside over crisis management. Once victory in general war left the reachable space, final authority over the weapon and over what may be planned concentrated in their hands: militaries advise within a framework leaders define. They gain crisis-stability leverage and freedom from military pressure to fight, and they pay in kind — they can neither delegate nor disown the arrangement, and every crisis lands on their desk.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaderships, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, political_leaderships, beneficiary).

% Maintain, operate, and would execute the forces that make general war unwinnable, while their traditional mission — fighting and winning the nation's wars — is formally unavailable at the top of the escalation ladder. Career structures, promotion, and professional esteem reorganized around deterrence management, delivery systems, and command-and-control. An officer who plans seriously for winning a nuclear exchange steps outside the profession's boundaries; leaving the profession altogether means abandoning the identity the profession confers.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, biographical, identity_locked, global).

% Analysts and planners in the warfighting tradition — damage limitation, disarming options, escalation control — whose research program the unwinnability consensus places outside respectable strategy. They hold faculty chairs, think-tank posts, and occasional government access, but their proposals are received as category error rather than policy. Their intellectual identity is bound to the foreclosed program; exit means retraining into a literature they regard as mistaken.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_strategists, excluded,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, counterforce_strategists, payer).

% Negotiates and verifies the treaty architecture — test bans, non-proliferation, strategic limits — that presupposes and reinforces the premise that general war cannot be won. The premise justifies the enterprise; the enterprise stabilizes the premise. Skills transfer readily to adjacent diplomatic work, so departure is possible, but the community's standing depends on the arrangement continuing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_diplomatic_community, beneficiary,
    institutional, generational, mobile, global).

% The majority of states live under a balance they did not build and cannot alter, exposed to fallout, escalation spillover, and economic shock from any breakdown, with no seat in the councils where doctrine is set. They register objections through UN channels and review conferences that the great powers can outlast.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nonaligned_rest_of_world, excluded,
    moderate, generational, constrained, global).

% Scholars and analysts who observe the whole structure — doctrine, force posture, crisis behavior, the professional politics of strategy — and produce the histories, models, and critiques through which the arrangement understands itself. They collect no rents and bear no direct costs; their stake is analytic.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_studies_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, political_leaderships).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes great-power total war from the set of actions available to rational states: by making general war unwinnable for both sides simultaneously, it addresses the oldest collective-action problem in international politics — how rival great powers avoid catastrophic war without trusting each other — through structure rather than agreement.
% TRANSFER_FUNCTION: Moves mission definition and planning authority from military establishments to civilian political leaderships; moves professional status from warfighting competence to deterrence management; moves existential risk from wartime populations to peacetime posture, where accident and escalation risk are borne diffusely by everyone under the balance.
% ABSENT_VOICES: Counterforce strategists and the warfighting schools object that winnability persists, but sit outside the deterrence consensus where doctrine is actually set. The nonaligned majority of states lives under the balance without having consented to it and registers dissent only in forums the great powers can outlast. Future generations inherit the posture without a seat. Dissent exists at the margins of the profession rather than in the councils where the arrangement is decided — the consensus is real, but unanimity was never tested with all affected seats in the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if the great powers came to believe general war winnable and resumed victory planning — force postures, alliance structures, military doctrine, defense budgets, and crisis behavior would all reorganize around restored warfighting competition; the eight-decade absence of great-power total war depends on the constraint holding, and its removal would be the largest single change in the international system since 1945.
% FOUNDING_PROBLEM: After 1945 — decisively after thermonuclear weapons and survivable second-strike forces — great-power war could no longer be fought to a decision without civilizational cost. The arrangement was built to answer how strategy proceeds when victory is unavailable: how to organize forces, doctrine, and crisis conduct around preventing a war that can no longer be won.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the Anglo-American deterrence beneficiary set: adversarial-bloc doctrinal statements (Soviet and Russian military writings, and parallel Chinese formulations, concede the unwinnability of general nuclear war), declassified crisis deliberations (Executive Committee recordings from 1962 showing leaders on both sides treating general war as catastrophe regardless of operational plans), and the cross-bloc empirical record assembled by international-relations scholars with no stake in the doctrine's maintenance. No corroborating source attests the founding problem is dead; the parties disputing its precise boundaries (the sibling readings) dispute the location of the contraction, not the existence of the founding rupture.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).
:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the arrangement takes real things from real seats — mission coherence and planning autonomy from military establishments, an entire research program from the warfighting tradition — against real compensation (generous budgets, institutional continuity, force-modernization programs) and an extraordinary coordination output (the longest stretch of great-power peace in modern history). Suppression 0.58: enforcement is institutional rather than violent — doctrine policing, budget control, security regimes, professional sanction — and alternatives are not fully collapsed, since the warfighting school survived at the margins throughout. Theater ratio 0.22: the premise is genuinely operative (planning really did shift to prevention), with declaratory ritual layered on top; this reading explicitly does NOT authorize the high-theater account, which belongs to the rhetorical_contraction sibling. Accessibility collapse 0.78: once the reading's premises are granted, victory planning collapses as a coherent activity, with residual space kept alive by the countervailing school. Resistance 0.55: sustained institutional resistance from service cultures, warfighting traditions, and periodic counterforce revivals (early 1980s, missile-defense era, 2020s modernization). The temporal series share one nine-point grid and trace a hump, not a monotonic ratchet: consolidation (1945-65) as thermonuclear weapons and survivable second-strike forces closed the victory space; mature orthodoxy (1965-85) with enforcement peaking around the Reagan-era counterforce challenge; post-Cold-War relaxation (1995-2005) when enforcement eased and warfighting thought briefly revived; multipolar revival (2015-2025) as renewed great-power rivalry re-tightened the logic. The oscillation is driven by external geopolitical phases, not by intermittent reinforcement — the cycle is a side effect of the threat environment, not itself the mechanism holding the arrangement in place. Base properties are measured at interval end (2025, early-revival phase). Suppression_requirement is tracked because the story specifically traces enforcement-capacity change (buildup, peak, decay, rebuild); suppression as a scalar remains a raw structural property, unscaled by power or scope — only extractiveness is scaled, by directionality and global scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the military-establishment seat the arrangement is experienced as professional dispossession: the profession's defining competence was ruled out at the top of the ladder, and the seat's identity_locked exit (an officer's self-concept is constituted through warfighting mastery; leaving means abandoning the identity the profession confers) pins it near the full-target end despite institutional power. From the civilian-population seat the same structure is the most successful life-protection arrangement ever built. From the political-leadership seat it is rightful civilian supremacy — the proper constitutional order of the sword and the purse. Same-level lateral dynamics differentiate the expert seats: counterforce strategists and the deterrence mainstream hold nominally similar standing (credentialed analysts with government access), but exit options diverge sharply — the mainstream's skills transfer across the policy world (mobile), while the warfighting school's identity is fused to the foreclosed program (identity_locked), so identical nominal power yields opposite directionalities. Inter-institutionally, the armed services, the diplomatic apparatus, and the political executive inhabit the same arrangement with different stakes: the services operate what they may not use, the diplomats verify what the premise makes negotiable, the executive decides what neither may. If the identity frame broke — if officers redefined professionalism around prevention rather than victory — the payer seat's effective burden would drop and the computed classification would trend toward pure coordination; the omega on structural-versus-internalized enforcement tracks exactly this possibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation chain. Civilian populations of the great powers declare as beneficiaries with trapped exit (nowhere outside the balance) — full-beneficiary end, d near 0. Political leaderships declare as agenda-setters with a beneficiary secondary role — they administer the arrangement and collect from it (planning supremacy, crisis leverage, freedom from military pressure to fight), placing them near the beneficiary end; they are also the receipt seat named in gain_flow, since the planning authority removed from the military instrument demonstrably accrues to their hands. The arms-control community declares as beneficiary — the premise justifies the enterprise and the enterprise stabilizes the premise. Military establishments declare as victims with identity_locked exit — trapped-or-identity-locked targets sit nearest the full-target end, so institutional power does not dampen their effective burden; what is taken from them (mission coherence, planning autonomy) is not returned in kind. Counterforce strategists, seated as excluded with a payer secondary role, derive high d: they bear the foreclosure directly and have no seat in the councils where doctrine is set. The nonaligned world derives moderate-high d: it bears the risk surface (fallout, escalation spillover, economic shock) without consent or representation. Global spatial scope scales effective extraction modestly upward for the target seats; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the arrangement as pure coordination erases the payer seat: the military establishment's mission incoherence and the warfighting school's foreclosure are real costs borne through the same structure that produces the peace, and a rope verdict would launder them as neutral overhead. Reading it as pure extraction erases the coordination triumph: whatever else is true, general great-power war has not recurred since 1945, and no plausible cover-story account explains away eight decades of non-recurrence produced by mutual second-strike survivability. Tangled rope holds both halves: genuine collective-action solution (war prevention without trust) and asymmetric extraction (mission and planning authority moved from military to civilian hands) through one structure that requires active enforcement to hold. On mandatrophy: the founding problem — how strategy proceeds when war cannot be fought to a decision — remains live as long as arsenals exist, so the kernel-level constraint shows no mandate atrophy; what HAS partially atrophied is the Cold War-era instrument layer (bilateral arms-control architecture), and the analysis distinguishes the live kernel constraint from decaying instruments rather than declaring the whole arrangement resolved. The R5 mismatch consumer will find status=live paired with verdict=world_rearranges — a coherent pairing, not a zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_unwinnability_empirical_status,
    'Does any physically feasible path to meaningful victory in great-power total war exist — disarming first strike, damage limitation, terminable exchange — or is unwinnability categorical?',
    'Independent counterforce-feasibility analysis: exchange modeling with explicit termination and damage criteria, declassified war-game archives, and force-posture stress tests unconstrained by declaratory doctrine.',
    'If feasible paths exist, this reading''s foundational axiom fails, the arrangement collapses toward the countervailing reading, and the measured costs become suppression of a live alternative; if none exists, the underlying fact approaches natural-law status and the arrangement''s costs are the price of the thermonuclear revolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_unwinnability_empirical_status, empirical, 'Whether unwinnability is a physical fact or a doctrinal artifact.').

omega_variable(
    kernel_reading_location_of_contraction,
    'This story instantiates the deterrence_unthinkable reading of the war_winnability_post_1945 kernel: is the post-1945 contraction of winnability operational (this reading), bounded-but-real (countervailing_thinkable), or merely rhetorical (rhetorical_contraction)?',
    'Compare operational plans, targeting doctrine, and exercised options against declaratory policy across crises; establish whether warfighting options were executable or performed.',
    'Adopting countervailing_thinkable adds counterforce-targeted societies to the victim set and shifts epsilon; adopting rhetorical_contraction converts the story into a speech-taboo account with a high theater ratio and different persistence mechanics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_contraction, conceptual, 'Committer structure: which reading of the winnability kernel correctly locates the contraction.').

omega_variable(
    military_net_cost_after_compensation,
    'Do military establishments bear net costs from the arrangement once deterrence-era budgets, institutional preservation, and force-modernization programs are counted as compensation?',
    'Budget-history comparison against counterfactual warfighting postures; officer-corps surveys and promotion-pattern analysis on professional-identity costs.',
    'If compensation covers the loss, the payer seat''s effective burden falls and the arrangement trends toward pure coordination; if not, the asymmetric-cost structure is confirmed and the victim declaration stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_net_cost_after_compensation, empirical, 'Net cost borne by the military payer seat after compensation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the foreclosure of victory planning enforced structurally (career sanction, budget control, security regimes) or internalized (professionals have absorbed unwinnability as settled belief)?',
    'Post-Cold-War natural experiment: when enforcement relaxed in the 1990s, warfighting planning partially revived (structural component) while much of the profession did not revisit the question (internalized component); estimate shares from doctrine-revision rates and professional curricula.',
    'Internalized foreclosure persists even if enforcement machinery is dismantled, raising effective suppression above the structural measure; purely structural foreclosure would fall quickly under enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the enforcement behind the planning foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wwp1945_du_tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.06).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1945, observed).
narrative_ontology:measurement(wwp1945_du_tr_t1955, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1955, 0.1).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1955, observed).
narrative_ontology:measurement(wwp1945_du_tr_t1965, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1965, 0.16).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1965, observed).
narrative_ontology:measurement(wwp1945_du_tr_t1975, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1975, 0.18).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1975, observed).
narrative_ontology:measurement(wwp1945_du_tr_t1985, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1985, 0.2).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1985, observed).
narrative_ontology:measurement(wwp1945_du_tr_t1995, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1995, 0.13).
narrative_ontology:measurement_basis(wwp1945_du_tr_t1995, observed).
narrative_ontology:measurement(wwp1945_du_tr_t2005, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2005, 0.12).
narrative_ontology:measurement_basis(wwp1945_du_tr_t2005, observed).
narrative_ontology:measurement(wwp1945_du_tr_t2015, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2015, 0.17).
narrative_ontology:measurement_basis(wwp1945_du_tr_t2015, observed).
narrative_ontology:measurement(wwp1945_du_tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(wwp1945_du_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wwp1945_du_be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement_basis(wwp1945_du_be_t1945, observed).
narrative_ontology:measurement(wwp1945_du_be_t1955, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1955, 0.32).
narrative_ontology:measurement_basis(wwp1945_du_be_t1955, observed).
narrative_ontology:measurement(wwp1945_du_be_t1965, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement_basis(wwp1945_du_be_t1965, observed).
narrative_ontology:measurement(wwp1945_du_be_t1975, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1975, 0.47).
narrative_ontology:measurement_basis(wwp1945_du_be_t1975, observed).
narrative_ontology:measurement(wwp1945_du_be_t1985, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement_basis(wwp1945_du_be_t1985, observed).
narrative_ontology:measurement(wwp1945_du_be_t1995, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement_basis(wwp1945_du_be_t1995, observed).
narrative_ontology:measurement(wwp1945_du_be_t2005, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(wwp1945_du_be_t2005, observed).
narrative_ontology:measurement(wwp1945_du_be_t2015, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(wwp1945_du_be_t2015, observed).
narrative_ontology:measurement(wwp1945_du_be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement_basis(wwp1945_du_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wwp1945_du_su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.28).
narrative_ontology:measurement_basis(wwp1945_du_su_t1945, observed).
narrative_ontology:measurement(wwp1945_du_su_t1955, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1955, 0.44).
narrative_ontology:measurement_basis(wwp1945_du_su_t1955, observed).
narrative_ontology:measurement(wwp1945_du_su_t1965, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement_basis(wwp1945_du_su_t1965, observed).
narrative_ontology:measurement(wwp1945_du_su_t1975, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1975, 0.56).
narrative_ontology:measurement_basis(wwp1945_du_su_t1975, observed).
narrative_ontology:measurement(wwp1945_du_su_t1985, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement_basis(wwp1945_du_su_t1985, observed).
narrative_ontology:measurement(wwp1945_du_su_t1995, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1995, 0.47).
narrative_ontology:measurement_basis(wwp1945_du_su_t1995, observed).
narrative_ontology:measurement(wwp1945_du_su_t2005, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2005, 0.43).
narrative_ontology:measurement_basis(wwp1945_du_su_t2005, observed).
narrative_ontology:measurement(wwp1945_du_su_t2015, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement_basis(wwp1945_du_su_t2015, observed).
narrative_ontology:measurement(wwp1945_du_su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(wwp1945_du_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the nuclear revolution' covers three structurally distinct claims that cannot share one story, because measuring the arrangement through each reading's observable yields different epsilon over the same referent (the post-1945 strategic arrangement). This file instantiates deterrence_unthinkable (operational contraction; epsilon 0.45; victim: military establishments; moderate theater). war_winnability_post_1945__countervailing_thinkable authors the bounded-winnability claim, with a different victim set (societies targeted by counterforce plans) and different failure modes. war_winnability_post_1945__rhetorical_contraction authors the speech-taboo claim (high theater ratio, persistence by discourse policing rather than operational reality). The upstream reading with the strongest empirical grounding influences the downstream contested ones, since each is cited as evidence in disputes over the others; every family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
