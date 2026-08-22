% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty Doctrine (Responsibility to Protect)
 *   domain: international law/political philosophy/global governance
 *
 * SUMMARY:
 *   This story instantiates the conditional_sovereignty reading of the
 *   westphalian_sovereignty kernel: the claim that sovereignty's protection
 *   from external interference is conditioned on the sovereign's conduct
 *   toward its own population, and that systematic human rights violations
 *   trigger legitimate external intervention, adjudicated in practice through
 *   Security Council authorization (ICISS 2001; 2005 World Summit Outcome
 *   paragraphs 138-139; Resolution 1973 on Libya, 2011). The arrangement the
 *   story is about — and the sole referent of epsilon — is this standing
 *   conditional-sovereignty practice as it operates, assessed by the
 *   reading's own lights: the reading endorses the principle, so the
 *   practice's selectivity (who gets named, who is shielded by a patron's
 *   veto) is the failure this reading must account for, not grounds for
 *   switching to a different referent. The claim/metric gap is deliberate and
 *   structural: the constraint is CLAIMED as tangled_rope — a genuine
 *   coordination function (the state-protected atrocity gap is real; the
 *   doctrine has stopped killings) joined to asymmetric extraction (decision
 *   authority over weak states' internal affairs, gated and selectively
 *   applied by the permanent five) — while the metrics are authored from the
 *   doctrine's observed operation. The absolute_sovereignty and
 *   graduated_sovereignty readings are separate constraints with their own
 *   epsilon values, beneficiary sets, and classifications; they are linked
 *   here, not averaged.
 *
 * KEY AGENTS:
 *   - security_council_p5: agenda-setting seat (institutional/arbitrage) — holds the authorization gate, is structurally exempt from being named, collects the selective license
 *   - intervening_powers: primary beneficiary with cost exposure (powerful/arbitrage) — converts would-be aggression into authorized operation; selects which cases to join
 *   - targeted_sovereign_governments: primary target (moderate/trapped) — loses decision authority over internal affairs when triggered; consent definitionally absent
 *   - weak_unprotected_states: structural target class (organized/constrained) — bear the conditionality in prospect without reciprocal protection
 *   - populations_at_risk_of_atrocity: dual-positioned intended protectee (powerless/trapped) — protected when authorization holds, harmed when it overreaches
 *   - intervention_advocacy_coalitions: norm entrepreneur (organized/mobile) — supplies threshold evidence and gains access when the doctrine is live
 *   - regional_organizations: delegated beneficiary (organized/mobile) — receive the subsidiarity role in authorizing and leading regional operations
 *   - general_assembly_member_states: excluded seat (organized/constrained) — debate and propose reform without a vote on the trigger
 *   - international_law_scholars: analytical observer (analytical/analytical) — maps the legality contest both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.43).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.43).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty Doctrine (Responsibility to Protect)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international law/political philosophy/global governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'ac9daad7-ebe5-405d-b319-21ea956bf2b1').
narrative_ontology:cs_kernel_codification('ac9daad7-ebe5-405d-b319-21ea956bf2b1', fixed_text).
narrative_ontology:cs_authority_grounding('ac9daad7-ebe5-405d-b319-21ea956bf2b1', extraction).
narrative_ontology:cs_interpretation_layer_present('ac9daad7-ebe5-405d-b319-21ea956bf2b1').
narrative_ontology:cs_reading_relation('ac9daad7-ebe5-405d-b319-21ea956bf2b1', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('ac9daad7-ebe5-405d-b319-21ea956bf2b1', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('ac9daad7-ebe5-405d-b319-21ea956bf2b1', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('ac9daad7-ebe5-405d-b319-21ea956bf2b1', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('ac9daad7-ebe5-405d-b319-21ea956bf2b1', foundational, systematic_atrocity_forfeits_nonintervention_shield).
narrative_ontology:cs_axiom_status(systematic_atrocity_forfeits_nonintervention_shield, holdable).
narrative_ontology:cs_axiom_grounding('ac9daad7-ebe5-405d-b319-21ea956bf2b1', systematic_atrocity_forfeits_nonintervention_shield, conventional).
narrative_ontology:cs_reference_frame('ac9daad7-ebe5-405d-b319-21ea956bf2b1', sovereignty_as_responsibility_framework).
narrative_ontology:cs_drift_state('ac9daad7-ebe5-405d-b319-21ea956bf2b1', post_libya_gridlock_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ac9daad7-ebe5-405d-b319-21ea956bf2b1', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, security_council_p5).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervening_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, regional_organizations).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, populations_at_risk_of_atrocity).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, weak_unprotected_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, populations_at_risk_of_atrocity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, intervening_powers).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, sovereignty_as_responsibility_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_norm).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states holding permanent seats and veto power over any authorization to act. They decide, in practice, when reports of systematic violations become grounds for action, and each can shield itself and its allies from ever being named. They drafted the 2005 compromise text and have blocked every proposal to widen the authorization vote beyond their chamber.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, security_council_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% States with the military capacity to act once action is authorized. The doctrine converts what their lawyers would otherwise call aggression into a legitimate operation with coalition cover, and they choose which authorized cases to join. They pay in blood, treasure, and post-operation obligations when they act, and in credibility when operations overreach.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervening_powers, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, intervening_powers, payer).

% Human rights organizations, legal scholars, and policy networks that document atrocities, draft the threshold arguments, and press governments to act. The doctrine gives their casework a recognized lever: when it is live, their reports become agenda items and their experts become advisers. When the doctrine falls into disrepute, their funding channels and access narrow.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocacy_coalitions, beneficiary,
    organized, biographical, mobile, global).

% Civilians living under governments that massacre or expel them. When the threshold is found met and action authorized, outside force may stop the killing, as in Sierra Leone and Libya's early weeks. They have no vote in the authorization, they cannot leave the territory being fought over, and when operations expand or the state collapses afterward, they bear the bombing, the civil war, and the statelessness that follow.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, populations_at_risk_of_atrocity, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, populations_at_risk_of_atrocity, payer).

% Governments whose conduct toward their own population becomes the grounds for outside action. They lose control over their airspace, finances, and ultimately their survival when authorization issues, and their consent is definitionally absent from the process that targets them. Their recourse is argument, patrons, and the veto of friendly permanent members.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_governments, payer,
    moderate, biographical, trapped, national).

% The class of states without permanent seats or great-power patrons. The doctrine's conditionality binds them in principle and in practice — they can be named, sanctioned, and intervened against — while the states that author the conditionality exempt themselves. They respond through the General Assembly, the Non-Aligned Movement, and counter-proposals, but cannot exit the Charter system that houses the doctrine.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, weak_unprotected_states, payer,
    organized, generational, constrained, global).

% Regional bodies — the African Union, ECOWAS, the Arab League — that the doctrine routes action through in practice. Subsidiarity gives them a recognized role in authorizing and leading operations in their regions, along with the mandates, staff, and standing they did not have under strict non-intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, regional_organizations, beneficiary,
    organized, generational, mobile, continental).

% The broad UN membership holds no vote on authorization. They debate, adopt resolutions, and propose reform — codes of conduct, veto-restraint pledges, a larger General Assembly role — but the trigger decision happens in a chamber most of them cannot enter as voters. They would restructure the trigger if they could.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, general_assembly_member_states, excluded,
    organized, generational, constrained, global).

% The academic and juristic community mapping the doctrine's legality. They produced the 'illegal but legitimate' verdict on Kosovo, track the gap between the 2005 text and practice, and supply both sides of the argument — the analysis the doctrine's defenders and its critics both cite.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, security_council_p5).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the state-protected atrocity gap: when the sovereign is the perpetrator, no domestic remedy exists and the non-intervention rule shields the perpetrator from outside response. The doctrine provides a shared threshold and a single authorization path, so response capacity coordinates through one gate instead of each state improvising unilaterally — the unilateral-improvisation baseline being itself a recurring source of wars.
% TRANSFER_FUNCTION: Moves decision authority over a targeted state's internal affairs — airspace, finances, regime survival — from that state's government to the Security Council and whatever coalition it authorizes; moves operational legitimacy to intervening powers and agenda control to the permanent members; moves protection, when authorization holds its mandate, to threatened populations.
% ABSENT_VOICES: The general UN membership holds no vote on authorization and would restructure the trigger if it could; its reform proposals (code of conduct, veto restraint) go nowhere. Populations under veto-blocked atrocities, as in Syria after 2011, have no seat at all. The targeted government's consent is definitionally absent, and affected populations are spoken for by advocates and intervening powers rather than seated.
% DISAPPEARANCE_RATIONALE: The parties genuinely dispute the counterfactual. Advocates hold the world rearranges: the non-intervention shield would again protect massacring sovereigns, and the Rwanda pattern would recur without a shared threshold to organize response. Skeptics hold the world stays roughly the same: the doctrine rarely fires, major interventions (Kosovo 1999) preceded it, and states would act, or not, through ordinary alliance politics as they did before 2001. The Libya and Syria records feed both readings, and no adjudicating evidence settles which.
% FOUNDING_PROBLEM: Mass atrocity committed by or tolerated by the sovereign itself, with the international community legally and politically paralyzed by the non-intervention rule — the gap exposed by Rwanda in 1994 and Srebrenica in 1995, and framed by Kofi Annan's question of how to respond to a sovereign who massacres his own people.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the 2005 World Summit Outcome adopting the responsibility-to-protect paragraphs was consented to by the full UN membership, including the states that later led the resistance; the African Union's Constitutive Act embeds a non-indifference principle authored by the states most exposed to intervention; United Nations inquiry commissions on Rwanda, Srebrenica, and Darfur documented the gap; survivor organizations attest it. No corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, contested).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.43, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.43 is moderate-to-substantial: the doctrine as practiced transfers decision authority over a targeted state's internal affairs to a great-power-gated coalition, but the transfer is conditional on a high threshold, rare in firing, and damped by genuine protective deliveries (Sierra Leone 2000, Cote d'Ivoire 2011, Gambia 2017). Suppression 0.58 reflects the coercive machinery available at trigger (sanctions, no-fly zones, force) plus the standing normative discipline short of trigger; suppression is an unscaled structural property and the engine scales only extractiveness. Theater_ratio 0.52: since the post-Libya gridlock most of the doctrine's visible activity is declaratory (annual Secretary-General reports, General Assembly informal dialogues, anniversary summits) while authorizations are rare; the 2011 dip in the theater series marks the interval when the doctrine did real enforcement work. Accessibility_collapse 0.42: the absolute-sovereignty alternative remains fully available as a live legal position — Russia and China invoke it explicitly — so understanding this structure does not close off alternatives; the contest is the point. Resistance 0.75: G77 and Non-Aligned Movement declarations, the BRICS pushback after Libya, Brazil's responsibility-while-protecting counter-proposal, and the ACT code-of-conduct campaign are organized, sustained resistance among the defining facts of this doctrine's politics. All three tracked metrics share one eight-point grid (2001-2025). Suppression_requirement is tracked because enforcement capacity itself changed over the interval: built to its Libya peak in 2011, then partially decaying into a sanctions-and-inquiry regime as the veto gate hardened — a decay-and-partial-rebuild trajectory, not a monotone ratchet.
 *
 * PERSPECTIVAL GAP:
 *   From the permanent-member seat the arrangement is an architecture its own chamber controls: the doctrine is whatever the veto permits, and its selectivity is not a defect but the design. From the targeted-government seat the same text is a standing threat that activates along alignment lines — allies of permanent members are never named. From the population seat it is a lottery: the same doctrine that stopped killings in Sierra Leone preceded state collapse in Libya. From the advocacy seat it is a lever that loses value each time it overreaches. The engine computes these per-seat classifications from the structural data; the authored tangled_rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the permanent five wrote the 2005 text, hold the authorization gate, and are structurally exempt from being named (each can veto its own targeting), placing them nearest the beneficiary end; intervening powers collect the operational license and choose their cases; advocacy coalitions collect access and relevance; regional organizations collect the subsidiarity role. Victim declarations drive high directionality: targeted governments lose decision authority with consent definitionally absent, and their exit is territorial impossibility — trapped. Weak unprotected states bear the conditionality in prospect without reciprocal protection; their collective organization through the G77 and Non-Aligned Movement damps but does not remove the exposure. Populations at risk are declared on both sides: protected when authorization holds its mandate, harmed when it expands or the state collapses afterward — the honest reading of the Libya/Sierra Leone contrast places them near symmetric, and the engine should derive that from the dual declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the state-protected atrocity gap exposed in Rwanda and Srebrenica — is live: Syria, Myanmar, Darfur, and Ethiopia document that the problem persists, so this is not a mandate outliving its function and mandatrophy is not resolved. The tangled_rope claim is what prevents both adjacent mislabels. Reading the doctrine as pure coordination (the advocates' rope) would hide the gate capture — the permanent five's exemption and selective license — that the Libya record made visible. Reading it as pure extraction (the G77 critics' snare) would erase the genuine protective deliveries and the real coordination problem the doctrine was built for; the coordination story is not cover. The theater rise after 2014 tracks declaratory drift, but the trigger has not atrophied to performance alone — it fired through ECOWAS in the Gambia in 2017 — so the structure is degraded and contested, not yet inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the westphalian_sovereignty kernel (reading: conditional_sovereignty). What structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Comparative structural analysis across the three instantiated readings: absolute_sovereignty removes the intervention trigger entirely (the unprotected victims become populations behind atrocity-committing sovereigns, and every sovereign gains unconditional shield status); graduated_sovereignty replaces the threshold event with a continuous capacity-and-legitimacy spectrum (eliminating trigger-determination politics but introducing assessor discretion). The disagreement is located in one element: whether the non-intervention shield''s validity depends on the sovereign''s conduct (this reading), nothing (absolute), or the sovereign''s capacity (graduated).',
    'Sibling readings produce different victim sets, different beneficiary sets, and different per-seat classifications; averaging across readings would fabricate an epsilon no party holds. Each reading must be classified on its own structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested sovereignty kernel; sibling readings instantiate different constraints.').

omega_variable(
    threshold_determination_capture,
    'Is the trigger (''systematic human rights violations'') a legal standard applied neutrally, or a political judgment made in practice by the actors with power to act?',
    'Case-comparison of trigger events against the target state''s alignment with permanent members: correlate which violation profiles produced authorization findings (Libya 2011, Cote d''Ivoire 2011, Gambia 2017) against which were blocked or ignored (Syria 2011 onward, Myanmar, Yemen), controlling for atrocity scale.',
    'If the trigger is political, the coordination gate is captured by the gating seat and the extraction component concentrates there; per-seat classification shifts toward the capture reading and the doctrine''s coordination claim weakens. If the trigger tracks atrocity scale independently of alignment, extraction is incidental to a functioning standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determination_capture, empirical, 'Whether the doctrine''s threshold is law or great-power preference.').

omega_variable(
    protection_regime_change_convertibility,
    'Is the drift from civilian-protection mandates to regime change (Libya 2011) intrinsic to military humanitarian operations, or contingent misuse correctable by mandate discipline?',
    'Compare operations that held their mandate (ECOWAS in the Gambia 2017, Sierra Leone 2000) against operations that expanded (Libya 2011) and identify the structural conditions under which expansion occurs: coalition composition, absence of follow-up political process, veto patronage of the target.',
    'If expansion is intrinsic, the protective function is systematically convertible into the very intervention it licenses and the genuine-coordination half of the structure is unstable. If contingent, mandate safeguards can separate protection from regime change and the coordination component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_regime_change_convertibility, empirical, 'Whether the doctrine''s protective mandate is structurally separable from its regime-change drift.').

omega_variable(
    selectivity_reciprocity_structure,
    'Is the doctrine''s asymmetry — binding states without patrons, exempting permanent members and their allies — contingent on the current power distribution, or intrinsic to any authorization structure that requires great-power consent?',
    'Institutional-design analysis and counterfactual modeling of veto-restrained or majority-vote authorization; observe whether the French-Mexican veto-restraint initiative and the ACT group code of conduct measurably change which states face trigger findings.',
    'If intrinsic, weak-state exposure is a permanent feature of any Security-Council-gated variant and only structural redesign changes who bears the conditionality. If contingent, incremental reform can equalize the burden without abandoning the authorization gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_reciprocity_structure, conceptual, 'Whether the doctrine''s selective application is design or circumstance.').

omega_variable(
    protected_population_net_position,
    'Across intervened and non-intervened atrocity cases, are the protected populations net beneficiaries or net payers of the doctrine''s operation?',
    'Welfare and casualty accounting across case pairs (Libya versus Syria; Sierra Leone versus Rwanda) with explicit, contestable counterfactual assumptions about what each non-intervention baseline would have produced.',
    'Flips the population seat''s directionality from the beneficiary side to the target side and changes its per-seat classification; also reweights how much of the doctrine''s measured extraction is offset by delivered protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protected_population_net_position, empirical, 'The intended protectees'' net position across the doctrine''s case history is genuinely unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(west_tr_t2001, observed).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(west_tr_t2005, observed).
narrative_ontology:measurement(west_tr_t2008, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2008, 0.46).
narrative_ontology:measurement_basis(west_tr_t2008, observed).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.26).
narrative_ontology:measurement_basis(west_tr_t2011, observed).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2014, 0.38).
narrative_ontology:measurement_basis(west_tr_t2014, observed).
narrative_ontology:measurement(west_tr_t2017, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2017, 0.45).
narrative_ontology:measurement_basis(west_tr_t2017, observed).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2020, 0.49).
narrative_ontology:measurement_basis(west_tr_t2020, observed).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(west_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2001, 0.25).
narrative_ontology:measurement_basis(west_be_t2001, observed).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement_basis(west_be_t2005, observed).
narrative_ontology:measurement(west_be_t2008, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(west_be_t2008, observed).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.56).
narrative_ontology:measurement_basis(west_be_t2011, observed).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement_basis(west_be_t2014, observed).
narrative_ontology:measurement(west_be_t2017, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2017, 0.47).
narrative_ontology:measurement_basis(west_be_t2017, observed).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement_basis(west_be_t2020, observed).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2025, 0.43).
narrative_ontology:measurement_basis(west_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement_basis(west_su_t2001, observed).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement_basis(west_su_t2005, observed).
narrative_ontology:measurement(west_su_t2008, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(west_su_t2008, observed).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.7).
narrative_ontology:measurement_basis(west_su_t2011, observed).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement_basis(west_su_t2014, observed).
narrative_ontology:measurement(west_su_t2017, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2017, 0.5).
narrative_ontology:measurement_basis(west_su_t2017, observed).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(west_su_t2020, observed).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(west_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, security_council_veto_structure).

% DUAL FORMULATION NOTE:
% The colloquial label 'Westphalian sovereignty' covers at least three structurally distinct claims about the non-intervention rule's conditionality. This story instantiates the conditional reading only; the absolute and graduated readings are separate constraints with their own epsilon, beneficiary/victim sets, and classifications. The conditional reading is historically downstream of the absolute reading (it emerged as a revision of the absolute norm after Rwanda) and upstream of the graduated reading (the post-Libya threshold-determination crisis generated the graduated reform proposals). The veto-structure arrangement is a load-bearing dependency: this reading's enforcement runs entirely through the Security Council gate, so degradation of that gate's legitimacy propagates directly into this constraint's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
