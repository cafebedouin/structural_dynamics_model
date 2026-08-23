% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right (Withdrawal-Sovereignty Reading)
 *   domain: international law / nuclear nonproliferation / regime theory
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty admits any party to withdraw after ninety
 *   days' notice when extraordinary events jeopardize its supreme interests.
 *   This story instantiates the withdrawal-sovereignty reading of that
 *   kernel: Article X is the treaty's sovereignty-preserving keystone, and
 *   treaty obligations are legitimately contingent on the security
 *   environment rather than permanently binding. The referent of every metric
 *   below is the standing membership arrangement under contest — voluntary,
 *   conditional commitments sustained by an exit guarantee — assessed by this
 *   reading's own evaluative lights: the reading affirms the mechanism's
 *   legitimacy while its structural accounting still registers who banks the
 *   option value (breakout-capable states and arsenal states) and who absorbs
 *   the repricing (committed non-weapon states and their neighbors). Sibling
 *   readings of the same text are separate constraints linked in
 *   network.affects_constraints; the inter-reading contest is routed to
 *   omegas and kernel_context, not folded into this file's classification.
 *   KEY AGENTS (by structural relationship): - nuclear_weapon_states:
 *   agenda-setter and collector (institutional / arbitrage) — administers the
 *   review process, defends the Article X text, holds the amendment veto -
 *   threshold_breakout_capable_states: primary beneficiary (powerful /
 *   mobile) — banks credible-exit option value - committed_nonweapon_states:
 *   primary payer (organized / constrained) — absorbs assurance dilution
 *   across the whole regime - regional_neighbors_of_threshold_states:
 *   localized payer (moderate / trapped) — first absorbers of exit shocks -
 *   un_security_council: adjudicative observer (institutional / analytical) -
 *   iaea_secretariat: dual-positioned beneficiary-payer (institutional /
 *   constrained) — gains membership breadth, loses verification coverage per
 *   exit - analytical observer: this reading's own seat, assessing the
 *   arrangement by its lights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.7).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.18).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right (Withdrawal-Sovereignty Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international law / nuclear nonproliferation / regime theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'ceed8656-97b1-45b4-bffc-9fca24a51178').
narrative_ontology:cs_kernel_codification('ceed8656-97b1-45b4-bffc-9fca24a51178', fixed_text).
narrative_ontology:cs_authority_grounding('ceed8656-97b1-45b4-bffc-9fca24a51178', practice).
narrative_ontology:cs_interpretation_layer_present('ceed8656-97b1-45b4-bffc-9fca24a51178').
narrative_ontology:cs_reading_relation('ceed8656-97b1-45b4-bffc-9fca24a51178', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('ceed8656-97b1-45b4-bffc-9fca24a51178', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('ceed8656-97b1-45b4-bffc-9fca24a51178', foundational, withdrawal_is_sovereign_prerogative).
narrative_ontology:cs_axiom_status(withdrawal_is_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('ceed8656-97b1-45b4-bffc-9fca24a51178', withdrawal_is_sovereign_prerogative, conventional).
narrative_ontology:cs_axiom('ceed8656-97b1-45b4-bffc-9fca24a51178', foundational, obligations_revocable_on_security_grounds).
narrative_ontology:cs_axiom_status(obligations_revocable_on_security_grounds, holdable).
narrative_ontology:cs_axiom_grounding('ceed8656-97b1-45b4-bffc-9fca24a51178', obligations_revocable_on_security_grounds, instrumental).
narrative_ontology:cs_reference_frame('ceed8656-97b1-45b4-bffc-9fca24a51178', conditional_consent_among_sovereigns).
narrative_ontology:cs_drift_state('ceed8656-97b1-45b4-bffc-9fca24a51178', contemporary_multipolar_erosion_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ceed8656-97b1-45b4-bffc-9fca24a51178', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_breakout_capable_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, committed_nonweapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regional_neighbors_of_threshold_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_secretariat).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_secretariat).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, westphalian_sovereignty_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, conditional_consent_treaty_theory).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_self_certification_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that tested weapons before 1967 and anchor the treaty's security order. They preside over review conferences, hold an effective veto over any amendment touching the treaty's core bargain, and publicly affirm every party's right to withdraw when extraordinary events jeopardize supreme interests. Their own arsenals sit outside the treaty's prohibition structure, and they treat membership terms as instruments adjustable to their security environment rather than as permanent undertakings.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, beneficiary).

% Industrialized states with complete fuel cycles, advanced missile programs, or weapons-usable material stocks that keep them weeks to months from weapons capability. They receive the treaty's peaceful-use cooperation and security assurances while knowing the exit door stays open on ninety days' notice; several maintain hedging programs sized to preserve that option. Leaving would carry diplomatic cost, but the credible ability to leave is itself bargaining capital spent in regional negotiations and review conferences.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_breakout_capable_states, beneficiary,
    powerful, biographical, mobile, regional).

% Dozens of states across Latin America, Africa, Southeast Asia, and Europe that renounced weapons pathways, accept intrusive verification, and build their security diplomacy around the assumption that neighbors' abstention is durable. Their assurance depends on other parties continuing to honor obligations, and each credible exit threat elsewhere converts their invested restraint into uncompensated risk. Leaving themselves would forfeit the assurances and cooperation their security planning relies on, so they stay and press instead for harder consequences against those who go; a subset of them carry a diplomatic self-conception as custodians of the regime that binds beyond material cost.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, committed_nonweapon_states, payer,
    organized, generational, constrained, global).

% States positioned near withdrawal-prone or hedging programs, above all peninsular and gulf neighbors. They cannot move away from the risk geography; their exposure is fixed by location. When a neighbor exits or approaches exit, they absorb the security shock first: extended-deterrence dependence deepens, defense spending rises, and domestic debates over their own weapons options reopen.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regional_neighbors_of_threshold_states, payer,
    moderate, biographical, trapped, regional).

% Fifteen-member body that determines whether a withdrawal constitutes a threat to international peace and security. It convened around the 1993 announced withdrawal and passed a resolution urging its reversal, but its five permanent members' own interest in keeping exit options open blunts any general consequences doctrine. Its posture toward any given withdrawal tracks the identities of the exiting state and the complaining neighbors.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% Verification agency whose safeguards coverage extends only to parties in good standing. A withdrawal severs its inspection access overnight, as occurred on the Korean peninsula, converting verified facilities into unverified ones. It gains from the treaty's near-universal membership and loses monitoring ground with every departure, reporting annually on the continuity gaps past exits left behind.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_secretariat, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_secretariat, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_breakout_capable_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees conditional exit so that states unwilling to bind perpetually will still join: the ninety-day Article X route converts a permanent surrender of weapons options into a revocable loan, which is what made near-universal adherence achievable and sustains the shared verification and assurance architecture resting on that membership.
% TRANSFER_FUNCTION: Moves strategic option value to exit-capable parties: threshold states bank credible-exit insurance they can spend in regional bargaining, and weapon states preserve unconstrained freedom of action; the premium is paid in assurance dilution by committed non-weapon states and in concentrated security risk by their neighbors.
% ABSENT_VOICES: Populations of the regions living under renewed nuclear shadow — Northeast Asia, the Gulf, Eastern Europe — have no seat: security is voiced exclusively by states, and the sovereignty frame treats civilian exposure to a neighbor's exit as an externality of legitimate state freedom. Constituencies harmed by past exercised withdrawals appear only as complaining governments, never as represented publics.
% DISAPPEARANCE_RATIONALE: If the withdrawal right vanished overnight, the membership calculus breaks: states that joined only because commitments stayed reversible would refuse accession or demand renegotiation; the universality that load-bears the entire verification bargain cracks; committed non-weapon states would hold harder-edged but narrower assurances; and threshold states would lose bargaining capital they currently monetize in regional negotiations.
% FOUNDING_PROBLEM: The 1960s drafters needed maximum adherence from sovereign states that would not accept perpetual constraint: Article X answered the objection that treaties bind forever by converting absolute commitment into conditional consent, so the bargain could reach near-universal adoption.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the 1965-1968 Eighteen-Nation Disarmament Committee negotiating records and depositary statements attest the universality-through-conditional-consent rationale, and legal scholarship on treaty-duration clauses independently documents the drafting intent. Non-threshold non-weapon-state delegations at the 1995 and 2010 Review Conferences acknowledged the founding rationale while contesting its present weight — attestation that the problem existed is broad; attestation that it remains live today is confined to the sovereignty coalition itself.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.70 is reading-indexed over the standing membership arrangement: this reading legitimizes the exit mechanism itself, yet the structural accounting still registers that credible exit converts committed restraint into uncompensated risk — the transfer is large and asymmetric even by sympathetic lights. The series shows a crisis-ratchet rather than smooth drift: the 1993 announced withdrawal, the 2003 completed withdrawal, and the post-2019 arms-control collapse each permanently reset the baseline upward. Suppression 0.18 is a raw structural property, unscaled by power or scope: the clause coerces almost nothing — exit is free, alternatives stay open (hence accessibility_collapse 0.25, far below mountain range), and the resistance it meets (0.55: consequences-doctrine proposals, referral drafts, review-conference conditioning campaigns) aims at modifying it, not escaping it. Theater 0.42: the mechanism demonstrably works end-to-end, but a growing share of activity is declaratory — review conferences affirming the 'unfettered right' while failing to reach consensus, vitality declarations issued amid architectural collapse — hence the rising series. requires_active_enforcement is true because the clause-text entrenchment must be actively defended at every review cycle against the payer coalition's modification attempts; enforcement MACHINERY around withdrawal management (the suppression_requirement series) built up 1993-2003, peaked, and has decayed with great-power consensus — an inverted U, with the endpoint pricing the decay branch (omega: withdrawal_machinery_trajectory). All three series run on one shared nine-point grid; the scalars equal the end-state series values, and every tracked metric is authored at every grid point.
 *
 * PERSPECTIVAL GAP:
 *   From the threshold-state and weapon-state seats the arrangement computes as freedom-preserving coordination: obligations voluntarily assumed, exit legitimately retained, residual costs externalities of others' liberty. From the committed-non-weapon-state and neighbor seats the identical clause computes as uncompensated risk transfer: restraint invested under one price schedule and repriced downward by every credible exit threat. Same-level lateral differentiation is sharp here: threshold states and committed non-weapon states hold nominally identical treaty status — both are non-weapon parties — yet exit capability alone splits their seats into opposite directionalities; the power difference is clause-specific, not a difference in global standing. Identity-lock compounds the divergence on the payer side: regime-steward states whose diplomatic self-conception is nonproliferation custodianship are materially constrained AND identity-fused, so their exit options would not loosen even if amendment arithmetic shifted; if that custodial identity broke, their seat would classify nearer mobile payers with real coalition leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map onto real structure: nuclear_weapon_states (arbitrage-grade exit, institutional power) derive maximal damping — the clause costs them nothing and insures their freedom of action; threshold_breakout_capable_states (mobile exit) bank positive option value and sit well below symmetric. Payers run the other way: committed_nonweapon_states (constrained exit) and regional_neighbors_of_threshold_states (trapped) sit near the target pole — their assurance is denominated in other parties' continued restraint, which this arrangement renders revocable; trapping amplifies their effective extraction because relocation cannot hedge geographically fixed risk. iaea_secretariat sits near-symmetric (breadth gained per member, coverage lost per exit), and un_security_council holds the analytical seat. Effective extraction is computed by the engine from these declarations, amplified for trapped targets and damped for arbitrage-grade beneficiaries, with the clause's global scope modestly amplifying the payer side; suppression stays raw and unscaled throughout — the clause coerces almost nothing, it reprices everyone else's promises. On the receipt surface: the arrangement's incremental gains demonstrably land on the threshold seat, which converts exit credibility directly into bargaining capital; the weapon-state seat shares structural freedom but does not collect the clause's marginal rents, so gain_flow names the threshold seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings are guarded against. Regime-stabilizers would label this arrangement pure extraction — an escape hatch devouring the bargain; that fails because suppression is low, exit is genuinely free, alternatives remain open, and the universality function is real: nothing here survives by suppressing exits, since the clause IS an exit. Sovereignty advocates would label it pure coordination or even natural law; that fails because the option value is asymmetrically banked by breakout-capable parties while committed states absorb uncompensated dilution, and because the clause persists only through actively defended text-entrenchment backed by the weapon-state amendment veto — hence tangled_rope: genuine coordination function plus asymmetric transfer plus enforced stability. Mandatrophy status: the founding problem (sovereigns will not bind perpetually) is contested-live, not dead — the conditional-consent demand never disappeared — so this is not a mandate that outlived its function; theater_ratio 0.42 remains below the Goodhart band, so no piton drift is claimed despite rising declaratory ritual. The classification prevents the regime-stability coalition from writing the clause off as vestigial while preventing the sovereignty coalition from laundering asymmetric option value as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'Which reading of kernel npt_treaty_1970 does this constraint instantiate, and what would the sibling readings change structurally?',
    'Cross-reading diff: hold the referent fixed (the standing membership arrangement) and swap the reading. Sibling files: npt_treaty_1970__oligopoly_enforcement_reading and npt_treaty_1970__reciprocal_disarmament_reading.',
    'Under oligopoly_enforcement_reading the victim set centers non-weapon states bound under Articles I-II while weapon-state obligations stay effectively contingent; under reciprocal_disarmament_reading the staleness of Article VI becomes the victim-generating defect; under this reading the regime-stability constituency pays and threshold states collect option value. The same text yields different classifications per reading by design — the disagreement is located in Article X''s legal-moral status (sovereign prerogative vs. loophole) and in which articles anchor bindingness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer-frame routing: this file is the withdrawal_sovereignty_reading of kernel npt_treaty_1970; sibling readings are separate constraints.').

omega_variable(
    natural_law_vs_constructed_clause,
    'Is sovereign exit from treaties a natural-law constant of the Westphalian order, or a constructed, revisable treaty provision maintained because identifiable agents collect from it?',
    'Comparator analysis across regimes with and without formal withdrawal clauses (the Chemical Weapons Convention retains one; the Montreal Protocol lacks a formal one), plus the historical counterfactual: would the clause survive a general amendment conference absent great-power protection?',
    'If natural law, the constraint trends mountain-side and naturalized certification is warranted; if constructed-and-defended, tangled_rope stands and the sovereignty coalition''s naturalization rhetoric functions as cover for option-value retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_clause, empirical, 'Whether the withdrawal right is a structural feature of the interstate order or an actively maintained construct.').

omega_variable(
    supreme_interests_self_certification,
    '"Extraordinary events jeopardizing supreme interests" is self-certified by the exiting state with no external adjudicator — is self-certification an abuse-prone defect of the clause, or constitutive of the sovereignty the clause exists to protect?',
    'Comparative jurisprudence of treaty-duration clauses (third-party adjudicated vs. self-certified), plus state-practice coding of invoked versus uninvoked withdrawal justifications across all exercised and attempted exits.',
    'If abuse-prone defect, extraction estimates rise (the clause operates as a unilateral escape hatch dressed as legal procedure); if constitutive of sovereignty, this reading''s legitimacy frame holds and the measured extraction stays priced as the externality of legitimate freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supreme_interests_self_certification, conceptual, 'Whether the self-certification feature is a flaw in or a feature of the sovereignty reading.').

omega_variable(
    option_value_operationalization,
    'What observable operationalizes the option value this reading predicts threshold states bank from a credible withdrawal threat?',
    'Panel data on extended-deterrence dependence, hedging-cycle investment (enrichment and reprocessing latency kept warm), and regional bargaining outcomes before and after the 1993 and 2003 withdrawal episodes on the Korean peninsula.',
    'A measurable option-value premium confirms the beneficiary-side asymmetry implied by the victim set; a null result would push this reading''s instantiation toward pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(option_value_operationalization, empirical, 'Empirical measurability of credible-exit option value.').

omega_variable(
    withdrawal_machinery_trajectory,
    'Is the post-1993 enforcement build-up around withdrawal management a peak-then-terminal-decay, or a crisis-triggered cycle that rebuilds at the next withdrawal attempt?',
    'Track review-conference agenda items, Security Council referral drafts, and consequences-doctrine proposals across the next exercised or credibly threatened withdrawal.',
    'Terminal decay leaves exits unmanaged and raises effective extraction on the payer side; cyclical rebuild sustains a managed-exit equilibrium. The endpoint values authored here price the decay branch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_machinery_trajectory, empirical, 'Trajectory of the enforcement machinery surrounding withdrawal episodes (documented inverted-U in the suppression_requirement series).').

omega_variable(
    payer_coalition_amendment_arithmetic,
    'Could the payer coalition — committed non-weapon states holding review-conference majorities — alter or attach consequences to the withdrawal clause?',
    'Amendment arithmetic: treaty amendments require a majority of parties including all nuclear-weapon-state parties, giving five states a structural veto; test whether any consequences proposal has ever cleared even agenda-level consensus.',
    'Confirmed veto fixes the cost of repair as prohibitive and stabilizes the current arrangement; any breach of weapon-state unity opens renegotiation space and could migrate the constraint toward revisable, scaffold-like status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payer_coalition_amendment_arithmetic, empirical, 'Feasibility of payer-side coalition repair of the clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t15, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(npt__tr_t15, observed).
narrative_ontology:measurement(npt__tr_t23, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 23, 0.2).
narrative_ontology:measurement_basis(npt__tr_t23, observed).
narrative_ontology:measurement(npt__tr_t25, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(npt__tr_t25, observed).
narrative_ontology:measurement(npt__tr_t33, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 33, 0.24).
narrative_ontology:measurement_basis(npt__tr_t33, observed).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(npt__tr_t40, observed).
narrative_ontology:measurement(npt__tr_t45, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement_basis(npt__tr_t45, observed).
narrative_ontology:measurement(npt__tr_t53, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 53, 0.4).
narrative_ontology:measurement_basis(npt__tr_t53, observed).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 55, 0.42).
narrative_ontology:measurement_basis(npt__tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t15, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(npt__be_t15, observed).
narrative_ontology:measurement(npt__be_t23, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 23, 0.48).
narrative_ontology:measurement_basis(npt__be_t23, observed).
narrative_ontology:measurement(npt__be_t25, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(npt__be_t25, observed).
narrative_ontology:measurement(npt__be_t33, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 33, 0.58).
narrative_ontology:measurement_basis(npt__be_t33, observed).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(npt__be_t40, observed).
narrative_ontology:measurement(npt__be_t45, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement_basis(npt__be_t45, observed).
narrative_ontology:measurement(npt__be_t53, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 53, 0.68).
narrative_ontology:measurement_basis(npt__be_t53, observed).
narrative_ontology:measurement(npt__be_t55, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 55, 0.7).
narrative_ontology:measurement_basis(npt__be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t15, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(npt__su_t15, observed).
narrative_ontology:measurement(npt__su_t23, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 23, 0.35).
narrative_ontology:measurement_basis(npt__su_t23, observed).
narrative_ontology:measurement(npt__su_t25, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(npt__su_t25, observed).
narrative_ontology:measurement(npt__su_t33, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 33, 0.5).
narrative_ontology:measurement_basis(npt__su_t33, observed).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(npt__su_t40, observed).
narrative_ontology:measurement(npt__su_t45, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement_basis(npt__su_t45, observed).
narrative_ontology:measurement(npt__su_t53, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 53, 0.38).
narrative_ontology:measurement_basis(npt__su_t53, observed).
narrative_ontology:measurement(npt__su_t55, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 55, 0.36).
narrative_ontology:measurement_basis(npt__su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'the NPT bargain' — covers three structurally distinct claims over the same fixed text. Each reading is a separate story with its own reading-indexed epsilon, beneficiary/victim sets, and classification: oligopoly_enforcement_reading (bindingness located in Articles I-II), reciprocal_disarmament_reading (bindingness located in Article VI reciprocity with temporal urgency), and this file, withdrawal_sovereignty_reading (obligations revocable via Article X). The referent — the standing membership arrangement — is common to all three; epsilon differs by reading, as required. Downstream structure: this reading's operation feeds both siblings (each credible exit devalues enforcement currency and revocabilizes reciprocity), hence the influences edges in cs_structure.reading_relations and the affects_constraints links here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
