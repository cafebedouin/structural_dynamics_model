% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Articles I-II Oligopoly Enforcement (Horizontal Nonproliferation Asymmetry)
 *   domain: international_law/security_regime
 *
 * SUMMARY:
 *   The Nuclear Nonproliferation Treaty, signed in 1970, binds 188
 *   non-nuclear-weapon states to permanent renunciation of nuclear weapons
 *   and submission to IAEA safeguards inspections. The P5 (US, USSR/Russia,
 *   UK, France, China) retained the sole legal right to possess nuclear
 *   weapons and committed themselves to negotiate disarmament (Article VI) —
 *   a commitment that has remained essentially unexecuted for 56 years. This
 *   constraint story instantiates the oligopoly-enforcement reading: Articles
 *   I-II are treated as primary binding obligations (horizontal
 *   nonproliferation is enforced), while Article VI (vertical disarmament) is
 *   treated as contingent and aspirational. This reading transforms the NPT
 *   into a tangled rope that coordinates horizontal nonproliferation while
 *   extracting a permanent security disadvantage from threshold states and
 *   NNWS, who bear inspection burdens and deterrent denial while the P5
 *   retain and modernize arsenals. The structural asymmetry is this reading's
 *   distinctive claim: enforcement effort is directed almost entirely at
 *   preventing new proliferation, while disarmament obligations remain
 *   rhetoric. This reading coexists with two sibling readings
 *   (reciprocal_disarmament_reading, which treats Article VI as equally
 *   binding, and withdrawal_sovereignty_reading, which treats Article X as a
 *   trump card overriding the rest). The claim/metric gap is deliberate: the
 *   oligopoly-enforcement reading claims the constraint is tangled_rope
 *   (genuine coordination function in Articles I-II + asymmetric extraction
 *   in enforcement asymmetry), while the authored extractiveness (0.68) and
 *   suppression (0.71) reflect the measurable burden on NNWS and threshold
 *   states. The engine's per-seat computation will diverge: from the P5 seat
 *   the arrangement is genuine coordination that keeps proliferation in
 *   check; from the threshold-state seat it is extractive enforcement with no
 *   reciprocal obligation.
 *
 * KEY AGENTS:
 *   - Permanent Security Council (P5): Agenda-setter; retains sole legal right to nuclear weapons; sets inspection protocols; benefits from monopoly on legitimate nuclear deterrent and on interpretation of treaty obligations.
 *   - Non-nuclear-weapon states (188): Payers; submit to comprehensive safeguards; renounce deterrence; receive technical cooperation and non-binding security assurances.
 *   - Threshold states (Iran, Egypt, Japan, South Korea, Brazil, etc.): Payers + excluded; face regional security threats but are barred by Articles I-II from pursuing nuclear deterrent; bear maximal inspection burden with zero security benefit from the treaty's Article V technology-sharing (which was conditional and never delivered); identity-locked into the treaty despite its asymmetry.
 *   - IAEA Board and Secretariat: Enforcer; implements Articles I-II inspections under P5 direction; has no mandate to inspect NWS programs (only France and UK accepted limited, voluntary safeguards); operates structural bias toward finding violations in NNWS while lacking tools to verify NWS compliance.
 *   - Nuclear exporters: Beneficiaries; trade civilian technology under NPT safeguards that constrain buyer nations to peaceful uses, capturing rents through supplier dependence and export restrictions.
 *   - Disarmament advocates and non-aligned coalition: Excluded observers; document the asymmetry and call for binding Article VI enforcement; present at Review Conferences but lack voting power (amendment requires all NWS assent).
 *   - Treaty-outside nuclear states (India, Pakistan, Israel, North Korea): Excluded; cannot participate in treaty governance; paradoxically demonstrate the treaty's failure (North Korea withdrew and weaponized; India and Pakistan acquired weapons outside the treaty and remain outside it; Israel maintains ambiguous arsenal).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Articles I-II Oligopoly Enforcement (Horizontal Nonproliferation Asymmetry)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/security_regime").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '6bce5981-0add-4067-b1e6-c2f9bb8fc9a9').
narrative_ontology:cs_kernel_codification('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', formalized).
narrative_ontology:cs_authority_grounding('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', extraction).
narrative_ontology:cs_interpretation_layer_present('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9').
narrative_ontology:cs_reading_relation('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', foundational, horizontal_nonproliferation_binding).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_binding, holdable).
narrative_ontology:cs_axiom_grounding('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', horizontal_nonproliferation_binding, conventional).
narrative_ontology:cs_axiom('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', foundational, article_vi_disarmament_aspirational).
narrative_ontology:cs_axiom_status(article_vi_disarmament_aspirational, overridden).
narrative_ontology:cs_axiom_grounding('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', article_vi_disarmament_aspirational, empirically_contingent).
narrative_ontology:cs_reference_frame('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', p5_nuclear_monopoly_legitimacy).
narrative_ontology:cs_drift_state('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', contemporary_post_cold_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6bce5981-0add-4067-b1e6-c2f9bb8fc9a9', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, permanent_security_council_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_exporters_cartel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The P5 (US, USSR/Russia, UK, France, China) negotiated and ratified the NPT, retain the sole legal right to possess nuclear weapons under Article IX, and dominate the International Atomic Energy Agency's enforcement structure via their representation in the UN Security Council. They set inspection protocols, approve nuclear fuel cycles for non-nuclear states, and interpret the treaty's obligations. They benefit from the treaty's codification of their nuclear monopoly and the legitimacy it provides for preventing others from acquiring the same weapons that secured their own power.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, permanent_security_council_states, agenda_setter,
    institutional, generational, arbitrage, global).

% All five P5 members plus India, Pakistan, Israel, and North Korea (outside or post-withdrawal) enjoy the option to retain and modernize nuclear arsenals while binding others to abstention. They gain security assurance from the treaty's enforcement asymmetry: their weapons are normalized while threshold states face intrusive inspection and deterrent denial.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% Comprise the 188 signatory states that renounce nuclear weapons. They submit to IAEA safeguards inspections of their entire nuclear fuel cycles, agree to full-scope inspections, and are prohibited from operating unsafeguarded nuclear facilities. In exchange they gain access to peaceful nuclear technology and receive security assurances from the P5 (though these assurances are non-binding under the NPT itself). Their exit is costlier than remaining: withdrawal invokes a 90-day notice period and international political pressure, while continued compliance yields technical cooperation and nominal security guarantees.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% States with advanced nuclear fuel cycles (Iran, Egypt, Turkey, Japan, South Korea, Brazil, Argentina, and others) are bound by NNWS obligations but face regional security threats that nuclear deterrence might address. They are excluded from the Articles V and VI nuclear-sharing arrangements that NATO members access, cannot legally pursue deterrent capability even under Article X withdrawal, and bear maximal inspection burden while denied the security benefit the weapons themselves would provide. Their professional scientific and security communities are profoundly identity-locked into the NPT's legitimacy framework despite its asymmetry.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, excluded).

% Implements Articles I-II inspection protocols on behalf of the P5-dominated UN Security Council. Designs and executes safeguards regimes, reports compliance findings to the Board (which has P5 majority influence), and designates which states receive 'routine' versus 'special' inspections. Operates under structural pressure to find violations in NNWS fuel cycles while lacking mandate to inspect NWS programs (only France and the UK accepted limited IAEA safeguards; US, Russia, China rejected outside inspection). Acts as enforcement machinery while maintaining institutional fiction of neutrality.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_board_and_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_board_and_secretariat, observer).

% Countries and private firms exporting civilian nuclear technology (US, Russia, France, Germany, Canada, Japan, South Korea) benefit from the NPT's authorization of 'peaceful' nuclear technology transfer while the treaty's safeguards regime constrains buyer nations to uses that do not enable weapon development. Exporters capture rents from technology licensing while the NPT's inspection burden makes it unprofitable for purchasers to develop indigenous fuel-cycle capabilities — maintaining supplier dependence and export market control.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_exporters_cartel, beneficiary,
    organized, biographical, constrained, global).

% States that have pursued or possess nuclear weapons outside the NPT framework (India, Pakistan, Israel, North Korea) are excluded from negotiations, cannot access Article V technology-sharing agreements, and face international sanctions and non-recognition of their weapons as legitimate. Yet they retain operational deterrents, demonstrating that NPT membership does not confer security and withdrawal does not prevent capability (North Korea). Their exclusion paradoxically proves the treaty's enforcement asymmetry: the NPT binds willing NNWS while outside-treaty weapons states operate unconstrained.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, treaty_challenger_states, excluded,
    moderate, biographical, trapped, regional).

% Civil society, non-aligned movement states, and humanitarian law advocates document the NPT's asymmetry and call for binding Article VI enforcement. They are excluded from treaty amendment (only NPCs can amend; a Review Conference cannot modify the text without all NWS assent). Their analysis forms a counternarrative to the official 'binding arms-control regime' framing but carries no institutional power to alter the treaty's operation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, disarmament_advocacy_coalition, observer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, disarmament_advocacy_coalition, excluded).

% Strategic analysts and military planners in threshold states who recognize that the NPT denies them a security tool that the treaty ratifiers themselves retained. They argue (non-publicly, often) that the treaty imposes asymmetric constraint incompatible with regional power-balance equilibrium. Yet institutional capture, alliance pressure, and the costliness of withdrawal keep them bound.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, unaligned_security_communities, excluded,
    moderate, generational, identity_locked, regional).

% Measures the structural relationship between Articles I-II (horizontal nonproliferation as binding obligation) and Article VI (vertical disarmament as aspirational contingency), noting that this reading authorizes enforcement of the former while treating the latter as perpetually deferred.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, analytical_observer_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, permanent_security_council_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing horizontal proliferation (new states acquiring nuclear weapons) by establishing an inspectable, enforceable baseline of NNWS compliance. Eliminates the risk-race dynamic where each potential nuclear threshold state's acquisition would provoke others to follow. Creates a transparent, audited nuclear fuel cycle that is verifiable and constrained. Provides technical cooperation on peaceful nuclear energy to compensate NNWS for foregoing weapons.
% TRANSFER_FUNCTION: Extracts a permanent security constraint from 188 NNWS and threshold states: renunciation of nuclear deterrent, submission to intrusive inspections, restricted fuel-cycle autonomy, and conditional dependence on nuclear exporters for technology. Transfers security and prestige benefits to the P5 (monopoly on legitimate weapons, veto over proliferation risk) and to nuclear exporters (captive markets, technological rent-capture through export restrictions). Transfers legitimacy to the NPT regime itself as an international law success, enabling the P5 to cast themselves as custodians of global order rather than as parties to an inequitable bargain.
% ABSENT_VOICES: States that would benefit from nuclear deterrence but are bound by the NNWS obligation are structurally excluded from renegotiating the treaty's terms — amendment requires all NWS assent and P5 ratification, so threshold states cannot force a review of Articles I-II enforcement symmetry. Nuclear-armed states outside the treaty (India, Pakistan, Israel, North Korea) cannot participate in treaty governance. Disarmament advocates calling for binding Article VI enforcement are present at Review Conferences but lack voting power — the treaty's amendment structure ensures their voice is heard but overrideable.
% DISAPPEARANCE_RATIONALE: If Articles I-II enforcement and the NNWS obligation disappeared, the global security landscape would reorganize rapidly: threshold states (Iran, Egypt, Japan, South Korea, Brazil) would face immediate domestic pressure to pursue deterrence; nuclear exporters would lose the NPT safeguards regime that justifies supplier authority; the P5 would lose their primary legitimacy claim for their own arsenals (that the treaty prevents others from acquiring them). Regional nuclear competitions would intensify. The NPT's collapse would be the single largest shift in international security architecture since its 1970 inception.
% FOUNDING_PROBLEM: In the 1960s, Cold War proliferation risks accelerated: Britain, France, and China had acquired nuclear weapons; advanced industrial states (Japan, Germany, India, Brazil, Egypt) possessed the technical capability and faced security incentives to follow. The prospect of 20+ nuclear-armed states by 1980 was genuine. The founding problem was preventing a proliferation cascade that would destabilize regional balances, complicate deterrence, and increase the likelihood of nuclear use through accident or miscalculation.
% FOUNDING_PROBLEM_CORROBORATION: The P5 attest the founding problem remains live, citing Iran's program, North Korea's withdrawal and weaponization, and continued proliferation risks. Non-aligned movement and disarmament advocates counter that the founding problem's core (cascade proliferation by security-motivated regional powers) has been substantially addressed by the NPT's success in keeping most NNWS non-nuclear. They argue the treaty now persists not to solve the original problem but to preserve the P5's monopoly. Independent security analysts are split: some cite the relative stability of the post-Cold-War period as evidence the problem is managed; others note that the treaty's asymmetry has driven three states (North Korea, India, Pakistan) to build weapons outside it, suggesting the original logic (provide security assurance so NNWS don't proliferate) has failed for those most threatened.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The oligopoly-enforcement reading reads the NPT as a constraint whose primary function is horizontal nonproliferation (Articles I-II) enforced through IAEA safeguards, inspection protocols, and P5 veto authority. Article VI disarmament is authored into the treaty as a rhetorical commitment with no mechanism, no timeline, and no enforcement. This reading is distinct from the reciprocal-disarmament reading, which treats Articles I and VI as a binding bargain: NNWS renounce weapons in exchange for a genuine path to eventual NWS disarmament. The oligopoly-enforcement reading concedes no such reciprocity. Extractiveness rises from 0.52 (1970) to 0.68 (2026) because over the interval the IAEA safeguards regime has intensified (special inspections, environmental sampling, algorithmic detection methods) while Article VI obligations have receded further from any serious enforcement track. Theater ratio rises from 0.15 to 0.42 because an increasing share of treaty rhetoric (Review Conference commitments, subsidiary agreements) promises disarmament progress and technical cooperation that is not delivered, while the substantive machinery of enforcement focuses entirely on keeping NNWS non-nuclear. Suppression rises from 0.48 to 0.71 because the cost to NNWS of withdrawal has increased (political isolation, sanctions threats, technological disruption) and the identity-lock deepens (scientific/diplomatic communities internalize the treaty's legitimacy even while resenting its terms). The time grid is shared: every metric is authored at six time points (1970, 1985, 2000, 2010, 2020, 2026) so temporal analysis has complete data. The measurement series tracks extraction accumulation (Goodhart drift): the founding problem (preventing cascade proliferation) is substantially solved by 2000, but the enforcement apparatus persists and intensifies, now serving to maintain the P5 monopoly rather than solve the original problem. This is the signal of a constraint transitioning toward piton-like operation (function atrophied, enforcement maintained for other reasons) — but the oligopoly-enforcement reading stops short of that classification because the horizontal nonproliferation function is still live and meaningful; what is dead is the reciprocal vertical disarmament.
 *
 * PERSPECTIVAL GAP:
 *   The P5 and IAEA Board will compute this constraint as coordination with high legitimacy: the treaty has succeeded in keeping the number of nuclear-armed states relatively stable (only 9 recognized nuclear powers despite 50+ technically capable states). NNWS will compute it differently: the constraint enforces their renunciation while the P5 retain, modernize, and exercise veto. Threshold states will compute it as pure extraction — they bear inspection burden and deterrent denial with zero security payoff. The engine will produce these divergent types from the structural data: a beneficiary seat (P5) seeing low effective extraction; a payer seat (NNWS) seeing moderate extraction; a threshold-state seat (identity-locked, constrained exit) seeing high extraction. The gap between the claimed tangled_rope and the computed seat-types is where this reading's truth lives: a constraint can be genuinely coordinative for some seats (keeping proliferation down benefits everyone including NNWS) while being extractive for others (the cost falls unequally on threshold states denied deterrence). The tangled-rope claim admits this asymmetry; a rope claim would deny extraction; a snare claim would deny coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 directionality (d ≈ 0.2): Beneficiary of the treaty's core function (they retain weapons while others renounce); powerful (institutional), arbitrage exit (can amend the treaty, can withdraw and retain weapons as proven by France). Their effective extraction is negative (they gain security subsidy). NNWS directionality (d ≈ 0.6): Modest payers; organized power; constrained exit (withdrawal invokes 90-day notice + international pressure, access to peaceful nuclear technology lost). They extract modest cost and extract modest benefit (security assurance, tech cooperation). Threshold-state directionality (d ≈ 0.85): Deep payers; moderate power; identity-locked exit (scientific/diplomatic professions internalize the treaty's legitimacy; withdrawal carries massive reputational cost in the security community even though it is legally permissible). They extract high cost (deterrent denial, inspection burden, export restrictions) with minimal tangible benefit (promised but not delivered technical cooperation under Article V). IAEA directionally (d ≈ 0.65): Structurally biased toward enforcement of Articles I-II over Article VI; constrained exit (operates under P5 mandate). The directionality derivation is straightforward from beneficiary/victim declarations + exit options + power, except for threshold states, where the identity-lock is critical: their exit is technically unconstrained (Article X permits withdrawal) but identity-locked because the scientific/security establishment of these states has fused with the NPT's legitimacy narrative ('we are a responsible nuclear state'). A directionality override is not needed here because identity_locked exit properly encodes this — but commentary must flag it.
 *
 * MANDATROPHY ANALYSIS:
 *   The oligopoly-enforcement reading diagnoses mandatrophy in Article VI. The founding problem (preventing proliferation cascade in the 1960s and 1970s) was genuine. By 2000, that problem was substantially solved: the treaty had 187 signatories and the only states that weaponized outside it (India, Pakistan, North Korea) had already been excluded. Article VI was always the reciprocal obligation meant to justify NNWS compliance — but its mandate expired because the P5 found they could preserve their monopoly without disarming. The result is a constraint whose primary function (horizontal nonproliferation) persists while its reciprocal (vertical disarmament) has atrophied into theater. This reading avoids mislabeling the constraint as pure snare by acknowledging that Articles I-II still solve a real coordination problem (preventing new proliferation does benefit all states, including NNWS). But it names the mandatrophy clearly: the treaty persists as an extraction mechanism because the P5 benefit from maintaining a nuclear monopoly indefinitely, not because disarmament is being negotiated in good faith. The oligopoly-enforcement reading explicitly rejects the reciprocal-disarmament reading's framing that Article VI is a binding legal obligation — that rejection is the mandatrophy claim in one sentence: 'the treaty's reciprocal obligation is dead, but the constraint persists as enforcement of its extraction half.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_mandate_extinction,
    'Is Article VI disarmament a dead mandate (the problem it was meant to solve is solved or abandoned) or a live obligation merely deferred indefinitely?',
    'Historical analysis of P5 and Review Conference statements on disarmament timelines; econometric modeling of P5 nuclear stockpile trends (are stockpiles shrinking or stabilizing as a new baseline); comparison of P5 disarmament rhetoric to actual deployment and modernization spending. A sustained 20+ year period of stockpile stability at high numbers with no credible disarmament pathway would support mandate extinction; a documented roadmap with milestones would support deferral.',
    'If mandate is extinct, the treaty is closer to a snare (pure extraction with coordination cover); if deferred, it remains tangled_rope (real coordination + asymmetric extraction). This distinction governs whether the constraint should be reclassified at a future Review Conference or reform cycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_mandate_extinction, empirical, 'Whether Article VI disarmament is a dead mandate or merely long-deferred.').

omega_variable(
    threshold_state_identity_lock_mechanism,
    'Is the suppression on threshold states (preventing Article X withdrawal and pursuit of deterrence) structural (external barriers: sanctions, technology denial, alliance pressure) or internalized (institutional identity fusion with the NPT''s legitimacy claim)?',
    'Qualitative analysis of threshold-state governmental and scientific-community discourse: do they frame withdrawal as illegal/immoral (internalized) or merely costly/unwise (structural)? Post-exit behavior of states that do withdraw (North Korea, Iran if it withdrew) — does suppression persist after barrier removal? If suppression persists after exit, reclassify as partially internalized.',
    'If internalized, the effective suppression on threshold states is higher than the structural measure suggests — they carry the suppression with them even if external barriers removed. This affects how pinned threshold states are to the constraint and whether reform can unlock their participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_identity_lock_mechanism, empirical, 'Whether threshold-state suppression is structural or internalized.').

omega_variable(
    reciprocal_reading_empirical_refutation,
    'Could the reciprocal-disarmament reading have been substantiated, or was the P5 asymmetry built into the treaty''s architecture from inception?',
    'Historical archival analysis of NPT negotiation records (1965-1970): did the P5 commit to concrete disarmament timelines or only to negotiations? Did the draft treaties proposed by non-aligned states include binding Article VI provisions that the P5 rejected? Comparison with the LTBT (1963) disarmament language — was NPT Article VI intentionally weaker?',
    'If the asymmetry was built in deliberately, the oligopoly-enforcement reading is the treaty''s true structural reading from inception. If the asymmetry emerged from post-treaty P5 defection on a genuine reciprocal commitment, the treaty was subverted rather than honestly instantiated. This is a conceptual question about what the treaty ''really is'' versus how it has been operated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_reading_empirical_refutation, conceptual, 'Whether the oligopoly-enforcement reading is the treaty''s intended structure or a post-hoc deviation from a reciprocal bargain.').

omega_variable(
    threshold_state_security_paradox,
    'Do threshold states that remain bound by the NPT actually enjoy greater security than threshold states that withdrew (North Korea) or remained outside (India, Pakistan, Israel)?',
    'Comparative security analysis: measure state security via military spending, alliance strength, conflict history, regional power balance, and deterrent credibility. Compare Iran (bound NNWS, maximum inspection burden) to India/Pakistan (outside treaty, weaponized, higher deterrent credibility in regional balance) and to North Korea (withdrew, weaponized, isolated but credibly deterred regional/global adversaries). Threshold states that remained bound should show higher security than those outside if the treaty''s security-assurance framing is true.',
    'If bound threshold states show lower security than equivalent outside states, the NPT''s reciprocal logic collapses and the oligopoly-enforcement reading is confirmed. If security is equivalent or higher for bound states, the treaty''s coordination function may be real even without disarmament.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_security_paradox, empirical, 'Whether NPT binding confers actual security benefits on threshold states.').

omega_variable(
    reading_coexistence_stability,
    'Can the oligopoly-enforcement reading and the reciprocal-disarmament reading coexist indefinitely in the same treaty framework, or does one reading''s dominance eventually exclude the other?',
    'Monitoring of Review Conference outcomes: if successive Reviews call for binding Article VI enforcement, the reciprocal reading gains institutional purchase and pressure mounts on the oligopoly-enforcement interpretation. If Reviews settle into accepting Article VI as aspirational while strengthening Articles I-II enforcement, the oligopoly reading crystallizes. Measurement of P5 resistance to disarmament propositions across decades.',
    'If readings cannot coexist, the treaty faces an institutional crisis where NNWS demand reciprocity or threaten withdrawal, forcing the P5 to choose between enforcing Articles I-II without reciprocity (snare classification) or negotiating real disarmament. This is the path-dependency question for whether the oligopoly reading becomes explicit and contested, or remains implicit in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'Whether the oligopoly-enforcement and reciprocal-disarmament readings can coexist as institutional equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(npt__tr_t1970, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(npt__tr_t2020, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(npt__be_t2020, observed).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1985, 0.56).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(npt__su_t2020, observed).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(npt__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime__inspection_asymmetry).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, fuel_cycle_export_control__supplier_dependence).

% DUAL FORMULATION NOTE:
% The NPT kernel instantiates as three distinct constraint stories corresponding to three live readings held by different institutional factions: (1) oligopoly-enforcement reading (this story) — Articles I-II as primary binding obligation, Article VI as contingent; favored by P5 and IAEA; (2) reciprocal-disarmament reading — Article VI as equally binding legal obligation with temporal urgency; favored by non-aligned movement and disarmament advocates; (3) withdrawal-sovereignty reading — Article X exit right as legitimate exercise of sovereignty; held by threshold states and countries considering withdrawal. These readings share the same kernel (the NPT text) but differ in which articles are primary, which are binding, and what obligations flow from them. The oligopoly-enforcement reading treats Articles I-II enforcement and Article VI asymptote as the constraint's core. The ε value (0.68) is stable across readings but is reading-indexed: the oligopoly reader sees extraction via enforcement asymmetry; the reciprocal reader sees the same constraint as a snare (broken reciprocal bargain); the withdrawal reader sees it as a trap (exit rights on paper but identity-locked in practice). Sibling stories should be authored independently with their own ε values if they diverge. This family demonstrates ε-invariance principle: if different readings of the same kernel produce substantially different ε values under honest assessment, decompose to separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
