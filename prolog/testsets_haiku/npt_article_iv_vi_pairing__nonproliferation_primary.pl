% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV-VI Pairing: Nonproliferation Primary Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (1968) pairs Article IV
 *   (guaranteeing non-weapon states' access to civil nuclear technology)
 *   conditionally on Article III (verification standards set by weapon
 *   states) while Article VI (disarmament obligation) is treated as
 *   aspirational and non-enforceable. This reading instantiates the
 *   nonproliferation_primary interpretation: weapon-state security interest
 *   in preventing horizontal proliferation (additional states acquiring
 *   weapons) is the organizing principle; the two-tier asymmetry (weapon
 *   states exempt from enforcement, non-weapon states subject to
 *   verification) is permanent and justified. The constraint is CLAIMED as
 *   tangled_rope (coordination of civil nuclear access + asymmetric
 *   restraint, requiring active enforcement) but metrics describe
 *   moderate-to-high extractiveness and rising theater as disarmament
 *   expectations decay and the regime becomes increasingly theatrically
 *   justified through humanitarian language while operationally enforcing
 *   restraint asymmetry.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Institutional power, arbitrage exit — set the rules, exempt from enforcement, control Article III standards.
 *   - non_weapon_states_civil_nuclear_aspirants: Moderate power, identity-locked exit — bear verification costs, cannot exit without proliferation-signal penalties.
 *   - disarmament_advocacy_states: Powerful, constrained exit — participate in diplomacy but cannot enforce Article VI via this reading.
 *   - iaea_verification_regime: Institutional power, analytical exit — administers verification on weapon-state behalf, structural conflict between independence and accountability.
 *   - analytical_observer_seat: Measures the asymmetry's persistence and the rising gap between Article VI expectation (disarmament timeline) and Article VI practice (no enforcement).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.71).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV-VI Pairing: Nonproliferation Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '3a3dc95c-3b25-4912-b1ce-1a5f2d98240f').
narrative_ontology:cs_kernel_codification('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', formalized).
narrative_ontology:cs_authority_grounding('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', extraction).
narrative_ontology:cs_interpretation_layer_present('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f').
narrative_ontology:cs_reading_relation('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', foundational, article_vi_non_enforceable).
narrative_ontology:cs_axiom_status(article_vi_non_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', article_vi_non_enforceable, conventional).
narrative_ontology:cs_axiom('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', foundational, horizontal_proliferation_prevention_justifies_asymmetry).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prevention_justifies_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', horizontal_proliferation_prevention_justifies_asymmetry, instrumental).
narrative_ontology:cs_reference_frame('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', permanent_two_tier_non_proliferation_order).
narrative_ontology:cs_drift_state('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', contemporary_enforcement_asymmetry_visible, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a3dc95c-3b25-4912-b1ce-1a5f2d98240f', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_security_establishment).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_civil_nuclear_aspirants).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocacy_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, civil_nuclear_supply_chain_actors).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_proliferation_prevention_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, security_asymmetry_stabilization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enforce the NPT framework. Set the conditions for Article IV access (civil nuclear technology) by controlling Article III verification standards. Their arsenals are structurally excluded from the treaty's enforcement machinery, giving them asymmetric freedom to develop and modernize nuclear capabilities. They frame Article VI disarmament timelines as aspirational and non-justiciable, meaning no enforcement mechanism exists to compel their disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Security-focused governments benefit from the nonproliferation regime by preventing rival states' nuclear acquisition, which would alter regional balances. They support Article IV restrictions and Article VI non-enforcement as stabilizing. Their benefit is the continuation of their power position under a non-proliferated baseline.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_security_establishment, beneficiary,
    organized, generational, constrained, global).

% Seek civil nuclear capacity (power generation, industrial applications, medical isotopes) but face Article III verification barriers designed to prevent military diversion. They must accept intrusive inspections, technology transfer restrictions, and supply-chain controls. Their exit options are limited by the international consensus on nonproliferation and by the fact that leaving the treaty is read as a proliferation signal that triggers security isolation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_civil_nuclear_aspirants, payer,
    moderate, generational, identity_locked, national).

% Include parties to the TPNW (Treaty on the Prohibition of Nuclear Weapons) and non-aligned states that view Article VI as the legitimate binding obligation. They pay through constrained diplomacy: their disarmament proposals are non-binding under this reading, their enforcement mechanisms are ruled out-of-order, and their moral authority is systematized as 'aspirational' rather than mandatory.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocacy_states, payer,
    powerful, generational, constrained, global).

% Administers Article III verification standards on behalf of the weapon states. Sets inspection protocols, decides technical adequacy, and certifies compliance. Derives its authority from the nonproliferation framework but remains accountable to the weapon states that fund and govern it; a structural conflict between independence and dependence that shapes which non-weapon-state activities are scrutinized.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_regime, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_regime, observer).

% Technology suppliers and fuel vendors benefit from Article IV's existence — it creates legitimate demand for civil nuclear capacity — while Article III verification creates barriers to entry for competitors and concentration of supply routes. They gain market position from the regime's existence while individual exporters have mobility (can exit specific sanctions) but remain dependent on the broader regime.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, civil_nuclear_supply_chain_actors, beneficiary,
    institutional, biographical, mobile, global).

% States seeking independent nuclear military capacity are excluded from direct participation; they are the regulatory TARGET of the framework, not its negotiating party. They would dispute the nonproliferation reading and argue for either Article IV access without Article III barriers, or for Article VI enforcement that would level the asymmetry.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_proliferation_aspirant_states, excluded,
    moderate, biographical, trapped, national).

% Views the constraint structure from outside: Notes that the nonproliferation reading preserves the two-tier order by making Article VI permanently unenforceable while Article IV remains conditionally enforced; observes the structural asymmetry (weapon state freedom to modernize vs. non-weapon state restraint) and measures how the constraint's persistence depends on this asymmetry.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, analytical_observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal proliferation (additional states acquiring nuclear weapons) by establishing civil nuclear access (Article IV) conditional on nonproliferation verification (Article III) while freezing the existing weapon-state arsenal at the 1968 baseline through non-enforcement of disarmament (Article VI rendered aspirational). Solves the coordination problem: how to enable civil nuclear technology diffusion without triggering arms-race proliferation.
% TRANSFER_FUNCTION: Transfers security asymmetry: non-weapon states bear the cost of restraint and inspection verification; weapon states retain arsenals and modernization capacity; civil nuclear suppliers gain market concentration and licensing power; IAEA and enforcement bodies gain institutional authority. The flow is from non-weapon-state capability-forgone to weapon-state security-preserved.
% ABSENT_VOICES: Horizontal-proliferation aspirant states (the nuclear capability-seeking nations this constraint targets) are excluded from setting its terms. They would argue for either Article IV access without Article III barriers, or for mandatory Article VI enforcement that would eliminate the two-tier asymmetry. Disarmament-focused states participate but their voice is systematized as 'aspirational' — non-binding.
% DISAPPEARANCE_RATIONALE: If Article IV-III pairing vanished (removing conditional civil nuclear access and verification regimes), non-weapon states would either pursue indigenous fuel-cycle capability to escape supply dependence or withdraw into non-nuclear energy. If Article VI non-enforcement disappeared and became binding, weapon-state arsenals would face disarmament timelines, triggering geopolitical realignment or breakout capacity races. If the asymmetry vanished entirely, the current non-proliferation baseline would not persist.
% FOUNDING_PROBLEM: Post-1968: preventing an unlimited arms race while allowing civil nuclear development. The constraint was built to freeze horizontal proliferation (keep the number of weapon states at 1968 levels) while permitting non-weapon states access to civilian nuclear fuel and technology.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the security establishment attest the founding problem is live: horizontal proliferation remains a real threat and the constraint continues to prevent state acquisitions. Disarmament states and humanitarian-law advocates attest the founding problem (horizontal proliferation) may be partially solved but has been decoupled from the broader problem (nuclear disarmament); they argue the constraint is preserved to protect weapon-state arsenals, not to solve proliferation itself. Independent analyses from non-aligned research institutions document the regime's success at preventing new weapon-state emergence while documenting its failure to produce disarmament progress.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval because the constraint's operational definition shifts: early NPT (t=0) was presented as contingent bargain ('restrain now, disarm later'); contemporary NPT (t=32) is operationally permanent two-tier order (disarmament timelines are routinely ignored, verification is asymmetric). Theater rises from 0.32 to 0.52 because the gap between Article VI language and Article VI non-enforcement grows: disarmament rhetoric persists, enforcement machinery never materializes, and the regime increasingly relies on diplomatic theater ('disarmament dialogue,' non-binding frameworks) to justify the asymmetry. Suppression rises modestly (0.58→0.71) because enforcement intensity concentrates on non-weapon states: inspections become more intrusive, fuel-supply controls tighten, breakout scenarios dominate planning. The measurements share one time grid (32 is the reference point for all three metrics at each time point), enabling the lifecycle drift detection system to track the simultaneous rise in extraction, theater, and suppression intensity — a signature of a constraint whose original coordination function is atrophying while its extraction mechanism hardens.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat, this constraint is a success: horizontal proliferation is prevented, their arsenals are secure, and Article VI non-enforcement is justified by security necessity. From the non-weapon-state-aspirant seat, it is unjust extraction: they bear permanent restraint while weapon states modernize indefinitely. From the disarmament-advocacy seat, it is broken bargain: Article VI was promised in 1968 as contingent reciprocal obligation; its conversion to aspirational status is a breach treated as legitimate. The engine measures these divergences by computing per-seat types from the structural data — weapon-state seat may compute as rope (genuine coordination with minor extraction cost), non-weapon-state-target seat as snare (pure extraction), advocacy seat as snare-adjacent (broken coordination). This is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states derive low directionality (d near 0.0) because they are net beneficiaries: they retain arsenals, set verification standards, and exempt themselves from disarmament enforcement. Non-weapon-state civil-nuclear aspirants derive high directionality (d near 1.0) because they are net targets: they must accept intrusive verification, supply restrictions, and technology controls, with identity-locked exit (leaving signals proliferation intent). The non-weapon-state security establishment occupies a middle position (d ~0.3): they benefit from nonproliferation (preventing rival capability) but bear costs (constrained diplomacy, inability to enforce Article VI). This divergence is structural, not metric-dependent: the engine computes d from the beneficiary/victim declarations and exit options, producing per-seat classifications without tuning them to any terminal type prediction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation) was live in 1968 and remains live: no new weapon states have emerged, which is presented as evidence of NPT success. However, the broader problem the constraint was *also* meant to address — arms control reciprocity, weapon-state disarmament, and the conditional logic of the bargain — has atrophied. Article VI timelines have passed repeatedly without enforcement; disarmament progress is negligible; the constraint increasingly relies on theater (disarmament committees, non-binding frameworks) to justify its persistence. The constraint exhibits mandatrophy: the original reciprocal-bargain function (non-weapon restraint conditional on weapon-state progress) has been abandoned, replaced by permanent asymmetry (non-weapon restraint independent of weapon-state behavior). The classification remains tangled_rope because genuine coordination (preventing horizontal proliferation) persists alongside extraction (permanent non-weapon-state restraint) and active enforcement (IAEA inspections, supply controls). But the ratio has shifted: coordination is narrowed to preventing new weapon states, not preventing weapons globally; extraction is broadened to include non-enforcement of the reciprocal obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness_kernel_dispute,
    'The kernel dispute: is Article VI a binding legal obligation or an aspirational commitment? This reading treats it as aspirational, foreclosing enforcement; the grand_bargain reading treats it as binding, foreclosing permanent asymmetry.',
    'ICJ advisory opinion on Article VI''s legal character under VCLT (Vienna Convention on Law of Treaties); state-party litigation; or formal amendment to clarify. Historical interpretation of the 1968 negotiation record and doctrine evolution.',
    'This is the axiomatic fault line: the nonproliferation_primary reading requires Article VI non-enforceability to justify permanent two-tier order; if Article VI is ruled binding, this reading''s core claim is falsified and the grand_bargain reading becomes operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_bindingness_kernel_dispute, conceptual, 'Whether Article VI is legally binding (grand_bargain/abolitionist terrain) or aspirational (nonproliferation_primary terrain).').

omega_variable(
    asymmetry_legitimacy_vs_inertia,
    'Is the two-tier asymmetry legitimate (justified by security necessity and horizontal-proliferation prevention) or persistent only by institutional inertia and weapon-state power (making it a snare disguised as tangled_rope)?',
    'Long-term stability: if the regime endures 50+ more years with maintained norms and non-proliferation, legitimacy hypothesis gains support; if state withdrawals cite the asymmetry as unjust breach, inertia hypothesis gains support. Doctrine evolution: shifts in international humanitarian law consensus (e.g., TPNW adoption and universalization) that delegitimize nuclear weapons would undermine this reading''s framing.',
    'Legitimacy verdict supports this reading as tangled_rope (genuine coordination + justified asymmetry). Inertia verdict would reclassify the constraint as snare (asymmetric extraction persisting by institutional capture and power, not by legitimate coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_legitimacy_vs_inertia, empirical, 'Whether the constraint''s persistence is justified or merely imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t8, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(npt__tr_t8, observed).
narrative_ontology:measurement(npt__tr_t16, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(npt__tr_t16, observed).
narrative_ontology:measurement(npt__tr_t24, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 24, 0.49).
narrative_ontology:measurement_basis(npt__tr_t24, observed).
narrative_ontology:measurement(npt__tr_t32, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 32, 0.52).
narrative_ontology:measurement_basis(npt__tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t8, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(npt__be_t8, observed).
narrative_ontology:measurement(npt__be_t16, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(npt__be_t16, observed).
narrative_ontology:measurement(npt__be_t24, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(npt__be_t24, observed).
narrative_ontology:measurement(npt__be_t32, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(npt__be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t8, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(npt__su_t8, observed).
narrative_ontology:measurement(npt__su_t16, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(npt__su_t16, observed).
narrative_ontology:measurement(npt__su_t24, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(npt__su_t24, observed).
narrative_ontology:measurement(npt__su_t32, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(npt__su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% The NPT's Article IV-VI pairing is a contested kernel instantiated across three distinct constraint readings. The nonproliferation_primary reading (this story) treats Article VI as aspirational and Article IV as conditionally justified; the grand_bargain reading treats both as binding reciprocal obligations; the abolitionist reading treats Article IV as illegitimate unless coupled with mandatory disarmament. The three readings have different ε values (this reading: 0.68 extraction; grand_bargain expects lower extraction due to reciprocity framing; abolitionist expects higher due to illegitimacy-of-dual-use-proliferation framing), different victim sets, and different authority groundings. They are linked via network.affects_constraints to enable cross-reading analysis and to track how the kernel's interpretation shifts the constraint's structural classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
