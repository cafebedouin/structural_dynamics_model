% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI Reciprocal Disarmament Bargain (1970 Treaty)
 *   domain: international_law/security/nonproliferation
 *
 * SUMMARY:
 *   The NPT (1970) establishes a two-tier structure: five declared
 *   nuclear-weapons states (NWS) are permitted to retain nuclear arsenals;
 *   190+ non-nuclear-weapons states (NNWS) commit to permanent
 *   non-acquisition. The reciprocal-disarmament reading interprets Article VI
 *   as a binding, temporally urgent obligation: the NWS commit to 'pursue
 *   negotiations in good faith on effective measures relating to cessation of
 *   the nuclear arms race at an early date and to nuclear disarmament.' This
 *   reading holds that 'in good faith' and 'at an early date' impose
 *   measurable obligations on NWS to reduce arsenals toward zero, and that
 *   the NNWS coalition's permanent renunciation is the quid pro quo. The
 *   competing reading (oligopoly-enforcement) treats Articles I-II
 *   (horizontal proliferation prevention) as the binding core and Article VI
 *   as contingent aspiration. The third reading (withdrawal-sovereignty)
 *   emphasizes Article X's withdrawal right as a sovereign escape hatch if
 *   security conditions change. This JSON instantiates ONLY the
 *   reciprocal-disarmament reading as a clean constraint with its own ε,
 *   beneficiary/victim structure, and enforcement apparatus.
 *
 * KEY AGENTS:
 *   - NWS strategic autonomy (institutional power, constrained exit) — bears the cost of interpreted disarmament obligation
 *   - NNWS coalition (organized power, identity-locked exit) — gains normative leverage to demand Article VI compliance in exchange for permanent non-acquisition
 *   - Developing security-vulnerable states (powerless, trapped exit) — face permanent strategic subordination if Article VI is not enforced
 *   - NNWS nuclear aspirants (moderate power, mobile exit) — excluded from verification apparatus but face incentive to proliferate if reciprocal bargain is broken
 *   - NWS verification apparatus (institutional power, analytical exit) — administers the regime but structurally lacks enforcement tools for Article VI
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.72).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI Reciprocal Disarmament Bargain (1970 Treaty)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/security/nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'c256f18a-5ba9-4efa-807a-0a950a096358').
narrative_ontology:cs_kernel_codification('c256f18a-5ba9-4efa-807a-0a950a096358', fixed_text).
narrative_ontology:cs_authority_grounding('c256f18a-5ba9-4efa-807a-0a950a096358', lineage).
narrative_ontology:cs_interpretation_layer_present('c256f18a-5ba9-4efa-807a-0a950a096358').
narrative_ontology:cs_reading_relation('c256f18a-5ba9-4efa-807a-0a950a096358', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('c256f18a-5ba9-4efa-807a-0a950a096358', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c256f18a-5ba9-4efa-807a-0a950a096358', foundational, article_vi_binding_reciprocal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c256f18a-5ba9-4efa-807a-0a950a096358', article_vi_binding_reciprocal_obligation, deontological).
narrative_ontology:cs_axiom('c256f18a-5ba9-4efa-807a-0a950a096358', foundational, nonproliferation_parity_temporal_bound).
narrative_ontology:cs_axiom_status(nonproliferation_parity_temporal_bound, holdable).
narrative_ontology:cs_axiom_grounding('c256f18a-5ba9-4efa-807a-0a950a096358', nonproliferation_parity_temporal_bound, instrumental).
narrative_ontology:cs_reference_frame('c256f18a-5ba9-4efa-807a-0a950a096358', reciprocal_disarmament_framework).
narrative_ontology:cs_drift_state('c256f18a-5ba9-4efa-807a-0a950a096358', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c256f18a-5ba9-4efa-807a-0a950a096358', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, developing_security_vulnerable_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, progressive_nws_analysts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five legally recognized NWS (US, USSR/Russia, UK, France, China) committed under Article VI to 'pursue negotiations in good faith' toward nuclear disarmament. This reading interprets the commitment as binding with temporal force and measurable progress obligations. The NWS argue the commitment is aspirational; this reading holds them to enforceability. Their exit is constrained by the NPT's legitimacy structure — withdrawing would signal treaty violation and destabilize the nonproliferation regime itself.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy, payer,
    institutional, generational, constrained, universal).

% The 190+ non-nuclear-weapons states agreed to permanent non-acquisition of nuclear weapons. Under this reciprocal reading, they receive a binding counterpart: NWS disarmament progress toward zero. They gain normative leverage to demand Article VI compliance and can invoke the breach to justify proliferation or withdrawal. Their exit is constrained by security dependence on the regime and on extended deterrence guarantees from NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_coalition, beneficiary,
    organized, biographical, constrained, universal).

% States without advanced security partnerships or indigenous deterrent capacity depend on the NPT regime and on UNSC commitments to shield them from nuclear threats. Under the reciprocal reading, they face a structural injustice: they surrendered the option to develop nuclear weapons (horizontal nonproliferation), but the NWS have not held to disarmament (Article VI failure). They have no exit: withdrawal is impossible without incurring isolation; staying means accepting permanent vulnerability.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, developing_security_vulnerable_states, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, developing_security_vulnerable_states, excluded).

% The IAEA, UNSC, and treaty bodies nominally monitor compliance, but Article VI verification is structurally absent — no binding inspection, no enforceable timeline, no measurement of disarmament progress. This reading treats the verification gap as an enforcement mechanism that protects NWS autonomy and renders Article VI unenforceable by design.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_verification_apparatus, agenda_setter,
    institutional, generational, analytical, universal).

% States that signed the NPT but view Article VI as broken may calculate that the reciprocal bargain is void and nuclear acquisition is justified. Under this reading, they would explicitly cite NWS disarmament failure as the cause of their withdrawal or covert acquisition — making the enforcement gap a direct causal vector for horizontal proliferation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_nuclear_aspirants, excluded,
    moderate, biographical, mobile, regional).

% Researchers, diplomats, and policy advocates within NWS who view Article VI as a binding obligation and advocate for meaningful disarmament measures. They argue the reciprocal reading reflects the treaty's original intent and that honoring it would strengthen the regime. Their power is moderate — they influence discourse and policy preference but not allocation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, progressive_nws_analysts, beneficiary,
    moderate, biographical, constrained, national).

% External analyst assessing the structural properties of the constraint: whether Article VI creates binding obligation or aspirational language, whether the reciprocal structure is enforced or performed, and whether the verification gap is structural injustice or implementation pragmatism.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a two-tier nonproliferation bargain: NNWS permanently renounce nuclear weapons acquisition in exchange for NWS commitment to negotiate disarmament; the regime solves a collective action problem (states prefer a world with fewer nuclear weapons but benefit individually from acquiring them) by coupling renunciation to verification and reciprocal progress.
% TRANSFER_FUNCTION: Moves from NNWS to NWS: permanently constrained security options and subordination to the nuclear umbrella. Moves from NWS to NNWS: a binding commitment to negotiate disarmament and (under this reading) measurable progress toward zero. The asymmetry is the core of the tangled rope: one party (NNWS) surrenders an option permanently; the other (NWS) commits to a process whose endpoint is indefinite.
% ABSENT_VOICES: NNWS nuclear aspirants are structurally excluded — they have no voice in setting the disarmament timeline or verification apparatus, yet their security depends on NWS compliance. States without security partnerships (developing vulnerable states) are nominally in the coalition but lack leverage to enforce Article VI.
% DISAPPEARANCE_RATIONALE: If this constraint (interpreted as a reciprocal bargain with binding Article VI) were enforced with teeth — real verification, timelines, and consequences for NWS non-compliance — NNWS would have leverage to demand disarmament progress; NWS would face pressure to reduce arsenals or accept treaty collapse; the global distribution of strategic autonomy would shift. If the constraint vanishes (Article VI returns to pure aspiration), NNWS lose their normative claim to disarmament; developing vulnerable states face permanent strategic subordination; some NNWS might calculate that proliferation is justified by the bargain's failure.
% FOUNDING_PROBLEM: In 1968, when the NPT was drafted, the world faced a choice: prevent the spread of nuclear weapons by making it illegal, but at what price to non-nuclear states? The founding problem was how to get 190+ nations to permanently forgo nuclear weapons while five powers kept theirs. The answer was a reciprocal bargain: NNWS agree to non-acquisition; NWS agree to disarm.
% FOUNDING_PROBLEM_CORROBORATION: The NWS have not materially disarmed since 1968 — arsenals were reduced after the Cold War but stabilized, modernization continues, and the NWS show no binding commitment to zero. The New Agenda Coalition, the Non-Aligned Movement, and successive Review Conferences document that NNWS view the founding problem as unresolved and Article VI as breached. Independent analysis (Stockholm International Peace Research Institute, Bulletin of the Atomic Scientists, UN disarmament commission reports) confirms arsenal persistence and absence of binding timelines. NWS analysts argue disarmament is progressing; NNWS and external scholars counter that the rate is negligible and the endpoint indefinite — the corroboration comes from outside the NWS beneficiary set.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint binds NNWS permanently to non-acquisition while NWS retain arsenals indefinitely (asymmetric structure). The gap between the reciprocal reading's claim and the measured extraction is deliberate: the reading asserts a binding mutual obligation that would equalize the asymmetry if enforced; the authored metrics measure the current state where NNWS have fulfilled their side and NWS have not. Theater is substantial (0.58) and rising because disarmament rhetoric is routine (UNSC resolutions, treaty preambles, ministerial statements) but material arsenals persist and modernize — the constraint's functional component (preventing NNWS proliferation) is robust, but the reciprocal component is increasingly performed. Suppression (0.72) is high because the constraint's persistence depends on keeping NNWS from calculating that the bargain is void; NNWS are identity-locked by the regime (withdrawal invokes isolation and security destabilization), and the regime actively suppresses arguments that Article VI non-compliance justifies horizontal proliferation. The measurement series shows extraction accumulating over 54 years (1970–2024): the founding problem (NWS disarmament) is recognized as dead by 2024, but NNWS remain bound to non-acquisition, so the extraction asymmetry widened.
 *
 * PERSPECTIVAL GAP:
 *   The NWS institutional seat and the NNWS organized seat should compute strikingly different types. From the NWS position, the constraint is a coordinated regime that prevents proliferation and serves security interests (rope or scaffold framing). From the NNWS position, it is asymmetric extraction: they surrendered an option permanently in exchange for a process whose endpoint is indefinite and whose enforcement is absent (tangled rope or snare). The engine will compute these divergences from the authored beneficiary/victim structure and exit options. The reciprocal-disarmament reading makes the asymmetry the point: by naming NWS strategic autonomy as a victim (constrained by Article VI), it claims the NWS side bears a cost they have not yet paid, and that cost justifies NNWS normative leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   The NNWS coalition is the beneficiary (receives the counterpart obligation to disarm; gains normative leverage to demand compliance). NWS strategic autonomy is the victim (constrained by the interpreted Article VI obligation — modernization and retention are harder to justify under binding disarmament language). Developing vulnerable states are payers (surrendered proliferation option, receive no enforcement on Article VI, face permanent vulnerability). Directionality for NWS should sit between 0.5 (symmetric) and 1.0 (full target), likely 0.65–0.75, because the constraint imposes interpretive burden and diplomatic cost but NWS retain actual arsenal capacity and can exit by withdrawal at some cost. Directionality for NNWS coalition should sit near 0.2–0.35 (partial beneficiary) because they gain leverage but do not collect material benefits if disarmament fails. No directionality overrides are needed — the derivation from beneficiary/victim + exit options should produce the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (NWS disarmament) is declared dead (founding_problem_status: dead) while the constraint persists (disappearance_verdict: world_rearranges). This is a classical mandatrophy signature: the founding justification for the constraint has expired, but the constraint's operation (NNWS non-acquisition; NWS retention) is valuable enough to the beneficiaries (NWS) that it persists. The tangled-rope classification prevents misreading this as a simple rope (which would assert both sides benefit and willingly participate); it also prevents misreading as a snare (which would emphasize victimhood exclusively). The tangled rope names the reality: NNWS genuinely benefit from having fewer competitors for nuclear weapons (coordination function) AND bear the cost of permanent renunciation while NWS modernize (extraction function). Both functions are live simultaneously. The constraint survives mandatrophy because the coordination benefit (fewer NNWS proliferators) continues to accrue to NWS and NNWS alike, even though the reciprocal part (disarmament) has died. This makes the theater_ratio rise: more energy goes into asserting the reciprocal narrative (statements on Article VI, review conferences) while less goes into actual disarmament.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness_ambiguity,
    'Is Article VI legally binding with enforceable obligations, or is it aspirational language that states pursued in good faith but without binding milestones?',
    'International Court of Justice advisory opinion on Article VI''s legal character; or a future NPT Review Conference that formally clarifies the treaty''s original intent via amendment or interpretive statement signed by all parties.',
    'If binding, NWS breach Article VI and NNWS have grounds for treaty withdrawal or renegotiation; if aspirational, the constraint is a rope (coordination without extraction) not a tangled rope. The classification hinges on this omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_bindingness_ambiguity, conceptual, 'Whether Article VI is a binding obligation or aspirational language.').

omega_variable(
    verification_gap_as_structural_injustice,
    'Is the absence of Article VI verification mechanisms a design flaw (structural injustice that renders the reciprocal bargain unenforceable), or a pragmatic compromise reflecting the impossibility of intrusive verification on sovereign NWS arsenals?',
    'Empirical: new verification technologies (satellite monitoring, declared warhead counts, transparency agreements) could enable enforcement; normative: NNWS coalition consensus on whether verification absence is a dealbreaker or acceptable trade-off.',
    'If structural injustice, the constraint''s enforcement asymmetry is the point — Article VI exists to be unenforceable, preserving NWS autonomy; if pragmatic compromise, the constraint is a rope with implementation delays, not a tangled rope extracting from NNWS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_gap_as_structural_injustice, empirical, 'Whether verification gap is intentional (structural) or contingent (pragmatic).').

omega_variable(
    nnws_nuclear_aspirant_exit_pathways,
    'Under the reciprocal-disarmament reading, do NNWS that calculate Article VI is broken have legitimate grounds to withdraw from the treaty, or does the withdrawal right (Article X) remain constrained by security obligation doctrine?',
    'Diplomatic precedent: if an NNWS formally withdraws citing NWS non-compliance with Article VI, how do the NWS and UNSC respond? Do they accept the withdrawal or argue bad faith?',
    'If legitimate grounds, the constraint''s suppression decreases and resistance increases as NNWS openly cite the bargain''s failure; if constrained, suppression holds and the constraint remains a tangled rope. If multiple NNWS follow the precedent, the regime destabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_nuclear_aspirant_exit_pathways, empirical, 'Whether Article VI breach opens a legitimate exit for NNWS.').

omega_variable(
    reciprocal_vs_oligopoly_reading_differentiation,
    'Is the NPT fundamentally a reciprocal bargain (Article VI binding, horizontal nonproliferation and vertical disarmament as paired obligations), or fundamentally an oligopoly enforcement mechanism (Articles I-II binding, Article VI contingent, designed to freeze NWS advantage)?',
    'Historical analysis of negotiating records from 1968 NPT drafting; survey of state parties on their understanding of treaty intent; comparison of amendment proposals from NNWS vs. NWS over time.',
    'If reciprocal, this reading''s structural claims are correct and mandatrophy analysis follows. If oligopoly, the classification should revert to rope (coordination in service of NWS interest) and the oligopoly-enforcement reading is the structurally accurate one. The treaty cannot coherently be both simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocal_vs_oligopoly_reading_differentiation, conceptual, 'Whether the treaty''s core function is reciprocal bargain or NWS oligopoly enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(npt__tr_t1970, observed).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(npt__tr_t1990, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.51).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement_basis(npt__tr_t2020, observed).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(npt__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement_basis(npt__be_t1990, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(npt__be_t2020, observed).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(npt__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(npt__su_t1990, observed).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(npt__su_t2020, observed).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(npt__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The NPT kernel (1970) supports three structurally distinct constraint readings. The reciprocal-disarmament reading instantiated here interprets Article VI as binding with temporal force. The oligopoly-enforcement reading (sibling) interprets Articles I-II as binding and Article VI as contingent, treating the regime as enforcement of NWS advantage. The withdrawal-sovereignty reading (sibling) emphasizes Article X as a legitimate sovereignty mechanism and treats obligations as contingent on security environment. All three readings share the same kernel text but produce different ε values, different beneficiary/victim structures, and different classifications. Each story is independently authored; the network edges document the family relationship and allow the analysis engine to compute cross-reading contamination (if one reading's purity degrades, how do the others respond).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
