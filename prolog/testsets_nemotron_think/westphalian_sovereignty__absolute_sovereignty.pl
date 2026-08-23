% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The absolute sovereignty reading asserts that states possess
 *   unconditional authority over their domestic affairs and that external
 *   interference is categorically illegitimate. This reading traces to the
 *   1648 Peace of Westphalia but was substantially constructed in
 *   19th-century European state practice and codified in UN Charter Article
 *   2(4) and 2(7). The constraint operates as a tangled rope: it coordinates
 *   international order by establishing clear jurisdictional boundaries
 *   (preventing endless intervention wars) while simultaneously extracting
 *   impunity for domestic repression — authoritarian regimes are the primary
 *   beneficiaries, their domestic populations the primary victims. The
 *   coordination function (interstate peace) is real but has been layered
 *   with extractive rent-seeking (regime protection). The claimed type is
 *   tangled_rope; the metrics show rising extraction and theater over the
 *   post-1945 period as the human rights regime created pressure that the
 *   absolute reading defends against through performative sovereignty
 *   rituals.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda_setter (institutional/generational/arbitrage/global) — sets and enforces the non-interference rule through UNSC vetoes, treaty regimes, diplomatic practice
 *   - authoritarian_regimes: Primary beneficiary (powerful/biographical/constrained/national) — gains impunity for domestic repression behind the sovereignty shield
 *   - domestic_populations_under_repression: Primary victim (powerless/biographical/trapped/national) — bears the extraction (rights violations, no external recourse)
 *   - oppressed_minorities: Victim (powerless/biographical/identity_locked/national) — identity-locked to territory, exit blocked by both regime and sovereignty norm
 *   - dissidents: Victim (powerless/immediate/trapped/local) — direct targets of repression, no exit
 *   - stateless_persons: Victim (powerless/biographical/trapped/universal) — fall through sovereignty cracks entirely
 *   - human_rights_ngos: Excluded (organized/generational/mobile/global) — would challenge but structurally excluded from UNSC decision-making
 *   - international_courts: Observer (institutional/generational/analytical/global) — ICJ, ICC interpret but lack enforcement
 *   - powerful_states: Agenda_setter (institutional/generational/arbitrage/global) — enforce sovereignty selectively; intervene when convenient, invoke sovereignty when not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'fff512b1-43f7-48a0-aabb-a8a72e6f5a38').
narrative_ontology:cs_kernel_codification('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', formalized).
narrative_ontology:cs_authority_grounding('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', lineage).
narrative_ontology:cs_interpretation_layer_present('fff512b1-43f7-48a0-aabb-a8a72e6f5a38').
narrative_ontology:cs_reading_relation('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', foundational, sovereignty_unconditional).
narrative_ontology:cs_axiom_status(sovereignty_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', sovereignty_unconditional, conventional).
narrative_ontology:cs_axiom('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', foundational, non_interference_categorical).
narrative_ontology:cs_axiom_status(non_interference_categorical, holdable).
narrative_ontology:cs_axiom_grounding('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', non_interference_categorical, conventional).
narrative_ontology:cs_reference_frame('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', westphalian_territorial_integrity).
narrative_ontology:cs_drift_state('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', post_r2p_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fff512b1-43f7-48a0-aabb-a8a72e6f5a38', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, sovereign_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, powerful_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, oppressed_minorities).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, dissidents).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, stateless_persons).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, non_interference_principle).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, territorial_integrity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively constitute the international legal order; set the sovereignty rules through UN Charter, customary law, diplomatic practice. Benefit from stable borders and non-interference. Can exit by withdrawing from treaties or ignoring norms (arbitrage-grade exit for powerful states, constrained for weak states). Administer the constraint through UNSC vetoes and treaty bodies.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, sovereign_states, beneficiary).

% Exploit the sovereignty shield to repress domestic populations without external accountability. Gain impunity for human rights violations. Constrained exit: they cannot leave the sovereign state system without losing power; they depend on the very norm they exploit. Some (North Korea, Myanmar) are more trapped; others (China, Russia) have arbitrage-grade exit via UNSC veto.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    powerful, biographical, constrained, national).

% Bear the full cost of repression (violence, displacement, impoverishment) with no external recourse. The sovereignty norm blocks intervention, asylum access, ICC referral. Exit is physically trapped (closed borders) and normatively trapped (sovereignty makes their suffering a domestic matter). No coalition power — fragmented, surveilled, repressed.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, biographical, trapped, national).

% Targeted repression based on identity (ethnic, religious, sexual, political). Identity-locked: their self-concept is fused to the territory and community; exit means abandoning identity. Sovereignty norm denies them both protection and self-determination. The constraint extracts their security and political agency for regime stability.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, oppressed_minorities, payer,
    powerless, biographical, identity_locked, national).

% Direct targets of state repression (imprisonment, torture, killing). Immediate time horizon: survival is daily. Exit options: prison, exile, death. Sovereignty shield means no diplomatic protection, no humanitarian corridor, no ICC arrest warrant execution. The constraint extracts their lives for regime continuity.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, dissidents, payer,
    powerless, immediate, trapped, local).

% Fall through the cracks of the sovereignty system entirely — no state claims them, all states exclude them. The sovereignty norm that protects state authority also denies them rights. Universal scope: statelessness is a global condition produced by the sovereign state system. Trapped: no state to exit to, no state to protect them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, stateless_persons, payer,
    powerless, biographical, trapped, universal).

% Document abuses, advocate for intervention, litigate in international courts. Structurally excluded from UNSC decision-making where sovereignty enforcement happens. Mobile exit: can operate across borders but cannot access the enforcement node. Their voice is the absent_voices in Q4 — they would object to absolute sovereignty but are not in the room where vetoes are cast.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_ngos, excluded,
    organized, generational, mobile, global).

% ICJ, ICC, regional courts interpret sovereignty and its limits. ICJ has upheld absolute sovereignty in some cases (Nicaragua v. USA) but also recognized erga omnes obligations. ICC can prosecute but lacks enforcement — depends on state cooperation. Analytical seat: they see the full structure but neither collect nor pay. Their judgments map the constraint's boundaries.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_courts, observer,
    institutional, generational, analytical, global).

% P5 UNSC members + other great powers. Enforce sovereignty selectively: invoke it to block intervention in their spheres (Russia in Syria, China in Xinjiang, USA in Iraq) while intervening elsewhere (Kosovo, Libya, Iraq 2003). Benefit from the coordination function (stable great power relations) while extracting impunity for their own interventions. Arbitrage-grade exit: they can ignore the norm when convenient because they control the enforcement machinery.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, powerful_states, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international order by establishing clear jurisdictional boundaries between states, preventing endless intervention wars, and providing a stable framework for diplomacy, trade, and treaty-making. Solves the Hobbesian problem of the international state of nature.
% TRANSFER_FUNCTION: Transfers impunity for domestic repression from accountability to state authority; moves the protection burden from populations to regimes. The extraction flows from repressed populations (who lose rights, security, recourse) to authoritarian regimes (who gain impunity) and powerful states (who gain non-interference for their spheres of influence). The coordination gain (interstate peace) is distributed broadly; the extraction gain (regime protection) is concentrated.
% ABSENT_VOICES: Domestic populations under repression, stateless persons, future generations who will inherit the sovereignty system, and diaspora communities who cannot vote in the UN. They are structurally excluded: no seat at UNGA, no veto at UNSC, no standing at ICJ. Their absence is not accidental — the sovereignty norm defines them as internal matters, not international subjects.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished overnight, the UNSC veto system would lose its primary justification, R2P would become the default framework, ICC jurisdiction would become universal, humanitarian intervention would be normalized, and authoritarian regimes would face immediate accountability pressure. The international order would reorganize around conditional sovereignty — the world would rearrange fundamentally.
% FOUNDING_PROBLEM: The post-1648 need to end European religious wars by establishing territorial integrity and non-interference as the basis for interstate coexistence. The Peace of Westphalia created a system where cuius regio, eius religio became cuius regio, eius imperium — the ruler's authority within territory became absolute to prevent external religious intervention.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars outside state beneficiaries (Antonio Cassese, James Crawford, Martti Koskenniemi) attest the founding problem was interstate war prevention, not domestic impunity. State practice since 1945 (UN Charter, Genocide Convention, human rights treaties, R2P) shows contested evolution: the founding problem was substantially solved for interstate war but the arrangement persisted and expanded into domestic impunity. No non-beneficiary scholar asserts the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is substantial: the sovereignty shield transfers the cost of repression from regimes to populations. Suppression (0.75) is high: the constraint persists through active enforcement (UNSC vetoes, non-intervention treaties, diplomatic pressure, military deterrence). Theater (0.42) is rising: sovereignty rituals (UNGA speeches, treaty ratifications) increasingly perform coordination while extraction continues. Accessibility collapse (0.72) is high for victims: once the sovereignty frame is accepted, alternatives (intervention, R2P, ICC referral) appear illegitimate or impossible. Resistance (0.58) is moderate: human rights movement, R2P, ICC create friction but have not displaced the core norm. The measurement series on a shared grid (0-100, normalized 1648-2024) shows extraction and theater rising together post-1945 — Goodhart drift where sovereignty performance substitutes for genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent per-seat types: from the sovereign_state/agenda_setter seat, the constraint appears as Rope (genuine coordination of interstate order). From the authoritarian_regime/beneficiary seat, it appears as Snare (pure extraction of impunity). From the domestic_population/payer seat, it appears as Snare (extraction with no coordination benefit). From the powerful_state seat, it appears as Tangled Rope (coordination for them, extraction for others). This divergence IS the measurement — the claimed tangled_rope captures the structural hybridity but the engine quantifies how each seat experiences it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and powerful states are structural beneficiaries (d ≈ 0.15-0.25): they collect the coordination gains (stable borders, non-interference) and control enforcement. Authoritarian regimes are concentrated beneficiaries (d ≈ 0.1): they extract maximum impunity. Domestic populations, minorities, dissidents, stateless persons are targets (d ≈ 0.85-0.95): they bear repression costs with trapped/identity_locked exit. Human rights NGOs are excluded (d ≈ 0.6): they pay advocacy costs but cannot access decision nodes. International courts are analytical observers (d = 0.5): they interpret but do not collect or pay. The derivation chain: beneficiary/victim declarations + power level + exit options → directionality. Overrides used for powerful_states (derived d would be ~0.15 as institutional beneficiary, but their selective intervention makes them partial targets too → override to 0.35).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending religious/interstate wars) is contested: states claim it remains live; human rights scholars argue it was substantially solved by 1945 and the arrangement now persists as mandate atrophy. The sovereignty shield has outlived its interstate peace function and now primarily serves regime protection — classic mandatrophy. The classification prevents mislabeling: calling this pure Snare ignores the genuine coordination function (interstate peace); calling it pure Rope ignores the asymmetric extraction (regime impunity). Tangled Rope captures both: the coordination is real but the extraction is structural and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the absolute sovereignty reading a genuine instantiation of the Westphalian kernel, or a constructed expansion that serves extractive state interests?',
    'Genealogical analysis of Westphalian texts vs. 19th-20th century state practice; compare what the 1648 treaties actually established vs. what later state practice claimed they established.',
    'If constructed expansion, the constraint''s claimed Mountain/Rope framing collapses; the high ε reflects rent-seeking by states, not coordination necessity. Reclassifies from contested coordination to extractive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether absolute sovereignty is the kernel''s original meaning or a later state-serving construction').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the non-interference principle a structural feature of the international system (like gravity) or a human-made convention maintained by power?',
    'Counterfactual: if all states ceased enforcing non-interference tomorrow, would the principle persist? Historical test: did non-interference survive the Concert of Europe, the League, the UN Charter revisions?',
    'If constructed convention, the constraint cannot be Mountain; high suppression and beneficiary structure indicate Tangled Rope or Snare. If natural structural feature, extraction metrics must be reinterpreted as coordination costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Mountain vs. constructed constraint ambiguity — core FSM trigger').

omega_variable(
    enforcement_mechanism_asymmetry,
    'Does the enforcement of non-interference operate symmetrically (all states equally constrained) or asymmetrically (powerful states intervene selectively while invoking sovereignty for themselves)?',
    'Dataset of interventions 1945-present: code by intervener power, target power, UNSC authorization, sovereignty invocation. Measure asymmetry index.',
    'Asymmetric enforcement means the coordination function is cover for powerful-state extraction; the constraint is Snare for weak states, Rope for powerful ones. Engine would compute per-seat type divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_asymmetry, empirical, 'Symmetry of enforcement — determines whether coordination is genuine or cover').

omega_variable(
    r2p_as_drift_or_new_kernel,
    'Is Responsibility to Protect (R2P) a drift within the Westphalian kernel (authority_erosion) or a rival kernel that forecloses absolute sovereignty?',
    'Analyze UNGA resolutions, UNSC practice, ICJ opinions: does R2P language treat sovereignty as conditional (forecloses absolute) or as responsibility-entailing (coexists with modified absolute)?',
    'If forecloses: absolute_sovereignty reading is structurally displaced, not just eroded. If coexists_with: both readings remain live in different institutional seats. Engine computes foreclosure from axiom contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_as_drift_or_new_kernel, conceptual, 'R2P''s structural relationship to the absolute sovereignty reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of external intervention structural (UNSC veto, treaty law, military deterrence) or internalized (states self-censor, populations accept impunity as order)?',
    'Post-intervention suppression trajectory: when intervention occurs (Kosovo, Libya), does sovereignty discourse collapse or rebound? Measure discursive recovery time.',
    'If internalized, effective suppression exceeds structural measure — populations and NGOs carry the constraint internally. Engine''s χ computation would understate extraction for identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in sovereignty enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsa_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.05).
narrative_ontology:measurement(wsa_tr_t25, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 25, 0.12).
narrative_ontology:measurement(wsa_tr_t50, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 50, 0.25).
narrative_ontology:measurement(wsa_tr_t75, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 75, 0.35).
narrative_ontology:measurement(wsa_tr_t90, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 90, 0.38).
narrative_ontology:measurement(wsa_tr_t95, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 95, 0.4).
narrative_ontology:measurement(wsa_tr_t100, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(wsa_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wsa_be_t25, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(wsa_be_t50, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(wsa_be_t75, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(wsa_be_t90, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 90, 0.48).
narrative_ontology:measurement(wsa_be_t95, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 95, 0.5).
narrative_ontology:measurement(wsa_be_t100, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wsa_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(wsa_su_t25, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(wsa_su_t50, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(wsa_su_t75, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 75, 0.7).
narrative_ontology:measurement(wsa_su_t90, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 90, 0.73).
narrative_ontology:measurement(wsa_su_t95, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 95, 0.74).
narrative_ontology:measurement(wsa_su_t100, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, r2p_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_norm).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, icc_jurisdiction).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, universal_jurisdiction_principle).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, refugee_protection_regime).

% DUAL FORMULATION NOTE:
% Westphalian sovereignty kernel decomposes into three readings: absolute_sovereignty (this story, high ε tangled_rope), conditional_sovereignty (lower ε rope/scaffold), graduated_sovereignty (variable ε by state). This story's high ε reflects the extraction layer authoritarian regimes add onto the coordination kernel. The conditional and graduated readings represent attempts to strip the extraction layer while preserving coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, institutional, 0.35).
constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
