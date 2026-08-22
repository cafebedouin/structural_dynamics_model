% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Non-Proliferation Binding on NNWS; NWS Disarmament Aspirational (NWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (1968) establishes a bargain: NNWS renounce
 *   nuclear weapons permanently; NWS commit to disarm 'at an early date'
 *   under Article VI. This story instantiates ONE reading of the contested
 *   NPT kernel — the NWS reading — in which Article VI's disarmament
 *   commitment is understood as aspirational long-term goal without binding
 *   timeline or verification, while Article II (NNWS non-proliferation) is
 *   binding, enforced, and asymmetric. Under this reading, the constraint
 *   extracts from NNWS by binding them to permanent renunciation while
 *   leaving NWS with indefinite arsenal retention and modernization rights.
 *   The constraint benefits NWS and their strategic hierarchy (the P5
 *   Security Council permanent members); it extracts from NNWS and threshold
 *   technology states. The NWS reading interprets 'early date' as compatible
 *   with indefinite deferral, making the entire 'disarmament' obligation
 *   toothless while keeping non-proliferation teeth sharp.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states (P5): agenda-setters, institutional power, define Article VI as non-binding aspiration; set IAEA verification budget and priorities
 *   - non_nuclear_weapon_states (188 signatories): payers, organized power, bound by Article II permanently, submit to asymmetric verification
 *   - threshold_technology_states (Japan, Germany, South Korea, Iran, etc.): payers with identity lock, powerful but constrained, carry the heaviest long-term regime instability risk
 *   - IAEA Secretariat: enforcement layer, controlled budget, asymmetric inspection authority (NNWS only)
 *   - disarmament advocacy coalitions: excluded, moderate power, advocacy that never reaches binding consensus
 *   - Security Council permanent members: beneficiaries, derive strategic authority from NWS status + P5 seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.72).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Non-Proliferation Binding on NNWS; NWS Disarmament Aspirational (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'd3abae9f-9f67-49ba-ae5f-6e77aa959d7e').
narrative_ontology:cs_kernel_codification('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', fixed_text).
narrative_ontology:cs_authority_grounding('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', lineage).
narrative_ontology:cs_interpretation_layer_present('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e').
narrative_ontology:cs_reading_relation('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', foundational, disarmament_aspirational_indefinite).
narrative_ontology:cs_axiom_status(disarmament_aspirational_indefinite, holdable).
narrative_ontology:cs_axiom_grounding('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', disarmament_aspirational_indefinite, conventional).
narrative_ontology:cs_axiom('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', foundational, nws_deterrence_stability_constraint_binding).
narrative_ontology:cs_axiom_status(nws_deterrence_stability_constraint_binding, holdable).
narrative_ontology:cs_axiom_grounding('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', nws_deterrence_stability_constraint_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', nws_strategic_hierarchy_1968).
narrative_ontology:cs_drift_state('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', contemporary_post_2000_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3abae9f-9f67-49ba-ae5f-6e77aa959d7e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, security_council_permanent_members).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, threshold_technology_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, iaea_secretariat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NWS (US, Russia, UK, France, China) set the interpretive frame of Article VI: they define 'disarmament at an early date' as indefinite postponement compatible with 'nuclear deterrence stability.' They control the NPT Review Conference agendas, block binding disarmament timelines, and enforce non-proliferation verification on NNWS through IAEA mechanisms they fund and oversee. They retain the right to upgrade arsenals under modernization doctrine while demanding NNWS renounce nuclear options permanently.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Bear binding non-proliferation obligations under Article II without reciprocal NWS obligation to disarm on any specified timeline. Submit to IAEA safeguards inspections that concentrate on horizontal proliferation detection while NWS arsenals remain opaque to international verification. Cannot develop nuclear deterrence as a rational security option despite living in regions where adversaries possess nuclear weapons or are NWS themselves. Withdrawal from the NPT creates diplomatic isolation and sanctions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% States like Japan, Germany, South Korea, Iran with advanced nuclear fuel cycles or weapons-adjacent technical capacity. Bound by non-proliferation commitments even when conventional threats are severe (North Korea, regional rivals). Their technical capacity to develop weapons is the exact reason the constraint exists — but the constraint's indefinite persistence without NWS disarmament creates long-term incentive misalignment. Excluded from disarmament conferences and strategic dialogue; their voice in NPT Review Conferences is heard but not decisive.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, threshold_technology_states, payer,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, threshold_technology_states, excluded).

% Enforces non-proliferation through safeguards inspections and technical verification of NNWS compliance. Operates under NWS-dominated funding (budget pressure from verification demands exceeds available resources). Cannot inspect NWS arsenals; its authority is asymmetric. Directors-General who call for NWS disarmament verification face funding cuts and renewal obstacles. Administers the constraint's enforcement layer for NNWS while structurally forbidden from applying reciprocal scrutiny to NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_secretariat, payer).

% States considering or pursuing nuclear weapons (Middle East, South Asia, potentially others) are structurally excluded from the NPT conversation about disarmament because they do not yet sit at the table as either NWS or signatories. Their exclusion is the enforcement object: the constraint exists to prevent them from ever reaching NWS status. If they do acquire weapons, they operate outside the NPT regime entirely (India, Pakistan, Israel) or threaten regime collapse by withdrawal (North Korea).
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, threshold_aspiring_states, excluded,
    moderate, biographical, trapped, regional).

% NGOs, humanitarian coalitions, and NNWS disarmament platforms call for binding NWS timelines and reciprocal verification. They remain outside the mechanism that produces NWS compliance: their advocacy influences NNWS Review Conference positions but cannot compel NWS reinterpretation of Article VI. The constraint's text nominally supports their goal (disarmament) but the NWS reading drains the goal of enforceability.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocacy_coalitions, excluded,
    moderate, generational, constrained, global).

% The P5 derive strategic authority from permanent Security Council membership coupled with NWS status. The NPT regime, under this reading, institutionalizes their authority by binding NNWS to non-proliferation while giving P5 indefinite freedom to maintain/modernize arsenals. The regime legitimizes their exceptional status globally and within UN structures.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, security_council_permanent_members, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Convene every five years to assess treaty compliance and agree on forward action. Under the NWS reading, Review Conferences produce consensus statements that reaffirm Article VI's 'disarmament at an early date' language while permitting indefinite NWS deferral of that obligation. The conference mechanism ratifies the asymmetry rather than compelling symmetry. The phrase is recycled every five years with no binding timeline or verification mechanism; this cyclical reaffirmation without enforcement is the theater that sustains the constraint.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, npt_review_conferences, agenda_setter,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear proliferation spread beyond five identified NWS by binding 188 NNWS to permanent renunciation, verified through IAEA safeguards. Solves a coordination problem: without the treaty, nuclear technology diffusion would accelerate and more regional powers would weaponize; with it, most states agree to forgo weapons. Also coordinates a strategic hierarchy: NWS retain deterrent arsenals while NNWS accept permanent asymmetry in exchange for security assurances and peaceful uses of nuclear energy.
% TRANSFER_FUNCTION: Moves security authority from states to the permanent Security Council: NNWS trade independent nuclear deterrence options for a P5-provided nuclear umbrella (extended deterrence for allies) or Security Council protection (theoretically). Moves technical verification capacity from NNWS to the IAEA, which inspects NNWS fuel cycles but cannot reciprocally inspect NWS arsenals. Moves disarmament commitment from binding obligation (Article VI) to aspirational long-term goal with indefinite deferral, extracting compliance from NNWS on a non-reciprocal basis.
% ABSENT_VOICES: Nuclear-armed non-signatories (India, Pakistan, Israel, North Korea) are structurally absent: they rejected the asymmetric regime or withdrew from it. Disarmament advocacy movements are in the room but do not control the consensus-building process; their interpretation of Article VI as binding never reaches binding status at Review Conferences. Threshold technology states participate but their voice that non-proliferation without reciprocal disarmament creates long-term regime instability is structurally subordinate to NWS consensus-seeking.
% DISAPPEARANCE_RATIONALE: If the NPT constraint disappeared overnight, the NNWS verification regime would collapse immediately; NNWS would accelerate nuclear programs; regional powers would weaponize more rapidly; Security Council authority would fragment; the permanent hierarchy of the P5 would no longer have a treaty-based legitimizing text. The world would reorganize around multiple emerging NWS, regional deterrence balances, and contested strategic authority.
% FOUNDING_PROBLEM: The 1960s saw rapid nuclear technology diffusion and the risk that dozens of states would weaponize. The constraint was built to prevent a multipolar nuclear world and preserve the P5's strategic monopoly by binding NNWS to non-proliferation in exchange for disarmament 'at an early date' (a commitment made by NWS at signature).
% FOUNDING_PROBLEM_CORROBORATION: The NWS reading asserts the horizontal proliferation problem persists (Iran, North Korea near-successes justify continued non-proliferation focus). NNWS and disarmament coalitions attest that the founding problem has been substantially solved — NPT verification prevented many states from weaponizing — but that the quid pro quo (NWS disarmament) has been abandoned, turning the regime from a temporary coordination into permanent asymmetric extraction. Independent analysis (academic arms control studies, NGO reports outside NWS circles) corroborates that Article VI disarmament timelines have been indefinitely deferred while non-proliferation remains binding.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78 at interval end) reflects that NNWS bear binding non-proliferation obligations while NWS retain indefinite arsenal rights — a non-reciprocal extraction from the NNWS seat. The suppression (0.72) is high because NNWS cannot exit the regime (withdrawal costs exceed staying; staying costs include verification intrusion + permanent strategic inferiority). The theater ratio (0.41 at end) captures that NPT Review Conferences spend significant effort reaffirming 'disarmament at an early date' language in consensus communiqués while no binding mechanism enforces it — the language persists performatively, not operationally. The measurement series over 51 years (NPT signature 1968 to 2019) shows extraction rising steeply from 1968–1980 (Cold War détente → renewed arms race), plateauing after ~2000 as the indefinite deferral becomes normalized. Theater_ratio rises through the interval as Review Conferences become more focused on restating the disarmament pledge without advancing it. Suppression_requirement plateaus after ~2000 because the NNWS regime is now locked in (verification infrastructure stable, exit costs well-understood); maintaining the constraint requires steady suppression but no escalation. The coercion grid shows structural-level suppression (P5 veto, treaty text control) is highest; individual-level suppression (state-level pressure, diplomatic isolation for withdrawal) is lower but still substantial. Resistance is consistent across the interval: NNWS and disarmament coalitions resist indefinite deferral throughout, but their resistance does not shift NWS interpretation because the NWS hold agenda-setting power.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, this is tangled_rope: genuine coordination (prevented proliferation spread) coupled with asymmetric extraction (preserved NWS strategic monopoly). The NWS frame disarmament as long-term aspiration because immediate disarmament would sacrifice their deterrent advantage; they frame non-proliferation as binding because it preserves their unique status. From the NNWS seat, particularly for threshold technology states, this reads as snare: permanent renunciation in exchange for security assurances (extended deterrence) that can be withdrawn unilaterally (the NWS can leave the regime or redefine 'early date' indefinitely). The payer seats compute higher extractiveness; the beneficiary seat computes lower. The agenda-setter (NWS) controls the interpretation, so the NWS reading is what binds legally; the NNWS reading (that disarmament is binding and non-proliferation is conditional) never reaches binding status at Review Conferences. The engine should compute this divergence from the structural data: NWS as beneficiary (arbitrage exit, institutional power) vs NNWS as target (constrained/identity-locked exit, organized power but subordinate to P5 agenda).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure is clear: NWS benefit from indefinite arsenal retention + NNWS permanent renunciation. The victim structure is the entire NNWS class, especially threshold technology states. The IAEA occupies a dual position: it administers enforcement (payer in terms of effort + budget pressure to verify NNWS only) and sets the agenda (what non-proliferation means operationally). From the engine's perspective: NWS directionality ≈ 0.1 (beneficiary, arbitrage exit, institutional power — low effective extraction to them). NNWS directionality ≈ 0.85 (target, constrained exit, organized power but subordinate agenda-setting power — high effective extraction from them). Threshold technology states ≈ 0.88 (target, identity-locked exit because their technical capacity defines their strategic relevance; they cannot escape being the focus of horizontal proliferation worry; high effective extraction). IAEA ≈ 0.65 (intermediate: administers extraction asymmetrically, dependent on NWS funding, cannot reciprocally inspect NWS). The directionality derivation is clean from the structural data: beneficiary/victim + power + exit_options produce the d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Article VI's mandated goal is disarmament 'at an early date.' The NWS reading has indefinitely deferred this mandate: 'early date' is interpreted as compatible with indefinite postponement contingent on 'nuclear deterrence stability' — a concept only NWS can define. The mandate is functionally obsolete (dead) under this reading, but the Treaty persists because non-proliferation (Article II) has become the operational focus and benefits NWS. The constraint exhibits mandatrophy: the founding problem (preventing proliferation in the 1960s when nuclear technology was diffusing) has been substantially solved by non-proliferation verification, but the disarmament mandate (the price NNWS were supposed to receive in exchange) has been shelved indefinitely. The theater ratio rising toward 0.41 signals this: Review Conferences spend resources reaffirming disarmament language that has no enforcement mechanism, maintaining the theatrical appearance of the quid pro quo while the NWS unilaterally abandon the disarmament half. The constraint persists not because the original mandate is live, but because non-proliferation extraction continues to benefit NWS and the P5 strategic hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    early_date_semantic_stability,
    'Is ''at an early date'' in Article VI inherently vague, or does the preparatory conference record (travaux préparatoires) establish a binding timeline the NWS reading ignores?',
    'Forensic analysis of NPT negotiation records (1965–1968) by independent legal scholars; comparison with how similar temporal language in other treaties has been interpreted (ICJ precedent review).',
    'If the preparatory record specifies ''early date'' as 10–15 years, the NWS reading is a deliberate misinterpretation, not a good-faith ambiguity resolution. If preparatory records show negotiators themselves disagreed, the ambiguity is genuine and the reading''s interpretation is one defensible frame among others. This affects whether the NWS reading''s interpretation is legitimate authority_grounding or post-hoc drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_date_semantic_stability, conceptual, 'Whether ''early date'' is genuinely ambiguous or deliberately drained of meaning by the NWS reading.').

omega_variable(
    nuclear_deterrence_stability_doctrine_empirical_status,
    'Does the coupling of disarmament to ''nuclear deterrence stability'' describe a real strategic constraint, or is it post-hoc justification for indefinite NWS arsenal retention?',
    'Comparative analysis of NWS disarmament proposals pre-1970 vs. post-1980: did the stability doctrine emerge when verification technology improved (suggesting it was the true constraint), or did it emerge after NWS chose not to disarm (suggesting it was retroactive justification)? Do non-NWS strategic analysts accept the doctrine''s constraints as real?',
    'If the doctrine is real and shared (e.g., Russian and US strategists both affirm it), disarmament may be genuinely constrained by verification and strategic risk — and the NWS reading is a realistic assessment. If the doctrine is post-hoc and contested (disarmament advocates argue it is unnecessary), the NWS reading is a rationalization for extraction. This affects whether the constraint is tangled_rope (real coordination + asymmetric extraction) or snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deterrence_stability_doctrine_empirical_status, empirical, 'Whether the NWS coupling of disarmament to deterrence stability is a genuine strategic requirement or a cover story.').

omega_variable(
    threshold_state_regime_instability_timeline,
    'What is the long-term endpoint of indefinite NPT persistence without NWS disarmament? Do threshold technology states eventually withdraw and weaponize, or does the regime remain stable with non-proliferation binding indefinitely?',
    'Scenario modeling based on threshold-state strategic incentives: at what point does the cost of forgoing nuclear deterrence exceed the cost of withdrawal + sanctions? Does Iran, Japan, Germany show evidence of shifting calculus after 2015, 2022 (near-term observation)? Do nuclear weapons programs in threshold states accelerate when NWS modernization is visible?',
    'If threshold states remain bound indefinitely (regime stable), the NWS reading is a sustainable constraint. If threshold states eventually defect (regime unstable), the NWS reading was a transient extraction that will reverse when the regime collapses. This affects the terminal type classification: tangled_rope assumes cooperation is stable; if cooperation is unstable, the structure is actually a snare that will fail.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_state_regime_instability_timeline, empirical, 'Whether indefinite non-proliferation without reciprocal NWS disarmament creates a stable equilibrium or an unstable one.').

omega_variable(
    iaea_verification_asymmetry_structural_necessity,
    'Is IAEA asymmetric inspection authority (NNWS only, not NWS) a necessary technical feature of verification, or a structural choice that could be changed?',
    'Feasibility analysis: could IAEA technically verify NWS arsenals under new agreements? Do NWS refuse on sovereignty grounds or technical impossibility grounds? Have any NWS ever submitted to international inspection of warhead stockpiles?',
    'If asymmetry is technically necessary (warhead design classification, security risks), then the NWS reading reflects real constraints. If asymmetry is a choice (and some NWS could technically accept inspection), then the NWS reading''s beneficiary structure is a political choice, not a technical requirement. This affects the extraction story: extraction by necessity vs. extraction by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iaea_verification_asymmetry_structural_necessity, empirical, 'Whether IAEA verification asymmetry is technically necessary or politically chosen.').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Is the NWS reading logically foreclosed by a different reading (e.g., the NNWS reading that disarmament is binding), or do both readings coexist as live positions held by different parties?',
    'Formal logical analysis: do the core premises of the two readings contradict each other such that no single framework could hold both? Or can both readings remain live within the same treaty text, held by different institutional actors?',
    'If foreclosed, one reading is definitively wrong and the engine should mark it as overridden. If coexistent, both readings are live and the engine should mark them as part of an ongoing interpretive contest. This affects how the engine handles reading_relations: do we use ''forecloses'' or ''coexists_with''?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, conceptual, 'Whether the NWS reading and NNWS reading are logically foreclosed or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 0, 51).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__nws_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(npt__tr_t7, npt_treaty_text__nws_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(npt__tr_t14, npt_treaty_text__nws_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(npt__tr_t21, npt_treaty_text__nws_reading, theater_ratio, 21, 0.32).
narrative_ontology:measurement(npt__tr_t28, npt_treaty_text__nws_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement(npt__tr_t35, npt_treaty_text__nws_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(npt__tr_t42, npt_treaty_text__nws_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement(npt__tr_t51, npt_treaty_text__nws_reading, theater_ratio, 51, 0.41).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt__be_t7, npt_treaty_text__nws_reading, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(npt__be_t14, npt_treaty_text__nws_reading, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(npt__be_t21, npt_treaty_text__nws_reading, base_extractiveness, 21, 0.7).
narrative_ontology:measurement(npt__be_t28, npt_treaty_text__nws_reading, base_extractiveness, 28, 0.75).
narrative_ontology:measurement(npt__be_t35, npt_treaty_text__nws_reading, base_extractiveness, 35, 0.77).
narrative_ontology:measurement(npt__be_t42, npt_treaty_text__nws_reading, base_extractiveness, 42, 0.78).
narrative_ontology:measurement(npt__be_t51, npt_treaty_text__nws_reading, base_extractiveness, 51, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(npt__su_t7, npt_treaty_text__nws_reading, suppression_requirement, 7, 0.54).
narrative_ontology:measurement(npt__su_t14, npt_treaty_text__nws_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(npt__su_t21, npt_treaty_text__nws_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(npt__su_t28, npt_treaty_text__nws_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(npt__su_t35, npt_treaty_text__nws_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(npt__su_t42, npt_treaty_text__nws_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(npt__su_t51, npt_treaty_text__nws_reading, suppression_requirement, 51, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=51
narrative_ontology:measurement(npt__grid_01, npt_treaty_text__nws_reading, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(npt__grid_02, npt_treaty_text__nws_reading, accessibility_collapse(class), 51, 0.58).
narrative_ontology:measurement(npt__grid_03, npt_treaty_text__nws_reading, accessibility_collapse(individual), 0, 0.38).
narrative_ontology:measurement(npt__grid_04, npt_treaty_text__nws_reading, accessibility_collapse(individual), 51, 0.52).
narrative_ontology:measurement(npt__grid_05, npt_treaty_text__nws_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(npt__grid_06, npt_treaty_text__nws_reading, accessibility_collapse(organizational), 51, 0.62).
narrative_ontology:measurement(npt__grid_07, npt_treaty_text__nws_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(npt__grid_08, npt_treaty_text__nws_reading, accessibility_collapse(structural), 51, 0.68).
narrative_ontology:measurement(npt__grid_09, npt_treaty_text__nws_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(npt__grid_10, npt_treaty_text__nws_reading, resistance(class), 51, 0.68).
narrative_ontology:measurement(npt__grid_11, npt_treaty_text__nws_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(npt__grid_12, npt_treaty_text__nws_reading, resistance(individual), 51, 0.62).
narrative_ontology:measurement(npt__grid_13, npt_treaty_text__nws_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(npt__grid_14, npt_treaty_text__nws_reading, resistance(organizational), 51, 0.76).
narrative_ontology:measurement(npt__grid_15, npt_treaty_text__nws_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(npt__grid_16, npt_treaty_text__nws_reading, resistance(structural), 51, 0.72).
narrative_ontology:measurement(npt__grid_17, npt_treaty_text__nws_reading, stakes_inflation(class), 0, 0.42).
narrative_ontology:measurement(npt__grid_18, npt_treaty_text__nws_reading, stakes_inflation(class), 51, 0.64).
narrative_ontology:measurement(npt__grid_19, npt_treaty_text__nws_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(npt__grid_20, npt_treaty_text__nws_reading, stakes_inflation(individual), 51, 0.56).
narrative_ontology:measurement(npt__grid_21, npt_treaty_text__nws_reading, stakes_inflation(organizational), 0, 0.46).
narrative_ontology:measurement(npt__grid_22, npt_treaty_text__nws_reading, stakes_inflation(organizational), 51, 0.68).
narrative_ontology:measurement(npt__grid_23, npt_treaty_text__nws_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(npt__grid_24, npt_treaty_text__nws_reading, stakes_inflation(structural), 51, 0.74).
narrative_ontology:measurement(npt__grid_25, npt_treaty_text__nws_reading, suppression(class), 0, 0.35).
narrative_ontology:measurement(npt__grid_26, npt_treaty_text__nws_reading, suppression(class), 51, 0.68).
narrative_ontology:measurement(npt__grid_27, npt_treaty_text__nws_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(npt__grid_28, npt_treaty_text__nws_reading, suppression(individual), 51, 0.62).
narrative_ontology:measurement(npt__grid_29, npt_treaty_text__nws_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(npt__grid_30, npt_treaty_text__nws_reading, suppression(organizational), 51, 0.72).
narrative_ontology:measurement(npt__grid_31, npt_treaty_text__nws_reading, suppression(structural), 0, 0.44).
narrative_ontology:measurement(npt__grid_32, npt_treaty_text__nws_reading, suppression(structural), 51, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_verification_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nuclear_deterrence_strategic_stability).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, security_council_p5_hierarchy).

% DUAL FORMULATION NOTE:
% The NPT kernel decomposes into three constraint stories per the ε-invariance principle. This story (nws_reading) treats disarmament as non-binding aspirational and non-proliferation as binding extraction. The sibling nnws_reading treats disarmament as binding and non-proliferation as conditional — different ε (higher at the NNWS seat, lower at the NWS seat). The withdrawal_threshold_reading examines Article X (withdrawal rights) as a separate kernel with competing interpretations. All three are linked via network.affects_constraints. The ε difference between this reading and nnws_reading is substantial (this reading has high extractiveness because it enables indefinite NWS arsenal retention while binding NNWS; the NNWS reading would have lower extractiveness at the NWS seat because it would require binding NWS timelines). Each reading has its own beneficiary/victim structure reflecting the reading's interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
