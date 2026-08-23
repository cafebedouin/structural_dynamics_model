% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Non-Proliferation-Primary Reading: Conditional Article IV, Non-Justiciable Article VI
 *   domain: international law / nuclear governance / treaty interpretation
 *
 * SUMMARY:
 *   Under the nonproliferation_primary reading, the NPT is a compact whose
 *   operative core is preventing horizontal proliferation: Article III
 *   verification is the regime's real machinery, Article IV's peaceful-energy
 *   rights are conditional licenses granted against safeguards compliance
 *   rather than inalienable entitlements, and Article VI's disarmament
 *   language is aspirational and non-justiciable. The arrangement's authority
 *   derives from the weapon states' security interest in capping the nuclear
 *   club. The structural result is a permanently stabilized two-tier order:
 *   five recognized arsenals outside the enforcement machinery, and the rest
 *   of the treaty parties as perpetual restraint-bearers whose reciprocal
 *   obligation carries no timeline and no remedy. The claim/metric
 *   relationship is deliberate: this story CLAIMS tangled_rope because the
 *   regime solves a genuine collective-action problem — verified
 *   multi-regional non-proliferation that no alternative arrangement has
 *   replicated — while the authored metrics describe substantially
 *   extractive, actively enforced operation whose extraction has ratcheted
 *   upward since the 1995 indefinite extension. Per the ε-invariance
 *   principle this story is one member of a three-story family: the same
 *   treaty text instantiates different constraints under the grand_bargain
 *   and abolitionist readings, which are linked via network edges but not
 *   folded into this file.
 *
 * KEY AGENTS:
 *   - recognized_nuclear_weapon_states: agenda-setting beneficiary (institutional/arbitrage) — hold recognized arsenals outside enforcement; control interpretation through Security Council seats and supplier dominance
 *   - compliant_non_nuclear_weapon_states: dual payer/beneficiary (moderate/constrained) — bear permanent restraint and verification burdens; receive the security dividend and peaceful technology
 *   - advanced_nuclear_program_states: primary target (moderate/trapped) — most intrusive verification, most conditional Article IV access, no affordable exit
 *   - iaea_safeguards_establishment: institutional beneficiary (institutional/mobile) — administers the functional core; grows with every crisis
 *   - nuclear_supplier_group: secondary beneficiary (institutional/mobile) — gatekeeping power legitimized by the conditional reading
 *   - humanitarian_initiative_states: excluded challenger (organized/constrained) — TPNW coalition; inside the conference rooms, outside the decision structure
 *   - arms_control_analyst_community: analytical observer — documents the rhetoric/practice gap; binds no one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.64).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.6).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.64).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Non-Proliferation-Primary Reading: Conditional Article IV, Non-Justiciable Article VI").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international law / nuclear governance / treaty interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '3fa1cec1-eb6a-4e20-bf72-b170f7b310d9').
narrative_ontology:cs_kernel_codification('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', fixed_text).
narrative_ontology:cs_authority_grounding('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', extraction).
narrative_ontology:cs_interpretation_layer_present('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9').
narrative_ontology:cs_reading_relation('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_reading_relation('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', foundational, article_vi_nonjusticiable).
narrative_ontology:cs_axiom_status(article_vi_nonjusticiable, holdable).
narrative_ontology:cs_axiom_grounding('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', article_vi_nonjusticiable, conventional).
narrative_ontology:cs_axiom('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', foundational, article_iv_access_conditional_on_safeguards).
narrative_ontology:cs_axiom_status(article_iv_access_conditional_on_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', article_iv_access_conditional_on_safeguards, instrumental).
narrative_ontology:cs_reference_frame('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', horizontal_proliferation_prevention_compact).
narrative_ontology:cs_drift_state('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', post_tpnw_entry_into_force, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3fa1cec1-eb6a-4e20-bf72-b170f7b310d9', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_establishment).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, compliant_non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, advanced_nuclear_program_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, compliant_non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five states recognized as nuclear weapon states under the treaty. They retain their arsenals outside the treaty's verification and enforcement machinery, control the interpretation of Articles III, IV, and VI through their permanent Security Council seats and dominance of the supplier regimes, and decide which safeguards demands and disarmament commitments become binding practice. They collect the arrangement's security dividend — no new peer proliferators — and the legitimacy of their own arsenals as 'recognized' rather than prohibited. Their exit is not exit but reinterpretation: they can ratchet or relax enforcement, waive supplier rules for allies, and decline Review Conference outcomes without material cost to themselves.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, beneficiary).

% The large majority of treaty parties without nuclear weapons. They accept intrusive safeguards on their civilian programs, forgo the deterrent option permanently, and fund the verification system through assessed contributions. In return they receive peaceful-nuclear cooperation and the security of knowing their neighbors are verified non-weapon states. Their Article VI counterpart — disarmament — carries no enforceable timeline under this reading, so their restraint is permanent while the reciprocity is rhetorical. Withdrawal under Article X is formally available, but the DPRK precedent shows it converts a state into a sanctions target; most stay because regional proliferation frightens them more than the asymmetry does.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, compliant_non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, compliant_non_nuclear_weapon_states, beneficiary).

% States with significant nuclear infrastructure whose programs attract the most intrusive verification and the sharpest application of the conditional Article IV reading. Their access to peaceful nuclear technology is gated by supplier-group consensus and safeguards performance; allegations of non-compliance bring Security Council referral and sanctions. They cannot withdraw without inviting attack or economic strangulation, and they cannot fully exercise the Article IV rights the treaty promises because supplier states condition exports on politics as well as compliance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, advanced_nuclear_program_states, payer,
    moderate, biographical, trapped, regional).

% The secretariat and inspectorate that administer Article III verification. Every expansion of the conditional reading and every new compliance crisis enlarges its budget, mandate, and staff. It performs the regime's genuinely functional core — inspections, material accounting, continuous monitoring — while its findings are politically filtered through a Board of Governors where the weapon states hold decisive weight. Its institutional health is structurally tied to the arrangement's persistence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_establishment, beneficiary,
    institutional, generational, mobile, global).

% The exporting states that control peaceful nuclear technology transfer. The conditional Article IV reading gives them gatekeeping power: they decide which states receive enrichment, reprocessing, and reactor technology, conditioning access on safeguards compliance and, in practice, on alignment. The arrangement legitimizes their cartel without requiring them to bear verification costs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group, beneficiary,
    institutional, biographical, mobile, global).

% The coalition of states that negotiated and adopted the Treaty on the Prohibition of Nuclear Weapons. They hold that the two-tier order is illegitimate and that Article VI requires complete disarmament, but the NPT's enforcement machinery gives their reading no standing: the weapon states boycott the TPNW, and Review Conference consensus rules let a single weapon state block any language they advance. They are inside the treaty's conference rooms and outside its decision structure.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, humanitarian_initiative_states, excluded,
    organized, generational, constrained, global).

% Independent scholars, think tanks, and former officials who track safeguards performance, arsenal modernization, and Review Conference outcomes. They document the widening gap between Article VI rhetoric and practice and the tightening conditionality of Article IV access. Their analyses circulate to every delegation and bind no one.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, arms_control_analyst_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the nuclear security dilemma: centralized verification under Article III gives states confidence that neighbors' civilian programs are not weapons programs, suppressing the preemptive proliferation cascade; a single safeguards standard and supplier framework lets peaceful nuclear commerce proceed without every state building a full fuel cycle.
% TRANSFER_FUNCTION: Moves verification burdens, inspection access, and the deterrent option from non-weapon states to a regime administered by the weapon states and the IAEA; moves security assurance and peaceful technology toward compliant non-weapon states; the restraint itself — the permanently surrendered weapons option — flows from the many to the security of the few arsenal possessors.
% ABSENT_VOICES: The humanitarian initiative coalition would object that permanent two-tier status is illegitimate; hibakusha and test-affected communities have no seat anywhere in the machinery; and no seat inside the arrangement is empowered to enforce Article VI against the weapon states — the parties who would benefit from enforcement are the ones it would bind.
% DISAPPEARANCE_RATIONALE: Verification confidence would collapse overnight; several hedge states would move toward weapons within years and regional cascades would follow in the Middle East, East Asia, and possibly Europe; peaceful nuclear commerce would need replacement arrangements; and the legitimacy structure separating 'recognized' from 'prohibited' arsenals would dissolve into an unmanaged competition the weapon states' own security depends on avoiding.
% FOUNDING_PROBLEM: The predicted 1970s–1980s proliferation cascade: dozens of states acquiring weapons as the technology spread, with the five 1967 possessors unable to prevent it by force. The treaty was built to stop horizontal spread while preserving peaceful nuclear energy access.
% FOUNDING_PROBLEM_CORROBORATION: Independent proliferation scholarship (the Sagan–Waltz debate and its successors) attests cascade risk as real; the TPNW coalition — outside the benefiting parties — attests the danger while disputing the remedy; and the DPRK and Iranian cases document live proliferation pressure corroborated by the IAEA's own findings. What no source outside the benefiting parties attests is the reading's remedy claim — that perpetual restraint with non-justiciable reciprocity is the only workable answer; that claim rests on the weapon states' own authority.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.64 because the transfers are real and asymmetric: restraint, inspection access, and the deterrent option flow permanently from the many to the security of the few, while the reciprocal obligation is, under this reading, deliberately unenforceable. It is not higher because the coordination is genuine — verified regional non-proliferation is a good most parties would not surrender — and because the extraction is capped by the regime's own legitimacy needs. Suppression (0.60) is authored as a raw structural property, unscaled by power or scope (the engine scales only extractiveness): it reflects the enforcement machinery's actual reach — supplier-gate conditions, Security Council referral, and the DPRK precedent that converts withdrawal into sanctions-target status — coercion aimed at exits and evasion rather than at ordinary compliance. Theater (0.38) captures the Review Conference cycle and Article VI reporting, which consume diplomatic energy while binding nothing, set against a safeguards core that genuinely functions. Accessibility collapse is moderate (0.52): alternatives exist (TPNW, regional weapon-free zones, withdrawal) but are partly suppressed and none replicates the verification function. Resistance is substantial (0.58): the humanitarian initiative, non-aligned timeline demands, and advanced-program state challenges are organized and persistent but have not moved the enforcement structure. The measurement series share one time grid; the 1995 indefinite extension is visible as the extraction ratchet (T25), the 2000 '13 Practical Steps' as the brief theater dip when Article VI language briefly became operative, and the post-2005 abandonment as the return to ritual. Suppression_requirement is tracked because this story specifically traces enforcement-capacity change: supplier controls after the 1974 Indian test, the 93+2 and Additional Protocol build-out, and the Iran/DPRK referral era.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the weapon-state seat the arrangement is a success it administers: proliferation prevented, its own arsenals a recognized exception, Article VI a horizon rather than an obligation — the structure reads as rope on favorable terms. From the compliant NNWS seat it is a tolerable trade whose asymmetry is real but whose alternative is worse. From the advanced-program seat the same machinery operates as targeted extraction: Article IV access is conditional on politics as much as compliance, and exit is unaffordable. From the excluded TPNW seat the arrangement is an illegitimate two-tier order whose non-justiciable Article VI is the point, not the flaw. The engine derives these divergences from power, exit options, and declared position; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   The weapon states sit nearest the beneficiary pole: they collect the security dividend and the legitimacy of recognized status while bearing no enforceable obligation, and their arbitrage-grade exit — reinterpretation rather than withdrawal — places them at very low d. The IAEA and supplier group are secondary beneficiaries: the first collects budget, mandate, and staff from the verification regime it administers; the second collects gatekeeping power legitimized by the conditional Article IV reading. Compliant NNWS are dual-positioned: declared in the victims array as the class that permanently bears restraint, verification intrusion, and unenforceable reciprocity, while receiving the diffuse security dividend and technology access — the derivation should place them mid-to-high d, pulled down by their beneficiary side but held up by constrained exit. Advanced-program states sit nearest the target pole: trapped exit, most intrusive verification, most conditional rights. The excluded TPNW coalition sits outside the exchange — it bears pressure for its repudiation but collects nothing from the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the proliferation cascade — is live, so this is not a mandatrophy case: the arrangement has not outlived its function. The classification work is different: preventing two symmetric mislabels. Reading the arrangement as pure snare (the TPNW seat's view) erases the genuine coordination — no alternative arrangement has ever produced verified multi-regional non-proliferation, and dismantling the machinery would not restore the reciprocity it failed to deliver. Reading it as pure rope (the weapon-state seat's preferred framing) erases the extraction — the 1995 indefinite extension traded permanence of restraint for promises never converted into enforcement, and the conditional Article IV reading has tightened rather than loosened. Tangled rope holds both: the same verification machinery that solves the security dilemma is the machinery through which the two-tier extraction is administered, and the reading under which this story is authored is itself part of what keeps Article VI non-justiciable. The theater series tracks the drift risk in the other direction: if the RevCon cycle and Article VI reporting continue to bind nothing while the verification core keeps functioning, the regime's diplomatic layer drifts toward piton-like ritual even as its enforcement layer hardens — the two layers' trajectories are diverging, which the shared-grid series makes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates only the nonproliferation_primary reading of the npt_article_iv_vi_pairing kernel; the same treaty text under the grand_bargain reading (reciprocal, conditional obligations) or the abolitionist reading (Article VI mandates disarmament) yields structurally different constraints — different victim sets, different epsilon, different classification. Which reading governs is not decidable from the text alone.',
    'Comparative classification of the sibling stories (npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist): divergence in computed extraction and victim structure across readings confirms the kernel is genuinely contested; convergence would indicate one reading is doing all the structural work and the others are rhetorical.',
    'Under the grand_bargain reading the two-tier order is contingent and extraction from non-weapon states drops materially as reciprocity becomes enforceable; under the abolitionist reading the arrangement loses its legitimacy basis entirely and every state under the deterrent umbrella joins the victim set. This story''s classification holds only for the nonproliferation_primary reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the Article IV/VI pairing governs determines the constraint''s victim set, epsilon, and classification.').

omega_variable(
    article_vi_legal_status,
    'Is Article VI genuinely non-justiciable, or does the ICJ''s 1996 advisory opinion — which found an obligation to pursue and bring to a conclusion negotiations on disarmament — establish justiciable content that this reading''s ''aspirational'' label suppresses?',
    'A contested case or advisory proceeding testing Article VI''s operative content; or systematic analysis of whether any state has ever been held to account for Article VI non-performance in any forum.',
    'If justiciable content exists, the reading''s central axiom is an interpretive achievement rather than settled law, the extraction borne by non-weapon states is higher than authored (unconditional restraint against a real obligation), and the grand_bargain sibling gains structural ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_status, conceptual, 'Whether Article VI''s non-justiciability is settled law or a maintained interpretation.').

omega_variable(
    two_tier_order_contingency,
    'Is the permanent two-tier order a structural feature of nuclear politics — arsenals are facts no treaty language reaches, and no weapon state will disarm under external pressure — or a constructed outcome of this reading''s success at excluding weapon-state arsenals from enforcement?',
    'Counterfactual analysis against the grand_bargain and abolitionist readings'' institutional track record: if enforceable reciprocity or prohibition frameworks have ever moved arsenals, permanence is constructed; if no framework has ever moved them, permanence is structural.',
    'If permanence is structural, this reading is a realistic description and its extraction is the price of the only workable non-proliferation order; if constructed, the reading is itself the enforcement mechanism stabilizing the extraction, and the arrangement is more snare-like than the metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(two_tier_order_contingency, conceptual, 'Whether the two-tier order''s permanence is structural fact or this reading''s achievement.').

omega_variable(
    withdrawal_exit_reality,
    'Does Article X constitute a genuine exit option for non-weapon states, or has post-DPRK sanctions practice converted formal withdrawal into a punished act, making the formal exit right a component of suppression rather than an alternative?',
    'Systematic comparison of withdrawal attempts and their consequences (DPRK, and the deterred cases — states that explored withdrawal and reversed); sanctions practice against withdrawers versus compliant states.',
    'If exit is real, authored suppression is overstated and non-weapon state participation is substantially consensual; if exit is punished, the arrangement holds its restraint-bearers by coercion and effective suppression is higher than the scalar suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(withdrawal_exit_reality, empirical, 'Whether Article X withdrawal is a live exit or a sanctioned trap.').

omega_variable(
    nnws_net_position_heterogeneity,
    'Are compliant non-weapon states net beneficiaries of the arrangement (regional non-proliferation security, peaceful technology access) or net payers (permanent second-class status, unenforceable reciprocity) — and does the answer vary systematically by region and alliance position?',
    'Per-state accounting of verification costs, technology access, and security dividends; comparison of allied NNWS (extended-deterrence beneficiaries) against non-aligned NNWS.',
    'If most NNWS are net beneficiaries, extraction concentrates on the advanced-program and non-aligned seats and the arrangement is closer to a rope with targeted extraction; if most are net payers, the two-tier extraction is broad-based and the tangled_rope reading understates the snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_net_position_heterogeneity, empirical, 'Heterogeneous net position of non-weapon states across the beneficiary/payer divide.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t5, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(npt__tr_t5, observed).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(npt__tr_t10, observed).
narrative_ontology:measurement(npt__tr_t15, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(npt__tr_t15, observed).
narrative_ontology:measurement(npt__tr_t25, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(npt__tr_t25, observed).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(npt__tr_t30, observed).
narrative_ontology:measurement(npt__tr_t35, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 35, 0.36).
narrative_ontology:measurement_basis(npt__tr_t35, observed).
narrative_ontology:measurement(npt__tr_t45, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(npt__tr_t45, observed).
narrative_ontology:measurement(npt__tr_t50, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(npt__tr_t50, observed).
narrative_ontology:measurement(npt__tr_t55, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 55, 0.38).
narrative_ontology:measurement_basis(npt__tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t5, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(npt__be_t5, observed).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(npt__be_t10, observed).
narrative_ontology:measurement(npt__be_t15, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(npt__be_t15, observed).
narrative_ontology:measurement(npt__be_t25, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(npt__be_t25, observed).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(npt__be_t30, observed).
narrative_ontology:measurement(npt__be_t35, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(npt__be_t35, observed).
narrative_ontology:measurement(npt__be_t45, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 45, 0.63).
narrative_ontology:measurement_basis(npt__be_t45, observed).
narrative_ontology:measurement(npt__be_t50, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(npt__be_t50, observed).
narrative_ontology:measurement(npt__be_t55, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 55, 0.64).
narrative_ontology:measurement_basis(npt__be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t5, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(npt__su_t5, observed).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(npt__su_t10, observed).
narrative_ontology:measurement(npt__su_t15, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(npt__su_t15, observed).
narrative_ontology:measurement(npt__su_t25, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(npt__su_t25, observed).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(npt__su_t30, observed).
narrative_ontology:measurement(npt__su_t35, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 35, 0.6).
narrative_ontology:measurement_basis(npt__su_t35, observed).
narrative_ontology:measurement(npt__su_t45, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(npt__su_t45, observed).
narrative_ontology:measurement(npt__su_t50, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(npt__su_t50, observed).
narrative_ontology:measurement(npt__su_t55, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 55, 0.6).
narrative_ontology:measurement_basis(npt__su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_article_iii_safeguards_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_nuclear_weapons_prohibition).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT's Article IV/VI pairing' covers three structurally distinct claims that this corpus models as a three-story family: nonproliferation_primary (this story — conditional Article IV, non-justiciable Article VI, weapon-state-security authority; substantially extractive tangled rope), grand_bargain (reciprocal conditional obligations; extraction depends on whether reciprocity is live), and abolitionist (Article VI as disarmament mandate; the arrangement itself illegitimate; highest epsilon, with the victim set extending to all states under the deterrent umbrella). In empirical-confidence terms the upstream story is this one — it is the reading institutionalized in practice — and it structurally influences the grand_bargain sibling (indefinite extension and conditional-Article-IV practice continuously degrade the reciprocity claim's institutional footing) while foreclosing the abolitionist sibling within any single legal framework. Each story carries its own epsilon, beneficiaries, and victims; none folds the others' contest into its classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
