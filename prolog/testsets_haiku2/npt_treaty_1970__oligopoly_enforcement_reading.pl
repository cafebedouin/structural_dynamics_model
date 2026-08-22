% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Articles I-II Horizontal Nonproliferation (Oligopoly Enforcement Reading)
 *   domain: international/legal/security
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (1970) is a contested kernel
 *   interpretable through multiple readings. This constraint instantiates the
 *   oligarchy enforcement reading: Articles I-II (prohibiting non-nuclear
 *   weapons states from acquiring weapons and obligating nuclear states not
 *   to transfer them) are the treaty's primary operative obligations,
 *   actively enforced through IAEA inspections, export controls, and
 *   diplomatic pressure. Article VI (committing all parties to eventual
 *   disarmament) is structured as aspirational rather than binding, with no
 *   enforcement mechanism or timeline. This reading generates an asymmetric
 *   constraint: the treaty's operational effect is to prevent horizontal
 *   proliferation (spreading to new states) while permitting vertical
 *   proliferation (P5 arsenal expansion). Non-nuclear states and
 *   threshold-capable states bear the verification burden and deterrent
 *   denial; the P5 retain weapons legitimacy and decision-making authority.
 *   The constraint is claimed as tangled_rope (genuine coordination problem
 *   solved — cascade prevention — plus asymmetric extraction and asymmetric
 *   enforcement) because the cascade-prevention function is real and
 *   coordination is necessary, but the asymmetry in burden-sharing and in
 *   Article VI enforcement reveals that the treaty functions as a
 *   status-quo-enforcement mechanism weaponized to extract from NNWS and
 *   suppress threshold states' security autonomy.
 *
 * KEY AGENTS:
 *   - P5 nuclear weapons states (Russia, US, UK, France, China): agenda-setters and beneficiaries, retain arsenals and enforce Articles I-II through IAEA control and export regimes
 *   - Non-nuclear weapons states (NNWS majority): payers, accept inspections and deterrent denial in exchange for non-binding security assurances
 *   - Threshold-capable states (Iran, Japan, South Korea, Brazil historical candidates): victims, face denial of deterrent capability and vulnerability to coercion when security guarantees fail
 *   - IAEA Inspectorate: enforcement mechanism administrator, operates under P5-controlled governance
 *   - Non-Aligned Movement and disarmament advocates: excluded from agenda-setting, propose reciprocal Article VI enforcement but lack voting power
 *   - Advanced industrial nations and alliance networks (NATO, US-Japan, US-South Korea): beneficiaries of the treaty's status hierarchy and alliance legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Articles I-II Horizontal Nonproliferation (Oligopoly Enforcement Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international/legal/security").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '519bc6c4-8445-461b-8bc3-9341822d8b61').
narrative_ontology:cs_kernel_codification('519bc6c4-8445-461b-8bc3-9341822d8b61', fixed_text).
narrative_ontology:cs_authority_grounding('519bc6c4-8445-461b-8bc3-9341822d8b61', extraction).
narrative_ontology:cs_interpretation_layer_present('519bc6c4-8445-461b-8bc3-9341822d8b61').
narrative_ontology:cs_reading_relation('519bc6c4-8445-461b-8bc3-9341822d8b61', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('519bc6c4-8445-461b-8bc3-9341822d8b61', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('519bc6c4-8445-461b-8bc3-9341822d8b61', foundational, articles_i_ii_primary_enforcement_obligation).
narrative_ontology:cs_axiom_status(articles_i_ii_primary_enforcement_obligation, holdable).
narrative_ontology:cs_axiom_grounding('519bc6c4-8445-461b-8bc3-9341822d8b61', articles_i_ii_primary_enforcement_obligation, conventional).
narrative_ontology:cs_axiom('519bc6c4-8445-461b-8bc3-9341822d8b61', foundational, article_vi_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('519bc6c4-8445-461b-8bc3-9341822d8b61', article_vi_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('519bc6c4-8445-461b-8bc3-9341822d8b61', secondary, enforcement_asymmetry_structural_necessity).
narrative_ontology:cs_axiom_status(enforcement_asymmetry_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('519bc6c4-8445-461b-8bc3-9341822d8b61', enforcement_asymmetry_structural_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('519bc6c4-8445-461b-8bc3-9341822d8b61', p5_oligopoly_nonproliferation_regime).
narrative_ontology:cs_drift_state('519bc6c4-8445-461b-8bc3-9341822d8b61', contemporary_post_iran_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('519bc6c4-8445-461b-8bc3-9341822d8b61', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, established_security_alliance_networks).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, security_alliance_networks).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, advanced_industrial_nations).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, aspiring_threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, horizontal_proliferation_prevention).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, status_quo_stabilization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the treaty's primary obligations (Articles I-II) through IAEA inspections, export controls, and diplomatic leverage. Retain nuclear arsenals under the Article VI commitment to eventual disarmament — a commitment structured as aspirational rather than binding with enforcement mechanism. Benefit from the status hierarchy the treaty instantiates: their own nuclear deterrent remains legitimate while preventing others from acquiring it. Control the IAEA Board of Governors and NPT Review Conference agendas. Can withdraw under Article X with 90 days notice, citing supreme national interest.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapons_states, beneficiary).

% Accept intrusive IAEA inspections (Full-Scope Safeguards) to demonstrate nonproliferation compliance. Renounce the right to acquire nuclear weapons even when threatened by neighbors. Receive security assurances from the P5 that are non-binding and historically unreliable (1994 Budapest Memorandum violated for Ukraine; Cold War assurances never invoked). Bear verification costs; their nuclear abstinence is continuously surveilled while P5 arsenals are largely unmonitored.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapons_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapons_states, beneficiary).

% Possess or can rapidly acquire fissile material and weapons design knowledge (Iran, Japan, South Korea, Brazil, Argentina are historical candidates). Face the choice: sign NPT and renounce deterrent, or refuse to sign and face isolation/sanctions. If they sign, they accept inspection and are denied the deterrent that would otherwise be available. If they refuse, they trigger P5 enforcement mechanisms (sanctions, military threats) designed to prevent exactly their acquisition path. The treaty's asymmetry is sharpest here: their denial of deterrent is presented as nonproliferation, not as coercion.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_states, payer,
    powerful, biographical, identity_locked, global).

% Administers Full-Scope Safeguards regime and technical verification. Operates under Board of Governors control (P5 have permanent seats). Conducts inspections of NNWS nuclear facilities while P5 nuclear arsenals remain outside IAEA mandate. Functions as the operational enforcement arm of Articles I-II but has no mandate over Article VI compliance. Derives legitimacy and budget from state parties, creating structural dependence on P5 members.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_inspectorate, agenda_setter,
    institutional, generational, analytical, global).

% Would argue that Article VI's binding enforcement and reciprocal timelines are the treaty's legitimate core; that Articles I-II without Article VI constitute a security bargain violated by the P5. Historically excluded from agenda-setting authority (NPT Review Conferences are consensus-dependent but P5 hold veto-equivalent power through their permanent IAEA seats). Their objections to enforcement asymmetry are formally registered but unacted.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_aligned_movement_nnws, excluded,
    organized, generational, constrained, global).

% States with plausible nuclear ambitions but no current safeguard agreement (Israel, India, Pakistan, North Korea — only North Korea signed, then withdrew). Face either isolation as non-treaty members or the bind of threshold state status. Their exclusion from the treaty apparatus and vulnerability to unilateral P5 enforcement (threats, sanctions, military strikes) constitutes the treaty's deterrent effect for the non-signatory edge case.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, aspiring_threshold_states, payer,
    powerful, biographical, trapped, regional).

% NATO, Japan's US alliance, South Korea's alliance, Gulf state partnerships with the US — benefit from the treaty's status hierarchy enforcement. Their members are NNWS (most), their security is underwritten by a P5 nuclear guarantor, and the treaty prevents their adversaries (or nervous neighbors) from acquiring independent deterrent. The treaty's asymmetry is their alliance glue.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, security_alliance_networks, beneficiary,
    institutional, civilizational, analytical, regional).

% NGOs advocating for disarmament and burden-sharing reform (ICAN, Pugwash, etc.) document the enforcement asymmetry and call for reciprocal Article VI enforcement. Participate in NPT Review Conferences as observers but have no voting power. Are structurally excluded from enforcement authority despite being the only actors without institutional incentive to preserve the status hierarchy.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, international_civil_society, excluded,
    moderate, generational, constrained, global).

% Benefit from open trade in dual-use nuclear technology, nuclear power exports, and uranium enrichment services — all gate-kept by the P5-controlled export control regimes (NSG, Australia Group) that ride on NPT enforcement. Can diversify in export markets while threshold states face supplier denial; can develop civilian nuclear technology under safeguards while acquisition of weapons-grade materials is blocked for others.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, advanced_industrial_nations, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapons_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a destabilizing cascade of weapons acquisition by establishing and maintaining an inspectable nonproliferation regime: centralized verification (IAEA), standardized safeguards, supplier discipline (nuclear export controls). Solves the prisoners' dilemma of security competition by converting the acquisition choice from independent (each state deterring unilaterally) to interdependent (one state's nonproliferation is conditional on others' commitment).
% TRANSFER_FUNCTION: Moves the legitimacy and operational capacity to maintain a nuclear deterrent from the NNWS to the P5. Non-nuclear states transfer the security value of potential deterrence to the P5 in exchange for security guarantees. Threshold states transfer denied deterrent option to the P5 in exchange for non-invasion commitment (weak and historically unreliable). Verification labor and inspection burden flow from NNWS to the IAEA; decision-making authority flows to the P5.
% ABSENT_VOICES: States that have withdrawn or refused to sign (India, Pakistan, Israel, North Korea pre-withdrawal) are permanently excluded from the treaty apparatus and cannot participate in its amendment or interpretation. The Non-Aligned Movement's threshold states, though treaty members, are systematically subordinated in agenda-setting (consensus structures give each P5 near-veto power). Disarmament advocates are present as observers only, without voting authority. The voices absent are those that would propose reciprocal Article VI enforcement timelines or would question the status hierarchy's legitimacy.
% DISAPPEARANCE_RATIONALE: If the NPT and its enforcement mechanisms collapsed, threshold states would revert to independent deterrent acquisition (Iran would accelerate enrichment, Japan and South Korea would rearm, Brazil and Argentina would resume weapons programs). The absence of coordinated verification would accelerate regional arms racing and destabilize alliance structures (NATO, US-Japan, US-South Korea partnerships depend on the treaty's reassurance that adversaries remain non-nuclear). The P5 would lose the treaty's legitimation of their own arsenals and would face competing claims to deterrent legitimacy from newly armed states.
% FOUNDING_PROBLEM: The 1960s nuclear proliferation cascade: China acquired weapons (1964), triggering security concerns across Asia, Middle East, and Europe. Each state's independent nuclear acquisition imposed security costs on its neighbors, creating a cycle that threatened to spread weapons to dozens of threshold-capable states. The treaty was negotiated as a collective security arrangement to halt this cascade by converting acquisition from a unilateral security choice to a coordinated interdependence.
% FOUNDING_PROBLEM_CORROBORATION: The P5 attest the founding problem is still live: Iran, North Korea, and potential threshold state proliferation remain threats. The NNWS attest the founding problem was substantially solved by the 1980s-1990s (the number of weapons states stabilized; acquisition rates slowed) but that the treaty's persistence now derives from the status hierarchy rather than the security coordination. Independent scholars (Sagan, Waltz, Rauchhaus) and the Non-Aligned Movement document that the treaty's primary effect is preventing NNWS deterrence rather than deterring inter-NWS conflict, and that Article VI's non-enforcement reveals the true function: status quo management, not shared security.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.71 at interval end) and rising because the treaty's operational structure systematically denies threshold states a deterrent capability while the P5 retain theirs; the benefit to threshold states (security assurance) is non-binding and historically unreliable. Suppression is substantial (0.68) because the treaty's enforcement mechanisms (IAEA inspections, export controls, sanctions threats) actively prevent the most direct security option for vulnerable states, and because Article X's withdrawal right is constrained by P5 diplomatic and economic pressure. Theater has risen from 0.18 to 0.42 because the treaty's original coordination rationale (preventing cascade acquisition) was largely achieved by the 1990s, yet enforcement machinery has intensified (more intrusive inspections, more aggressive export controls, more frequent military threat-making); the growing share of effort devoted to status-quo maintenance and P5 privilege defense relative to genuine cascade-prevention coordination indicates rising theater. Accessibility of alternatives has declined (0.62) because the treaty's 50+ years of entrenchment have made exit costly (isolation, sanctions, military threat) and alternative security arrangements (regional deterrence alliances) unstable without the treaty's legitimation. Resistance is moderate (0.58) because NNWS and threshold states formally object to the asymmetry and propose Article VI enforcement timelines, but lack institutional power to compel change; the P5's control of the IAEA Board and NPT Review Conference agendas suppresses institutional challenge. The measurement series show extractiveness accelerating over the interval: from 1970 to 2000, the treaty was primarily about cascade prevention (lower extraction, genuine coordination); from 2000 to 2024, intensification of enforcement asymmetry (Iran sanctions, North Korea isolation, Japanese and South Korean security guarantees tightening dependence) has driven extraction upward, revealing the treaty's function shift from shared security to status hierarchy management.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the treaty is a successful coordination mechanism that has prevented destabilizing proliferation, legitimizes their own arsenals as necessary deterrence, and provides enforcement tools (IAEA) for collective security. From the NNWS seat, the treaty is an asymmetric burden: intrusive verification, deterrent denial, and unreliable assurances. From the threshold-state seat, the treaty is coercive: it denies the most effective security option (independent deterrent) while offering no credible protection (Budapest Memorandum violated for Ukraine). From the disarmament-advocate seat, the treaty is a capture operation: Article VI's non-enforcement reveals that P5 commitment to disarmament is theatrical, not genuine. The engine computes these divergences from the power atoms and exit-option differentiation: the P5's institutional power and arbitrage exit generate a beneficiary classification; the NNWS's organized power and constrained exit generate a payer/target classification; threshold states' powerful status but identity-locked exit generate a trapped-and-coerced classification. These are structural facts; the claim/metric independence rule allows the authored claim (tangled_rope coordination) and the metrics (high extraction, high suppression, rising theater) to diverge — the divergence itself is the signal the engine is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 nuclear weapons states have d near 0.0 (full beneficiary): they set agenda, control enforcement, retain arsenals, and extract status and veto authority. NNWS majority have d near 0.8 (substantial target): they bear inspection burden, accept deterrent denial, and receive non-binding assurances; exit is constrained by isolation threat and alliance dependence. Threshold-capable states have d near 0.9 (full target): they face the most explicit denial of deterrent option, are subject to unilateral enforcement (sanctions, military threat), and have identity-locked exit (if they refrain, they are vulnerable; if they acquire, they are threatened). The treaty's asymmetry is sharpest here. Advanced industrial nations have d near 0.3 (net beneficiary through alliance security): they benefit from the status hierarchy and supplier discipline while avoiding inspection themselves (most are P5 or P5-protected). The IAEA Inspectorate is analytically positioned (d = 0.5, symmetric) as the enforcement arm without independent decision-making authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The treaty was founded to solve a specific problem: prevent a cascade of weapons acquisitions in the 1960s-70s (China's test triggered security spirals in Asia, Middle East, Europe). By the 1980s-90s, this founding problem was substantially solved: the number of weapons states stabilized at 9 (P5 + India, Pakistan, Israel, North Korea). Yet the treaty persisted and intensified enforcement. The founding_problem_status is contested: P5 attest ongoing threats (Iran, North Korea), while NNWS and analysts attest the problem is largely solved and the treaty now functions as status-quo enforcement. The disappearance verdict is world_rearranges: if the treaty collapsed, threshold states would accelerate deterrent acquisition, destabilizing the current alliance architecture. The mandatrophy is partial and contestable: the founding problem is not entirely dead (regional threats remain), but the treaty's primary enforcement energy now goes toward maintaining the P5's monopoly and denying NNWS deterrence rather than solving the founding coordination problem. The rising theater ratio (from 0.18 to 0.42) indicates this drift: security briefings still cite cascade-prevention rationales, but the actual enforcement effort concentrates on status-quo maintenance and P5 privilege. A genuine mandatrophy resolution would require either (1) reclassifying the treaty as a snare (pure status hierarchy, no real coordination function) if the founding problem is judged dead, or (2) restructuring the treaty as a rope with binding Article VI and reciprocal burden-sharing if the founding coordination problem is judged live but unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Which reading of the npt_treaty_1970 kernel is empirically supported by state practice: oligopoly_enforcement_reading (I-II primary, VI aspirational), reciprocal_disarmament_reading (VI binding, I-II conditional), or withdrawal_sovereignty_reading (Article X as live exit)?',
    'Empirical analysis of NPT Review Conference outcomes (1975–2024), IAEA mandate drift, P5 compliance behavior, and state party practice when security interests conflict with treaty obligations. Historical case studies: UK''s Falklands nuclear alert (1982, Article X margin), Ukraine''s Budapest Memorandum violation (2022, test of P5 assurance credibility), Iran''s breakout acceleration (2019–2024, response to US withdrawal threat), North Korea''s withdrawal (2003, assertion of Article X sovereignty).',
    'If oligopoly_enforcement_reading is empirically dominant, this constraint''s classification as tangled_rope holds: genuine cascade-prevention coordination meets asymmetric P5 benefit and NNWS suppression. If reciprocal_disarmament_reading is empirically correct, this constraint is a mischaracterization of the treaty''s legitimate operation and should reclassify toward snare (the coordination was never reciprocal, only apparent). If withdrawal_sovereignty_reading is empirically dominant, suppression erodes significantly because Article X provides a real exit option that states have historically used when threatened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Which reading of the npt_treaty_1970 kernel is empirically supported by state practice and treaty operative history.').

omega_variable(
    coordination_extraction_separability,
    'Is the nonproliferation coordination structurally inseparable from the enforcement asymmetry (so asymmetry is coordination cost), or could reciprocal enforcement structures (binding Article VI, IAEA inspection of P5 arsenals, reciprocal disarmament timelines) achieve the same cascade-prevention coordination while eliminating the extraction?',
    'Game-theoretic analysis: model cascade-prevention coordination under reciprocal enforcement (equal verification burden, binding disarmament timelines) and compare equilibrium stability to current asymmetric structure. Counterfactual scenario analysis: hypothetical treaty with Articles I-II enforced equally on P5 and NNWS, and Article VI binding with enforcement mechanism. Historical precedent: analysis of arms control regimes that achieved reciprocal verification (START treaties, JCPOA verification structures) to assess whether reciprocity undermined coordination efficacy.',
    'If separable, the asymmetry is purely extractive and reveals the treaty as a snare-with-coordination-cover: the coordination could work better with reciprocity, so the choice to maintain asymmetry is a value capture decision. If inseparable, the asymmetry is the price of P5 cooperation in a coordination mechanism that otherwise cannot hold (P5 would not sign if subjected to equal constraints), making it tangled_rope where extraction finances coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether nonproliferation coordination is structurally separable from enforcement asymmetry.').

omega_variable(
    article_vi_enforceability_intent,
    'Was Article VI written with intent to be binding (with enforcement mechanisms to be negotiated) or knowingly drafted as aspirational to preserve P5 arsenal legitimacy without constraint?',
    'Historical document analysis: NPT negotiation records (1965–1970), draft language evolution, negotiator statements on intent. Vienna Convention interpretation analysis: does Article VI''s language (''undertakes to pursue negotiations'') meet the threshold of binding obligation under international law? ICJ advisory opinion analysis (if available): how have courts interpreted Article VI''s enforceability?',
    'If binding intent existed but was subverted by P5 power, Article VI represents an unresolved mandate (betrayed coordination, high extraction). If aspirational intent was explicit, the treaty was designed as status-quo-enforcement, not reciprocal bargain (high extraction, low coordination authenticity). If intent is genuinely ambiguous, the treaty is a constitutive document that admits multiple readings, supporting the kernel-reading framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforceability_intent, empirical, 'Legislative intent and textual analysis of Article VI enforceability.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is threshold-state deterrent suppression primarily structural (denial of materials, export controls, military threat) or internalized (states have adopted nonproliferation as a core legitimacy norm)?',
    'Behavioral analysis: states'' revealed preferences when structural constraints weaken (Iran''s acceleration when JCPOA lifts; North Korea''s escalation after sanctions intensify/relax). Attitudinal data: surveys and statements from NNWS and threshold states on whether nonproliferation is a self-imposed norm or external coercion. Institutional analysis: how frequently and credibly do states invoke Article X withdrawal threat when security environment deteriorates?',
    'If primarily structural, suppression depends on continuous enforcement and erodes if enforcement weakens (costs the P5 resources). If internalized, suppression persists without enforcement (threshold states self-constrain because nonacquisition is now a legitimacy marker). Mixed suppression determines the sustainability of the constraint: structural + internalized permits lower active enforcement; structural only requires continuous costly defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in NNWS deterrent denial.').

omega_variable(
    threshold_state_security_outcome_comparison,
    'Are threshold states'' security outcomes better or worse under the NPT constraints (denied deterrent, dependent on guarantees) than they would be under an alternative (independent deterrent, alliance-network deterrence)?',
    'Comparative security analysis: threshold states (Japan, South Korea, Brazil, Argentina) under treaty constraints vs. counterfactual security modeling (armed independently). Historical precedent: security trajectories of non-signatory threshold states (Israel, India, Pakistan) vs. NNWS treaty members. Analysis of guarantee reliability: Budapest Memorandum, US security commitments, and their enforcement record.',
    'If threshold-state security is worse under NPT constraints, the treaty''s suppression is coercive extraction (denies the most effective defense). If security is better (guarantees more reliable than independent deterrence; alliance security more stable), suppression is coordination cost. If outcomes are mixed (some threshold states benefit, others are worse off), the constraint reclassifies per-agent: beneficiaries (secure, alliance-guaranteed) vs. payers (isolated, unguaranteed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_security_outcome_comparison, empirical, 'Whether threshold states'' security outcomes improved or degraded under NPT constraints.').

omega_variable(
    p5_enforcement_asymmetry_intent,
    'Is the enforcement asymmetry (IAEA inspections of NNWS, no inspection of P5 arsenals; binding I-II, aspirational VI) a deliberate structural choice to preserve P5 strategic advantage, or an operational accident of treaty architecture?',
    'Historical analysis: NPT negotiation records (did P5 explicitly negotiate exemptions from inspection?). Operational analysis: has the IAEA ever proposed or attempted to extend inspection authority to P5 arsenals? How have P5 responded? Treaty amendment history: have NNWS proposed reciprocal inspection amendments? How have review conferences handled such proposals?',
    'If deliberate, the asymmetry is intentional extraction and the constraint is straightforwardly a snare with coordination cover (the P5 chose to capture the regime). If accidental, the asymmetry is organizational inertia or governance failure (what begins as a coordination mechanism drifts into asymmetric benefit due to power imbalance). This distinction affects whether the constraint is reclassifiable (deliberate extraction is harder to reform) or reformable (accidental asymmetry can be corrected if recognized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_enforcement_asymmetry_intent, empirical, 'Whether enforcement asymmetry is deliberate P5 choice or operational accident.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligo_tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(npt_oligo_tr_t0, observed).
narrative_ontology:measurement(npt_oligo_tr_t6, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(npt_oligo_tr_t6, observed).
narrative_ontology:measurement(npt_oligo_tr_t12, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(npt_oligo_tr_t12, observed).
narrative_ontology:measurement(npt_oligo_tr_t18, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement_basis(npt_oligo_tr_t18, observed).
narrative_ontology:measurement(npt_oligo_tr_t24, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(npt_oligo_tr_t24, observed).
narrative_ontology:measurement(npt_oligo_tr_t30, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(npt_oligo_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(npt_oligo_be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt_oligo_be_t0, observed).
narrative_ontology:measurement(npt_oligo_be_t6, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(npt_oligo_be_t6, observed).
narrative_ontology:measurement(npt_oligo_be_t12, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement_basis(npt_oligo_be_t12, observed).
narrative_ontology:measurement(npt_oligo_be_t18, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(npt_oligo_be_t18, observed).
narrative_ontology:measurement(npt_oligo_be_t24, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(npt_oligo_be_t24, observed).
narrative_ontology:measurement(npt_oligo_be_t30, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(npt_oligo_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligo_su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(npt_oligo_su_t0, observed).
narrative_ontology:measurement(npt_oligo_su_t6, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(npt_oligo_su_t6, observed).
narrative_ontology:measurement(npt_oligo_su_t12, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(npt_oligo_su_t12, observed).
narrative_ontology:measurement(npt_oligo_su_t18, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement_basis(npt_oligo_su_t18, observed).
narrative_ontology:measurement(npt_oligo_su_t24, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(npt_oligo_su_t24, observed).
narrative_ontology:measurement(npt_oligo_su_t30, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(npt_oligo_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_system).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_suppliers_group_export_controls).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, comprehensive_test_ban_treaty).

% DUAL FORMULATION NOTE:
% The npt_treaty_1970 kernel decomposes into three constraint stories corresponding to three live readings of the treaty's core operative structure. (1) oligopoly_enforcement_reading: Articles I-II as primary, VI as aspirational; P5 benefit from status hierarchy and deterrent denial of NNWS/threshold states. (2) reciprocal_disarmament_reading: Article VI as binding reciprocal obligation with temporal urgency; I-II enforced conditionally on P5 disarmament progress. (3) withdrawal_sovereignty_reading: Article X withdrawal right as legitimate sovereignty exercise; treaty obligations contingent on security environment, collapsing enforcement asymmetry. These are three structurally distinct constraints with different ε values, different victim/beneficiary sets, and different classifications. Each story must be authored independently; the decomposition is necessary because the same treaty text instantiates different constraints under different readings, per the ε-invariance principle. The network.affects_constraints array links them: this story (oligopoly_enforcement_reading) influences the other two by structuring the enforcement apparatus they would have to operate within or reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
