% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV-VI Pairing: Abolitionist Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The NPT Article IV-VI pairing, under the abolitionist reading, is a
 *   constraint that legitimizes the indefinite possession of nuclear weapons
 *   by five states while constraining all others from acquiring them. Article
 *   IV grants 'inalienable rights' to peaceful nuclear technology
 *   (enrichment, reprocessing, reactor design) without binding limitations on
 *   dual-use proliferation risk. Article VI commits weapon states to 'pursue
 *   negotiations in good faith' toward disarmament but with no timeline,
 *   enforcement mechanism, or definition of 'disarmament.' Under this
 *   reading, the pairing is a tangled rope: it coordinates non-proliferation
 *   (non-weapon states renounce weapons) but extracts compliance through
 *   legal asymmetry and the perpetual deferral of weapon states' disarmament
 *   obligation. The abolitionist reading treats the NPT itself as
 *   insufficient and delegitimized by humanitarian-law norms and the TPNW's
 *   prohibition standard. This constraint story instantiates that reading.
 *   The claim/metric divergence is structural: weapon states and
 *   nonproliferation scholars CLAIM this is a rope (genuine coordination for
 *   collective security), while the authored metrics describe substantially
 *   extractive, actively suppressed structure — the engine computes this
 *   divergence.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states (NWS): Russia, USA, France, UK, China — set the interpretation; treat Article VI as indefinitely deferred; refuse binding disarmament timelines or verification mechanisms
 *   - Non-nuclear weapon states (NNWS): ~185 states that renounced weapons in exchange for access to peaceful technology; lack voting power in NPT treaty decisions; bear the asymmetric legal burden
 *   - IAEA verification regime: verifies NNWS peaceful programs but has no mandate to audit NWS disarmament; structural asymmetry embedded in its authority
 *   - TPNW signatory states (~65 at 2026): claim the prohibition norm supersedes the NPT; treated as legally separate; excluded from NPT decision-making
 *   - Nuclear abolition movements and humanitarian advocates: argue weapons are categorically prohibited under humanitarian law; treated as political speech, not binding legal interpretation
 *   - Dual-use technology exporters and nuclear power operators: benefit from Article IV's ambiguity; have commercial incentive to preserve the status quo
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV-VI Pairing: Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '4b4a2861-c5b7-4b73-853a-04f6cd19dcea').
narrative_ontology:cs_kernel_codification('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', fixed_text).
narrative_ontology:cs_authority_grounding('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', extraction).
narrative_ontology:cs_interpretation_layer_present('4b4a2861-c5b7-4b73-853a-04f6cd19dcea').
narrative_ontology:cs_reading_relation('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', foundational, weapons_categorically_prohibited).
narrative_ontology:cs_axiom_status(weapons_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', weapons_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', foundational, article_vi_disarmament_mandatory_now).
narrative_ontology:cs_axiom_status(article_vi_disarmament_mandatory_now, holdable).
narrative_ontology:cs_axiom_grounding('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', article_vi_disarmament_mandatory_now, deontological).
narrative_ontology:cs_axiom('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', secondary, article_iv_illegitimate_if_perpetuates_proliferation_risk).
narrative_ontology:cs_axiom_status(article_iv_illegitimate_if_perpetuates_proliferation_risk, holdable).
narrative_ontology:cs_axiom_grounding('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', article_iv_illegitimate_if_perpetuates_proliferation_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', humanitarian_law_supremacy_prohibition).
narrative_ontology:cs_drift_state('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4b4a2861-c5b7-4b73-853a-04f6cd19dcea', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, established_nuclear_power_operators).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_abolition_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, security_dependent_non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, security_dependent_non_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and administer Article IV and Article VI on their own terms. They read Article IV as granting unconditional access to nuclear technology for peaceful purposes (electricity, medicine, research) while treating Article VI as a long-term aspirational commitment without binding disarmament timelines or enforceable mechanisms. They set the standards for dual-use technology assessment and control export of fissile material and enrichment technology. Their exit would require abandoning nuclear deterrence doctrine and strategic autonomy — functionally impossible without collective security guarantees they do not trust other states to honor.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, trapped, global).

% Renounce nuclear weapons and accept inspections of their peaceful programs under the NPT and IAEA verification protocols. They argue they have fulfilled Article IV restraint in exchange for a disarmament commitment (Article VI) that weapon states have not honored. Their exit options are non-membership (triggering security risks and trade/technology isolation) or breakout (risking sanctions and international isolation). Many face genuine security threats they believe nuclear deterrence would address but are barred from pursuing.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Benefit from Article IV's protection of their existing nuclear power infrastructure and access to enriched fuel supplied by weapon states or authorized fuel banks. They can leverage Article IV rights in trade negotiations and maintain energy independence through nuclear power. Their exit options include diversification to renewables (costly, long-term) or dependence on fuel imports (politically vulnerable). They are typically located in allied states of the nuclear weapon powers.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, established_nuclear_power_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Advocate for the Treaty on the Prohibition of Nuclear Weapons (TPNW) as the binding legal standard for complete disarmament and criminalization of weapons possession. They argue the NPT Article IV-VI pairing legitimizes nuclear weapons indefinitely by treating Article IV as unconditional while treating Article VI as perpetually deferred. They are excluded from the NPT's decision-making structures (weapon states set the interpretation) and their advocacy is treated as non-binding political speech rather than legally coherent treaty interpretation. Their identity as abolitionists is constituted through opposition to nuclear weapons, making exit (accepting the status quo) tantamount to identity dissolution.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_abolition_movements, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_abolition_movements, excluded).

% Administers verification of non-nuclear-weapon states' peaceful programs while accepting as beyond its mandate the verification of weapon states' disarmament progress. The regime verifies Article IV compliance (non-weapons intent) but has no comparable mandate to verify Article VI compliance. This asymmetry in verification authority is a structural feature: the regime's legitimacy rests on nuclear weapon states' consent, so it cannot audit them without losing access. Its exit would require abandoning the verification system entirely, which would trigger proliferation risks and loss of institutional authority.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_verification_regime, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, iaea_verification_regime, observer).

% Signed and ratified the Treaty on the Prohibition of Nuclear Weapons (2017–present), establishing a competing legal standard that prohibits all nuclear weapons possession and mandates complete disarmament. These states argue the TPNW supersedes the NPT by establishing a jus cogens (peremptory) norm prohibiting weapons. They are excluded from NPT decision-making (non-nuclear-weapon states have limited voting weight; weapon states dominate interpretation) and their treaty is treated as legally independent of the NPT, creating a parallel and conflicting authority structure.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states, excluded,
    organized, generational, constrained, global).

% States and corporations with enrichment/reprocessing technology that can be sold to non-weapon states under Article IV protections (ostensibly for peaceful programs). They benefit from the ambiguity in Article IV that Article VI produces: as long as Article VI remains non-binding, states have no urgent disarmament imperative, and dual-use technology exports remain commercially viable and politically acceptable. Their exit options include shifting to non-nuclear technology development (costly but possible) or accepting export restrictions in a stricter regime (reducing profit). They typically operate in states allied with nuclear weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters, beneficiary,
    powerful, biographical, mobile, global).

% Argue that the humanitarian harm of nuclear weapons — indiscriminate, uncontrollable, catastrophic civilian casualties — violates the prohibition on weapons causing unnecessary suffering (foundational principle of armed conflict law). They take the position that no weapons law can condition weapon possession on future disarmament promises; the weapons themselves must be prohibited now. They are outside the NPT structure and their legal analysis is treated as political advocacy rather than binding treaty interpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_advocates, observer,
    moderate, biographical, analytical, global).

% Non-nuclear states that rely on extended nuclear deterrence (security guarantees from allied weapon states) for their security. They benefit from Article IV access to peaceful nuclear technology and from the NPT's status-quo framing, which preserves their allies' deterrent. But they also pay a cost: they renounce weapons while their adversaries may pursue them; they depend on the durability of the security guarantee (which can be withdrawn). Under an abolitionist reading that delegitimizes weapon states' arsenals, their security guarantee becomes incoherent — they would lose both the weapons protection and the legal justification for nuclear-armed allies' posture.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, security_dependent_non_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, security_dependent_non_weapon_states, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: The founding problem was preventing horizontal proliferation (acquisition of nuclear weapons by non-weapon states) while preserving weapon states' arsenals as a temporary measure during a transition toward complete disarmament. The NPT Article IV-VI pairing was intended as a reciprocal bargain: non-weapon states renounce weapons in exchange for access to peaceful nuclear technology, and weapon states commit to negotiate in good faith toward disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration that the founding problem is dead comes from independent nonproliferation analysts (SIPRI, IPFM, FAS) who document zero progress on weapon-state disarmament treaties since the NPT-era bilateral treaties (SALT, START) concluded in the 1990s; from humanitarian organizations (ICRC, Physicians for Social Responsibility) arguing weapons cannot be conditional on future disarmament under humanitarian law; and from TPNW advocates citing 56 years of perpetual deferral as evidence the bargain was never genuine. Weapon states counter that the founding problem remains live (horizontal proliferation is still a threat; proliferation attempts by Iran and North Korea confirm it) and that disarmament is 'in progress' (China, Russia, and the USA maintain dialogue on arms control). The divergence is central to the kernel contest.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint extracts restraint from non-weapon states (renounce weapons, accept inspections) while weapon states extract a permanent exemption from the same restraint. This is not a symmetrical bargain: the constraint has produced zero binding disarmament on the weapon-state side over 56 years (1970–2026), while non-weapon states have maintained full renunciation. Suppression is elevated (0.72) because the constraint requires active suppression of alternative readings: the TPNW is kept structurally separate from the NPT, humanitarian-law arguments are deflected as 'political' rather than binding, and NNWS dissent in Review Conferences is routinely overridden by NWS consensus-blocking. Theater is moderate (0.41): there is real verification activity on the NNWS side and genuine diplomatic engagement, but the core extraction mechanism (indefinite NWS exemption from disarmament) is maintained through procedural control and blocked access to decision-making, not through technical necessity. The measurement series shows rising extractiveness, theater, and suppression requirement over the 56-year interval: extractiveness accumulates as dual-use technology access expands and NWS disarmament rhetoric diverges further from action; theater rises as periodic Review Conference reaffirmations of Article VI become more rhetorical and less actionable; suppression rises as TPNW advocacy and humanitarian-law arguments gain ground and require more active institutional defense of the status quo.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nuclear weapon states) reads this as a successful coordination mechanism: horizontal proliferation has been constrained, the NPT has held for over 50 years, and ongoing disarmament discussions honor Article VI. The payer seat (non-nuclear weapon states) reads it as extraction: they renounced weapons but got no reciprocal disarmament from weapon states, face dual-use technology constraints, and are locked into legal inferiority. The IAEA verification regime sits in asymmetry: it verifies non-weapon states' compliance but has no mandate to verify weapon states' progress, making it structurally captured. The TPNW advocates and abolition movements read the whole arrangement as illegitimate under humanitarian law — weapons must be prohibited now, not indefinitely postponed. These divergent readings flow from the structural asymmetry the constraint instantiates: the same rule set produces radically different payoffs depending on whether you are exempt from it (weapon states) or subject to it (everyone else).
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are full beneficiaries (d near 0.0): they collect the exemption from disarmament, set the interpretation of Article IV and VI, control dual-use technology flows, and face no enforced obligations. Non-nuclear weapon states are targets (d near 1.0): they renounce weapons, accept perpetual IAEA inspections, depend on weapon states for fuel and technology access, and bear legal inferiority. Dual-use technology exporters and established nuclear power operators are partial beneficiaries (d near 0.2–0.3): they benefit from Article IV's ambiguity and the deferral of strict proliferation limits, but they also depend on NNWS demand for their technology (if NNWS withdrew or broke out, demand would collapse). TPNW advocates and abolition movements are targets (d near 0.9): they are excluded from the decision-making structure, their legal arguments are dismissed as political, and the constraint actively suppresses their proposed alternative (the prohibition norm). The abolitionist reading produces the most asymmetric directionality profile in the NPT reading family: it treats the entire structure as asymmetric extraction, whereas the grand_bargain reading treats it as reciprocal asymmetry (weapon states accept strategic exposure, NNWS accept restraint), and the nonproliferation_primary reading treats weapon-state dominance as justified by security necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing horizontal proliferation through a bargain for eventual disarmament — is dead (the disarmament part never materialized), but the extraction machinery persists and has hardened over time. This is a classic mandatrophy pattern: the original coordination problem (preventing proliferation) remains live enough that the structure cannot be dismantled, but the reciprocal obligation (disarmament) has been indefinitely deferred, transforming what was supposed to be interim into permanent extraction. The theater_ratio rising from 0.22 to 0.41 over the interval documents the degradation: early NPT years saw more serious disarmament discussions and actual arsenal reductions (Cold War de-escalation, arms control treaties); by 2010–2026, disarmament discussions became increasingly rhetorical while NWS arsenals stabilized at high levels and dual-use technology proliferation expanded. The constraint survives not because the bargain works, but because NWS have the power to keep it stable and NNWS have no exit route without accepting unilateral vulnerability. Fixing it requires either NWS disarmament (catastrophically unlikely given security dilemma dynamics) or NNWS breakout (triggering sanctions and security crisis). The cost to fix is prohibitive from every seat except the weapon states (whose cost is accepting disarmament), so the extraction persists as inertial institutional performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness,
    'Does Article VI constitute a binding legal obligation requiring disarmament on a specified timeline, or is it an aspirational commitment permitting indefinite deferral?',
    'International Court of Justice advisory opinion; textual analysis of the original NPT negotiating record; comparison with other treaty disarmament language (e.g., Biological Weapons Convention''s unambiguous disarmament language).',
    'If binding with timeline, weapon states are in material breach and the abolitionist reading gains structural force — Article IV access becomes conditional on disarmament progress. If aspirational, the nonproliferation_primary reading''s authority is sustained — the asymmetry is justified by security necessity, not legal infidelity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_bindingness, conceptual, 'Whether Article VI is a binding legal obligation or aspirational political commitment.').

omega_variable(
    dual_use_technology_inseparability,
    'Are enrichment and reprocessing technology capabilities structurally inseparable from weapons development, or can they be reliably constrained to peaceful use?',
    'Technical analysis by independent nonproliferation experts (IPFM, SIPRI); case studies of dual-use technology exports and breakout pathways (Iran, North Korea, potential future NNWS); assessment of verification technology (environmental sampling, remote monitoring, information barriers).',
    'If inseparable, Article IV''s unconditional access clauses are fundamentally illegitimate — they guarantee proliferation risk. If separable, Article IV can be preserved under stronger verification regimes, and the extraction story weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_technology_inseparability, empirical, 'Whether peaceful and weapons-capable nuclear technology can be reliably separated through verification.').

omega_variable(
    humanitarian_law_supremacy,
    'Does humanitarian law''s prohibition on weapons causing indiscriminate harm override treaty regimes that condition weapon possession on future disarmament?',
    'International Court of Justice advisory opinion on the relationship between humanitarian law norms and the NPT; comparison with other weapons-prohibition precedents (biological, chemical, landmines); state practice and opinio juris on the legal status of nuclear weapons under humanitarian law.',
    'If humanitarian law is supreme, nuclear weapons are per se prohibited now, not conditionally permitted pending disarmament — the abolitionist reading''s core premise is sustained. If NPT regimes can override humanitarian norms through consent of parties, weapon states retain legal cover indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_law_supremacy, conceptual, 'Whether humanitarian law norms supersede or are subordinate to treaty-based weapon-state exemptions.').

omega_variable(
    nnws_security_vulnerability,
    'Does the constraint''s extraction (indefinite weapon-state arsenals, NNWS restraint) increase or decrease NNWS security, relative to scenarios where disarmament occurred or where all states retained weapons?',
    'Security studies analysis of arms-race dynamics, extended deterrence stability, and the security dilemmas created by asymmetric arsenals; case studies of NNWS security decisions (Japan, South Korea, Australia, Middle Eastern alignment choices).',
    'If extraction decreases NNWS security (because indefinite NWS weapons increase accident/escalation risk and undermine security guarantees), the payer-seat analysis shifts toward higher resistance to the constraint. If extraction increases security relative to alternatives (because global arms control is better than arms races), the grand_bargain framing gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_security_vulnerability, empirical, 'Whether asymmetric nuclear arsenals increase or decrease non-weapon-state security.').

omega_variable(
    tpnw_legal_status,
    'Does the TPNW constitute a separate legal regime with binding force only for signatories, or does it establish a jus cogens norm (peremptory international law) that binds all states?',
    'International law doctrine analysis; state practice on nuclear weapon possession by signatories vs. non-signatories; International Court of Justice guidance on jus cogens determination.',
    'If jus cogens, the abolitionist reading''s legal ground is solid and the NPT is superseded for all parties. If merely a separate regime, the NPT and TPNW coexist as parallel legal orders — the abolitionist reading applies only to TPNW signatories, and the constraint''s legitimacy becomes drawing-dependent (which regime governs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_legal_status, conceptual, 'Whether the TPNW establishes a universal peremptory norm or a separate regime binding only signatories.').

omega_variable(
    security_dilemma_tractability,
    'Is the security dilemma that locks weapon states into arsenals (mutual fear of disarming first) solvable through NPT renegotiation, or is it a structural feature of multipolarity that no treaty text can overcome?',
    'Game-theoretic analysis of first-mover disadvantage in disarmament; historical analysis of successful disarmament-verification regimes; assessment of verification technology that could overcome mutual-suspicion barriers (on-site inspections, satellite monitoring, confidence-building measures).',
    'If tractable through renegotiation, stronger Article VI language or verification mechanisms could restore the grand_bargain framing. If structural, the extraction persists regardless of treaty language — only multipolarity collapse (hegemon, bipolar clarity, or unified world government) would enable disarmament.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_dilemma_tractability, conceptual, 'Whether the security dilemma locking weapon states into arsenals can be overcome through treaty renegotiation or verification innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(npt__tr_t1970, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.26).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.36).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2018, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2018, 0.39).
narrative_ontology:measurement_basis(npt__tr_t2018, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2018, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement_basis(npt__be_t2018, observed).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2018, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement_basis(npt__su_t2018, observed).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(npt__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.18).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_weapons_prohibition).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, iaea_verification_asymmetry).

% DUAL FORMULATION NOTE:
% The NPT Article IV-VI pairing is a single contested kernel instantiated across three constraint stories (three readings). Each reading is a separate story with its own ε, beneficiary/victim structure, and classified type. Abolitionist (this story): treats the pairing as tangled_rope with high extraction; Article IV illegitimate under humanitarian law. Grand Bargain: treats pairing as rope with symmetric reciprocal obligations; both valid if each party fulfills its side. Nonproliferation Primary: treats pairing as rope with justified asymmetry; Article IV supreme, Article VI aspirational. Each story links to its siblings via network.affects_constraints. The abolitionist reading forecloses grand_bargain (if disarmament is mandatory now, the bargain's reciprocal deferral collapses) but coexists with nonproliferation_primary (they are held by different institutional seats). All three stories are required to model the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
