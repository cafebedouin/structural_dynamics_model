% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation Primary Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (NPT) pairs Article IV (non-weapon
 *   states' 'inalienable right' to peaceful nuclear energy, conditional on
 *   Article III safeguards verification) with Article VI (weapon states'
 *   obligation to pursue nuclear disarmament negotiations in good faith).
 *   This reading — nonproliferation_primary — holds that Article IV's benefit
 *   is real but conditional on verification compliance; Article VI is
 *   aspirational and non-justiciable, creating no enforceable timeline; the
 *   treaty's authority derives from weapon states' security interest in
 *   preventing horizontal proliferation. This produces a permanent two-tier
 *   order: weapon states retain arsenals excluded from treaty enforcement
 *   while non-weapon states accept permanent restraint. The coordination
 *   function (preventing horizontal proliferation) is genuine but asymmetric
 *   — the constraint extracts permanent compliance from non-weapon states
 *   without reciprocal, enforceable disarmament from weapon states.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiary (institutional/arbitrage) — retain nuclear arsenals, control verification regime, extract compliance without reciprocal disarmament
 *   - nuclear_alliance_partners: Secondary beneficiary (organized/constrained) — extended deterrence beneficiaries, influence non-proliferation agenda
 *   - non_nuclear_weapon_states: Primary payer (organized/identity_locked) — bear verification costs, forego weapons option, receive conditional energy access
 *   - late_developers: Secondary payer (moderate/trapped) — face stricter verification, technology denial, higher compliance costs
 *   - iaea_secretariat: Agenda setter (institutional/analytical) — administers verification, technical authority, enforcement mechanism
 *   - non_aligned_movement: Excluded (organized/constrained) — collective voice for Article VI implementation, structurally marginalized in decision-making
 *   - tpnw_proponents: Excluded (moderate/identity_locked) — humanitarian law frame, delegitimize two-tier order, no seat in NPT governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.65).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '6218ea0d-2981-4038-86f8-c94b0cca011b').
narrative_ontology:cs_kernel_codification('6218ea0d-2981-4038-86f8-c94b0cca011b', formalized).
narrative_ontology:cs_authority_grounding('6218ea0d-2981-4038-86f8-c94b0cca011b', extraction).
narrative_ontology:cs_interpretation_layer_present('6218ea0d-2981-4038-86f8-c94b0cca011b').
narrative_ontology:cs_reading_relation('6218ea0d-2981-4038-86f8-c94b0cca011b', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('6218ea0d-2981-4038-86f8-c94b0cca011b', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('6218ea0d-2981-4038-86f8-c94b0cca011b', foundational, horizontal_proliferation_prevention_primacy).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('6218ea0d-2981-4038-86f8-c94b0cca011b', horizontal_proliferation_prevention_primacy, instrumental).
narrative_ontology:cs_axiom('6218ea0d-2981-4038-86f8-c94b0cca011b', foundational, article_vi_aspirational_non_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('6218ea0d-2981-4038-86f8-c94b0cca011b', article_vi_aspirational_non_justiciable, conventional).
narrative_ontology:cs_axiom('6218ea0d-2981-4038-86f8-c94b0cca011b', foundational, weapon_state_arsenals_exempt_from_enforcement).
narrative_ontology:cs_axiom_status(weapon_state_arsenals_exempt_from_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6218ea0d-2981-4038-86f8-c94b0cca011b', weapon_state_arsenals_exempt_from_enforcement, conventional).
narrative_ontology:cs_reference_frame('6218ea0d-2981-4038-86f8-c94b0cca011b', id_1968_grand_bargain_reciprocity).
narrative_ontology:cs_drift_state('6218ea0d-2981-4038-86f8-c94b0cca011b', post_2010_review_conference_failures, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6218ea0d-2981-4038-86f8-c94b0cca011b', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_alliance_partners).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, late_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain nuclear arsenals excluded from treaty enforcement; control verification agenda through UNSC veto and IAEA Board of Governors dominance; collect security benefit of horizontal nonproliferation without enforceable disarmament obligation. Can withdraw legally but regime serves their security interest — exit is theoretically available but structurally irrational.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from extended deterrence and influence over nonproliferation agenda (NSG, IAEA Board). Host nuclear weapons or support deployment. Bear some verification costs but gain security umbrella. Exit constrained by alliance commitments and security architecture.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_alliance_partners, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_alliance_partners, agenda_setter).

% Bear full IAEA safeguards costs (comprehensive safeguards agreements, additional protocols); forego nuclear weapons option permanently; receive conditional access to nuclear technology/fuel subject to NSG guidelines and supplier state discretion. Identity-locked: sovereignty doctrine, security commitments, energy infrastructure investments, and NPT membership itself make withdrawal politically and economically prohibitive — withdrawal signals weapons intent and triggers sanctions/isolation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states, payer,
    organized, generational, identity_locked, global).

% Face stricter verification (e.g., Iran Additional Protocol+, JCPOA), technology denial (enrichment/reprocessing restrictions), higher compliance costs, and political pressure. Less institutional weight in NPT governance. Trapped: need nuclear energy for development but cannot access full fuel cycle; withdrawal invites severe sanctions/military threat; compliance yields diminishing returns.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, late_developers, payer,
    moderate, biographical, trapped, regional).

% Administers verification regime (safeguards inspections, compliance reporting); technical authority on peaceful use vs. diversion; enforcement mechanism via Board of Governors/UNSC referral. Derives institutional legitimacy and budget from the regime. Neither collects extraction nor bears its costs directly — operationalizes the constraint.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Collective voice for Article VI implementation (disarmament), Article IV access (technology transfer), and NPT universality. Structurally marginalized in decision-making: no veto, no Board seats by right, consensus rules allow weapon states to block. Exit constrained: leaving NPT loses Article IV benefits and legitimacy; staying legitimizes asymmetry.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_aligned_movement, excluded,
    organized, generational, constrained, global).

% Advocate humanitarian law frame (TPNW): nuclear weapons categorically prohibited, Article VI mandates elimination, Article IV illegitimate if it enables dual-use proliferation. No seat in NPT governance — TPNW and NPT are separate treaty regimes. Identity-locked: humanitarian identity commits them to prohibition frame; cannot participate in NPT review conferences as equal parties; their exclusion is structural.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_proponents, excluded,
    moderate, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation by verifying non-diversion of peaceful nuclear programs (Article III/IV), providing a universal legal framework for nuclear trade and cooperation, and creating a security architecture where weapon states forego horizontal proliferation in exchange for non-weapon state restraint.
% TRANSFER_FUNCTION: Moves permanent verification compliance, foregone weapons option, and conditional technology access from non-weapon states to the regime; moves security benefit (horizontal nonproliferation) and institutional control to weapon states; Article VI disarmament obligation moves nothing enforceable — aspirational timeline, no transfer of weapons or capability.
% ABSENT_VOICES: Non-aligned movement (collective Article VI/disarmament voice), TPNW states parties (humanitarian prohibition frame), late developers facing technology denial (Iran, Brazil, Egypt, Saudi Arabia), civil society/humanitarian organizations (ICAN, ICRC). They are absent from NPT decision-making structures: no veto, no permanent Board seats, consensus rules enable blocking. TPNW proponents are in a separate treaty regime entirely.
% DISAPPEARANCE_RATIONALE: If the NPT Article IV/VI pairing vanished overnight: horizontal proliferation constraints would collapse (no universal verification regime, no legal barrier to withdrawal); nuclear technology trade would reorganize around bilateral agreements and NSG would lose treaty anchor; weapon states would lose legal framework constraining horizontal spread; non-weapon states would lose Article IV energy access guarantees but also Article III verification burden; TPNW would become the sole universal prohibition framework. The global nuclear order would reorganize fundamentally.
% FOUNDING_PROBLEM: 1968: Prevent horizontal nuclear proliferation (more weapon states = greater instability/risk) while enabling peaceful nuclear energy access for development; weapon states accept disarmament pursuit (Article VI) as reciprocal commitment to justify non-weapon state restraint.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and allies attest horizontal proliferation risk persists (founding problem live) and disarmament progresses incrementally (Article VI implemented). Non-aligned movement, TPNW states, ICJ Nuclear Weapons Advisory Opinion (1996), and independent arms control experts attest: horizontal risk managed but Article VI disarmament obligation hollowed out — reciprocal foundation collapsed, founding problem transformed into permanent hierarchy. No consensus on status.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because non-weapon states bear permanent verification costs and foregone weapons option while weapon states face no enforceable disarmament timeline. Suppression (0.65) is substantial: the regime actively prevents withdrawal (X-Article withdrawal clause never invoked successfully), controls technology transfer via NSG, and marginalizes dissenting voices. Theater ratio (0.42) is moderate-high: Article VI review conferences produce ritualistic 'action plans' with no enforcement; Article IV access is increasingly gated. Accessibility collapse (0.71) is high because the treaty's legal framework, security architecture, and energy economics make alternatives (withdrawal, indigenous fuel cycle, TPNW) structurally prohibitive. Resistance (0.58) is significant but fragmented: NAM statements, TPNW adoption, occasional safeguards disputes — but no coalition has altered the core asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat (beneficiary, institutional power, arbitrage exit), the constraint is a successful coordination mechanism preventing horizontal proliferation — the regime works. From the non-weapon state seat (payer, organized power, identity_locked exit), the same structure is asymmetric extraction: permanent restraint without reciprocal disarmament, verification as gatekeeping, review conferences as theater. From the late developer seat (payer, moderate power, trapped exit), extraction is more severe: stricter verification, technology denial, no pathway to parity. The engine computes these divergences from the structural data; the claim (tangled_rope) reflects the genuine coordination function coexisting with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are structural beneficiaries (d ~ 0.15): they collect the non-proliferation benefit (security from horizontal proliferation) while bearing no enforceable disarmament cost. Their exit is arbitrage — they could withdraw but the regime serves their security interest. Nuclear alliance partners are secondary beneficiaries (d ~ 0.25): extended deterrence + influence without direct verification burden. Non-nuclear weapon states are primary payers (d ~ 0.85): they bear verification costs, forego weapons option, accept conditional energy access, and cannot exit without severe security/economic penalty — identity_locked by sovereignty, security doctrine, and energy infrastructure commitments. Late developers are more severely targeted (d ~ 0.9): same burdens plus technology denial and stricter verification. IAEA secretariat is agenda_setter (d ~ 0.4): administers the constraint, bears implementation costs, but derives institutional legitimacy from it. NAM and TPNW proponents are excluded: they would object to the asymmetry but have no structural seat in NPT governance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation while enabling peaceful nuclear energy) remains live — horizontal proliferation risk persists. But the reciprocal obligation (Article VI disarmament) has atrophied into aspirational rhetoric. The constraint now extracts permanent compliance from non-weapon states to maintain a hierarchy whose original justification (temporary bargain pending disarmament) has collapsed. This is mandatrophy: the coordination function (nonproliferation) persists but the reciprocal foundation has hollowed out, leaving asymmetric extraction as the structural reality. Classification as tangled_rope (not snare) is mandated because the nonproliferation coordination function is genuine and valued by all parties — but the extraction is real, enforceable, and one-sided.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the NPT''s Article IV/VI pairing instantiate a genuine reciprocal bargain (grand_bargain reading) or a permanent two-tier order where non-weapon states bear permanent restraint without enforceable disarmament reciprocity (nonproliferation_primary reading)?',
    'Legal-historical analysis of 1968 negotiating record; ICJ advisory opinions; state practice since 1970; TPNW negotiation positions as revealed preference.',
    'If grand_bargain is structurally correct, non-weapon state exit options are stronger (legitimacy of withdrawal conditional on Article VI breach). If nonproliferation_primary is correct, exit is identity-locked — withdrawal is materially possible but politically and security-cost prohibitive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Contested kernel reading: whether the treaty''s authority structure is reciprocal or hierarchical').

omega_variable(
    article_vi_justiciability,
    'Is Article VI legally binding with enforceable timeline, or aspirational and non-justiciable as this reading claims?',
    'ICJ Nuclear Weapons Advisory Opinion (1996) para 105; subsequent NPT Review Conference outcomes; state practice on disarmament reporting.',
    'If Article VI is justiciable with timeline, extraction from non-weapon states is conditional and time-bounded (lower ε). If non-justiciable, extraction is permanent and one-sided (higher ε, supports tangled_rope/snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'Whether Article VI creates enforceable disarmament obligation or aspirational pledge').

omega_variable(
    security_interest_legitimacy,
    'Does the treaty''s authority genuinely derive from weapon states'' security interest in preventing horizontal proliferation, or is that a post-hoc rationalization for a static hierarchy?',
    'Counterfactual: if horizontal proliferation risk vanished, would weapon states maintain the treaty? Compare to CWCC/BWCC where possessor states accepted destruction obligations.',
    'If security interest is genuine and reciprocal, coordination function is stronger (rope-ward). If rationalization, extraction is primary (snare-ward).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_interest_legitimacy, preference, 'Whether the declared authority grounding reflects genuine security logic or constructed hierarchy').

omega_variable(
    civilian_nuclear_access_reality,
    'Is Article IV''s ''inalienable right'' to peaceful nuclear energy meaningfully exercisable by non-weapon states under Article III verification, or is the verification regime itself a gatekeeping mechanism?',
    'IAEA safeguards implementation data; NSG export control denials; technology transfer disputes (e.g., Iran, Brazil, South Korea).',
    'If Article IV access is real, coordination function is genuine (rope component). If gatekeeping, Article IV is theater for extraction (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_nuclear_access_reality, empirical, 'Whether the Article IV benefit is structurally delivered or rhetorically promised').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1995, 0.31).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iii_verification_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nsg_export_controls).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_treaty_framework).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_system).

% DUAL FORMULATION NOTE:
% Kernel npt_article_iv_vi_pairing decomposes into three constraint stories: nonproliferation_primary (this file), grand_bargain, and abolitionist. Each has distinct ε, beneficiary/victim structure, and claimed_type. nonproliferation_primary extracts from non-weapon states via permanent restraint without reciprocal disarmament; grand_bargain frames extraction as conditional and time-bounded; abolitionist frames the entire pairing as illegitimate extraction. All three share the treaty text as referent but instantiate different constraints per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.15).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, organized, 0.85).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
