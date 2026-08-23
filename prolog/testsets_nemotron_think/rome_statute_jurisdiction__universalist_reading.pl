% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal Jurisdiction Mandate (Universalist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The universalist reading of the Rome Statute asserts that the ICC's
 *   jurisdiction over core crimes is truly universal — extending to nationals
 *   of non-party states via the territorial principle (Article 12(2)(a)) and
 *   UN Security Council referrals (Article 13(b)). This reading treats state
 *   consent as irrelevant to the Court's jurisdiction over the most serious
 *   crimes, positioning the ICC as a supra-national authority that overrides
 *   sovereignty. The claimed_type is tangled_rope: the Statute has a genuine
 *   coordination function (complementarity, permanent court for atrocities)
 *   but the universalist extension extracts sovereign authority from
 *   non-consenting states without their representation. The metrics reflect
 *   increasing extractiveness over time as the Court has asserted
 *   jurisdiction over non-party nationals (e.g., Afghanistan investigation
 *   covering US personnel, Palestine investigation, Ukraine investigation
 *   covering Russian nationals) while facing mounting non-cooperation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.72).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.78).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Jurisdiction Mandate (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'b2fed428-7191-4a66-9536-b31d15547a8f').
narrative_ontology:cs_kernel_codification('b2fed428-7191-4a66-9536-b31d15547a8f', formalized).
narrative_ontology:cs_authority_grounding('b2fed428-7191-4a66-9536-b31d15547a8f', extraction).
narrative_ontology:cs_interpretation_layer_present('b2fed428-7191-4a66-9536-b31d15547a8f').
narrative_ontology:cs_reading_relation('b2fed428-7191-4a66-9536-b31d15547a8f', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('b2fed428-7191-4a66-9536-b31d15547a8f', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('b2fed428-7191-4a66-9536-b31d15547a8f', foundational, universal_jurisdiction_over_core_crimes).
narrative_ontology:cs_axiom_status(universal_jurisdiction_over_core_crimes, holdable).
narrative_ontology:cs_axiom_grounding('b2fed428-7191-4a66-9536-b31d15547a8f', universal_jurisdiction_over_core_crimes, deontological).
narrative_ontology:cs_axiom('b2fed428-7191-4a66-9536-b31d15547a8f', secondary, complementarity_is_procedural_not_substantive).
narrative_ontology:cs_axiom_status(complementarity_is_procedural_not_substantive, holdable).
narrative_ontology:cs_axiom_grounding('b2fed428-7191-4a66-9536-b31d15547a8f', complementarity_is_procedural_not_substantive, conventional).
narrative_ontology:cs_axiom('b2fed428-7191-4a66-9536-b31d15547a8f', foundational, territorial_principle_binds_non_parties).
narrative_ontology:cs_axiom_status(territorial_principle_binds_non_parties, holdable).
narrative_ontology:cs_axiom_grounding('b2fed428-7191-4a66-9536-b31d15547a8f', territorial_principle_binds_non_parties, conventional).
narrative_ontology:cs_reference_frame('b2fed428-7191-4a66-9536-b31d15547a8f', rome_statute_universal_mandate).
narrative_ontology:cs_drift_state('b2fed428-7191-4a66-9536-b31d15547a8f', contemporary_icc_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2fed428-7191-4a66-9536-b31d15547a8f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_institution).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states_sovereignty).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_individuals).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, universal_jurisdiction_over_core_crimes).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, complementarity_as_procedural_safeguard).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, international_criminal_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ICC (Office of the Prosecutor, Chambers, Registry) administers the Rome Statute and asserts jurisdiction over nationals of non-party states via territorial principle (crimes committed on party state territory) and UNSC referrals. It collects institutional authority, budget, and legitimacy from exercising this universal mandate. Its exit options are arbitrage-grade: it can shift focus, select situations, and leverage complementarity determinations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_institution, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, icc_institution, beneficiary).

% States parties gain a permanent court for core crimes without bearing full enforcement costs. They participate in the Assembly of States Parties, fund the Court, and can refer situations. Their exit is mobile: they could withdraw (with notice) but gain significant coordination benefit from the Court's existence.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, party_states, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, party_states, agenda_setter).

% Victims of genocide, crimes against humanity, war crimes, and aggression gain access to international justice regardless of their state's consent. They participate in proceedings, receive reparations. Their exit is trapped: they cannot access alternative justice mechanisms when domestic systems fail or are complicit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, global).

% Nationals of non-party states (e.g., US, Russia, China, Israel citizens) can be investigated and prosecuted for crimes committed on party state territory or via UNSC referral, without their state's consent. They bear the full cost of the Court's jurisdiction with no representation in the ASP. Exit is constrained: they can avoid party state territory, but UNSC referrals (e.g., Darfur, Libya) are inescapable.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals, payer,
    moderate, biographical, constrained, global).

% Non-party states lose sovereign control over prosecution of their nationals for core crimes. They cannot opt out of territorial jurisdiction or UNSC referrals (where they may hold veto power but not always). They bear diplomatic, political, and legal costs. Exit is constrained: they can refuse cooperation, sign bilateral immunity agreements, but the Court's legal authority persists.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states_sovereignty, payer,
    powerful, generational, constrained, global).

% Individuals accused by the ICC face prosecution with no alternative forum. The Court's jurisdiction is mandatory once triggered; they cannot choose a different court. Exit is trapped: they must defend before the ICC or face arrest warrant and international isolation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_individuals, payer,
    powerless, immediate, trapped, global).

% The UNSC can refer situations in non-party states to the ICC (Article 13(b)), triggering universal jurisdiction without state consent. P5 members control this trigger. They collect geopolitical leverage. Exit is arbitrage: they can use or withhold referrals strategically.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars analyze the Court's jurisprudence, complementarity decisions, and universal jurisdiction claims. They do not collect rents or bear costs directly. Their exit is analytical: they can change interpretive frameworks.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a permanent international court for core crimes (genocide, crimes against humanity, war crimes, aggression) that solves the collective action problem of prosecuting atrocities when domestic systems are unwilling or unable. Complementarity ensures the Court only acts when states fail.
% TRANSFER_FUNCTION: Transfers sovereign adjudicative authority over core crimes from states (including non-party states) to the ICC. Moves prosecutorial power, legal costs, and legitimacy from national systems to the international institution. Non-party states and their nationals bear the transfer; the ICC and victims receive it.
% ABSENT_VOICES: Non-party states that reject the Statute entirely (US, Russia, China, India, Israel, etc.) are structurally excluded from the Assembly of States Parties but remain subject to jurisdiction. Their objections to universal jurisdiction over their nationals without consent are not represented in the Court's governance. Populations in non-party states who might benefit from ICC intervention but whose governments block referral are also excluded.
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight, the ICC would lose its claimed authority over non-party state nationals via territorial and UNSC triggers. Prosecutions of US, Russian, Israeli, etc. nationals for crimes on party territory or referred by UNSC would collapse. The Court would revert to a purely consensual body. The international justice landscape would reorganize around strict sovereignty.
% FOUNDING_PROBLEM: The post-WWII/Nuremberg gap: no permanent court existed to prosecute core international crimes when states were unwilling or unable. Ad hoc tribunals (ICTY, ICTR) were temporary, selective, and politically contingent. The founding problem was creating a permanent, universal institution that could act without case-by-case UNSC authorization.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and party states attest the problem remains live (ongoing atrocities, domestic impunity). Non-party states and sovereigntist scholars attest the problem is substantially solved by improved domestic capacity and the complementarity principle, and the universalist extension now serves institutional expansion. The 2010 Kampala Review Conference documents and the 2020 Independent Expert Review report (by Richard Goldstone et al., outside the beneficiary set) corroborate the contested status.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the universalist reading claims authority over non-consenting states' nationals, transferring sovereign adjudicative power to an institution they did not join. Suppression (0.78) is high because the constraint's persistence depends on active enforcement: arrest warrants, state cooperation obligations, UNSC referral mechanism, and the complementarity test that displaces national jurisdiction. Theater_ratio (0.42) is moderate: complementarity proceedings and preliminary examinations perform coordination, but a growing share of activity (situation selection, non-party territorial jurisdiction) serves institutional self-justification. Accessibility_collapse (0.81) is high for non-party nationals — once the Court asserts jurisdiction, alternatives (domestic prosecution, immunity) collapse. Resistance (0.74) is high: US non-cooperation (ASPA, bilateral immunity agreements), African Union withdrawal debates, Russian/Chinese rejection, Israeli non-engagement.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC/party-state seat, the arrangement is a rope with coordination benefits: a permanent court ending impunity. From the non-party state/national seat, it is a snare: jurisdiction imposed without consent, enforced by an institution they cannot influence. The engine will compute this divergence from the power/exit/scope data. The universalist reading itself denies the gap — it claims universal benefit — but the structural data reveals it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC institution and party states are structural beneficiaries (d near 0.0-0.2): they gain authority, legitimacy, and justice infrastructure without bearing full costs. Non-party states and their nationals are structural targets (d near 0.8-1.0): they bear jurisdictional exposure, diplomatic costs, and prosecution risk with no governance voice. Victims of core crimes are beneficiaries with trapped exit (d near 0.3 — they gain access but cannot choose the forum). Accused individuals are full targets with trapped exit (d=1.0). UNSC holds arbitrage power (d variable). The universalist reading maximizes the directionality gap between beneficiaries and non-consenting targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading prevents mislabeling the ICC as pure coordination (rope) by exposing the asymmetric extraction on non-consenting states. It also prevents mislabeling as pure extraction (snare) because complementarity and the victim-access function are genuine coordination. The mandate (ending impunity for core crimes) is live but the universalist extension has outpaced the consent base — a classic mandatrophy pattern where the institutional mission expands beyond its legitimating consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_mandate_natural_vs_constructed,
    'Is the universal jurisdiction mandate over non-party nationals a genuine natural law of international justice (crimes so grave they transcend consent) or a constructed institutional power grab by the ICC and party states?',
    'Longitudinal analysis of state practice and opinio juris on universal jurisdiction for core crimes; whether non-party states increasingly accept territorial jurisdiction as customary law or persistently reject it.',
    'If natural law, the high extractiveness metrics reflect enforcement of pre-existing obligation (lower effective extraction). If constructed, the metrics reflect genuine institutional extraction (higher effective extraction). Determines whether the constraint is a mountain (natural law) or tangled_rope/snare (constructed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_mandate_natural_vs_constructed, conceptual, 'Natural-law vs. constructed status of universal jurisdiction over non-consenting states.').

omega_variable(
    complementarity_coordination_vs_cover,
    'Is the complementarity mechanism (Article 17) a genuine coordination function that limits ICC overreach, or a cover story that legitimates universal jurisdiction while the Court selectively targets non-party nationals?',
    'Empirical analysis of complementarity decisions: proportion of situations where Court defers to national proceedings vs. proceeds; whether deferrals disproportionately favor party states or powerful non-party states.',
    'If genuine coordination, the tangled_rope classification holds (coordination + extraction). If cover, the constraint trends toward snare (extraction dominant, coordination performative). Affects theater_ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_coordination_vs_cover, empirical, 'Whether complementarity is functional constraint or legitimating theater.').

omega_variable(
    unsc_referral_legitimacy,
    'Do UNSC referrals under Article 13(b) confer genuine legitimacy on ICC jurisdiction over non-party states, or are they geopolitical instruments that replicate power asymmetries?',
    'Case study of referrals (Darfur 2005, Libya 2011) vs. non-referrals (Syria vetoed, Myanmar not referred): analyze whether referral patterns track gravity/impunity or P5 interests.',
    'If referrals are legitimate triggers, UNSC role is coordination. If geopolitical instruments, the UNSC trigger is an extraction amplifier for P5 interests. Affects UNSC stakeholder classification and network effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_legitimacy, empirical, 'Legitimacy of UNSC referral trigger for universal jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_statute_univ_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(rome_statute_univ_tr_t2006, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2006, 0.3).
narrative_ontology:measurement(rome_statute_univ_tr_t2010, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(rome_statute_univ_tr_t2014, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(rome_statute_univ_tr_t2018, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(rome_statute_univ_tr_t2022, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2022, 0.41).
narrative_ontology:measurement(rome_statute_univ_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(rome_statute_univ_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.45).
narrative_ontology:measurement(rome_statute_univ_be_t2006, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2006, 0.52).
narrative_ontology:measurement(rome_statute_univ_be_t2010, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(rome_statute_univ_be_t2014, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(rome_statute_univ_be_t2018, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2018, 0.69).
narrative_ontology:measurement(rome_statute_univ_be_t2022, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2022, 0.71).
narrative_ontology:measurement(rome_statute_univ_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rome_statute_univ_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement(rome_statute_univ_su_t2006, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2006, 0.62).
narrative_ontology:measurement(rome_statute_univ_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(rome_statute_univ_su_t2014, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(rome_statute_univ_su_t2018, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(rome_statute_univ_su_t2022, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2022, 0.77).
narrative_ontology:measurement(rome_statute_univ_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, icc_complementarity_principle).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, unsc_referral_authority).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, universal_jurisdiction_customary_law).

% DUAL FORMULATION NOTE:
% This universalist_reading decomposes the rome_statute_jurisdiction kernel with sovereigntist_reading and hybrid_complementarity_reading. The universalist reading claims the Statute's text and object/purpose establish universal mandate transcending consent (ε=0.72). The sovereigntist reading claims consent is jurisdictional prerequisite (ε≈0.15 for non-party nationals). The hybrid reading balances both via complementarity (ε≈0.45). These are distinct constraints with different ε, beneficiaries, victims, and types — linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, powerful, 0.85).
constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
