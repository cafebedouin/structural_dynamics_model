% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country Two Systems — Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The sovereignty primacy reading of One Country Two Systems holds that
 *   Hong Kong's autonomy is a delegation from PRC sovereign authority,
 *   revocable at will, and subordinate to national security and territorial
 *   integrity imperatives. This reading became operationally dominant after
 *   the 2019 protests through the 2020 National Security Law (promulgated by
 *   NPCSC, bypassing HK legislature), the 2021 electoral overhaul ('patriots
 *   administering Hong Kong'), and the 2024 Article 23 legislation
 *   (Safeguarding National Security Ordinance). The constraint extracts
 *   political autonomy, civil liberties, and judicial independence from Hong
 *   Kong residents and institutions, transferring control to PRC central
 *   authority and its local proxies. The coordination function — managing
 *   sovereign integration without economic rupture — is real but increasingly
 *   vestigial; the extraction function — securing regime stability and
 *   eliminating opposition — is now primary. Claim/metric independence: the
 *   constraint is CLAIMED as tangled_rope (coordination + extraction hybrid)
 *   while metrics show high extraction (0.78) and high suppression (0.85) —
 *   the engine computes per-seat types from this structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country Two Systems — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '6b792ae8-235f-4355-8ab9-63e43b0becb3').
narrative_ontology:cs_kernel_codification('6b792ae8-235f-4355-8ab9-63e43b0becb3', formalized).
narrative_ontology:cs_authority_grounding('6b792ae8-235f-4355-8ab9-63e43b0becb3', lineage).
narrative_ontology:cs_interpretation_layer_present('6b792ae8-235f-4355-8ab9-63e43b0becb3').
narrative_ontology:cs_reading_relation('6b792ae8-235f-4355-8ab9-63e43b0becb3', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6b792ae8-235f-4355-8ab9-63e43b0becb3', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('6b792ae8-235f-4355-8ab9-63e43b0becb3', foundational, prc_sovereignty_source_of_hk_autonomy).
narrative_ontology:cs_axiom_status(prc_sovereignty_source_of_hk_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6b792ae8-235f-4355-8ab9-63e43b0becb3', prc_sovereignty_source_of_hk_autonomy, conventional).
narrative_ontology:cs_axiom('6b792ae8-235f-4355-8ab9-63e43b0becb3', foundational, national_security_override_autonomy).
narrative_ontology:cs_axiom_status(national_security_override_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6b792ae8-235f-4355-8ab9-63e43b0becb3', national_security_override_autonomy, conventional).
narrative_ontology:cs_axiom('6b792ae8-235f-4355-8ab9-63e43b0becb3', secondary, npcsc_interpretation_power_final).
narrative_ontology:cs_axiom_status(npcsc_interpretation_power_final, holdable).
narrative_ontology:cs_axiom_grounding('6b792ae8-235f-4355-8ab9-63e43b0becb3', npcsc_interpretation_power_final, conventional).
narrative_ontology:cs_reference_frame('6b792ae8-235f-4355-8ab9-63e43b0becb3', basic_law_promulgation_1990).
narrative_ontology:cs_drift_state('6b792ae8-235f-4355-8ab9-63e43b0becb3', post_nsl_2020, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6b792ae8-235f-4355-8ab9-63e43b0becb3', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_political_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_apparatus).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_camp).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary_independence).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_media).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents_political_participation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elite).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elite).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, prc_constitutional_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_primacy_over_local_autonomy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, sovereignty_as_delegation_not_treaty_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the Basic Law, interprets its provisions through NPCSC, enacted the 2020 National Security Law directly into HK law, and maintains final authority over national security and foreign affairs. Collects political control, territorial integrity assurance, and regime stability from the constraint.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Controls HK executive and legislative branches through electoral architecture redesigned after 2021. Benefits from reduced opposition competition, access to mainland resources, and political patronage networks. Identity fused with the sovereignty framework — exit means political irrelevance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_beijing_political_establishment, beneficiary,
    organized, biographical, identity_locked, local).

% Office for Safeguarding National Security operates in HK with extraterritorial jurisdiction. Mainland security agents can operate in HK. Cases involving 'state secrets' or 'national security' tried under mainland procedural rules. Directly extracts operational freedom from HK legal autonomy.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_apparatus, agenda_setter).

% Elected representatives disqualified en masse (2020-2021), primary organizers imprisoned under NSL (47+ in primary case), organizations disbanded (HKCTU, Civil Human Rights Front, Apple Daily). Political participation effectively criminalized when it challenges the sovereignty framework. No viable electoral path; exile or imprisonment are primary exits.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_camp, payer,
    organized, biographical, trapped, local).

% Unions, professional associations, NGOs, student groups, religious organizations face NSL prosecution risk for 'collusion with foreign forces' or 'subversion'. Funding channels cut. Many self-censor or dissolve. Exit through relocation or dormancy.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations, payer,
    moderate, biographical, constrained, local).

% Common law tradition with CFA as final appellate court. NSL Article 55 allows mainland jurisdiction for 'complex' or 'serious' cases; NPCSC interpretation power overrides CFA. Judges appointed by CE on recommendation of JSC — political vetting. Judicial review of national security matters practically nullified.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary_independence, payer,
    institutional, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary_independence).

% Apple Daily shut down, assets frozen, executives jailed. Stand News, Citizen News, Mad Dog Daily closed. RTHK restructured under government control. Remaining outlets practice anticipatory self-censorship. Exit through offshore relocation (e.g. Initium, The Witness) or closure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_media, payer,
    moderate, biographical, constrained, local).

% Voter turnout collapsed (30.2% in 2021 LegCo vs 58.3% in 2016; 27.5% in 2023 DC). District Councils stripped of elected majority. Public assembly effectively banned since 2020. Political expression carries NSL risk. Exit through emigration (BN(O) route, skilled migration) or political withdrawal.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents_political_participation, payer,
    powerless, biographical, constrained, local).

% Retains capital mobility, mainland market access, property rights protection. Supports stability for commercial predictability. Bears reputational risk and talent retention costs. Can relocate capital and operations — arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elite, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_elite, payer).

% UK (Sino-British Joint Declaration), US (HK Policy Act), EU, G7 — claim treaty obligations violated. Diplomatic protests, sanctions on officials, BN(O) visa routes. No enforcement mechanism within HK. Structural exclusion from interpretation and implementation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_governments_treaty_parties, excluded,
    institutional, generational, analytical, global).

% UN human rights bodies, ICJ scholars, common law jurists, NGOs (HRW, Amnesty, ICJ) document erosion of autonomy, fair trial rights, freedoms. Analytical seat — no enforcement power, but produces the normative benchmark against which the constraint is measured.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the integration of a distinct legal-economic system (HK) into a sovereign state (PRC) without immediate systemic rupture — provides a transitional framework for sovereignty transfer that preserves commercial utility and international connectivity while political integration proceeds.
% TRANSFER_FUNCTION: Moves political autonomy, civil liberties, judicial independence, and legislative power from HK institutions and residents to PRC central authority and its local proxies, in exchange for economic stability, mainland market access, and the 'one country' framework's continuity.
% ABSENT_VOICES: The HK electorate as constituted pre-2021 (pro-democracy majority), the HK Bar Association speaking independently, the Legislative Council opposition, independent district councilors — all structurally excluded by electoral redesign, disqualification, and NSL prosecution risk. They would argue for substantive autonomy, judicial independence, and treaty compliance but are not in the room where the constraint operates.
% DISAPPEARANCE_RATIONALE: If the sovereignty primacy constraint vanished overnight, HK would revert to a high-autonomy model with independent judiciary, free elections, civil liberties, and meaningful legislative opposition — the political system would fundamentally reorganize. The PRC would lose its primary mechanism for directing HK's political trajectory. The commercial and legal infrastructure would face acute uncertainty during transition.
% FOUNDING_PROBLEM: How to recover sovereignty over Hong Kong after 155 years of British colonial rule without destroying the territory's economic value, international financial status, and social stability — and without triggering mass capital flight or international conflict.
% FOUNDING_PROBLEM_CORROBORATION: PRC official narrative: founding problem remains live — 'national security' threats (foreign interference, separatism, subversion) require continued central control. HK pro-democracy camp (outside beneficiaries): founding problem substantially solved by 1997 handover; post-2020 measures exceed recovery needs and constitute political suppression. UK government (Sino-British Joint Declaration co-signatory): declares China in 'state of ongoing non-compliance' (annual reports 2020-2024). International legal scholars: Basic Law Articles 5, 8, 18, 23, 158, 159 establish autonomy as substantive right, not revocable delegation — the founding problem was sovereignty recovery with autonomy guarantee, not sovereignty recovery with autonomy revocation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the constraint removes the core political rights promised in the Basic Law (universal suffrage Articles 45/68, civil liberties Articles 27-39, judicial independence Article 85, legislative autonomy Article 73) and replaces them with centrally controlled mechanisms. Suppression 0.85: NSL Article 55 (mainland jurisdiction), Article 38 (extraterritorial application), Article 43 (Office for Safeguarding National Security with extraterritorial powers), electoral disqualification mechanisms, and organizational bans create near-total closure of political alternatives. Theater ratio 0.32: the 'high degree of autonomy' rhetoric, Basic Law formalities, CFA procedural hearings, and 'patriots' electoral theater maintain performative continuity while substantive power has transferred. Accessibility collapse 0.68: political alternatives exist formally (elections, courts, legislation) but are structurally constrained to pre-approved outcomes — alternatives collapse upon engagement. Resistance 0.62: significant but fragmented — 2019 protests (millions), 47+ NSL prosecutions, media closures, emigration wave (200k+ BN(O) applications), but no coordinated reversal mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC central authority seat, this is a rope/tangled_rope: a coordination mechanism that successfully managed sovereign recovery while preserving economic utility, now securing stability against foreign-backed subversion. From the HK pro-democracy camp and civil society seats, this is a snare: the coordination story (autonomy, rights, rule of law) was cover for a sovereignty transfer that always intended central control. From the HK business elite seat, this is a degraded rope: coordination function (commercial predictability) persists but extraction (political repression) degrades the environment. From the international observer seat, this is a treaty violation masquerading as constitutional implementation. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC central authority and national security apparatus are structural beneficiaries (d ~0.15): they collect political control, regime stability, and territorial integrity assurance. Pro-Beijing establishment is identity-locked beneficiary (d ~0.25): political identity fused with the framework; exit means irrelevance. HK business elite is mobile beneficiary/payer hybrid (d ~0.45): collects economic stability and mainland access, pays reputational and talent costs, retains capital exit. HK pro-democracy camp, civil society, judiciary independence, independent media, and residents are trapped/constrained payers (d ~0.85-0.95): bear extraction of political rights with no viable exit. Foreign treaty parties are excluded observers (d = analytical): structural exclusion from interpretation/implementation. The engine derives d from these declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign recovery without economic rupture) was substantially achieved by ~2003. The constraint's mandate — autonomy as the mechanism for that recovery — has atrophied. The arrangement persists because the PRC central authority extracts regime stability and the pro-Beijing establishment extracts political monopoly from its continuation. The national security apparatus has institutionalized around the extraction function. This is mandatrophy: the original coordination purpose is dead; the current function is extraction stabilized by institutional inertia and active suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_delegation_vs_treaty_right,
    'Is Hong Kong''s autonomy a revocable delegation of PRC sovereign authority (this reading) or a treaty-guaranteed right under the Sino-British Joint Declaration (autonomy_primacy_reading)?',
    'International arbitration or ICJ advisory opinion on Joint Declaration interpretation — but PRC rejects third-party jurisdiction. De facto resolution: whether HK institutions can meaningfully constrain NPCSC/NSL power. Current trajectory: they cannot.',
    'If delegation: PRC unilateral revision is legally valid; extraction is constitutional. If treaty right: PRC actions are breach; extraction is illegitimate; international law provides normative benchmark for resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_delegation_vs_treaty_right, conceptual, 'The foundational legal nature of HK autonomy — delegation vs. treaty right — determines whether the constraint''s extraction is constitutional operation or treaty violation.').

omega_variable(
    national_security_scope_boundary,
    'Where does ''national security'' end and ordinary governance begin? The NSL defines four crimes (secession, subversion, terrorism, collusion) but ''state secrets'' and ''national security'' are undefined and determined by mainland authorities.',
    'Judicial interpretation by HK courts under NSL Article 55 (mainland jurisdiction) vs. CFA common law tradition. Empirical: track scope of NSL prosecutions — political speech, union organizing, academic research, journalism, election participation have all been targeted.',
    'If scope is unbounded: the constraint extracts all political activity that challenges central authority — extraction approaches 1.0. If scope is bounded: genuine autonomy space survives in non-security domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_scope_boundary, empirical, 'Whether the national security exception swallows the autonomy rule — the operational boundary of the constraint''s extraction.').

omega_variable(
    judicial_independence_residual,
    'Does the HK judiciary retain any meaningful independence in non-national-security cases, or has the NSL''s chilling effect and NPCSC interpretation power collapsed the common law system entirely?',
    'Track CFA and lower court rulings on commercial law, family law, administrative review (non-security), and bail standards in NSL cases. Monitor foreign judge resignations (2 UK SC justices resigned 2022; others remain).',
    'If residual independence survives: the constraint is tangled_rope (coordination in non-political domains + extraction in political). If fully collapsed: the constraint approaches snare — coordination story is fully vestigial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_residual, empirical, 'Whether the common law judiciary survives as a coordination mechanism outside the national security domain.').

omega_variable(
    economic_utility_vs_political_extraction_tradeoff,
    'At what point does political extraction degrade the economic coordination function (HK as international financial center) enough to threaten the PRC''s own interest in the constraint''s coordination value?',
    'Monitor capital flows, foreign talent retention, IPO activity, RMB internationalization via HK, multinational regional HQ decisions. Compare to Singapore, Dubai, Shanghai benchmarks.',
    'If economic degradation crosses a threshold: PRC may recalibrate extraction (relax political control to preserve utility) — constraint could shift toward balanced_coexistence. If PRC accepts economic cost for political control: extraction is primary, coordination is expendable — constraint is snare with rope vestige.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_utility_vs_political_extraction_tradeoff, empirical, 'Whether the constraint''s extraction function threatens its own coordination foundation — the sustainability of the tangled_rope hybrid.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (NSL, electoral redesign, media bans, organizational dissolution) or partially internalized (anticipatory self-censorship, identity fusion with ''patriot'' narrative, fear-induced withdrawal)?',
    'Post-exit trajectory analysis: if HK emigrants resume political activism abroad (e.g. in UK, Taiwan, US), suppression was structural. If they remain politically quiescent, internalization may persist. Survey data on self-censorship among remaining residents.',
    'If internalized: effective suppression exceeds structural measure — targets carry suppression with them. If structural: suppression is reversible if constraint lifts. This omega also applies to pro-Beijing establishment''s identity_locked exit — is their loyalty structural or internalized?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an interpersonal-political constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_spr_tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.08).
narrative_ontology:measurement(octs_spr_tr_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement(octs_spr_tr_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(octs_spr_tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(octs_spr_tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(octs_spr_tr_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(octs_spr_tr_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement(octs_spr_tr_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2023, 0.32).
narrative_ontology:measurement(octs_spr_tr_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(octs_spr_be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.12).
narrative_ontology:measurement(octs_spr_be_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2003, 0.18).
narrative_ontology:measurement(octs_spr_be_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2014, 0.28).
narrative_ontology:measurement(octs_spr_be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.45).
narrative_ontology:measurement(octs_spr_be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(octs_spr_be_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement(octs_spr_be_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2022, 0.75).
narrative_ontology:measurement(octs_spr_be_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2023, 0.77).
narrative_ontology:measurement(octs_spr_be_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(octs_spr_su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.15).
narrative_ontology:measurement(octs_spr_su_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(octs_spr_su_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(octs_spr_su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(octs_spr_su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(octs_spr_su_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2021, 0.84).
narrative_ontology:measurement(octs_spr_su_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2022, 0.85).
narrative_ontology:measurement(octs_spr_su_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2023, 0.85).
narrative_ontology:measurement(octs_spr_su_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hk_national_security_law_2020).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hk_electoral_overhaul_2021).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hk_article_23_legislation_2024).

% DUAL FORMULATION NOTE:
% This constraint (sovereignty_primacy_reading) and its siblings (autonomy_primacy_reading, balanced_coexistence_reading) form a kernel family decomposing the 'One Country Two Systems' label. Each has distinct ε: sovereignty_primacy (0.78, high extraction), autonomy_primacy (~0.15, low extraction), balanced_coexistence (~0.45, moderate extraction). They share the Basic Law text as kernel but instantiate different constraints. The sovereignty reading structurally influences the autonomy reading by foreclosing its institutional pathway (NPCSC interpretation power, NSL).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, organized, 0.3).
constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
