% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Nikah as Quranic/Hadith-Governed Civil Contract (Muslim Personal Law, India)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the muslim_shariat_reading of the
 *   family_law_authority kernel: nikah as a civil contract whose terms (mahr,
 *   consent, dissolution procedure, permissible plural marriage) are derived
 *   from Quranic text and hadith and administered through community clerical
 *   authority (AIMPLB, qazis) rather than a uniform secular civil code. The
 *   measured interval (1985-2024) tracks India specifically, from the Shah
 *   Bano controversy and the 1986 Act through the 2017 Shayara Bano judgment
 *   and 2019 legislative ban on instant triple talaq. Extraction and
 *   suppression both decline over the interval as the most acute gender
 *   asymmetry (unilateral instant talaq) was judicially and legislatively
 *   closed, while theater ratio rises slightly as compliance-oriented reform
 *   (revised nikahnama templates, notice-and-waiting-period procedures) is
 *   adopted by some clerical bodies alongside continued resistance from
 *   others — a genuine partial reform coexisting with persistent
 *   administrative bottlenecking of khula and continued mahr/maintenance
 *   asymmetry.
 *
 * KEY AGENTS:
 *   - muslim_husbands: beneficiary of unilateral historical dissolution power and polygyny option
 *   - aimplb_and_clergy_authorities: agenda_setter interpreting text into applied law and resisting codification
 *   - muslim_wives_pre_2019: primary payer under instant talaq regime
 *   - divorced_women_seeking_maintenance: payer under compressed post-divorce support obligations
 *   - indian_supreme_court_and_legislature: external institutional check with partial reform authority
 *   - reformist_muslim_womens_organizations: excluded voice offering competing textual readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.52).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Nikah as Quranic/Hadith-Governed Civil Contract (Muslim Personal Law, India)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '4a4c5f42-4390-4e86-9d3b-3337529fd517').
narrative_ontology:cs_kernel_codification('4a4c5f42-4390-4e86-9d3b-3337529fd517', fixed_text).
narrative_ontology:cs_authority_grounding('4a4c5f42-4390-4e86-9d3b-3337529fd517', lineage).
narrative_ontology:cs_interpretation_layer_present('4a4c5f42-4390-4e86-9d3b-3337529fd517').
narrative_ontology:cs_reading_relation('4a4c5f42-4390-4e86-9d3b-3337529fd517', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a4c5f42-4390-4e86-9d3b-3337529fd517', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a4c5f42-4390-4e86-9d3b-3337529fd517', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a4c5f42-4390-4e86-9d3b-3337529fd517', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('4a4c5f42-4390-4e86-9d3b-3337529fd517', foundational, marriage_as_dissoluble_civil_contract).
narrative_ontology:cs_axiom_status(marriage_as_dissoluble_civil_contract, holdable).
narrative_ontology:cs_axiom_grounding('4a4c5f42-4390-4e86-9d3b-3337529fd517', marriage_as_dissoluble_civil_contract, conventional).
narrative_ontology:cs_axiom('4a4c5f42-4390-4e86-9d3b-3337529fd517', secondary, unilateral_extrajudicial_talaq_valid).
narrative_ontology:cs_axiom_status(unilateral_extrajudicial_talaq_valid, overridden).
narrative_ontology:cs_axiom_grounding('4a4c5f42-4390-4e86-9d3b-3337529fd517', unilateral_extrajudicial_talaq_valid, empirically_contingent).
narrative_ontology:cs_axiom('4a4c5f42-4390-4e86-9d3b-3337529fd517', foundational, mahr_as_enforceable_wife_entitlement).
narrative_ontology:cs_axiom_status(mahr_as_enforceable_wife_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('4a4c5f42-4390-4e86-9d3b-3337529fd517', mahr_as_enforceable_wife_entitlement, deontological).
narrative_ontology:cs_reference_frame('4a4c5f42-4390-4e86-9d3b-3337529fd517', classical_fiqh_consensus_authority).
narrative_ontology:cs_drift_state('4a4c5f42-4390-4e86-9d3b-3337529fd517', post_2019_legislative_reform, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4a4c5f42-4390-4e86-9d3b-3337529fd517', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, muslim_husbands).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, aimplb_and_clergy_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, personal_law_board_institutions).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_wives_pre_2019).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, divorced_women_seeking_maintenance).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_denied_khula_parity).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_textual_supremacy_in_family_matters).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, nikah_as_civil_not_sacramental_contract).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold unilateral extrajudicial dissolution power (pre-2019 instant triple talaq) and the right to contract up to four concurrent marriages under classical fiqh readings. Bear the mahr obligation as a formal cost but retain structurally superior exit and reformation options relative to wives under the same contract.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_husbands, beneficiary,
    moderate, biographical, mobile, national).

% The All India Muslim Personal Law Board and associated clerical bodies interpret Quranic verses and hadith corpora into applied personal law, resist codification or judicial override as interference with religious autonomy, and administer qazi-issued nikahnamas and fatwas that structure the marriage's practical terms. They set the interpretive agenda and lobby to preserve it against legislative and judicial encroachment.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, aimplb_and_clergy_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Prior to the 2019 Muslim Women (Protection of Rights on Marriage) Act, could be divorced instantly and extrajudicially by triple talaq pronounced by the husband with no judicial review, no waiting period, and no requirement of cause. Recourse to khula (wife-initiated dissolution) required either husband consent or protracted court process, asymmetric to the husband's unilateral power.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_wives_pre_2019, payer,
    powerless, biographical, trapped, national).

% Post-divorce maintenance under classical readings is often limited to the iddat period (roughly three menstrual cycles), contested against Section 125 CrPC secular maintenance claims (Shah Bano, 1985) and the subsequent 1986 Muslim Women (Protection of Rights on Divorce) Act that narrowed post-divorce state-law maintenance for Muslim women specifically. They must navigate two competing legal regimes to secure ongoing support.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, divorced_women_seeking_maintenance, payer,
    powerless, biographical, constrained, national).

% Even where khula exists in principle, its exercise is frequently mediated by qazis or family arbitration bodies applying husband-favorable interpretations, producing years-long delay relative to the husband's historically instantaneous exit. Their formal contractual right to seek dissolution exists on paper but is administratively bottlenecked by the same authorities who benefit from preserving asymmetric practice.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_denied_khula_parity, payer,
    powerless, biographical, trapped, national).

% Adjudicates constitutional challenges to personal law practices (Shayara Bano v. Union of India, 2017, striking down instant triple talaq as unconstitutional; the 2019 Act criminalizing it) while remaining constrained by Article 25/26 religious freedom protections and by the political sensitivity of touching minority personal law. Functions as both external check and, through legislation, a competing agenda-setter.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, indian_supreme_court_and_legislature, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, indian_supreme_court_and_legislature, agenda_setter).

% Groups such as the Bharatiya Muslim Mahila Andolan advocate for codified, gender-equitable nikahnama terms and litigated against instant triple talaq, but are structurally outside the AIMPLB's interpretive authority and are frequently characterized by clerical authorities as inauthentic voices on Islamic law despite representing affected women directly.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, reformist_muslim_womens_organizations, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, aimplb_and_clergy_authorities).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, textually-grounded contractual form for marriage — specifying mahr, consent, and dissolution procedures — that functions across a large, doctrinally diverse minority community without requiring a single centralized civil registry, while claiming continuity with scriptural authority that many adherents value as a marker of religious identity and practice.
% TRANSFER_FUNCTION: Moves control over the marriage's continuation and dissolution disproportionately toward the husband: unilateral talaq (pre-2019) cost him little beyond pronouncement, while the wife bore the burden of either negotiating khula through husband consent or pursuing lengthy arbitration; post-divorce maintenance obligations were correspondingly compressed relative to the wife's ongoing dependency.
% ABSENT_VOICES: Muslim women's reform organizations that litigated and organized against instant triple talaq were largely outside the AIMPLB's official interpretive process; their readings of the same Quranic verses (e.g., the three-month reconciliation period implied in Quran 2:228-232) were treated as external to 'authentic' fiqh rather than as competing orthodox readings.
% DISAPPEARANCE_RATIONALE: If Quranic/hadith-derived personal law authority disappeared entirely, marriage for Indian Muslims would default to whatever secular or uniform civil code regime replaced it — a genuine rearrangement of dissolution procedure, mahr enforceability, and polygyny status. But proponents dispute that this would constitute improvement rather than loss of religious self-governance and communal identity protection, and note much of the asymmetry (instant talaq) has already been legislatively closed without dissolving the broader personal-law framework, suggesting partial decoupling of the coordination function from the extractive element.
% FOUNDING_PROBLEM: To provide marriage, dissolution, and inheritance rules for the Muslim community consistent with Quranic injunction and Prophetic practice, at a time (7th century Arabia and subsequent centuries of jurisprudential development) when the alternative was either no formal marital contract protection for women at all or tribal customary practices offering women markedly less: the mahr requirement and the notice/waiting-period provisions in classical fiqh were themselves reforms relative to pre-Islamic Arabian practice.
% FOUNDING_PROBLEM_CORROBORATION: Reformist Islamic scholars (e.g., Asghar Ali Engineer, Flavia Agnes writing on Muslim women's rights) and the Supreme Court in Shayara Bano attest that the instant-talaq practice diverged from the Quranic text's own procedural requirements (attempted reconciliation, arbitration, waiting period) and was itself a later juristic accretion rather than the founding textual mandate — corroboration from outside AIMPLB that the extractive asymmetry was not required by the founding problem's own terms. AIMPLB and traditionalist clergy dispute this reading and maintain the classical fiqh consensus reflects the authentic founding intent.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, contested).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects substantial but partial asymmetry: mahr is a genuine offsetting obligation running toward the wife, and post-2019 the most extreme extraction vector (instant unilateral talaq) is formally closed, so this is not maximal extraction. Suppression (0.58) captures that exit for wives remains administratively harder than for husbands even after legal reform, mediated by clerical gatekeeping of khula. Theater ratio (0.28) is moderate: real reform occurred (2019 Act, Shayara Bano) but coexists with continued informal practice of talaq-adjacent pressure and slow khula processing that functions as compliance theater around the letter of the new law. Accessibility collapse (0.45) is moderate-low because, unlike a mountain, alternatives are visible and increasingly exercised (civil registration, judicial recourse, reform advocacy) — this is not a constraint where alternatives are foreclosed, it is one where alternatives exist but remain costlier for one party than the other.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (AIMPLB/clergy), this is coordination: a stable, doctrinally coherent contractual form protecting community religious autonomy against homogenizing state intrusion. From the payer seats (women navigating instant talaq or contested maintenance), the same structure operated as enforced extraction backed by the threat of religious/community sanction for seeking alternatives. The engine's per-seat computation should reflect this: institutional/arbitrage-exit seats compute closer to rope/tangled_rope-as-coordination, powerless/trapped seats compute closer to tangled_rope-as-extraction or snare, which is the seat divergence the classification is designed to surface rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands derive toward the beneficiary end: historically minimal-cost unilateral exit, polygyny option, mobile exit options generally. Clerical/institutional authorities also sit near the beneficiary end structurally (arbitrage exit, institutional power) because they administer and profit reputationally/organizationally from preserving interpretive authority. Wives and divorced women sit toward the target end: trapped or constrained exit, powerless power atom, and the asymmetric cost-bearing the transfer_function describes. The Indian state sits as an institutional observer with partial agenda-setting capacity via legislation — its d is genuinely mixed and not derived cleanly from beneficiary/victim declarations, which is why it is modeled with a dual role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (providing textually-grounded marital protection, historically an improvement over pre-Islamic customary practice) is contested as live versus dead: reformist scholars and the Supreme Court's own reasoning in Shayara Bano hold that the extractive instant-talaq practice was itself a later juristic accretion inconsistent with the Quran's own procedural requirements — meaning part of what was defended as 'the founding mandate' was actually mandatrophy: institutional practice that outlived or diverged from its own textual justification. The 2019 reform is best read as a partial mandatrophy correction, closing the gap between the founding problem (fair, contractually-bounded dissolution) and accumulated extractive practice (instant unilateral extrajudicial dissolution), without resolving the remaining asymmetries in maintenance and khula administration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instant_talaq_textual_fidelity,
    'Was the historically dominant practice of instant, unilateral, extrajudicial triple talaq a faithful application of Quranic injunction (Quran 2:228-232, which specifies a graduated procedure with reconciliation attempts and witnesses), or a later juristic/customary accretion that diverged from the founding text?',
    'Comparative textual and historical jurisprudential analysis across madhhab traditions; examination of the Shayara Bano judgment''s own reasoning and dissenting opinions; comparison with reformist scholarship (Engineer, Agnes) versus AIMPLB doctrinal positions.',
    'If instant talaq is accretion rather than founding mandate, the extractive asymmetry this story measures is properly attributed to administered practice and clerical interpretive choice rather than to the kernel''s founding text itself — strengthening the case that this is tangled_rope (genuine coordination function plus separable extractive practice) rather than the coordination function itself being inherently asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instant_talaq_textual_fidelity, conceptual, 'Whether pre-2019 instant talaq practice reflects the Quranic text or a later juristic divergence from it.').

omega_variable(
    khula_administrative_capture,
    'Is the continued difficulty women face in exercising khula post-2019 attributable to genuine doctrinal requirements for husband consent/arbitration, or to clerical/qazi administrative discretion exercised in ways that favor husbands independent of doctrine?',
    'Empirical study of khula case outcomes and timelines across different qazi jurisdictions and dar-ul-qaza bodies; comparison with jurisdictions applying more codified, time-bound khula procedures (e.g., certain Pakistani or Bangladeshi statutory reforms).',
    'If administrative discretion rather than doctrine drives the delay, the extraction is attributable to enforcement/administration (supporting active continued enforcement as a live driver) rather than to an irreducible feature of the contractual form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(khula_administrative_capture, empirical, 'Whether khula delay is doctrinally required or an artifact of discretionary clerical administration.').

omega_variable(
    kernel_reading_boundary_location,
    'This story treats the muslim_shariat_reading as bounded by classical/AIMPLB fiqh interpretation; the sibling reformist reading (reading the same Quranic verses toward gender parity) is treated here as an excluded voice within this reading rather than as a distinct sixth reading of the kernel. Is that boundary drawing correct, or does the reformist position constitute a structurally distinct reading that should be its own constraint story?',
    'Determine whether reformist practice (e.g., BMMA-drafted nikahnamas with built-in gender-parity clauses) has achieved sufficient institutional uptake and distinct enforcement mechanisms to constitute a separate reading rather than a contested minority position within this one.',
    'If the reformist position should be a sixth sibling reading, this story''s beneficiary/victim structure and ε would need to be reassessed to exclude cases governed under reformist nikahnama templates, which would likely show a lower ε and different victim structure — an ε-invariance decomposition question rather than a resolved fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the reformist Muslim women''s reading constitutes a distinct kernel reading warranting decomposition, per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__muslim_shariat_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(fami_tr_t1995, family_law_authority__muslim_shariat_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(fami_tr_t2005, family_law_authority__muslim_shariat_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(fami_tr_t2015, family_law_authority__muslim_shariat_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(fami_tr_t2017, family_law_authority__muslim_shariat_reading, theater_ratio, 2017, 0.27).
narrative_ontology:measurement(fami_tr_t2019, family_law_authority__muslim_shariat_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t1985, family_law_authority__muslim_shariat_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(fami_be_t1995, family_law_authority__muslim_shariat_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(fami_be_t2005, family_law_authority__muslim_shariat_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(fami_be_t2015, family_law_authority__muslim_shariat_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(fami_be_t2017, family_law_authority__muslim_shariat_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(fami_be_t2019, family_law_authority__muslim_shariat_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1985, family_law_authority__muslim_shariat_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(fami_su_t1995, family_law_authority__muslim_shariat_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(fami_su_t2005, family_law_authority__muslim_shariat_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(fami_su_t2015, family_law_authority__muslim_shariat_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(fami_su_t2017, family_law_authority__muslim_shariat_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(fami_su_t2019, family_law_authority__muslim_shariat_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel (christian_canonical, hindu_dharmashastra, muslim_shariat [this story], parsi_zoroastrian, secular_contractual). Each reading has its own ε, beneficiary/victim structure, and classification. The muslim_shariat_reading most directly influences the secular_contractual_reading's legitimacy conditions in the Indian context because personal-law carve-outs from a Uniform Civil Code are litigated and legislated against the backdrop of this reading's practice (Shah Bano, Shayara Bano, the 2019 Act) — legislative and judicial reform of this reading creates structural pressure on the boundary between personal law and the secular civil code without foreclosing either reading. It coexists with the other religious readings as parallel, non-foreclosing personal-law regimes under the same constitutional framework (Articles 25-26 read against Article 44's aspirational Uniform Civil Code directive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
