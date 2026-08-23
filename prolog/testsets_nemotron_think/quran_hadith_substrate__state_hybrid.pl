% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Hybrid Sharia: Selective Classical Adoption in Family/Criminal Law with Secular Commercial Framework
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   Post-colonial Muslim states constructed a hybrid legal order: classical
 *   fiqh retained in family law and criminal codes as the visible marker of
 *   Islamic identity, while commercial, banking, and administrative law were
 *   secularized to integrate with global capitalism. The state presents this
 *   as a balanced 'Islamic modernity' but the boundary is drawn by sovereign
 *   discretion, not doctrinal principle. Classical rulings are codified
 *   selectively — those reinforcing patriarchal family structures and state
 *   penal power are kept; those constraining fiscal policy or commercial
 *   freedom are discarded. Traditionalist scholars are co-opted as legitimacy
 *   validators; reformist scholars are tolerated only when their ijtihad
 *   serves state economic goals. The constraint extracts interpretive
 *   authority from the scholarly class and disciplinary compliance from
 *   citizens, transferring both to the state's sovereign command.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.35).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.6).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Hybrid Sharia: Selective Classical Adoption in Family/Criminal Law with Secular Commercial Framework").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/religious/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '4ef33dbf-4962-47ca-9cc1-338a9fed4242').
narrative_ontology:cs_kernel_codification('4ef33dbf-4962-47ca-9cc1-338a9fed4242', fixed_text).
narrative_ontology:cs_authority_grounding('4ef33dbf-4962-47ca-9cc1-338a9fed4242', extraction).
narrative_ontology:cs_interpretation_layer_present('4ef33dbf-4962-47ca-9cc1-338a9fed4242').
narrative_ontology:cs_reading_relation('4ef33dbf-4962-47ca-9cc1-338a9fed4242', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('4ef33dbf-4962-47ca-9cc1-338a9fed4242', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('4ef33dbf-4962-47ca-9cc1-338a9fed4242', foundational, political_sovereignty_grounds_legal_legitimacy).
narrative_ontology:cs_axiom_status(political_sovereignty_grounds_legal_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4ef33dbf-4962-47ca-9cc1-338a9fed4242', political_sovereignty_grounds_legal_legitimacy, conventional).
narrative_ontology:cs_axiom('4ef33dbf-4962-47ca-9cc1-338a9fed4242', foundational, selective_sharia_adoption_serves_public_order).
narrative_ontology:cs_axiom_status(selective_sharia_adoption_serves_public_order, holdable).
narrative_ontology:cs_axiom_grounding('4ef33dbf-4962-47ca-9cc1-338a9fed4242', selective_sharia_adoption_serves_public_order, instrumental).
narrative_ontology:cs_reference_frame('4ef33dbf-4962-47ca-9cc1-338a9fed4242', classical_fiqh_as_comprehensive_order).
narrative_ontology:cs_drift_state('4ef33dbf-4962-47ca-9cc1-338a9fed4242', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ef33dbf-4962-47ca-9cc1-338a9fed4242', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_actors).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_activists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, citizens_under_classical_codes).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, political_sovereignty_grounds_legal_legitimacy).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, selective_sharia_adoption_serves_public_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control legislative and judicial apparatus; selectively codify classical fiqh rulings in family law (marriage, inheritance, custody) and criminal codes (hudud, qisas) while enacting secular commercial, banking, and administrative codes. Harvest legitimacy from Islamic identity without accepting doctrinal constraint on economic policy. Can shift the boundary of 'sharia domains' as regime interests evolve.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, state_elites, beneficiary).

% Institutional ulama bodies and madhhab-affiliated jurists whose comprehensive sharia vision is truncated — family and criminal law retain classical form but are stripped of independent interpretive authority; commercial law escapes sharia entirely. Their endorsement is solicited for legitimacy; their dissent on economic matters is marginalized. Exit means losing state patronage and institutional platforms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, national).

% Scholars, lawyers, and civil society actors arguing for ijtihad grounded in contemporary ethics, human rights, or maslaha. Find their critical readings tolerated in commercial/administrative reform but suppressed when applied to family law or criminal codes — the state co-opts reformist language for economic modernization while treating it as sedition in 'core' sharia domains. Face professional blacklisting, travel bans, or imprisonment.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, biographical, constrained, national).

% Women and minorities subject to classical family law (guardianship, testimony weight, inheritance shares) and criminal codes (hudud punishments) without the procedural protections or interpretive flexibility that classical fiqh itself provided. Cannot opt out of personal status courts; emigration is the only full exit. Bear the disciplinary force of 'sharia' without the classical scholars' restraining interpretive traditions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, citizens_under_classical_codes, payer,
    powerless, biographical, trapped, local).

% Domestic and foreign capital operating under secular commercial, banking, corporate, and arbitration codes. Gain legal predictability, interest-based finance, and international enforceability. Their domain is explicitly carved out from sharia oversight; they lobby to keep it so. Can relocate capital if the secular enclave shrinks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_actors, beneficiary,
    powerful, biographical, mobile, global).

% UN treaty bodies, INGOs, and foreign governments monitoring compliance with CEDAW, ICCPR, CRC. Document the gap between state's human rights commitments and classical family/criminal codes. Their reports create diplomatic pressure but lack enforcement leverage; the state treats them as external interference in sovereign domain.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified legal order that signals Islamic identity to domestic constituencies and international Muslim audiences while delivering the legal predictability and financial instruments required for global economic integration — a single framework that serves both legitimacy and development imperatives.
% TRANSFER_FUNCTION: Moves interpretive authority and material resources from traditionalist scholars and reformist critics to state elites, who control the boundary between 'sharia domains' (family, criminal) and 'secular domains' (commercial, administrative). Moves disciplinary costs onto citizens subject to classical codes without classical procedural safeguards. Moves economic rents to commercial actors via interest-based finance and secular corporate law.
% ABSENT_VOICES: Classical madhhab jurists operating outside state structures (e.g., independent seminaries in Qom, Najaf, Deoband, Cairo) who would insist on comprehensive sharia application across all domains. Feminist Quranic exegetes arguing for ethical reinterpretation from within the tradition. Exiled opposition movements demanding either full sharia implementation or full secular codification. They are absent because the state controls licensing of religious institutions, academic appointments, and public discourse.
% DISAPPEARANCE_RATIONALE: If the hybrid system vanished overnight, three competing orders would contend: traditionalist demand for comprehensive classical fiqh reinstatement; reformist push for rights-based codification across all domains; and secular-liberal demand for full legal unification. The commercial enclave would lose its legal basis; family courts would lose their statutory mandate. The state would face a legitimacy crisis with no ready replacement.
% FOUNDING_PROBLEM: Post-colonial Muslim states inherited fragmented legal pluralism: colonial secular codes in commercial/criminal spheres, classical fiqh in personal status, customary law in rural areas. The founding problem was constructing a unified national legal system that could claim Islamic authenticity (to legitimate the new state) while enabling capitalist development (to survive economically).
% FOUNDING_PROBLEM_CORROBORATION: State elites attest the problem persists: Islamic identity remains contested, economic integration requires continuous legal updating. Traditionalist scholars attest the problem was solved by classical fiqh's comprehensive scope — the hybrid is a deviation, not a solution. Reformist activists attest the problem was misdiagnosed: the need was for ethical ijtihad, not sovereign selectivity. Independent historians of Islamic law (e.g., Wael Hallaq, Knut Vikør) corroborate that the hybrid structure is a 20th-century state construction, not a continuous tradition.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate but variable: the state extracts legitimacy rents and interpretive monopoly without the full coercive overhead of a comprehensive sharia system. Suppression (0.6) is significant but targeted — focused on preventing challenges to the boundary-drawing authority, not on enforcing every classical ruling. Theater ratio (0.5) is high: the performance of sharia compliance in family/criminal courts masks the sovereign's unrestricted discretion to define what counts as sharia. Accessibility collapse (0.6) reflects that alternatives exist (traditionalist comprehensive vision, reformist ethical vision) but are institutionally blocked. Resistance (0.5) comes from both traditionalists (who want comprehensive application) and reformists (who want ethical reinterpretation) — their opposition is structurally asymmetric but persistent.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elite seat, the hybrid is a pragmatic coordination achievement: Islamic identity preserved where it matters symbolically, economic modernity enabled where it matters materially. From the traditionalist seat, it is a snare: sharia's comprehensive authority is hijacked for regime legitimacy. From the reformist seat, it is a tangled rope: the coordination function (modern legal order) is real but the extraction (sovereign boundary control) blocks ethical ijtihad. From the citizen seat, it is a snare with no coordination benefit — they experience only the classical discipline without the classical safeguards. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are the primary beneficiaries (d ~ 0.1) — they collect legitimacy rents and control the legal boundary. Commercial actors are secondary beneficiaries (d ~ 0.2) — they gain a secular enclave but depend on state maintenance of the boundary. Traditionalist scholars are payers (d ~ 0.7) — they lose interpretive independence and see their tradition truncated. Reformist activists are payers (d ~ 0.8) — their critical capacity is selectively suppressed. Citizens under classical codes are trapped payers (d ~ 0.9) — they bear the disciplinary force without classical procedural protections. International observers are analytical (d = 0.5) — they perceive the structure but lack structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified Islamic-modern legal order) is contested: state says it persists, traditionalists say it was solved by classical fiqh, reformists say it was misdiagnosed. The hybrid persists not because the founding problem is live but because it serves current regime interests — a classic mandatrophy signature. The sunset clause is absent; the arrangement has become the constitutive structure of the post-colonial legal state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the state_hybrid reading''s claim to sovereign boundary-drawing authority logically foreclose the traditionalist_taqlid reading''s claim to comprehensive doctrinal authority, or do they coexist as competing legitimacy claims within the same Muslim polity?',
    'Historical analysis of whether any state has successfully maintained both: a traditionalist scholarly class with independent interpretive authority over the ''sharia domains'' AND a sovereign legislature that unilaterally defines those domains'' boundaries. If no such coexistence exists, the readings foreclose each other.',
    'If forecloses, the state_hybrid reading structurally displaces traditionalist_taqlid — the latter becomes a performative remnant. If coexists_with, traditionalist scholars retain latent interpretive power that could reassert under regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether state sovereign boundary-drawing and traditionalist comprehensive authority are logically compatible within one framework.').

omega_variable(
    reformist_cooption_vs_suppression,
    'Is the state''s selective tolerance of reformist ijtihad in commercial/administrative law a genuine coordination function (updating law for economic efficiency) or an instrumental co-option that strengthens the hybrid''s legitimacy while suppressing reformist critique in family/criminal law?',
    'Track whether reformist scholars permitted to operate in commercial law reform are the same individuals/institutions suppressed in family law reform. If distinct populations, it suggests functional specialization. If same individuals face domain-dependent permission/suppression, it confirms instrumental co-option.',
    'If instrumental co-option, the hybrid''s coordination function is partly performative — reformist language serves legitimacy, not legal updating. This would increase theater_ratio and support snare classification for the reformist seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_cooption_vs_suppression, empirical, 'Whether reformist ijtihad''s domain-limited permission is functional or instrumental.').

omega_variable(
    extraction_variability_across_states,
    'The ε bin (0.25-0.45) reflects high variability across state contexts. What structural factors explain why some state_hybrid systems extract more (e.g., Saudi Arabia pre-2010s) while others extract less (e.g., Tunisia post-2014, Indonesia)?',
    'Comparative analysis of: (a) oil rent dependency vs. tax-dependent fiscal base, (b) traditionalist scholarly class institutionalization level, (c) reformist movement organizational strength, (d) global economic integration depth. Regression of extractiveness proxies against these variables across 20+ Muslim-majority states.',
    'If extractiveness correlates with rentier state structure, the hybrid is a rent-management tool. If it correlates with traditionalist institutional weakness, it''s a legitimacy-compensation mechanism. Different drivers imply different reform trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_variability_across_states, empirical, 'Structural drivers of extractiveness variance across state_hybrid implementations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.6) primarily structural (state coercive apparatus: courts, police, licensing) or internalized (citizens and scholars self-censor because the hybrid''s legitimacy narrative has been absorbed as ''Islamic modernity'')?',
    'Post-reform trajectory analysis: in states where the hybrid was disrupted (e.g., Tunisia 2011, Sudan 2019), did suppression of traditionalist/reformist critique persist in civil society discourse after state coercion relaxed? Persistence indicates internalization.',
    'If substantially internalized, effective suppression is higher than structural measures suggest — the constraint travels with agents after formal exit. This would affect classification for the citizen and scholar seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the hybrid system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.4).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.45).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__state_hybrid, theater_ratio, 60, 0.5).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__state_hybrid, theater_ratio, 80, 0.55).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__state_hybrid, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__state_hybrid, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__state_hybrid, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(qura_be_t100, quran_hadith_substrate__state_hybrid, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__state_hybrid, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__state_hybrid, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__state_hybrid, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, state_family_law_codification).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, state_commercial_secular_codes).

% DUAL FORMULATION NOTE:
% This constraint is one reading (state_hybrid) of the quran_hadith_substrate kernel. The kernel decomposes into three constraint stories: traditionalist_taqlid (Mountain-claiming, low extraction), reformist_ijtihad (Tangled Rope, moderate extraction), and state_hybrid (Tangled Rope, moderate extraction with high theater). The state_hybrid reading draws on the kernel's authority while denying its comprehensive binding force — it affects both siblings by restructuring the institutional field in which they operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.65).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, moderate, 0.75).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
