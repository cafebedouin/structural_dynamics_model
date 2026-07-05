% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Selective Hybrid Application of Sharia (Political Sovereignty Reading)
 *   domain: Islamic Jurisprudence / Legal Theory / Religious Authority
 *
 * SUMMARY:
 *   This story instantiates the state_hybrid reading of the
 *   quran_hadith_substrate kernel: a modern state selectively codifies
 *   classical rulings in family and criminal law while adopting reformist or
 *   secular frameworks in commercial and administrative law, grounding its
 *   legitimacy in political sovereignty rather than in a single doctrinally
 *   consistent position. The sibling readings — traditionalist_taqlid
 *   (comprehensive classical fidelity across all domains) and
 *   reformist_ijtihad (contextual reinterpretation across all domains) — are
 *   NOT described here as competing accounts of the same domain; they are
 *   separate constraints with their own ε values, beneficiary/victim
 *   structures, and classifications. This story's ε sits in the
 *   Low-to-Moderate band (0.25-0.45) reflecting substantial cross-context
 *   variability: the extraction is real but moderate, concentrated in
 *   specific domains (family law) rather than diffused across the entire
 *   legal order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.36).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.5).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.36).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Selective Hybrid Application of Sharia (Political Sovereignty Reading)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "Islamic Jurisprudence / Legal Theory / Religious Authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '3ffd4fb5-e1b2-44bd-af63-3219809c5124').
narrative_ontology:cs_kernel_codification('3ffd4fb5-e1b2-44bd-af63-3219809c5124', distributed).
narrative_ontology:cs_authority_grounding('3ffd4fb5-e1b2-44bd-af63-3219809c5124', extraction).
narrative_ontology:cs_interpretation_layer_present('3ffd4fb5-e1b2-44bd-af63-3219809c5124').
narrative_ontology:cs_reading_relation('3ffd4fb5-e1b2-44bd-af63-3219809c5124', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('3ffd4fb5-e1b2-44bd-af63-3219809c5124', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('3ffd4fb5-e1b2-44bd-af63-3219809c5124', foundational, political_sovereignty_legitimates_domain_selection).
narrative_ontology:cs_axiom_status(political_sovereignty_legitimates_domain_selection, holdable).
narrative_ontology:cs_axiom_grounding('3ffd4fb5-e1b2-44bd-af63-3219809c5124', political_sovereignty_legitimates_domain_selection, conventional).
narrative_ontology:cs_axiom('3ffd4fb5-e1b2-44bd-af63-3219809c5124', secondary, doctrinal_consistency_subordinate_to_governance_function).
narrative_ontology:cs_axiom_status(doctrinal_consistency_subordinate_to_governance_function, holdable).
narrative_ontology:cs_axiom_grounding('3ffd4fb5-e1b2-44bd-af63-3219809c5124', doctrinal_consistency_subordinate_to_governance_function, instrumental).
narrative_ontology:cs_reference_frame('3ffd4fb5-e1b2-44bd-af63-3219809c5124', post_colonial_sovereign_codification_settlement).
narrative_ontology:cs_drift_state('3ffd4fb5-e1b2-44bd-af63-3219809c5124', contemporary_islamist_mobilization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ffd4fb5-e1b2-44bd-af63-3219809c5124', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, regime_aligned_clerics).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_capital_holders).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_jurists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_and_family_law_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls which classical rulings get codified into positive law and which are quietly displaced by administrative or commercial statute. Draws legitimacy from appearing doctrinally faithful in family and criminal law while retaining full policy flexibility in economic governance. Can reshuffle the boundary between 'sacred' and 'secular' domains whenever political incentives shift.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Issues fatwas and legal opinions ratifying the state's selective codification in exchange for institutional funding, appointments, and protected status. Their authority depends on continued state patronage, which makes independent doctrinal positions costly to hold.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, regime_aligned_clerics, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, regime_aligned_clerics, agenda_setter).

% Operates under secular or reformist commercial and banking codes that permit interest-bearing finance, corporate structures, and international contract law unencumbered by classical prohibitions. Benefits directly from the state's decision to wall off commercial law from classical doctrine.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_capital_holders, beneficiary,
    powerful, biographical, mobile, global).

% Holds that sharia is a comprehensive legal-ethical system, not a menu of selectively applied rulings. Watches the state apply classical rulings only where politically convenient (family law, criminal hudud) while abandoning them where inconvenient (finance, administration). Cannot exit the national legal order and has limited platforms to contest the state's selective doctrine without being labeled extremist or disloyal.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    moderate, civilizational, constrained, national).

% Argues that classical family and criminal rulings themselves need contextual reinterpretation for human rights and public-interest reasons. Their critical readings are suppressed precisely in the domains (family, criminal law) where the state needs the appearance of traditional fidelity, while being tolerated in commercial law where the state already wants flexibility. Their voice is welcomed selectively, not on its own terms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_jurists, payer,
    moderate, generational, trapped, national).

% Lives under classical-rule family codes (marriage, divorce, inheritance, custody) precisely because this is the domain the state retains for legitimacy signaling, even as the same state applies liberalized frameworks elsewhere. Exit requires costly emigration or informal circumvention; formal legal recourse is bounded by the state's chosen codification.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_and_family_law_subjects, payer,
    powerless, biographical, trapped, national).

% Evaluates the state's commercial and administrative predictability as a condition for investment and trade agreements, indirectly reinforcing the state's incentive to keep commercial law secularized regardless of what happens in family or criminal law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_investors_and_trade_partners, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable division of legal labor: classical rulings supply legitimacy and continuity in family/criminal law where doctrinal fidelity is politically valuable, while reformist or secular frameworks supply predictability and flexibility in commercial/administrative law where economic integration is valuable. This lets the state govern a religiously plural, economically integrated society without resolving the underlying doctrinal question.
% TRANSFER_FUNCTION: Moves legitimacy and doctrinal capital from traditionalist and reformist scholarly communities to the state and its aligned clerical establishment; moves practical legal certainty and economic flexibility to commercial actors; moves the cost of doctrinal inconsistency onto family-law subjects (especially women) and onto scholars whose comprehensive visions are truncated by selective codification.
% ABSENT_VOICES: Family-law subjects bound by classical rulings rarely have a formal channel to contest the selectivity itself — they can seek judicial remedy within the classical framework but cannot challenge why that framework, and not a reformed one, governs their domain while a secular one governs commerce. Reformist jurists are heard in commercial law fora but excluded from family/criminal law reform commissions where their arguments would be most consequential.
% DISAPPEARANCE_RATIONALE: If the hybrid arrangement collapsed into a single consistent doctrinal position (either full classical application or full reformist/secular application across all domains), state elites would lose the dual legitimacy-plus-flexibility arrangement, commercial actors would face either doctrinal uncertainty or loss of the 'Islamic legitimacy' branding, and family-law subjects' legal status would shift substantially in either direction. Clerical patronage networks built around selective ratification would need to reorganize entirely.
% FOUNDING_PROBLEM: Post-colonial and modernizing states needed to reconcile mass religious legitimacy demands with the practical requirements of participating in an international commercial and administrative order built on secular/positive law — a single doctrinal system satisfying both was not readily available.
% FOUNDING_PROBLEM_CORROBORATION: State legal scholars and regime-aligned clerics attest the arrangement is a live, necessary accommodation of plural obligations. Independent comparative law scholars and human rights monitors (outside both the state and the clerical establishment) attest the arrangement functions primarily to insulate economically consequential law from doctrinal contest while concentrating doctrinal fidelity's costs on family-law subjects who have no comparable capacity to contest the framing — suggesting the founding problem of 'reconciling plural obligations' has substantially given way to a legitimacy-management function.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.36, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.36 by interval end) is moderate because the hybrid arrangement genuinely solves a coordination problem (governing a plural, economically integrated society) even as it transfers real costs onto family-law subjects and truncates both traditionalist and reformist visions. Theater ratio rises to 0.48 because an increasing share of the state's doctrinal signaling (public fidelity claims in family/criminal law) is performative relative to the actual governing logic, which is political-sovereignty management rather than doctrinal commitment. Suppression sits at a moderate 0.5, reflecting variable enforcement: some regimes tolerate reformist commercial-law argument while suppressing reformist family-law argument, and vice versa depending on incentive structure — this variability across state contexts is itself the structural signature of the state_hybrid reading, distinguishing it from the more doctrinally uniform sibling readings.
 *
 * PERSPECTIVAL GAP:
 *   From the state elite seat, this is prudent statecraft managing plural legitimacy demands. From the family-law subject seat, the same structure is an enforced doctrinal choice made for reasons that have nothing to do with them. The engine computes these as structurally different experiences of one arrangement, not as competing opinions about it.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and regime-aligned clerics sit near the beneficiary end: they set the boundary between codified-classical and codified-secular domains and capture legitimacy plus policy flexibility from the arrangement. Commercial capital holders benefit from the secular commercial carve-out without bearing family-law costs. Traditionalist scholars and reformist jurists are both victims, but through different mechanisms — traditionalists lose comprehensiveness (their vision is chosen only where useful), reformists lose critical space (their arguments are welcomed in commerce, suppressed in family/criminal law). Family-law subjects, especially women, are the clearest victims: trapped within classical codification precisely because that domain has the highest legitimacy value to the state, with the least individual capacity to exit or contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: there is a genuine coordination function here — the hybrid arrangement lets a state govern without forcing an unresolvable doctrinal settlement, and commercial actors, the state, and international partners all derive real benefit from the commercial-law carve-out. But this coordination function is bundled with asymmetric extraction from family-law subjects and both scholarly communities, sustained by active enforcement (selective ratification, licensing of clerics, suppression of reformist family-law argument). Treating this purely as extraction would miss the real governance problem it solves; treating it purely as coordination would erase the family-law subjects who bear its cost without proportionate voice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_grounding_vs_doctrinal_grounding,
    'Is the state_hybrid arrangement''s legitimacy genuinely grounded in political sovereignty as an independent normative source, or is ''political sovereignty'' itself a legitimating gloss over what is functionally selective doctrinal capture by state elites?',
    'Comparative analysis of state legal reform trajectories: if sovereignty-grounded legitimacy claims predict consistent patterns of domain selection across different regime types (democratic, authoritarian, monarchic), that supports genuine sovereignty-grounding; if domain selection tracks regime survival incentives regardless of stated legitimacy claims, that supports the capture reading.',
    'If sovereignty-grounding is genuine, the coordination function is more robust and the tangled_rope classification is well-supported; if it is a capture gloss, the constraint drifts toward snare, with ''political sovereignty'' functioning as cover narrative rather than independent justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_grounding_vs_doctrinal_grounding, conceptual, 'Whether political sovereignty is an independent legitimating ground or a capture narrative for selective doctrinal application.').

omega_variable(
    family_law_selection_mechanism,
    'Why does classical doctrine persist specifically in family/criminal law rather than commercial/administrative law — is this because family law carries higher symbolic/legitimacy value to religious constituencies, or because family-law subjects (disproportionately women) have the least political leverage to resist codification choices made against their interest?',
    'Cross-national comparison of which domains states select for classical retention, correlated with (a) constituency size and mobilization capacity of affected groups and (b) measured public legitimacy value of domain-specific religious signaling.',
    'If selection tracks leverage/mobilization capacity rather than symbolic value alone, this strengthens the tangled_rope-to-snare reading for the family-law subjects specifically, since the domain choice would be explained by exploitability rather than coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_law_selection_mechanism, empirical, 'Whether classical-law domain retention tracks legitimacy value or victim exploitability.').

omega_variable(
    kernel_reading_stability_under_regime_change,
    'Does the state_hybrid reading remain a stable, distinct constraint across regime transitions, or does it tend to collapse toward either traditionalist_taqlid or reformist_ijtihad under political stress (e.g., Islamist mobilization pushing toward taqlid, secularizing coups pushing toward ijtihad)?',
    'Longitudinal tracking of legal codification changes across regime transitions in multiple state contexts, coded against the kernel''s three reading types.',
    'If state_hybrid proves unstable under stress, it may be better modeled as a transitional equilibrium between the other two readings rather than a fully independent long-run reading, which would affect how this story''s civilizational time-horizon claims should be weighted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability_under_regime_change, empirical, 'Whether the state_hybrid reading is a stable equilibrium or a transitional state between the two more doctrinally committed sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__state_hybrid, theater_ratio, 8, 0.34).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__state_hybrid, theater_ratio, 16, 0.38).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.42).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__state_hybrid, theater_ratio, 32, 0.45).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__state_hybrid, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__state_hybrid, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__state_hybrid, base_extractiveness, 32, 0.34).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__state_hybrid, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__state_hybrid, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__state_hybrid, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% This constraint is part of the quran_hadith_substrate kernel family (3 readings: traditionalist_taqlid, state_hybrid, reformist_ijtihad). Each reading is authored as an independent constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not measurement-parameter variants of one constraint. state_hybrid sits structurally between the other two: it borrows classical retention from traditionalist_taqlid (in family/criminal law) and reformist/secular displacement from reformist_ijtihad (in commercial/administrative law), which is why it influences both siblings' operating environments without foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
