% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary with Non-Commercial Carveout
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_carveout_reading of the
 *   derivative_work_statutory_boundary kernel. Under this reading, copyright
 *   law distinguishes commercial from non-commercial transformative use:
 *   non-commercial remixes and adaptations are permitted without
 *   authorization, while commercial exploitation of derivative works requires
 *   a license from the original rights holder. The constraint is structurally
 *   a tangled rope: it carries a genuine coordination function (protecting
 *   non-commercial creativity from licensing friction) while simultaneously
 *   extracting from commercial derivative developers through an authorization
 *   requirement that channels revenue to rights holders. The arrangement is
 *   actively enforced through infringement litigation, statutory damages,
 *   DMCA takedowns, and marketplace licensing norms. The claim (tangled_rope)
 *   and the metrics are authored independently; the engine will compute
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - statutory_authority: Legislative and judicial bodies that set and interpret the derivative work boundary (institutional/constrained)
 *   - rights_holder_industry: Copyright owners and collecting societies that extract licensing revenue from commercial uses (powerful/mobile)
 *   - commercial_derivative_developers: Commercial creators who must secure licenses for transformative works (moderate/constrained)
 *   - non_commercial_transformative_users: Remixers, fan creators, and academics who rely on the non-commercial carveout (organized/mobile)
 *   - copyright_reform_advocates: Excluded voices arguing for broader permissive use regardless of commerciality (organized/constrained)
 *   - empirical_ip_economists: Analytical observers studying incentive effects (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.65).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary with Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '49a5dde7-d881-4852-af23-fca411036de7').
narrative_ontology:cs_kernel_codification('49a5dde7-d881-4852-af23-fca411036de7', formalized).
narrative_ontology:cs_authority_grounding('49a5dde7-d881-4852-af23-fca411036de7', lineage).
narrative_ontology:cs_interpretation_layer_present('49a5dde7-d881-4852-af23-fca411036de7').
narrative_ontology:cs_reading_relation('49a5dde7-d881-4852-af23-fca411036de7', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('49a5dde7-d881-4852-af23-fca411036de7', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('49a5dde7-d881-4852-af23-fca411036de7', foundational, commercial_exploitation_triggers_authorization).
narrative_ontology:cs_axiom_status(commercial_exploitation_triggers_authorization, holdable).
narrative_ontology:cs_axiom_grounding('49a5dde7-d881-4852-af23-fca411036de7', commercial_exploitation_triggers_authorization, conventional).
narrative_ontology:cs_axiom('49a5dde7-d881-4852-af23-fca411036de7', foundational, non_commercial_transformative_use_permitted).
narrative_ontology:cs_axiom_status(non_commercial_transformative_use_permitted, holdable).
narrative_ontology:cs_axiom_grounding('49a5dde7-d881-4852-af23-fca411036de7', non_commercial_transformative_use_permitted, conventional).
narrative_ontology:cs_reference_frame('49a5dde7-d881-4852-af23-fca411036de7', statutory_carveout_equilibrium).
narrative_ontology:cs_drift_state('49a5dde7-d881-4852-af23-fca411036de7', digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49a5dde7-d881-4852-af23-fca411036de7', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_industry).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_exploitation_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, transformative_use_carveout).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and interprets the derivative work boundary through legislation and judicial decisions, distinguishing commercial from non-commercial transformative use. Bound by statutory text and precedent, but retains formal capacity to revise the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, statutory_authority, agenda_setter,
    institutional, generational, constrained, national).

% Holds copyright portfolios and extracts licensing revenue from commercial derivative uses that fall within the statutory boundary. Lobbies for maintaining the authorization requirement for commercial exploitation. Benefits from the constraint's asymmetric treatment of commercial and non-commercial spheres.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_industry, beneficiary,
    powerful, generational, mobile, global).

% Develop transformative works for commercial release and must secure authorization from rights holders or risk infringement liability. Bears licensing costs, transaction friction, and legal uncertainty. Exit is limited to obtaining licenses, litigating fair use claims, or abandoning commercial derivative projects.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers, payer,
    moderate, biographical, constrained, national).

% Create remixes, fan works, and transformative content without commercial intent, relying on the statutory carveout that permits non-commercial transformative use without authorization. Their freedom to operate is contingent on the boundary's stability and judicial interpretation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    organized, biographical, mobile, global).

% Argue that the derivative work boundary should be narrower regardless of commerciality, emphasizing access and follow-on creativity. Systematically underrepresented in legislative drafting and industry-led norm-setting; their preferred permissive framework is not reflected in the hybrid carveout.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_reform_advocates, excluded,
    organized, generational, constrained, global).

% Study whether the commercial versus non-commercial distinction actually affects creator incentives and consumer welfare. Their findings are rarely dispositive in statutory or judicial settings but provide external analytical leverage on the constraint's claimed coordination function.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, empirical_ip_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_industry).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates creative production by allowing non-commercial transformative uses without transaction costs while channeling commercial derivative activity through licensing markets, ostensibly preserving incentives for original creation.
% TRANSFER_FUNCTION: Transfers licensing revenue and authorization obligations from commercial derivative developers to original rights holders; transfers freedom-to-operate to non-commercial transformative users.
% ABSENT_VOICES: Copyright reform advocates who argue for a unified permissive standard regardless of commerciality, and empirical economists who question whether the commercial versus non-commercial distinction tracks actual market harm, are systematically underrepresented in legislative hearings and industry-led norm-setting.
% DISAPPEARANCE_RATIONALE: If the hybrid carveout vanished, commercial derivative developers would face either blanket permission or blanket prohibition, reorganizing licensing markets; non-commercial users would lose their safe harbor and face the same clearance costs as commercial actors; rights holders would lose a key licensing lever.
% FOUNDING_PROBLEM: How to sustain incentives for original expression without stifling the socially valuable follow-on creativity that builds on existing works, particularly in an environment where digital copying blurs traditional commercial boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Rights holder industry attests the problem is live, citing market substitution. Independent empirical studies and copyright reform advocates contest whether the commercial versus non-commercial carveout actually solves the incentive problem or instead creates inefficient licensing friction; legislative history shows heavy rights-holder influence with limited independent corroboration.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate because the authorization requirement channels real revenue from commercial developers to rights holders, but only applies to a subset of uses and is partially offset by the coordination benefit to non-commercial users. Suppression (0.65) reflects active enforcement via litigation, statutory damages, and platform-level takedown regimes. Theater ratio (0.40) captures the performative dimension of rights-holder enforcement that sometimes targets non-commercial uses despite the carveout, creating uncertainty. Accessibility collapse (0.60) is moderate-high: alternatives like Creative Commons and the public domain exist but do not cover the majority of commercially valuable source material. Resistance (0.45) comes from technology sectors, copyleft movements, and commercial developers who bear licensing costs. The measurement series tracks rising extraction as licensing markets become more institutionalized and enforcement technology improves.
 *
 * PERSPECTIVAL GAP:
 *   The statutory authority seat experiences the constraint as a balanced compromise between competing interests. The rights holder industry experiences it as a legitimate property-enforcement mechanism. Commercial derivative developers experience it as a tax on innovation and a barrier to market entry. Non-commercial users experience it as a protective safe harbor. The engine will compute these divergent types from the same structural data: the agenda-setter and rights-holder seats should compute toward coordination-leaning types, while the commercial developer seat should compute toward extraction-leaning types.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders are declared beneficiaries: the constraint subsidizes their position by creating a licensable gate on commercial derivative markets, giving them low directionality. Non-commercial transformative users are also declared beneficiaries: the carveout subsidizes their freedom to operate, also yielding low directionality. Commercial derivative developers are declared victims (payers): they bear the cost of licensing and legal risk, yielding high directionality. The statutory authority is agenda-setter, neither beneficiary nor victim in the extraction flow, with directionality derived from its structural position as rule-maker. Copyright reform advocates are excluded, and analytical observers are neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction: if the non-commercial carveout were removed, the constraint would become pure extraction (snare), because commercial developers would still pay while non-commercial users would lose their coordination benefit. If the commercial authorization requirement were removed, the constraint would dissolve into a permissive coordination mechanism (rope). The hybrid structure is what makes it a tangled rope. The founding problemâincentive preservation versus follow-on creativityâremains contested, and the constraint's persistence is partly justified by the genuine coordination it provides to non-commercial users, even as it extracts from commercial actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_noncommercial_harm_correlation,
    'Does commercial exploitation of transformative works actually correlate with measurable harm to the market for the original work, compared to non-commercial use?',
    'Empirical economic studies measuring substitution effects and licensing market impact across commercial and non-commercial derivative uses.',
    'If no differential harm exists, the carveout is arbitrary extraction from commercial developers rather than genuine coordination; if harm is differential, the carveout tracks a real coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_noncommercial_harm_correlation, empirical, 'Whether the commercial versus non-commercial distinction maps to actual market harm.').

omega_variable(
    statutory_capture_by_rights_holders,
    'To what extent does the statutory authority independently formulate the derivative work boundary, versus transpose rights-holder industry preferences into law?',
    'Comparative legislative history analysis and lobbying expenditure data across jurisdictions with similar carveout regimes.',
    'High capture would reclassify the constraint toward snare (extraction dressed as coordination); low capture would support the tangled rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_capture_by_rights_holders, empirical, 'Degree of rights-holder capture in statutory formulation.').

omega_variable(
    noncommercial_subsidy_stability,
    'Is the non-commercial exemption a stable coordination benefit or a contingent political concession that could retract without structural warning?',
    'Tracking legislative and judicial drift toward narrowing the non-commercial safe harbor over time.',
    'If unstable, non-commercial users are effectively identity-locked into a temporary subsidy rather than secure beneficiaries, altering the directionality for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(noncommercial_subsidy_stability, conceptual, 'Stability of the non-commercial carveout as a coordination benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_carveout_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hybrid_carveout_tr_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(hybrid_carveout_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(hybrid_carveout_tr_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(hybrid_carveout_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(hybrid_carveout_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(hybrid_carveout_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hybrid_carveout_be_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(hybrid_carveout_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(hybrid_carveout_be_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(hybrid_carveout_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(hybrid_carveout_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_carveout_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hybrid_carveout_su_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(hybrid_carveout_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(hybrid_carveout_su_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(hybrid_carveout_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(hybrid_carveout_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the derivative_work_statutory_boundary kernel. The hybrid_carveout_reading differs from its siblings in its foundational bifurcation between commercial and non-commercial use. Each reading carries a different epsilon, beneficiary structure, and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
