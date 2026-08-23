% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Normative Framework
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   The evolutionary framework reading treats common law precedent as an
 *   adaptive normative system where contemporary values legitimately reshape
 *   the meaning of prior decisions. Overruling is normalized as a corrective
 *   mechanism, not an extraordinary departure. This reading empowers the
 *   judiciary as the primary normative updater, particularly in
 *   constitutional domains where legislative correction is slow or blocked.
 *   The constraint extracts normative authority from historical decisions and
 *   reliance interests, transferring it to present judicial interpretation.
 *   Coordination persists — courts still cite precedent, follow hierarchy,
 *   and provide reasoned decisions — but the coordination function is
 *   subordinated to normative evolution. The claimed type (tangled_rope)
 *   reflects genuine coordination (stable citation practices, hierarchical
 *   compliance) combined with asymmetric extraction (judiciary gains updating
 *   power; reliance interests bear unchosen transition costs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.45).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Normative Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '6cb04e30-9043-4064-a023-51c2c2d7c934').
narrative_ontology:cs_kernel_codification('6cb04e30-9043-4064-a023-51c2c2d7c934', distributed).
narrative_ontology:cs_authority_grounding('6cb04e30-9043-4064-a023-51c2c2d7c934', practice).
narrative_ontology:cs_interpretation_layer_present('6cb04e30-9043-4064-a023-51c2c2d7c934').
narrative_ontology:cs_reading_relation('6cb04e30-9043-4064-a023-51c2c2d7c934', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('6cb04e30-9043-4064-a023-51c2c2d7c934', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('6cb04e30-9043-4064-a023-51c2c2d7c934', foundational, precedent_serves_contemporary_justice).
narrative_ontology:cs_axiom_status(precedent_serves_contemporary_justice, holdable).
narrative_ontology:cs_axiom_grounding('6cb04e30-9043-4064-a023-51c2c2d7c934', precedent_serves_contemporary_justice, instrumental).
narrative_ontology:cs_axiom('6cb04e30-9043-4064-a023-51c2c2d7c934', secondary, overruling_as_corrective_not_exception).
narrative_ontology:cs_axiom_status(overruling_as_corrective_not_exception, holdable).
narrative_ontology:cs_axiom_grounding('6cb04e30-9043-4064-a023-51c2c2d7c934', overruling_as_corrective_not_exception, conventional).
narrative_ontology:cs_reference_frame('6cb04e30-9043-4064-a023-51c2c2d7c934', classical_stare_decisis).
narrative_ontology:cs_drift_state('6cb04e30-9043-4064-a023-51c2c2d7c934', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6cb04e30-9043-4064-a023-51c2c2d7c934', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_profession).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_with_novel_claims).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, parties_bound_by_overruled_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, legal_stability_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, lower_courts).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, litigants_with_novel_claims).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_courts).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, law_adapts_to_contemporary_values).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, judicial_role_includes_normative_updating).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits at the apex of the precedent hierarchy; decides when to overrule, distinguish, or extend prior decisions. Gains normative authority as the designated updater of legal meaning. Faces institutional legitimacy constraints but controls the framework's evolution. Exit means leaving the bench — analytically, they observe the system they administer.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Gain broader pathways to challenge existing norms through evolutionary arguments (e.g., 'evolving standards of decency,' 'changed circumstances'). But bear high litigation costs and face uncertainty — the same flexibility that admits their claim admits counter-arguments. Cannot easily exit the legal system when rights are at stake.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_with_novel_claims, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, litigants_with_novel_claims, payer).

% Structured their affairs, contracts, or conduct around precedent that is later overruled. Bear the cost of normative shifts they did not choose and cannot easily anticipate. Legal remedies for reliance are limited (prospective overruling is rare). Exit from the legal consequences is nearly impossible once a decision is rendered.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, parties_bound_by_overruled_precedent, payer,
    powerless, biographical, trapped, national).

% Institutional actors (businesses, agencies, lower courts) that invest in compliance structures built on stable precedent. Overruling imposes transition costs — retraining, system redesign, regulatory revision. Can lobby for legislative fixes or prospective application but cannot exit the legal framework that governs their domain.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interests, payer,
    organized, generational, constrained, national).

% Gains interpretive work, advisory roles, and litigation opportunities from precedent fluidity. The more contested the evolutionary trajectory, the more professional services are demanded. Mobile across practice areas and jurisdictions — can shift focus if evolutionary framework becomes too unstable or too rigid.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Bound by vertical stare decisis but gain flexibility in horizontal application (distinguishing, predictive overruling). Bear the burden of applying shifting standards without final authority. Benefit from discretion in fact-intensive evolutionary inquiries. Cannot exit the hierarchy; constrained by appellate review.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_courts, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, lower_courts, beneficiary).

% Disproportionately affected by precedent evolution (e.g., criminal procedure, voting rights, due process) but rarely appear as named parties in precedent-setting appeals. Their normative perspectives enter only through amicus briefs or empirical studies cited by courts. Cannot exit the legal system's reach; trapped by structural exclusion from the interpretive conversation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, marginalized_communities, excluded,
    powerless, generational, trapped, national).

% Analyze, critique, and theorize the evolutionary framework from outside the adjudicative process. Influence judicial reasoning through publications and testimony but hold no decisional authority. Their exit is analytical — they can change frameworks, jurisdictions, or methodologies without material consequence.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable-but-adaptable legal framework that coordinates expectations across time while allowing normative corrections when prior decisions drift from contemporary justice. Solves the problem of legal ossification without legislative action at every turn.
% TRANSFER_FUNCTION: Transfers normative authority from past decisions to present judicial interpretation; transfers reliance costs from the judiciary (which avoids accountability for outdated rules) to parties who structured affairs around overruled precedent; transfers interpretive labor and professional rents to the legal profession.
% ABSENT_VOICES: Marginalized communities most affected by precedent evolution in criminal law, immigration, and social welfare are structurally excluded from the appellate docket that shapes evolutionary trajectories. Their interests appear only as data points in empirical studies or amicus filings curated by institutional repeat players. Future generations who will live with today's overrulings have no voice at all.
% DISAPPEARANCE_RATIONALE: If the evolutionary framework vanished overnight, courts would revert to strict stare decisis or legislative-only change. Novel rights claims (privacy, dignity, equality) that depend on evolutionary reasoning would lose their doctrinal pathway. Legislative dockets would flood with corrective statutes. The legal profession's interpretive economy would contract. Reliance interests would gain stability but lose corrective mechanisms.
% FOUNDING_PROBLEM: Classical stare decisis became a barrier to correcting entrenched errors (e.g., separate-but-equal, economic substantive due process) when legislative correction was politically blocked. The evolutionary framework was built to let courts correct course without waiting for legislative majorities that might never form.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary attests the problem remains live (citing ongoing need for adaptive interpretation in technology, privacy, equality). Originalist scholars and some legislators attest the founding problem is dead — legislative processes now function and judicial updating usurps democratic authority. Comparative law scholars note other common law systems (UK, Canada) manage correction without U.S.-style evolutionary framework, suggesting the problem was jurisdictional-structural, not universal.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the judiciary's capture of normative updating authority and the transfer of reliance costs to bound parties. Suppression (0.45) is moderate — courts do not forbid legislative override or constitutional amendment, but the framework's self-reinforcing logic (precedent begets precedent) makes exit from evolutionary reasoning difficult for lower courts and litigants. Theater ratio (0.30) captures performative adherence to stare decisis language while substantive analysis follows evolutionary logic. Accessibility collapse (0.55) reflects that alternatives exist (legislation, constitutional amendment, Article V) but are politically costly. Resistance (0.50) comes from originalist jurisprudence, reliance-interest litigation, and legislative pushback (jurisdiction-stripping, court-curbing).
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, the framework is genuine coordination — they provide reasoned, constrained evolution. From the parties_bound_by_overruled_precedent seat, it is extraction — their reliance is disregarded without remedy. From the legal_profession seat, it is a rent-generating coordination mechanism. The engine computes this divergence from the structural power/exit data; the evolutionary framework's legitimacy narrative obscures the extraction from the agenda-setter's view.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary sits at d≈0.15 (strong beneficiary: controls the framework, gains authority, faces minimal personal cost). Litigants with novel claims sit at d≈0.45 (near-symmetric: gain challenge pathways but bear high costs and uncertainty). Parties bound by overruled precedent sit at d≈0.85 (near-full target: bear unchosen costs, trapped exit). Reliance interests sit at d≈0.70 (target: organized but constrained exit). Legal profession sits at d≈0.20 (beneficiary: mobile, gains rents). Lower courts sit at d≈0.55 (slight target: constrained by hierarchy, gain some discretion). Marginalized communities sit at d≈0.90 (target: trapped, excluded from interpretive conversation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (correcting entrenched errors when legislature fails) remains partially live but has mutated: today's evolutionary framework also enables judicial policymaking in domains where legislative majorities exist but disagree with judicial outcomes. The constraint now serves a dual mandate — error correction AND normative leadership — and the second function extracts authority beyond the founding justification. This dual mandate is the mandatrophy signature: the original coordination function persists but has been layered with an extractive judicial supremacy function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_normative_constraint,
    'Does the evolutionary framework provide enough constraint on judicial discretion to distinguish it from unguided policymaking, or does ''contemporary normative evolution'' function as a blank check for judicial preferences?',
    'Empirical study of overruling patterns: if overrulings correlate with measurable shifts in public opinion, professional consensus, or international law rather than judicial ideology, the constraint is real. If they track appointing-party ideology, the framework is cover.',
    'If the framework is a blank check, extractiveness is underestimated — the judiciary extracts unconstrained policymaking authority. If genuinely constrained, the coordination function is stronger and extraction is the price of adaptive governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_discretion_vs_normative_constraint, empirical, 'Whether ''normative evolution'' is a discernible constraint or a rhetorical cover for judicial discretion.').

omega_variable(
    reliance_protection_adequacy,
    'Are reliance interests adequately protected by prospective overruling, stare decisis factors, and legislative correction, or does the evolutionary framework systematically externalize transition costs onto the least powerful parties?',
    'Longitudinal analysis of prospective-overruling frequency, reliance-interest amicus participation, and legislative override rates in domains with high evolutionary activity.',
    'If protection is systematically inadequate, the framework is more extractive (snare-adjacent) than tangled_rope — the coordination story masks systematic cost externalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_protection_adequacy, empirical, 'Whether the framework''s extraction from reliance interests is mitigated or systematic.').

omega_variable(
    evolutionary_framework_legitimacy_source,
    'Does the evolutionary framework''s legitimacy derive from its functional success (adapting law to justice) or from the judiciary''s institutional interest in maintaining interpretive monopoly?',
    'Counterfactual: if a legislative commission with equal expertise and democratic legitimacy produced better-adapted rules, would courts defer? Historical episodes of court-commission interaction (e.g., sentencing guidelines, administrative law) provide evidence.',
    'If legitimacy is functional, the framework is a genuine (if extractive) coordination mechanism. If legitimacy is institutional monopoly, the framework is a self-justifying power grab — closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_framework_legitimacy_source, conceptual, 'The grounding of the framework''s legitimacy claim — functional or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 15, 0.2).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.25).
narrative_ontology:measurement(comm_tr_t45, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 45, 0.28).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 60, 0.29).
narrative_ontology:measurement(comm_tr_t74, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 74, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(comm_be_t45, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(comm_be_t74, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 74, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(comm_su_t45, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 45, 0.42).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(comm_su_t74, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 74, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'common law precedent' label into an evolutionary adaptive framework. The strict_stare_decisis reading treats the same corpus as a Mountain (binding constraint); the pluralist_balancing reading treats it as a Scaffold (context-dependent, transitional balancing). The ε values differ substantially: strict_stare_decisis ε≈0.15 (coordination only), pluralist_balancing ε≈0.40 (moderate extraction), evolutionary_framework ε≈0.65 (substantial extraction from reliance interests). They are linked as a constraint family because the evolutionary reading cites the failure of strict stare decisis as its founding justification, and pluralist_balancing positions itself as the moderate alternative to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, institutional, 0.15).
constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
