% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Licensing as Graduated Access Filter
 *   domain: labor_economic/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory occupational licensing began as a consumer-protection response
 *   to genuine information asymmetries in high-stakes services (medicine,
 *   law, engineering). Over 1970-2025, the regime expanded to cover 25-30% of
 *   the U.S. workforce, including occupations with low consumer risk
 *   (interior design, hair braiding, athletic training, funeral attendance).
 *   This reading — graduated_access_filter — analyzes the constraint as it
 *   operates today: a tiered barrier system where the cost and difficulty of
 *   credential acquisition sorts workers by class and prior resource access.
 *   The credentialed class (incumbents, administrators, education providers)
 *   benefits from restricted supply and captured revenue; marginalized
 *   workers without credential-acquisition resources are structurally
 *   excluded. The constraint's persistence depends on active enforcement
 *   (unlicensed practice penalties) and legislative capture by beneficiary
 *   groups. The coordination function (competence verification) is real but
 *   has become a cover story for the extraction function (rent capture via
 *   supply restriction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.82).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Licensing as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economic/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'f1ecb0e0-9886-4680-becf-e3879d1f44be').
narrative_ontology:cs_kernel_codification('f1ecb0e0-9886-4680-becf-e3879d1f44be', formalized).
narrative_ontology:cs_authority_grounding('f1ecb0e0-9886-4680-becf-e3879d1f44be', lineage).
narrative_ontology:cs_interpretation_layer_present('f1ecb0e0-9886-4680-becf-e3879d1f44be').
narrative_ontology:cs_reading_relation('f1ecb0e0-9886-4680-becf-e3879d1f44be', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('f1ecb0e0-9886-4680-becf-e3879d1f44be', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('f1ecb0e0-9886-4680-becf-e3879d1f44be', foundational, credential_costs_sort_regressively_by_class).
narrative_ontology:cs_axiom_status(credential_costs_sort_regressively_by_class, holdable).
narrative_ontology:cs_axiom_grounding('f1ecb0e0-9886-4680-becf-e3879d1f44be', credential_costs_sort_regressively_by_class, empirically_contingent).
narrative_ontology:cs_axiom('f1ecb0e0-9886-4680-becf-e3879d1f44be', secondary, statutory_monopoly_unnecessary_for_competence_verification).
narrative_ontology:cs_axiom_status(statutory_monopoly_unnecessary_for_competence_verification, holdable).
narrative_ontology:cs_axiom_grounding('f1ecb0e0-9886-4680-becf-e3879d1f44be', statutory_monopoly_unnecessary_for_competence_verification, empirically_contingent).
narrative_ontology:cs_reference_frame('f1ecb0e0-9886-4680-becf-e3879d1f44be', new_deal_consumer_protection_settlement).
narrative_ontology:cs_drift_state('f1ecb0e0-9886-4680-becf-e3879d1f44be', contemporary_credential_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1ecb0e0-9886-4680-becf-e3879d1f44be', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialing_education_providers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, displaced_workers_without_credential_resources).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_income_career_changers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, occupational_closure_theory).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, credentialism_as_class_reproduction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold valid licenses that grant legal monopoly over service provision in regulated occupations. Benefit from restricted supply that elevates wages and professional status. Can exit by practicing across state lines via reciprocity agreements or moving to less regulated jurisdictions, but rarely need to — the constraint protects their market position.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, incumbent_credentialed_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Administer licensing statutes, set examination standards, approve educational programs, and enforce compliance. Collect fees from applicants and licensees. Their institutional survival and budget depend on the licensing regime's continuation. Can transition to other regulatory roles or private consulting, but their authority is constituted by this specific constraint.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, beneficiary).

% Operate accredited programs (vocational schools, university departments, continuing education providers) that gate entry to licensure. Capture tuition revenue from mandatory credentialing pathways. Their business model depends on statutory requirements creating captive demand. Can pivot to non-credentialed training, but the statutory mandate is their primary revenue anchor.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialing_education_providers, beneficiary,
    organized, biographical, mobile, national).

% Face compounding barriers to credential acquisition: tuition costs, foregone earnings during training, examination fees, and geographic immobility. Often lack social capital to navigate opaque requirements. The constraint legally excludes them from occupations where they could otherwise compete on skill. Exit means accepting lower-wage unregulated work or prolonged unemployment — no viable path around the statutory barrier.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_labor_market_entrants, payer,
    powerless, biographical, trapped, national).

% Experienced workers displaced by automation, trade, or industry decline who possess relevant skills but lack formal credentials. Cannot afford multi-year retraining or credentialing programs. The constraint treats their experiential competence as legally irrelevant. Exit options are structurally blocked — they are too old for traditional pathways, too poor for accelerated ones, and the statute recognizes no equivalence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, displaced_workers_without_credential_resources, payer,
    powerless, immediate, trapped, national).

% Workers with some resources and labor market attachment seeking to enter regulated occupations. Face high opportunity costs and debt aversion. May eventually acquire credentials but at significant financial and temporal penalty that higher-income entrants do not face. Exit is theoretically possible (save, borrow, study) but the constraint imposes a regressive cost curve that sorts by class.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_income_career_changers, payer,
    moderate, biographical, constrained, regional).

% Argue licensing protects consumers from incompetent practitioners. Their position aligns with the public_safety_coordination reading. They participate in legislative hearings and rulemaking but do not bear the constraint's costs or collect its rents. Their analytical seat sees the constraint as potentially justified but requiring evidence-based calibration.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_protection_advocates, observer,
    organized, generational, analytical, national).

% Analyze licensing as occupational closure that restricts supply, raises prices, and redistributes income upward. Provide empirical evidence on wage premiums, quality effects, and demographic disparities. Their seat is purely analytical — they neither enforce nor suffer the constraint, but their research shapes the policy discourse that could reform it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, labor_economists_critique, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates minimum competence verification for services where consumer information asymmetry is high (healthcare, electrical work, structural engineering) — solves a genuine market failure where uninformed consumers cannot assess provider quality ex ante.
% TRANSFER_FUNCTION: Moves economic rents (wage premiums, tuition revenue, licensing fees) from excluded workers and consumers (higher prices) to incumbent practitioners, education providers, and licensing administrators. The transfer scales with the stringency and breadth of statutory scope.
% ABSENT_VOICES: Workers excluded by the credential barrier who would enter if not for statutory prohibition — they are not represented in licensing board hearings, legislative testimony, or professional association governance. Their absence is structural: the constraint legally silences their market participation. Also absent: consumers who would choose lower-cost uncredentialed providers for routine services but are prohibited from doing so.
% DISAPPEARANCE_RATIONALE: If statutory licensing vanished overnight, labor markets in regulated occupations would immediately reorganize: private certification would compete with public licensing, prices would fall as supply expanded, marginalized workers would enter previously closed occupations, and incumbent practitioners would lose statutory rent protection. The world rearranges because the constraint actively suppresses market structure.
% FOUNDING_PROBLEM: Late 19th/early 20th century: unregulated practice in medicine, law, and trades caused demonstrable consumer harm (quackery, unsafe construction, legal malpractice) with no reliable quality signal for consumers. Statutory licensing was the state's response to a genuine information-asymmetry market failure.
% FOUNDING_PROBLEM_CORROBORATION: Historical records (Flexner Report 1910, state legislative archives) corroborate the consumer-harm founding problem from outside the beneficiary set. However, contemporary labor economists (Kleiner, Blair, Koumenta) and consumer advocates document that the founding problem has been substantially solved for many occupations — the constraint has expanded beyond its founding scope into occupations where information asymmetry is low and consumer harm is minimal. The expansion phase is attested by legislative history showing industry lobbying for new licensure statutes, not consumer demand.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) reflects the scale of economic rents transferred: wage premiums of 10-15% for licensed occupations, billions in tuition revenue for credentialing programs, and administrative fees — all extracted from workers who could perform the work but cannot afford the statutory gateway. Suppression (0.82) is high because the constraint criminalizes unlicensed practice, creating legal barriers that cannot be circumvented by skill demonstration alone — enforcement is active and escalating (cease-and-desist orders, fines, criminal misdemeanors). Theater ratio (0.28) is moderate: the consumer-protection rationale is genuinely operative in high-stakes fields but increasingly performative in expanded scopes where quality differentials between licensed and unlicensed providers are empirically negligible. The measurement grid shows extractiveness and suppression rising together over 55 years — the constraint has become more extractive and more coercive as its scope expanded beyond its founding justification.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (boards, incumbents, schools), the constraint appears as necessary coordination — they experience the quality assurance function and the revenue it generates. From the payer seats (excluded workers), the same structure appears as an insurmountable barrier that sorts by class, not competence. The engine computes this divergence from the declared power/exit/role structure: organized/institutional actors with mobile/arbitrage exit compute as beneficiaries; powerless/moderate actors with trapped/constrained exit compute as extraction targets. The claimed type (snare) reflects the payer-seat reality; the beneficiary seats would claim rope or mountain. This seat divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners are structural beneficiaries (d ≈ 0.15): they collect wage premiums, control entry, and face mobile exit. Licensing administrators are agenda-setters with beneficiary capture (d ≈ 0.10): their institutional existence depends on the constraint. Education providers are beneficiaries (d ≈ 0.20): captive revenue stream. Marginalized entrants and displaced workers are full targets (d ≈ 0.95): trapped, no exit, bear full cost of exclusion. Low-income career changers are constrained targets (d ≈ 0.75): theoretically mobile but practically blocked by regressive cost curve. Consumer advocates and economists are analytical observers (d ≈ 0.50): symmetric, neither collecting nor paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer harm from information asymmetry) is substantially solved for core high-stakes occupations but has been dead for decades in the expanded scope where most licensing growth occurred. The arrangement persists because the beneficiary coalition (incumbents + administrators + educators) captures the policy process — a classic mandatrophy where the mandate outlived its function but the extraction machinery remains. The constraint is not a piton (theater is not dominant — extraction is active and growing) and not a tangled rope (the coordination function in expanded scopes is largely pretextual; the genuine coordination in core occupations could be served by private certification). It is a snare: pure extraction with active enforcement and identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the licensing regime''s tiered structure a natural consequence of competence verification requirements, or a constructed barrier that benefits identifiable agents?',
    'Compare licensing stringency and scope across jurisdictions with similar consumer protection outcomes — if stringency varies widely without quality differentials, the tiered structure is constructed. Also examine whether grandfathering clauses and reciprocity agreements treat experiential competence as equivalent (they typically do not).',
    'If natural, the constraint trends toward mountain/rope; if constructed with identifiable beneficiaries, it confirms snare/tangled_rope. The FSM signature would trigger on any mountain claim with beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the graduated access filter is an inevitable feature of competence verification or a constructed class-sorting mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, statutory barriers) or internalized (workers self-exclude believing they cannot succeed)?',
    'Track worker behavior in jurisdictions that temporarily suspended enforcement (e.g., COVID emergency orders) — if entry surges when legal barriers lift, suppression is primarily structural. If entry remains low despite legal permission, internalized suppression dominates.',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s reach extends beyond its enforcement apparatus into workers'' self-conception.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in occupational exclusion.').

omega_variable(
    kernel_reading_framing,
    'Does the graduated_access_filter reading foreclose, coexist with, or influence the public_safety_coordination and rent_seeking_suppression readings of the licensing_statute_mandate kernel?',
    'Analyze whether a single regulatory framework could simultaneously hold the graduated_access_filter''s class-sorting claim and the public_safety_coordination''s consumer-protection claim. If the class-sorting mechanism is demonstrated empirically, does it logically eliminate the consumer-protection justification, or can both be true in different occupational scopes?',
    'If forecloses: the readings cannot coexist in one policy framework — choosing one reading structurally determines the constraint''s classification. If coexists_with: all three readings remain live in political discourse, producing classification instability. If influences: this reading''s evidence creates pressure on sibling readings'' legitimacy conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Structural relationship between this kernel reading and its siblings.').

omega_variable(
    coordination_extraction_separability,
    'Is the genuine coordination function (competence verification in high-stakes fields) structurally separable from the extraction function (supply restriction in low-stakes fields)?',
    'Examine occupations where private certification competes with public licensing (e.g., IT certifications, financial planning). If quality outcomes are equivalent without statutory monopoly, the functions are separable — the extraction component is removable without losing coordination.',
    'If separable, the snare classification is robust: the extraction is not the price of coordination but a separable layer. If inseparable, the constraint may be tangled_rope — extraction as the cost of the coordination mechanism itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether competence verification requires statutory monopoly or can be achieved through competitive certification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lsmaf_tr_t1970, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(lsmaf_tr_t1985, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(lsmaf_tr_t2000, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(lsmaf_tr_t2010, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(lsmaf_tr_t2018, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(lsmaf_tr_t2025, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(lsmaf_be_t1970, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(lsmaf_be_t1985, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement(lsmaf_be_t2000, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(lsmaf_be_t2010, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(lsmaf_be_t2018, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement(lsmaf_be_t2025, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lsmaf_su_t1970, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(lsmaf_su_t1985, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(lsmaf_su_t2000, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(lsmaf_su_t2010, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(lsmaf_su_t2018, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement(lsmaf_su_t2025, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, identity_coordination).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.1).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, labor_market_credentialism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, immigrant_professional_relicensing).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, vocational_training_funding_regime).

% DUAL FORMULATION NOTE:
% Part of the licensing_statute_mandate constraint family with public_safety_coordination and rent_seeking_suppression. This reading (graduated_access_filter) emphasizes the class-sorting mechanism of credential costs; public_safety_coordination emphasizes consumer protection; rent_seeking_suppression emphasizes intentional rent capture. The three readings share the same statutory text but instantiate different constraints with different beneficiary/victim structures and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, moderate, 0.75).
constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, powerless, 0.95).
constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, organized, 0.15).
constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
