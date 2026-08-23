% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Context-Dependent Woman/Female Category Membership Policy
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The hybrid contextual reading of the 'woman/female' category kernel
 *   asserts that biological sex should determine category membership in
 *   medical, sports, and safety contexts (where physiological differences are
 *   material), while gender identity should determine membership in social,
 *   legal, and administrative contexts (where dignity and inclusion are
 *   paramount). This reading emerged as an institutional compromise between
 *   2015-2025 as universalist positions on both sides produced escalating
 *   litigation and policy gridlock. The constraint is claimed as a tangled
 *   rope: it performs genuine coordination by allocating each criterion to
 *   its domain of strongest justification, but it extracts asymmetrically
 *   from both constituent groups in the domains where their preferred
 *   criterion is subordinated. Institutional actors benefit from conflict
 *   minimization without resolving the underlying dispute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.65).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Context-Dependent Woman/Female Category Membership Policy").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '907fd926-651d-4833-8ba8-0eb75fe124c2').
narrative_ontology:cs_kernel_codification('907fd926-651d-4833-8ba8-0eb75fe124c2', distributed).
narrative_ontology:cs_authority_grounding('907fd926-651d-4833-8ba8-0eb75fe124c2', practice).
narrative_ontology:cs_interpretation_layer_present('907fd926-651d-4833-8ba8-0eb75fe124c2').
narrative_ontology:cs_reading_relation('907fd926-651d-4833-8ba8-0eb75fe124c2', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('907fd926-651d-4833-8ba8-0eb75fe124c2', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('907fd926-651d-4833-8ba8-0eb75fe124c2', foundational, context_dependent_category_membership).
narrative_ontology:cs_axiom_status(context_dependent_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('907fd926-651d-4833-8ba8-0eb75fe124c2', context_dependent_category_membership, instrumental).
narrative_ontology:cs_axiom('907fd926-651d-4833-8ba8-0eb75fe124c2', foundational, domain_specific_criteria_legitimacy).
narrative_ontology:cs_axiom_status(domain_specific_criteria_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('907fd926-651d-4833-8ba8-0eb75fe124c2', domain_specific_criteria_legitimacy, conventional).
narrative_ontology:cs_reference_frame('907fd926-651d-4833-8ba8-0eb75fe124c2', pragmatic_domain_separation).
narrative_ontology:cs_drift_state('907fd926-651d-4833-8ba8-0eb75fe124c2', contemporary_gender_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('907fd926-651d-4833-8ba8-0eb75fe124c2', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, medical_boards).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, courts_legislatures).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, employers_hr_departments).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_critical_women_in_social_legal_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men_in_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, detransitioners_in_legal_contexts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, gender_critical_women_in_social_legal_contexts).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, medical_professionals).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, contextual_pragmatism_in_category_membership).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, domain_specific_criteria_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, legislatures, sports governing bodies, medical boards, and employers who adopt hybrid policies to minimize litigation risk and political conflict. They benefit from the appearance of compromise while avoiding decisive commitment to either universal criterion. Their exit is arbitrage-grade: they can shift policy domains independently and face minimal personal cost.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_actors, beneficiary).

% Trans women who are recognized as women in social/legal contexts but classified by biological sex in sports competition, prison placement, and some medical protocols. They bear exclusion costs in domains where the sex criterion applies while gaining recognition where the gender identity criterion applies. Exit is constrained: they cannot easily change legal recognition or medical transition status, and sports participation is structurally gatekept.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts, beneficiary).

% Women who organize around sex-based rights and spaces. They benefit from sex-based classification in sports, prisons, and medical contexts but pay costs in social/legal contexts where gender identity determines access to women's spaces, pronouns, and legal protections. Exit is constrained: their advocacy is identity-locked to sex-based feminism; disengaging means abandoning a core political self-concept.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_critical_women_in_social_legal_contexts, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, gender_critical_women_in_social_legal_contexts, beneficiary).

% Trans men who are recognized as men socially/legally but face sex-based classification in reproductive healthcare, obstetrics, and some research protocols. They pay costs of misgendering and access barriers in medical contexts where biological sex criteria override legal gender. Exit is constrained by medical necessity and institutional record-keeping.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_men_in_medical_contexts, payer,
    moderate, biographical, constrained, national).

% People who detransition and seek to revert legal gender markers. They face barriers in legal contexts where gender identity self-declaration creates easy entry but difficult exit, while medical contexts may still treat them according to natal sex. Exit is trapped: legal systems optimized for forward transition lack clear detransition pathways.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, detransitioners_in_legal_contexts, payer,
    powerless, biographical, trapped, national).

% Clinicians and researchers who use sex-based criteria for clinical decision-making (dosing, screening, risk assessment) while respecting gender identity for patient interaction. They benefit from clinical clarity in biological domains and professional legitimacy in social domains. Exit is mobile: they can specialize, relocate, or shift practice focus.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_professionals, beneficiary,
    organized, biographical, mobile, national).

% International and national federations (IOC, World Athletics, NCAA) that set eligibility policies. They benefit from the hybrid framework's claim to balance fairness and inclusion while avoiding the political cost of a universal rule. Their exit is arbitrage-grade: they can modify policies per sport, per competition level, and across jurisdictions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Judiciaries interpreting anti-discrimination law, prison placement, identity documents, and family law. They benefit from the hybrid approach's case-by-case flexibility but bear enforcement costs when contexts conflict. Exit is analytical: they interpret rather than create policy, constrained by precedent and legislative frameworks.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Activists who reject any gender identity criterion as capitulation to male supremacy. They are structurally excluded from the hybrid compromise because their position (universal sex classification) is treated as the extreme the hybrid is designed to marginalize. Exit is identity-locked: their political self-concept is constituted through opposition to gender identity ideology.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, radical_feminist_activists, excluded,
    organized, biographical, identity_locked, national).

% Activists who reject any sex-based exception as transphobic gatekeeping. They are structurally excluded because their position (universal gender identity classification) is treated as the extreme the hybrid is designed to marginalize. Exit is identity-locked: their advocacy identity is constituted through total affirmation of self-identification.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_activists, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages intractable conflict between two universalist claims (sex determines category everywhere vs. gender identity determines category everywhere) by assigning each criterion to domains where its costs are most visible and its benefits most accepted, thereby reducing total social friction.
% TRANSFER_FUNCTION: Moves recognition and access rights between groups across domains: trans women gain social/legal recognition but lose sports/medical access; gender-critical women retain sports/medical protections but lose exclusive control of social/legal women's categories. Institutional actors collect legitimacy and conflict-reduction benefits.
% ABSENT_VOICES: Radical feminists who reject all gender identity criteria and gender identity activists who reject all sex-based exceptions are both structurally excluded from the hybrid compromise. They would object that the hybrid legitimizes the opposing criterion in any domain. Detransitioners and intersex people are also largely absent from the policy design process.
% DISAPPEARANCE_RATIONALE: If the hybrid policy vanished overnight, institutions would face immediate pressure to adopt either universal sex classification or universal gender identity classification. Sports bodies would face litigation from both sides; prisons would face constitutional challenges; medical protocols would lose their pragmatic justification. The world would reorganize around a more polarized conflict with higher total suppression.
% FOUNDING_PROBLEM: The post-2015 escalation of conflict between sex-based rights advocacy and gender identity affirmation created institutional paralysis: sports bans, bathroom bills, prison placement lawsuits, medical protocol disputes, and pronoun compelled-speech cases all proceeding simultaneously with no stable settlement.
% FOUNDING_PROBLEM_CORROBORATION: The hybrid reading's founding problem is attested by institutional actors (IOC framework documents, UK Cass Review, US executive orders 2021-2025) who explicitly cite conflict reduction as justification. Both radical feminist organizations (WoLF, Fair Play for Women) and gender identity advocacy groups (HRC, GLAAD, Transgender Law Center) corroborate that the conflict exists but reject the hybrid as the solution — each attests the problem is live only because the other side refuses to accept their universal criterion.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.55) is moderate but distributed: each group pays in some domains, gains in others. Suppression (0.65) is substantial because the hybrid requires active enforcement across multiple legal and policy regimes (Title IX, Equality Act interpretations, sports eligibility rules, prison placement policies, medical guidelines). Theater ratio (0.42) is significant: institutional actors perform inclusion and fairness simultaneously while the substantive conflict persists. Accessibility collapse (0.52) reflects that alternatives (universal sex, universal gender identity) remain politically live but institutionally marginalized. Resistance (0.75) is high because both universalist movements treat the hybrid as betrayal, not settlement.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setter seat experiences the constraint as coordination (rope-like): it solves the problem of competing universalist demands. The payer seats (trans women in sports, gender-critical women in law) experience it as extraction (snare-like): they lose in domains they consider essential. The excluded seats experience it as suppression without representation. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the cross-seat disagreement without resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors are structural beneficiaries (d ~ 0.15): they collect conflict-reduction rents and legitimacy while distributing costs to constituent groups. Trans women in sports/medical contexts and gender-critical women in social/legal contexts are structural targets (d ~ 0.75-0.85): they bear concentrated exclusion costs in domains where the opposing criterion applies, with constrained exit. Medical professionals and sports bodies have arbitrage-grade exit (domain-specific policy authority). Detransitioners are trapped (d ~ 0.95): legal systems lack detransition pathways while medical systems revert to sex classification. Radical feminists and gender identity activists are identity-locked excluded (d ~ 0.9): their exclusion is structural, not incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading was built to solve the founding problem of institutional paralysis from competing universalist claims. That problem remains contested: both sides attest the conflict persists because the other side refuses their universal criterion, not because the hybrid fails. The hybrid has not become a piton — it is actively maintained and litigated, not inert. But its mandatrophy risk is high: if one universalist position achieves decisive political victory, the hybrid's coordination function evaporates while its extraction mechanisms (the domain-specific rules) persist as the victor's enforcement tools.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_stability,
    'Are the domain boundaries (medical/sports/safety vs. social/legal) stable and mutually exclusive, or do they bleed into each other in practice?',
    'Case law analysis of boundary disputes: prison healthcare (medical + safety + legal), school sports (sports + social + educational), insurance coverage (medical + legal + administrative). Track whether courts and agencies maintain clean separation or create hybrid sub-domains.',
    'If boundaries bleed, the hybrid collapses into de facto universal gender identity (if social/legal expands) or universal sex (if medical/safety expands). Extraction would concentrate on one group rather than distributing. The tangled rope classification depends on stable domain separation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_boundary_stability, empirical, 'Whether the contextual domains remain operationally distinct or merge under institutional pressure.').

omega_variable(
    institutional_beneficiary_capture,
    'Do institutional actors genuinely benefit from conflict reduction, or have they captured the hybrid to entrench their own discretionary power?',
    'Compare policy stability: if institutions modify domain boundaries opportunistically (e.g., expanding ''safety'' to include social spaces, or ''medical'' to include cosmetic procedures), the hybrid serves institutional capture. If boundaries hold despite political pressure, conflict reduction is genuine.',
    'If capture, the constraint reclassifies toward snare (institutional extraction disguised as coordination). If genuine coordination, tangled rope holds. The beneficiary set would shift from ''institutional actors seeking conflict minimization'' to ''institutional actors maximizing discretionary authority.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_capture, conceptual, 'Whether the hybrid''s coordination function is authentic or a cover for institutional power expansion.').

omega_variable(
    committer_framing_underdetermination,
    'Does the hybrid reading represent a single coherent constraint, or does it decompose into multiple constraints (one per domain) with different ε values?',
    'Apply ε-invariance test: measure extraction separately in sports, prison, medical, legal, administrative domains. If ε varies significantly across domains (e.g., 0.7 in sports, 0.3 in administrative), the hybrid label conflates multiple constraints. Write separate stories per domain and link via network.affects_constraints.',
    'If the hybrid decomposes, this single story is analytically invalid. Each domain-constraint would have its own classification (some rope, some snare, some tangled rope). The kernel reading framework would need to model domain-specific readings rather than a single hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Whether the hybrid reading satisfies ε-invariance or must be decomposed into a constraint family per domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__hybrid_contextual_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(woma_tr_t2017, woman_female_category__hybrid_contextual_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(woma_tr_t2019, woman_female_category__hybrid_contextual_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(woma_tr_t2021, woman_female_category__hybrid_contextual_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(woma_tr_t2023, woman_female_category__hybrid_contextual_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(woma_tr_t2025, woman_female_category__hybrid_contextual_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(woma_be_t2015, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(woma_be_t2017, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(woma_be_t2019, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(woma_be_t2021, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement(woma_be_t2023, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement(woma_be_t2025, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2015, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(woma_su_t2017, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2017, 0.52).
narrative_ontology:measurement(woma_su_t2019, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(woma_su_t2021, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(woma_su_t2023, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2023, 0.64).
narrative_ontology:measurement(woma_su_t2025, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This hybrid reading decomposes the woman/female category kernel into domain-specific applications. The sex_biology_reading and gender_identity_reading are universalist constraints; this reading distributes their criteria across domains. The three form a constraint family where the hybrid's domain boundaries are the contested interface. Upstream: the kernel's existence as a contested category creates demand for all three readings. Downstream: domain-specific policies (trans_athlete_eligibility, prison_placement_protocols, gender_recognition_acts) are children of these readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
