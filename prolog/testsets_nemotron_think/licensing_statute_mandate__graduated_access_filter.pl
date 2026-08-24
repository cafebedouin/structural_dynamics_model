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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: economic/political/administrative
 *
 * SUMMARY:
 *   Statutory credential requirements for occupational licensing began as
 *   genuine safety coordination in high-harm fields (medicine, engineering)
 *   but have expanded into a tiered system of graduated barriers across
 *   hundreds of occupations. The constraint operates as a filter: each tier
 *   of credential (apprentice, journeyman, master; LPN, RN, NP; paralegal,
 *   attorney) requires escalating time, capital, and institutional
 *   gatekeeping. The coordination story — minimum competence for public
 *   protection — is real at the core but functions as cover for a structure
 *   that sorts market access by class and prior resource access. Marginalized
 *   workers without credential-acquisition resources are the structural
 *   victims; credentialed incumbents, education institutions, and licensing
 *   administrators are the beneficiaries. The constraint persists through
 *   active enforcement (legal penalties for unlicensed practice, board
 *   discretion over reciprocity, statutory scope-of-practice protection) and
 *   suppression of alternatives (non-recognition of experience, foreign
 *   credentials, and competency-based pathways).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.85).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "economic/political/administrative").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '38b8e365-4246-4665-b24b-4a49bf0f440c').
narrative_ontology:cs_kernel_codification('38b8e365-4246-4665-b24b-4a49bf0f440c', formalized).
narrative_ontology:cs_authority_grounding('38b8e365-4246-4665-b24b-4a49bf0f440c', extraction).
narrative_ontology:cs_interpretation_layer_present('38b8e365-4246-4665-b24b-4a49bf0f440c').
narrative_ontology:cs_reading_relation('38b8e365-4246-4665-b24b-4a49bf0f440c', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('38b8e365-4246-4665-b24b-4a49bf0f440c', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('38b8e365-4246-4665-b24b-4a49bf0f440c', foundational, tiered_barriers_are_necessary_for_competence_assurance).
narrative_ontology:cs_axiom_status(tiered_barriers_are_necessary_for_competence_assurance, holdable).
narrative_ontology:cs_axiom_grounding('38b8e365-4246-4665-b24b-4a49bf0f440c', tiered_barriers_are_necessary_for_competence_assurance, instrumental).
narrative_ontology:cs_axiom('38b8e365-4246-4665-b24b-4a49bf0f440c', secondary, statutory_mandate_legitimizes_credential_hierarchy).
narrative_ontology:cs_axiom_status(statutory_mandate_legitimizes_credential_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('38b8e365-4246-4665-b24b-4a49bf0f440c', statutory_mandate_legitimizes_credential_hierarchy, conventional).
narrative_ontology:cs_reference_frame('38b8e365-4246-4665-b24b-4a49bf0f440c', progressive_era_professionalization_settlement).
narrative_ontology:cs_drift_state('38b8e365-4246-4665-b24b-4a49bf0f440c', contemporary_occupational_licensing_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38b8e365-4246-4665-b24b-4a49bf0f440c', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialing_education_institutions).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_credential_resources).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_competent_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_income_aspirants_to_licensed_occupations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, occupational_licensing_protects_public_safety).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, state_competence_assurance_justifies_entry_barriers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold valid licenses and benefit from restricted labor supply that elevates wages and reduces competition. Their credential investment is sunk; the licensing regime protects the return on that investment. They can move across state lines via reciprocity agreements or leverage their credential for mobility.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary,
    organized, biographical, arbitrage, national).

% Operate the accredited programs that gate entry to licensed occupations. They capture tuition revenue and often sit on advisory boards that shape curriculum and hour requirements. Their business model depends on the statutory mandate for their specific credentials.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialing_education_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, credentialing_education_institutions, agenda_setter).

% Administer the licensing statutes: set examination standards, approve curricula, enforce compliance, collect fees. Their institutional survival and budget depend on the licensing regime's continuation. They frame their role as public protection while controlling the pipeline.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_board_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Face statutory barriers they cannot afford to overcome: tuition, unpaid apprenticeship hours, examination fees, time away from earning. Many have relevant skills or experience but no recognized credential. The constraint sorts them into lower-wage, unlicensed work or informal economy with no pathway up.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers_without_credential_resources, payer,
    powerless, biographical, trapped, national).

% Possess demonstrated competence through experience, informal training, or foreign credentials that the licensing regime does not recognize. They are legally barred from practicing despite ability. Exit means leaving the occupation entirely or operating in legal gray zones with enforcement risk.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_competent_workers, payer,
    moderate, biographical, constrained, national).

% Would enter licensed occupations as a mobility pathway but cannot front the capital and time costs. The tiered structure (e.g., tiered licenses with escalating requirements) creates a staircase where each rung requires resources the previous rung's wages cannot provide.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_income_aspirants_to_licensed_occupations, payer,
    powerless, biographical, trapped, regional).

% Receive the purported safety benefit of minimum competence standards but pay inflated prices due to restricted supply. In many markets, the quality difference between licensed and unlicensed providers is negligible for routine services, making the price premium a transfer to incumbents.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, beneficiary,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, consumers_of_licensed_services, payer).

% Evaluate whether licensing statutes exceed the minimum necessary for public protection. They commission economic studies, review scope-of-practice restrictions, and occasionally challenge anticompetitive provisions — but lack authority to dismantle the statutory framework.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, competition_authorities, observer,
    institutional, generational, analytical, national).

% Hold credentials and experience from other jurisdictions that the licensing regime refuses to recognize fully. They would expand supply and lower prices if admitted. Their exclusion is maintained by board discretion and reciprocity barriers that protect domestic incumbents.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, foreign_trained_professionals, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a verifiable minimum competence floor for occupations where consumer harm from incompetence is severe and difficult for consumers to detect ex ante (e.g., surgery, electrical work, aviation).
% TRANSFER_FUNCTION: Moves economic rents from aspiring workers (who pay tuition, fees, forgone wages, and time) and consumers (who pay higher prices) to incumbent practitioners, education institutions, and licensing administrators. The tiered structure extracts at each rung.
% ABSENT_VOICES: Workers excluded by cost barriers (especially low-income, rural, and immigrant populations) are structurally absent from legislative hearings and board meetings where requirements are set. Foreign-trained professionals are excluded by non-recognition policies. Consumer advocacy groups are rarely resourced to counter organized practitioner lobbies.
% DISAPPEARANCE_RATIONALE: If licensing statutes vanished overnight, incumbent practitioners would lose protected rents, education institutions would lose mandated enrollment, and licensing boards would lose their mandate. Labor supply would expand rapidly in previously restricted occupations, prices would fall, and alternative quality-assurance mechanisms (certification, reputation markets, insurance requirements) would emerge — but consumer harm from genuine incompetence would also rise in high-stakes fields.
% FOUNDING_PROBLEM: Late 19th/early 20th century: unregulated practice in medicine, engineering, and trades led to demonstrable consumer harm (deaths, structural failures, fraud) with no reliable way for consumers to distinguish competent from incompetent providers.
% FOUNDING_PROBLEM_CORROBORATION: Historical records confirm genuine safety crises in medicine and engineering (Flexner Report era, boiler explosions, bridge collapses). However, labor economists (Kleiner, Morris) and competition authorities (FTC, state AG offices) document that the founding problem has been substantially solved for many occupations, and current requirement levels far exceed the minimum for safety — the regime has expanded into occupations with low harm potential (interior design, hair braiding, tour guiding). No independent body corroborates that the current tiered structure is necessary for the founding problem.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extraction is high (0.78) because the credential premium in wages and the tuition/fee revenue captured by institutions far exceed the cost of genuine competence verification. Suppression is very high (0.85) because the constraint's persistence depends on legally barring uncredentialed practice, refusing recognition of alternative competence signals, and lobbying against deregulation. Theater ratio is moderate (0.32): the safety function exists but a growing share of requirements (continuing education mandates, escalating hour requirements, specialty tiers) serve revenue and supply restriction rather than competence assurance. Accessibility collapse is high (0.82) because once the statutory framework is understood, alternatives (apprenticeship-only pathways, competency exams, foreign credential recognition) are legally foreclosed or practically inaccessible. Resistance is moderate (0.45): deregulation efforts exist but face concentrated incumbent opposition and diffuse consumer benefits.
 *
 * PERSPECTIVAL GAP:
 *   From the licensing board's seat, the constraint is a rope: genuine coordination solving a real information asymmetry. From the marginalized worker's seat, it is a snare: a barrier they cannot afford that protects incumbents. From the competent uncredentialed worker's seat, it is a tangled rope: they see the coordination function but experience it as extraction because their competence is ignored. The engine computes this divergence from the structural data — the declared beneficiaries/victims and exit options drive the per-seat effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents and education institutions are structural beneficiaries (d near 0.0): they collect rents and revenue directly from the constraint. Licensing administrators are agenda-setters with beneficiary characteristics (institutional budget depends on fees). Marginalized workers, uncredentialed competent workers, and low-income aspirants are structural targets (d near 1.0): they bear the full cost of barriers with trapped or constrained exit. Consumers sit near symmetric (d ~0.5): they receive some safety benefit but pay monopoly prices. Foreign-trained professionals are excluded (d ~1.0 but structurally outside the coordination function). Competition authorities are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer harm from undetectable incompetence in high-stakes fields) was real and partially solved. But the mandate has atrophied into a graduated extraction machine: tiered requirements now exist for occupations where harm is low and detectable (hair braiding, interior design, tour guiding), and requirement levels in core occupations exceed safety minimums. The mandate persists because the beneficiaries (incumbents, institutions, administrators) are concentrated and organized, while the victims are diffuse, powerless, and structurally excluded from the policy process. This is not a scaffold (no sunset, no transition logic) and not a piton (the function has not atrophied — extraction is active and growing). It is a snare with a genuine coordination core that has been captured and expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''graduated_access_filter'' reading a distinct constraint from the ''public_safety_coordination'' and ''rent_seeking_suppression'' readings, or are they measurement perspectives on one constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint via tiered barrier heights yields a different ε than measuring via average competence outcomes, they are distinct constraints. The tiered structure''s ε (0.78) differs substantially from the safety-outcome ε (likely <0.3 for core occupations).',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. The graduated_access_filter is a snare; public_safety_coordination may be a rope; rent_seeking_suppression may be a snare with different victim/beneficiary sets. Link via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate distinct constraints per epsilon-invariance.').

omega_variable(
    safety_outcome_vs_barrier_height_correlation,
    'Do higher credential tiers and stricter requirements actually correlate with better safety outcomes, or is the tiered structure decoupled from the coordination function?',
    'Empirical studies comparing safety outcomes across jurisdictions with different tier structures (e.g., states with/without master tiers, different hour requirements). If no correlation, the tiers are pure extraction.',
    'If decoupled, the theater_ratio is understated — the coordination cover is thinner than measured. The constraint would be more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_outcome_vs_barrier_height_correlation, empirical, 'Whether the tiered structure serves the coordination function or pure extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of uncredentialed workers primarily structural (legal barriers, non-recognition) or internalized (workers believe they need the credential, self-exclude from attempts)?',
    'Survey uncredentialed workers in licensed occupations: do they avoid entry because of legal risk, or because they believe the credential signals genuine competence they lack? Track entry attempts vs. deterrence.',
    'If substantially internalized, the constraint''s effective suppression is higher than the legal barriers alone suggest — the target carries the suppression with them. This would increase the omega-adjusted suppression measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded workers.').

omega_variable(
    tiered_structure_as_extraction_mechanism,
    'Does the tiered/graduated structure itself function as an extraction mechanism (each rung extracts fees, tuition, time) independent of the top-level licensing mandate?',
    'Analyze revenue flows at each tier: licensing fees, renewal fees, continuing education mandates, specialty certification fees. Compare to administrative costs. If each tier generates net revenue for administrators/institutions, the graduation is an extraction ladder.',
    'If the tiered structure is an independent extraction mechanism, the constraint is not one snare but a family of linked snares (one per tier), each with its own beneficiaries and victims. This would require decomposition per epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tiered_structure_as_extraction_mechanism, conceptual, 'Whether the graduation mechanism independently extracts beyond the base license.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t1900, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t1930, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t1960, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t1980, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1980, 0.27).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t2000, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_tr_t2025, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t1900, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t1930, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1930, 0.42).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t1960, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t1980, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1980, 0.63).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t2000, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_be_t2025, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t1900, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t1930, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t1960, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t1980, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t2000, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2000, 0.81).
narrative_ontology:measurement(licensing_statute_mandate__graduated_access_filter_su_t2025, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This constraint family (licensing_statute_mandate) decomposes the statutory licensing mandate into three structurally distinct readings: graduated_access_filter (this story, snare — tiered barriers sort by class), public_safety_coordination (rope/mountain — minimum competence for harm prevention), and rent_seeking_suppression (snare — supply restriction for incumbent rents). They share the same statutory text but have different ε, different victim/beneficiary structures, and different classifications. The epsilon-invariance principle requires separate stories because the tiered barrier measurement yields ε=0.78 while the safety-outcome measurement yields ε<0.3.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
