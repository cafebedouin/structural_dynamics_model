% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccine Mandate Authority â Public Health Protective Reading
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the public_health_primary reading of the
 *   mandate_legitimacy_scope kernel: the claim that state authority to compel
 *   vaccination is legitimate when necessary to protect vulnerable
 *   populations from serious harm. Under this reading, immunocompromised and
 *   medically fragile individuals enter the beneficiary set because they
 *   cannot achieve protection through personal vaccination alone, while
 *   unvaccinated individuals bear the duty and cost of compelled medical
 *   intervention. The constraint has a genuine coordination function (herd
 *   immunity protecting the vulnerable) but asymmetrically extracts bodily
 *   autonomy from the compelled population. It is actively enforced through
 *   school entry requirements, employment mandates, and exclusion from public
 *   accommodations. The temporal measurements capture the COVID-19 pandemic
 *   cycle: pre-pandemic baseline, enforcement spike during 2021â2022, and
 *   partial post-mandate rollback.
 *
 * KEY AGENTS:
 *   - state_public_health_authority: Agenda-setter (institutional/arbitrage) â administers and enforces mandate policy
 *   - vulnerable_populations: Primary beneficiary (powerless/constrained) â receives protection via herd immunity but cannot exit the population
 *   - unvaccinated_individuals: Primary payer (moderate/constrained) â bears cost of compelled vaccination and associated penalties
 *   - public_health_legal_scholars: Observer (institutional/analytical) â maps constitutional boundaries of police power
 *   - independent_epidemiologists: Observer (organized/analytical) â confirms disease externality without adjudicating coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.72).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccine Mandate Authority â Public Health Protective Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'c6a2e2b3-a7b2-4bb1-b402-536ab624711d').
narrative_ontology:cs_kernel_codification('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', formalized).
narrative_ontology:cs_authority_grounding('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', lineage).
narrative_ontology:cs_interpretation_layer_present('c6a2e2b3-a7b2-4bb1-b402-536ab624711d').
narrative_ontology:cs_reading_relation('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', foundational, protective_police_power_overrides_bodily_integrity).
narrative_ontology:cs_axiom_status(protective_police_power_overrides_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', protective_police_power_overrides_bodily_integrity, conventional).
narrative_ontology:cs_axiom('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', foundational, vulnerable_hold_positive_claim_to_communal_protection).
narrative_ontology:cs_axiom_status(vulnerable_hold_positive_claim_to_communal_protection, holdable).
narrative_ontology:cs_axiom_grounding('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', vulnerable_hold_positive_claim_to_communal_protection, deontological).
narrative_ontology:cs_reference_frame('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', constitutional_police_power_tradition).
narrative_ontology:cs_drift_state('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', post_pandemic_legal_backlash, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c6a2e2b3-a7b2-4bb1-b402-536ab624711d', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces vaccination mandates through police power, setting eligibility criteria, exemption thresholds, and penalty structures. Justifies compulsion as necessary to protect those who cannot be vaccinated. Can expand or contract mandate scope by policy or regulation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Immunocompromised, elderly, and medically fragile individuals who cannot mount adequate immune responses to vaccines. Depend on high community vaccination coverage for protection from communicable disease. Cannot practically exit the population or the mandate regime.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Required to undergo unwanted vaccination or face exclusion from schools, workplaces, and public accommodations. Bear the direct cost of bodily autonomy infringement, medical risk perception, and social sanction. Legal exit is costly; physical exit from the jurisdiction is disruptive.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Map the constitutional boundaries of state police power versus individual rights. Publish analysis of Jacobson lineage, proportionality tests, and human-rights limitations. Do not collect from or pay into the mandate structure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_legal_scholars, observer,
    institutional, generational, analytical, national).

% Document population-level disease transmission and vaccine-effectiveness data. Corroborate the externality problem (unvaccinated populations increase risk to vulnerable) without adjudicating the legitimacy of the coercive remedy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, independent_epidemiologists, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of herd immunity for communicable diseases where individual vaccination decisions fail to protect those who cannot be vaccinated, by achieving population-level coverage that interrupts transmission to vulnerable groups.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making from unvaccinated individuals to the state, and shifts disease-risk burden from vulnerable populations to the compliant general population.
% ABSENT_VOICES: Libertarian constitutional scholars who reject police-power expansion beyond traditional limits; religious objectors whose exemption claims are narrowed or denied; vaccine-hesitant communities excluded from policy deliberation; future citizens who may face precedent-expanded compulsory medical interventions beyond vaccination.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (state authority to compel vaccination were delegitimized), vulnerable populations would lose structural herd-immunity protection; disease incidence among immunocompromised would rise; the unvaccinated would regain full bodily autonomy; and the legal framework of police power would contract, forcing reliance on voluntary coordination or private exclusion.
% FOUNDING_PROBLEM: Communicable diseases create externalities where individual vaccination decisions fail to protect those who cannot be vaccinated, leading to preventable outbreaks and serious harm to vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists (observer seat) attest to the population-level transmission externality. Disability-rights advocates (organized seat, overlapping with beneficiaries but speaking from advocacy rather than benefiting position) corroborate ongoing vulnerability. Contestation is over the coercive remedy, not the existence of the disease risk itself.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) reflects the severe bodily intrusion of compelled medical intervention. Suppression (0.78) is high because the constraint's persistence depends on active legal enforcement and the structural displacement of non-compliant behavior; alternatives (pure voluntarism) are sharply narrowed during mandate periods. Theater ratio (0.35) captures the politicized performance of compliance (vaccine passports, public signage) that partially decoupled from epidemiological necessity as the pandemic progressed. Accessibility collapse (0.60) indicates that once mandates are enacted, non-compliance routes narrow sharply (exemptions restricted, social participation gated). Resistance (0.72) is high due to sustained legal challenges, political mobilization, and non-compliance movements. The temporal series shows an enforcement spike: metrics were measured at the post-rollback interval end, but the peak at T=3 (2022) represents maximum extraction during peak mandate enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable_populations seat and the unvaccinated_individuals seat experience opposite directionalities despite occupying the same national scope. The beneficiary seat sees the constraint as life-sustaining coordination; the payer seat experiences it as state extraction of bodily decision-making. The state seat perceives itself as solving a collective-action problem; the analytical seats see both functions simultaneously. The engine computes this divergence from structural data without requiring a single 'true' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vulnerable_populations) receive protective externalities; their derived directionality is toward the beneficiary pole. Victims (unvaccinated_individuals) bear direct compulsion and penalty; their directionality is toward the target pole. The state_public_health_authority is agenda_setter with arbitrage exit (can modify policy), placing it near the beneficiary/administrator pole. The high extraction and suppression scores are experienced primarily by the unvaccinated, while the coordination benefit accrues to the vulnerable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling because it carries both genuine coordination (herd immunity protecting those who cannot vaccinate) and asymmetric extraction (compelled medical intervention on the unvaccinated). Claiming it as a pure Rope would ignore the bodily autonomy cost; claiming it as a pure Snare would ignore the real protective function for immunocompromised populations. The Tangled Rope classification is structurally required by the simultaneous presence of beneficiaries, victims, and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_blocking_efficacy,
    'Do the vaccines subject to mandate actually block transmission sufficiently to protect vulnerable populations via herd immunity, or do they primarily reduce individual severity?',
    'Population-level studies measuring infection rates in immunocompromised contacts under varying community vaccination coverage; if infection rates do not covary with coverage, the coordination function is weakened.',
    'If vaccines do not block transmission, the constraint''s coordination story collapses toward pure extraction; if they do, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_blocking_efficacy, empirical, 'Whether mandated vaccines provide the population-level protective externality claimed').

omega_variable(
    less_restrictive_alternatives_available,
    'Are less restrictive alternatives (targeted shielding, regular testing, prophylactic treatments) capable of achieving equivalent protection for vulnerable populations without compelled vaccination?',
    'Comparative effectiveness studies and natural experiments in jurisdictions that relied on non-coercive measures.',
    'If less restrictive alternatives are equivalently effective, the mandate''s asymmetry is harder to justify as coordination and slides toward snare; if no such alternatives exist, the coordination function is indispensable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(less_restrictive_alternatives_available, empirical, 'Availability of non-coercive protective alternatives').

omega_variable(
    kernel_reading_boundary,
    'Does the public_health_primary reading foreclose bodily_autonomy_primary logically, or can both be held as defeasible principles within a single proportionality framework?',
    'Jurisprudential analysis of whether constitutional frameworks treat public health police power as subject to absolute bodily autonomy limits or merely as balancing factors.',
    'If foreclosable, the constraint family is structurally disjoint; if co-holdable, the readings are better modeled as a single constraint with a proportionality gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between public health and bodily autonomy readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t1, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1, 0.15).
narrative_ontology:measurement(mand_tr_t2, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2, 0.32).
narrative_ontology:measurement(mand_tr_t3, mandate_legitimacy_scope__public_health_primary, theater_ratio, 3, 0.42).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__public_health_primary, theater_ratio, 4, 0.36).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__public_health_primary, theater_ratio, 5, 0.32).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t1, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(mand_be_t2, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2, 0.78).
narrative_ontology:measurement(mand_be_t3, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 3, 0.85).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 4, 0.76).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 5, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mand_su_t1, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1, 0.35).
narrative_ontology:measurement(mand_su_t2, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2, 0.82).
narrative_ontology:measurement(mand_su_t3, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 3, 0.9).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 4, 0.78).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 5, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the mandate_legitimacy_scope kernel, which decomposes into three structurally distinct constraints: public_health_primary (state protective authority), bodily_autonomy_primary (inviolable bodily integrity), and proportionality_reading (context-dependent balancing). Each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
