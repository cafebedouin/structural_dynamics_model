% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Framework for Public Health Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the proportionality reading of the
 *   coercion legitimacy boundary kernel. The reading holds that state
 *   coercion for medical intervention (primarily vaccination mandates) is
 *   legitimate only when disease severity and transmission dynamics cross a
 *   threshold that makes the collective harm prevention sufficiently weighty
 *   to outweigh individual autonomy. Measles (high R0 ~12-18, high
 *   complication rate, effective vaccine) clears this threshold; seasonal
 *   influenza (lower R0 ~1.3, lower severity in general population, variable
 *   vaccine efficacy) does not. The constraint operates through case-by-case
 *   adjudication — courts and legislatures assess each disease-vaccine pair
 *   against the proportionality standard. This creates a genuine coordination
 *   function (preventing outbreaks of severe diseases) combined with
 *   asymmetric extraction (some individuals bear the burden of mandates for
 *   collective benefit), maintained by active enforcement (school exclusion,
 *   fines, employment conditions). The victim set varies by pathogen: for
 *   measles mandates, objecting parents and religious minorities are the
 *   primary payers; for diseases below the threshold, no one is compelled, so
 *   the victim set is empty. The moderate ε (0.45) reflects the extraction
 *   from compelled individuals balanced against the real coordination
 *   benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.65).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Framework for Public Health Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '72086c6c-8361-4457-ae91-6dacbe345d4d').
narrative_ontology:cs_kernel_codification('72086c6c-8361-4457-ae91-6dacbe345d4d', formalized).
narrative_ontology:cs_authority_grounding('72086c6c-8361-4457-ae91-6dacbe345d4d', lineage).
narrative_ontology:cs_interpretation_layer_present('72086c6c-8361-4457-ae91-6dacbe345d4d').
narrative_ontology:cs_reading_relation('72086c6c-8361-4457-ae91-6dacbe345d4d', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('72086c6c-8361-4457-ae91-6dacbe345d4d', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('72086c6c-8361-4457-ae91-6dacbe345d4d', foundational, proportionality_as_constitutional_principle).
narrative_ontology:cs_axiom_status(proportionality_as_constitutional_principle, holdable).
narrative_ontology:cs_axiom_grounding('72086c6c-8361-4457-ae91-6dacbe345d4d', proportionality_as_constitutional_principle, conventional).
narrative_ontology:cs_axiom('72086c6c-8361-4457-ae91-6dacbe345d4d', foundational, least_restrictive_means_requirement).
narrative_ontology:cs_axiom_status(least_restrictive_means_requirement, holdable).
narrative_ontology:cs_axiom_grounding('72086c6c-8361-4457-ae91-6dacbe345d4d', least_restrictive_means_requirement, conventional).
narrative_ontology:cs_reference_frame('72086c6c-8361-4457-ae91-6dacbe345d4d', jacobson_proportionality_framework).
narrative_ontology:cs_drift_state('72086c6c-8361-4457-ae91-6dacbe345d4d', post_covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('72086c6c-8361-4457-ae91-6dacbe345d4d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, compelled_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, religious_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, parents_of_minors).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, police_power_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the proportionality framework: define the threshold criteria, evaluate each disease-vaccine pair, issue mandates, and enforce compliance. They gain institutional legitimacy and operational authority from the framework. Their exit is arbitrage-grade — they can shift to other public health tools (persuasion, incentives, surveillance) if mandates become politically costly.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Immunocompromised individuals, infants too young for vaccination, elderly with waning immunity. They gain herd immunity protection from mandates on high-transmission diseases without bearing the mandate burden themselves. Their exit is constrained — they cannot individually opt out of disease exposure risk, but they benefit from the collective shield mandates create.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Hospital systems, public health departments, emergency services. They gain surge capacity protection and reduced outbreak response costs when mandates prevent large epidemics. They can adapt to different mandate regimes (mobile exit) but prefer the stability of the proportionality framework.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, healthcare_infrastructure, beneficiary,
    organized, generational, mobile, national).

% Adults and parents facing mandates for themselves or their children for diseases above the proportionality threshold (measles, polio, potentially COVID-19). They bear the physical intrusion, autonomy loss, risk of adverse events (rare), and penalty risk for non-compliance. Exit options: home schooling, relocation to exemption-friendly jurisdictions, medical exemptions (narrow), or compliance — all costly.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, compelled_individuals, payer,
    moderate, biographical, constrained, national).

% Individuals and communities whose sincere religious beliefs prohibit vaccination. When mandates override religious exemptions (as in several states post-2019), they face the most intense coercion: loss of community, educational exclusion, employment barriers. Exit requires abandoning religious identity and community — identity_locked. They are also partially excluded from the proportionality adjudication, which treats religious objection as a factor to be weighed rather than a trump.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, religious_objectors, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, religious_objectors, excluded).

% Parents making vaccination decisions for children subject to school-entry mandates. They bear decisional burden, potential conflict with their own risk assessment, and penalties for non-compliance (school exclusion). Exit options: home schooling, private schools with different policies, relocation, medical exemptions — all costly and disruptive to family life.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, parents_of_minors, payer,
    moderate, biographical, constrained, national).

% Organizations and individuals who hold that medical intervention without consent is categorically impermissible. They are structurally excluded from the proportionality framework — their position is treated as outside the legitimate range of debate within the framework. They cannot exit the constraint's reach (trapped) but can advocate for its replacement.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, generational, trapped, national).

% Actors who hold that the state may compel any vaccine with positive net benefit (the public_health_primary reading). They are partially excluded from the proportionality adjudication — their preferred broader mandate authority is constrained by the proportionality threshold. They have arbitrage exit through legislative and executive channels to expand mandate scope.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_maximalists, excluded,
    institutional, generational, arbitrage, national).

% Judges adjudicating mandate challenges, constitutional scholars, bioethicists. They observe and shape the proportionality boundary through rulings and analysis. Their analytical exit means they can evaluate the framework from outside its coercive force.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, courts_and_scholars, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled framework for distinguishing diseases that justify state coercion from those that do not, preventing both unrestricted police power over bodies and paralysis in the face of severe epidemics. Solves the collective action problem of achieving herd immunity for high-threat diseases while preserving individual autonomy for lower-threat diseases.
% TRANSFER_FUNCTION: Moves decision-making authority from individual to state for diseases above the severity/transmission threshold (measles, polio, smallpox historically, potentially COVID-19); preserves individual authority for diseases below the threshold (seasonal flu, HPV in most jurisdictions, hepatitis B for adults). The transfer is calibrated by R0, severity, vaccine efficacy, and availability of less restrictive alternatives.
% ABSENT_VOICES: Those who would reject any state authority over medical decisions (bodily_autonomy_primary reading) — they are excluded from the proportionality adjudication which presupposes that some coercion can be legitimate. Also absent: those who would mandate all recommended vaccines (public_health_primary reading) — they are constrained by the proportionality threshold. Children subject to mandates have no direct voice; their interests are represented by parents (who may object) and the state (which mandates).
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, the coercion legitimacy boundary would collapse to one of the two absolutist poles: either all vaccine mandates become illegitimate (bodily_autonomy_primary victory) or the state claims authority to mandate any vaccine with net benefit (public_health_primary victory). Current mandate patterns (measles yes, flu no) would destabilize. Legislative and judicial battles would erupt in every jurisdiction. The world would rearrange around a new boundary.
% FOUNDING_PROBLEM: How to justify state coercion for vaccination without either (a) adopting a principle that permits unlimited medical mandates whenever authorities claim collective benefit, or (b) adopting a principle that makes mandatory vaccination impossible even for diseases like smallpox or measles that kill millions without herd immunity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by constitutional jurisprudence (Jacobson v. Massachusetts 1905 establishing police power with limits; Zucht v. King 1922 upholding school mandates; recent COVID mandate cases), by public health historians documenting the evolution from compulsory smallpox vaccination to modern school-entry requirements, and by bioethicists across the spectrum (Childress et al. on public health ethics, Gostin on public health law). No single partisan or beneficiary group owns this genealogy — it is a live contested problem in constitutional law and public health ethics.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint does extract compliance from objectors for high-threshold diseases, but the extraction is bounded by the proportionality principle and does not extend to all vaccines. Suppression is moderate-high (0.65) because mandates carry legal penalties and exclusion consequences, but exemptions exist (medical, sometimes religious/philosophical) and the threshold limits scope. Theater ratio is low-moderate (0.25) — the proportionality framework does genuine analytical work in distinguishing diseases, but the COVID-19 period saw performative expansion of mandate logic to diseases that may not have cleared the traditional threshold. Accessibility collapse is moderate (0.55) — alternatives (home schooling, relocation, exemption claims) exist but are costly. Resistance is moderate (0.55) — sustained legal and political contestation exists but has not overturned the core framework. The measurement series shows rising extractiveness and theater during the COVID-19 period (2020 peak) with partial reversion by 2025, and suppression peaking in the same period.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities, legislatures, courts) experiences this as genuine coordination with bounded extraction — the proportionality test is a real analytical filter. The compelled individual seats experience it as extraction with a coordination cover story — the case-by-case adjudication feels like a ritual that usually produces mandates for the diseases authorities want to mandate anyway. The vulnerable population seat experiences it as coordination with minimal personal cost. The engine will compute these divergences from the structural power/exit/scope data: authorities have institutional power + arbitrage exit + national scope; compelled individuals have moderate/powerless power + identity_locked/constrained exit + national scope; vulnerable populations have powerless/moderate power + constrained exit + national scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are structural beneficiaries (d near 0.0) — they gain legitimacy, institutional power, and operational capacity from the mandate authority. Vulnerable populations are beneficiaries (d ~ 0.1-0.2) — they gain herd immunity protection without bearing mandate burdens. Compelled individuals are targets (d ~ 0.7-0.9) — they bear the physical intrusion, autonomy loss, and penalty risk. Religious objectors are more intensely targeted (d ~ 0.8-0.95) when their objections are overridden; their exit options are identity_locked (exit requires abandoning religious community/identity). Parents of minors are targets (d ~ 0.6-0.8) with constrained exit (can home school or relocate but at high cost). Healthcare infrastructure is a beneficiary (d ~ 0.15) — gains surge capacity protection. Rival frameworks (bodily_autonomy_primary, public_health_primary) are excluded from the proportionality adjudication — they would reject the case-by-case structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing collective disease prevention with individual liberty without collapsing into either absolutism) remains live and contested. The proportionality framework prevents mislabeling pure public health authoritarianism (public_health_primary) as coordination, and prevents mislabeling genuine outbreak prevention (measles mandates) as pure extraction. However, the COVID-19 period tested whether the framework's adjudicative structure had become a ritual that legitimates whatever mandates authorities prefer — the theater spike in 2020 suggests mandatrophy risk. The constraint is not resolved mandatrophy because the founding problem persists and the framework still does real discriminative work (flu mandates remain rare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the proportionality reading a distinct constraint from the kernel''s other readings, or a parameterization of a single constraint?',
    'Apply the epsilon-invariance test: if measuring coercion legitimacy under measles vs flu parameters yields structurally different extraction profiles (different victim sets, different enforcement intensity), they are separate constraints. The proportionality reading instantiates a case-by-case adjudication structure with moderate ε; the public_health_primary reading would instantiate a broad police-power structure with higher ε; the bodily_autonomy_primary reading would instantiate a near-zero-coercion structure with near-zero ε.',
    'If separate constraints, each gets its own story with its own ε, stakeholders, and classification. If one constraint, the proportionality reading must absorb the full variance into a single ε, which would misrepresent the structural differences between mandating MMR vs recommending flu vaccine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s three readings are three constraints or one parameterized constraint').

omega_variable(
    severity_threshold_location,
    'Where exactly does the proportionality boundary fall between diseases that justify mandates and those that do not?',
    'Empirical survey of actual mandate patterns across jurisdictions and time, correlated with R0, IFR, transmission mode, and vaccine characteristics. Legal analysis of court decisions drawing the line.',
    'If the boundary is sharp (measles yes, flu no, little contested middle), the constraint operates as a clear rule with low theater. If the boundary is a wide contested zone (COVID-19, pertussis, HPV), the constraint operates as an adjudicative process with higher theater and extraction from the adjudication itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_location, empirical, 'Precision of the disease-severity threshold that triggers coercion legitimacy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by compelled individuals primarily structural (legal penalties, school exclusion, employment consequences) or internalized (moral injury, identity conflict, community alienation)?',
    'Post-mandate longitudinal studies tracking psychological and social outcomes for objectors who comply vs. those who resist. Comparison of suppression persistence after mandate removal.',
    'If substantially internalized, the effective suppression is higher than legal penalties alone indicate — the constraint continues extracting compliance through identity channels after the formal mechanism lifts. This would increase measured suppression and affect classification toward snare for objector seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for vaccine objectors').

omega_variable(
    adjudication_extraction,
    'Does the case-by-case adjudication process itself constitute an extraction mechanism (legal costs, uncertainty, administrative burden) that falls disproportionately on the less powerful?',
    'Analysis of litigation patterns, administrative costs, and compliance burdens across socioeconomic groups facing mandate challenges.',
    'If adjudication extracts disproportionately from powerless agents, the constraint''s effective extraction is higher for those seats than the base ε suggests, pushing their classification toward snare. This would also create intra-class divergence among compelled_individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adjudication_extraction, empirical, 'Whether proportional adjudication is itself an extractive process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_pr_tr_t1905, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1905, 0.15).
narrative_ontology:measurement(clb_pr_tr_t1925, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1925, 0.18).
narrative_ontology:measurement(clb_pr_tr_t1955, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(clb_pr_tr_t1980, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(clb_pr_tr_t2000, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(clb_pr_tr_t2020, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(clb_pr_tr_t2025, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(clb_pr_be_t1905, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1905, 0.35).
narrative_ontology:measurement(clb_pr_be_t1925, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1925, 0.4).
narrative_ontology:measurement(clb_pr_be_t1955, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement(clb_pr_be_t1980, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(clb_pr_be_t2000, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(clb_pr_be_t2020, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(clb_pr_be_t2025, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(clb_pr_su_t1905, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1905, 0.7).
narrative_ontology:measurement(clb_pr_su_t1925, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement(clb_pr_su_t1955, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(clb_pr_su_t1980, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(clb_pr_su_t2000, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clb_pr_su_t2020, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(clb_pr_su_t2025, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__proportionality_reading, 0.1).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, school_vaccine_mandates).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, healthcare_worker_vaccine_mandates).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, travel_vaccine_requirements).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'coercion legitimacy boundary' kernel into three structurally distinct constraints with different ε, victim sets, and enforcement profiles. The proportionality reading sits between the two absolutist readings, instantiating a case-by-case adjudication structure that generates moderate extraction from its selective application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, moderate, 0.85).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
