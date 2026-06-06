% ============================================================================
% CONSTRAINT STORY: regulatory_adequacy_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_adequacy_gap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_adequacy_gap
 *   human_readable: Regulatory Adequacy Gap in UK Genomic Data Governance
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The UK regulatory framework for genomic data governance contains
 *   structural ambiguities that create a gap between legal requirements and
 *   technological reality. UK GDPR and Data Protection Act 2018 define
 *   'personal data' and 'special category data' but lack clear thresholds for
 *   when genomic/phenotype data crosses these boundaries. Consent frameworks
 *   inherited from pre-genomic medical research permit broad consent for
 *   'future research' without specifying AI training, commercial licensing,
 *   or cross-border data sharing as distinct use cases. This gap enables
 *   healthcare AI developers and genomic research institutions to access
 *   large datasets under permissive interpretations, but extracts control and
 *   privacy from data subjects who cannot meaningfully consent to uses they
 *   cannot foresee. The constraint is claimed as scaffold because regulatory
 *   clarification is actively underway: the 2023 Data Protection and Digital
 *   Information Bill includes provisions for research data processing, ICO is
 *   developing genomic data guidance (expected 2025-2026), and NHS England's
 *   Federated Data Platform governance framework is building granular consent
 *   models. The sunset logic is explicit — the gap is transitional, not
 *   steady-state. However, the measurements show rising theater_ratio (0.35 →
 *   0.58) and modest extraction increase (0.28 → 0.35), indicating that
 *   performative compliance (ethics board approvals, broad consent forms) is
 *   growing faster than substantive protection, and the gap may be
 *   transitioning from ambiguous extraction to formalized extraction rather
 *   than genuine resolution.
 *
 * KEY AGENTS:
 *   - Genomic Data Subjects: Primary victim (powerless/trapped) — cannot exit genomic data ecosystem once sequenced; faces irreversible identifiability risk and unconsented secondary use
 *   - Patient Advocacy Groups: Secondary victim (moderate/constrained) — constrained by resource and access barriers but benefit from research ecosystem; mixed coordination-extraction experience
 *   - Healthcare AI Developers: Primary beneficiary (institutional/arbitrage) — benefit from regulatory flexibility enabling data access under broad consent; net beneficiary of the gap
 *   - Genomic Research Institutions: Mixed beneficiary-victim (institutional/constrained) — benefit from permissive data access but bear compliance risk and reputational cost from consent interpretation challenges
 *   - ICO and DHSC Policy Team: Institutional actor (institutional/mobile) — sees gap as temporary and is actively building sunset mechanisms through policy development
 *   - NHS Digital Infrastructure: Beneficiary (institutional/arbitrage) — benefits from data aggregation enabled by permissive interpretations; Federated Data Platform depends on broad data access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_adequacy_gap, 0.35).
domain_priors:suppression_score(regulatory_adequacy_gap, 0.42).
domain_priors:theater_ratio(regulatory_adequacy_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_adequacy_gap, extractiveness, 0.35).
narrative_ontology:constraint_metric(regulatory_adequacy_gap, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(regulatory_adequacy_gap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_adequacy_gap, scaffold).
narrative_ontology:human_readable(regulatory_adequacy_gap, "Regulatory Adequacy Gap in UK Genomic Data Governance").
narrative_ontology:topic_domain(regulatory_adequacy_gap, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(regulatory_adequacy_gap).
narrative_ontology:has_sunset_clause(regulatory_adequacy_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_adequacy_gap, healthcare_ai_developers).
narrative_ontology:constraint_beneficiary(regulatory_adequacy_gap, genomic_research_institutions).
narrative_ontology:constraint_beneficiary(regulatory_adequacy_gap, nhs_digital_infrastructure).
narrative_ontology:constraint_victim(regulatory_adequacy_gap, genomic_data_subjects).
narrative_ontology:constraint_victim(regulatory_adequacy_gap, patient_advocacy_groups).
narrative_ontology:constraint_vindicates(regulatory_adequacy_gap, innovation_requires_regulatory_flexibility).
narrative_ontology:constraint_vindicates(regulatory_adequacy_gap, consent_model_sufficiency_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENOMIC DATA SUBJECT (SNARE) — Cannot exit the genomic data ecosystem once sequenced; faces irreversible identifiability risk. Ambiguous legal definitions mean consent given for one purpose (clinical diagnosis) is routinely interpreted to permit secondary uses (AI training, commercial licensing) without re-consent. The regulatory gap extracts control and privacy with no structural exit — genomic data is permanent and uniquely identifying.
constraint_indexing:constraint_classification(regulatory_adequacy_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT ADVOCACY GROUP (TANGLED ROPE) — Constrained by resource limitations and institutional access barriers, but benefits from the genomic research ecosystem through improved diagnostics and treatment options. The regulatory ambiguity both enables research that helps patients and permits extraction through unconsented secondary use. Mixed coordination (research access) and extraction (loss of control over data use).
constraint_indexing:constraint_classification(regulatory_adequacy_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE AI DEVELOPER (ROPE) — Benefits from regulatory flexibility that permits data access under broad consent interpretations. The ambiguity solves a genuine coordination problem: enabling AI development for clinical benefit requires large genomic datasets, and overly restrictive consent would fragment data access. Net beneficiary — the gap enables business model and research pipeline.
constraint_indexing:constraint_classification(regulatory_adequacy_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ICO/DHSC POLICY TEAM (SCAFFOLD) — Sees the regulatory gap as temporary coordination failure with explicit sunset logic. The 2023 Data Protection and Digital Information Bill, NHS England's Federated Data Platform governance framework, and ICO's forthcoming genomic data guidance (expected 2025-2026) are building granular consent models and clear identifiability thresholds. The gap is transitional — regulatory clarification is the mandate, not steady-state ambiguity.
constraint_indexing:constraint_classification(regulatory_adequacy_gap, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: GENOMIC RESEARCH INSTITUTION (TANGLED ROPE) — Benefits from permissive data access under current ambiguity but also bears compliance risk and reputational cost when consent interpretations are challenged. The regulatory gap both enables research (coordination) and creates legal uncertainty that constrains institutional behavior (extraction through compliance overhead and litigation risk). Mixed beneficiary-victim position.
constraint_indexing:constraint_classification(regulatory_adequacy_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational perspective, the regulatory gap is a transitional mismatch between technological capability (AIGHP, genomic sequencing) and legal infrastructure (consent models designed for pre-genomic era). The gap is being actively closed through policy development, not maintained as steady-state extraction. Sunset mechanisms are real: DPDI Bill, ICO guidance pipeline, NHS governance frameworks. The constraint is temporary support for innovation during regulatory catch-up.
constraint_indexing:constraint_classification(regulatory_adequacy_gap, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_adequacy_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_adequacy_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_adequacy_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(regulatory_adequacy_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The regulatory gap extracts control and privacy from data subjects through unconsented secondary use, but extraction is not as severe as pure snare because some coordination function exists (enabling beneficial research) and sunset mechanisms are real. The value reflects that the gap both enables legitimate research and permits extractive commercial uses, with the balance tilted toward extraction. Suppression (0.42): Moderate. Data subjects face significant barriers to meaningful consent (complexity of genomic data uses, inability to foresee AI applications, no granular control mechanisms) and cannot exit once sequenced (genomic data is permanent and uniquely identifying). But suppression is not total — some advocacy groups can challenge interpretations, and regulatory development is responsive to patient concerns. Theater ratio (0.58): Moderate-high. Much of the current compliance activity is performative: ethics board approvals that cannot assess AI-specific risks, broad consent forms that do not specify secondary uses, anonymization claims that ignore re-identification risk. The theater has increased over the interval (0.35 → 0.58) as genomic data uses have outpaced consent model granularity, and performative compliance has substituted for substantive protection. The rising theater_ratio is the key signal that the scaffold may be degrading — if the sunset produces formalized permissive interpretations rather than genuine granular consent, the gap transitions from temporary coordination failure to permanent extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same regulatory ambiguity appears as different constraint types depending on structural position. Genomic data subjects see pure extraction (Snare) — they are trapped in the genomic data ecosystem with no exit and no control over secondary uses. Patient advocacy groups see mixed coordination-extraction (Tangled Rope) — the gap both enables beneficial research and permits unconsented commercial use. Healthcare AI developers see coordination (Rope) — the ambiguity solves the legitimate problem of enabling AI development for clinical benefit. ICO/DHSC policy team and the analytical observer see temporary support (Scaffold) — the gap is transitional, with explicit sunset mechanisms through policy development. Genomic research institutions see mixed coordination-extraction (Tangled Rope) — they benefit from data access but bear compliance risk. The perspectival gap reveals that 'regulatory adequacy' is not a single structural fact but a position-dependent classification: the gap is simultaneously enabling coordination (for developers), extracting control (from data subjects), and being actively closed (by regulators). The key uncertainty is whether the sunset is real (genuine granular consent) or illusory (formalized permissive interpretations that naturalize extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Genomic data subjects are victims with trapped exit options — they experience maximum extraction because they cannot exit the genomic data ecosystem and have no control over secondary uses once data is collected. The engine derives high d (near 1.0) from victim status + trapped exit, producing high effective extraction. Patient advocacy groups are victims with constrained exit — they experience moderate extraction because they have some agency (can challenge interpretations, influence policy) but face resource barriers. Healthcare AI developers and NHS digital infrastructure are beneficiaries with arbitrage exit — they experience low or negative effective extraction because they benefit from the gap and can exit to more permissive jurisdictions if UK rules tighten. Genomic research institutions are mixed beneficiary-victim with constrained exit — they benefit from data access but bear compliance risk, producing moderate directionality. ICO/DHSC policy team are institutional actors with mobile exit (can shift policy frameworks) — they experience low extraction because they have agency to close the gap. The perspectival gap is structural: beneficiaries see coordination (enabling research), victims see extraction (loss of control), and the analytical observer sees a transitional scaffold with real but uncertain sunset.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification depends on the reality of the sunset mechanisms. If ICO guidance, DPDI Bill provisions, and NHS governance frameworks produce genuine granular consent models that restore data subject control, the gap is temporary coordination failure and scaffold is accurate. If policy development is captured by industry interests and produces formalized permissive interpretations that naturalize broad consent, the gap transitions from ambiguous extraction to permanent extraction and the scaffold degrades to tangled_rope or snare. The rising theater_ratio (0.35 → 0.58) is a warning signal: performative compliance is growing faster than substantive protection, suggesting the sunset may produce formalized theater rather than genuine resolution. The omega variables document the irreducible uncertainties: identifiability thresholds are technically ambiguous, consent granularity may create fatigue without improving control, regulatory development may be captured, and secondary uses may be extractive rather than necessary. The mandatrophy is resolved by recognizing that scaffold is a time-indexed claim: the constraint is scaffold NOW (2024-2025) because sunset mechanisms are being built, but may degrade to tangled_rope or snare by 2027-2028 if those mechanisms formalize extraction rather than close the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identifiability_threshold_ambiguity,
    'What technical threshold distinguishes ''identifiable'' from ''anonymized'' genomic data when re-identification risk is probabilistic rather than binary?',
    'ICO guidance on genomic data identifiability; case law establishing re-identification risk thresholds; technical standards for genomic anonymization (e.g., k-anonymity for genomic variants)',
    'If threshold is strict (low re-identification probability required): most genomic data becomes special category, requiring explicit consent for all uses. If threshold is permissive (high re-identification probability tolerated): current broad consent interpretations remain valid, gap persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identifiability_threshold_ambiguity, empirical, 'Technical threshold for genomic data identifiability under UK GDPR').

omega_variable(
    consent_granularity_sufficiency,
    'Does granular consent (purpose-specific, use-case-specific) genuinely protect data subject autonomy, or does it create consent fatigue that reduces meaningful control?',
    'Behavioral studies of consent comprehension and decision quality under granular vs. broad consent models; longitudinal tracking of consent withdrawal rates; comparison of data subject satisfaction across consent frameworks',
    'If granular consent improves autonomy: scaffold sunset is justified — regulatory clarification toward granularity resolves the gap. If granular consent creates fatigue without improving control: the scaffold logic fails, and the gap may be structurally irresolvable through consent alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_granularity_sufficiency, empirical, 'Whether granular consent models improve data subject autonomy').

omega_variable(
    regulatory_capture_risk,
    'Is the regulatory gap being closed through genuine patient-protective policy, or through industry-influenced frameworks that formalize permissive interpretations?',
    'Analysis of DPDI Bill amendments and ICO guidance drafting process; identification of industry vs. patient advocacy influence on final policy; comparison of UK frameworks to GDPR-compliant EU member states with stricter genomic data rules',
    'If policy development is captured: the ''sunset'' is illusory — the gap transitions from ambiguous extraction to formalized extraction. If policy development is patient-protective: scaffold classification is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_risk, preference, 'Whether regulatory clarification process is captured by industry interests').

omega_variable(
    secondary_use_necessity,
    'Are secondary uses of genomic data (AI training, commercial licensing) genuinely necessary for clinical benefit, or are they extractive uses justified post-hoc by innovation rhetoric?',
    'Counterfactual analysis: clinical outcomes in jurisdictions with stricter consent requirements; identification of AI models trained on consented-only vs. broad-consent datasets; assessment of whether commercial genomic AI products deliver clinical value proportional to data extraction',
    'If secondary uses are necessary: the coordination function is real, and scaffold/rope classifications are justified. If secondary uses are extractive: the coordination story is cover, and snare classification is accurate from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_use_necessity, empirical, 'Whether secondary genomic data uses are necessary for clinical benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_adequacy_gap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reg_gap_theater_2018, regulatory_adequacy_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(reg_gap_theater_2020, regulatory_adequacy_gap, theater_ratio, 2, 0.42).
narrative_ontology:measurement(reg_gap_theater_2022, regulatory_adequacy_gap, theater_ratio, 4, 0.51).
narrative_ontology:measurement(reg_gap_theater_2024, regulatory_adequacy_gap, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(reg_gap_extract_2018, regulatory_adequacy_gap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(reg_gap_extract_2020, regulatory_adequacy_gap, base_extractiveness, 2, 0.31).
narrative_ontology:measurement(reg_gap_extract_2022, regulatory_adequacy_gap, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(reg_gap_extract_2024, regulatory_adequacy_gap, base_extractiveness, 6, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(reg_gap_suppress_2018, regulatory_adequacy_gap, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(reg_gap_suppress_2021, regulatory_adequacy_gap, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(reg_gap_suppress_2024, regulatory_adequacy_gap, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_adequacy_gap, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_adequacy_gap, aighp_deployment_consent_gap).
narrative_ontology:affects_constraint(regulatory_adequacy_gap, nhs_federated_data_platform_governance).

% DUAL FORMULATION NOTE:
% The regulatory adequacy gap is downstream of the data consent paradox (upstream constraint: consent models designed for pre-genomic era cannot handle AI-specific uses) but represents a distinct structural constraint. The upstream constraint has its own extractiveness reflecting the conceptual inadequacy of consent frameworks; the regulatory adequacy gap has its own extractiveness reflecting the legal ambiguity and enforcement vacuum that permits unconsented secondary use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
