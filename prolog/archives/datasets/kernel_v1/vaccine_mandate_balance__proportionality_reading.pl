% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Threshold (Context-Dependent Legitimacy Reading)
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel:
 *   vaccine_mandate_balance. The kernel itself is a stabilized but ambiguous
 *   commitment: when (if ever) may the state mandate vaccination to achieve
 *   collective immunity? Three readings compete: bodily_autonomy_primary
 *   (never — individual consent is inviolable), proportionality_reading
 *   (conditionally — when disease severity, transmission risk, and vaccine
 *   safety meet strict thresholds), and public_health_primary (when
 *   collective protection supersedes individual consent and voluntary
 *   compliance fails). This constraint instantiates the
 *   proportionality_reading: mandates are legitimate only when they are
 *   proportional to the actual threat. The reading is context-dependent —
 *   mandate legitimacy for smallpox differs structurally from mandate
 *   legitimacy for seasonal influenza. The extractiveness value (0.52)
 *   reflects the constraint as it operates under high-severity pathogen
 *   conditions (R₀ >10, CFR >5%, vaccine safety established). Under
 *   low-severity conditions, extractiveness drops to rope-level or lower. The
 *   suppression value (0.48) reflects administrative and social barriers to
 *   exemption-seeking and non-compliance, which this reading constrains to
 *   cases where proportionality thresholds are met. The theater ratio (0.35)
 *   is relatively low because the proportionality reading explicitly rejects
 *   mandate theater — decisions are evidence-based on explicit thresholds,
 *   not bureaucratic ritual. This reading structures the mandate constraint
 *   as a tangled_rope: genuine coordination function (protecting vulnerable
 *   populations from lethal exposure) coexists with asymmetric extraction
 *   (hesitant individuals and exemption-seekers bear suppression costs).
 *
 * KEY AGENTS:
 *   - Vaccine-hesitant individuals: Primary victims when proportionality thresholds are met (trapped exit under high-severity pathogen; constrained exit under low-severity). Structural relationship depends entirely on disease parameters.
 *   - Medical exemption-seekers: Secondary victims with conditional exemption access. Face suppression via proof requirements and narrow criteria. Mixed coordinator-victim experience.
 *   - Vulnerable populations at lethal risk: Primary beneficiaries when proportionality thresholds are met. Benefit from herd immunity coordination; also bear extraction costs through exemption denial affecting collective immunity.
 *   - Public health authority: Institutional beneficiary with arbitrage exit. Experiences mandate as coordination mechanism (legitimate authority to protect public health) when proportionality holds; loses legitimacy when thresholds are unmet.
 *   - Legacy institutional infrastructure: Piton perspective — mandate requirements persist in employment/school forms even after threat recedes, maintained through institutional inertia rather than functional justification.
 *   - Analytical observer: Risks naturalizing the proportionality principle as immutable constitutional law rather than recognizing it as a foundational axiom of THIS reading's framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.52).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.48).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Threshold (Context-Dependent Legitimacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '713a71c1-f0e6-4a27-8155-89822d9491a3').
narrative_ontology:cs_kernel_codification('713a71c1-f0e6-4a27-8155-89822d9491a3', formalized).
narrative_ontology:cs_authority_grounding('713a71c1-f0e6-4a27-8155-89822d9491a3', lineage).
narrative_ontology:cs_interpretation_layer_present('713a71c1-f0e6-4a27-8155-89822d9491a3').
narrative_ontology:cs_reading_relation('713a71c1-f0e6-4a27-8155-89822d9491a3', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('713a71c1-f0e6-4a27-8155-89822d9491a3', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_axiom('713a71c1-f0e6-4a27-8155-89822d9491a3', foundational, proportionality_principle_required).
narrative_ontology:cs_axiom_status(proportionality_principle_required, holdable).
narrative_ontology:cs_axiom_grounding('713a71c1-f0e6-4a27-8155-89822d9491a3', proportionality_principle_required, deontological).
narrative_ontology:cs_axiom('713a71c1-f0e6-4a27-8155-89822d9491a3', foundational, empirical_thresholds_determinative).
narrative_ontology:cs_axiom_status(empirical_thresholds_determinative, holdable).
narrative_ontology:cs_axiom_grounding('713a71c1-f0e6-4a27-8155-89822d9491a3', empirical_thresholds_determinative, empirically_contingent).
narrative_ontology:cs_reference_frame('713a71c1-f0e6-4a27-8155-89822d9491a3', proportionality_constrained_mandate_authority).
narrative_ontology:cs_drift_state('713a71c1-f0e6-4a27-8155-89822d9491a3', contemporary_endemic_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('713a71c1-f0e6-4a27-8155-89822d9491a3', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations_at_lethal_risk).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authority_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, medical_exemption_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINE-HESITANT INDIVIDUAL / HIGH-SEVERITY PATHOGEN (SNARE) — When disease severity (smallpox-like R₀ >15, CFR >30%) and transmission risk justify mandate under this reading's proportionality frame, the hesitant individual faces trapped exit: medical exemption criteria are narrow, religious/philosophical exemptions may be foreclosed, and social/employment consequences enforce compliance. Experiences maximum extraction because the proportionality threshold has been met, legitimizing coercion.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE-HESITANT INDIVIDUAL / LOW-SEVERITY PATHOGEN (ROPE) — When disease severity (seasonal flu-like R₀ <2, CFR <0.1%) does NOT justify mandate under proportionality thresholds, the hesitant individual faces constrained but surmountable costs: voluntary vaccination recommendations, occupational risk disclosure, optional social restrictions. The constraint appears as coordination (information sharing, risk communication) rather than coercion. No warrant for suppression.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL EXEMPTION SEEKER / LEGITIMATE CONTRAINDICATION (TANGLED ROPE) — Agents with documented contraindications (immunocompromise, prior severe adverse reaction, rare genetic susceptibility) face mixed coordination and extraction. The mandate provides genuine coordination benefit (protects vulnerable through herd immunity) AND legitimate extraction via administrative barriers (proof requirements, time delays, narrowly-defined approval criteria). Experiences suppression through proof burdens while also benefiting from collective protection.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY / HIGH-SEVERITY PATHOGEN (ROPE) — When proportionality thresholds are met (severe pathogen, high transmission, vaccine safety established), the authority experiences the mandate as coordination: communicating risk, allocating vaccines, managing exemption documentation. Net beneficiary via legitimacy and capacity preservation. Effective extraction is negative — authority's power is enhanced, not reduced.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH AUTHORITY / PROPORTIONALITY GUARDRAILS (SCAFFOLD) — This reading itself is a sunset clause: mandates persist only when severity/transmission/safety thresholds remain met. As pathogen virulence declines (endemic drift), transmission drops (behavioral adaptation), or safety signals emerge, mandate legitimacy erodes and the constraint automatically contracts. Theater is minimal — the proportionality framework explicitly rejects mandate theater; decisions are evidence-based. Sunset is structural, not merely hoped-for.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: VULNERABLE POPULATION / LETHAL RISK (TANGLED ROPE) — Immunocompromised, elderly, and pre-vaccine-availability populations benefit from mandate-driven herd immunity (genuine coordination) AND experience extraction through mandate's collateral burden on medical exemption seekers (reduced herd immunity from denied exemptions, potential iatrogenic harm from inappropriate medical review). Mobile exit exists (migration, private risk management) but costly. Mixed experience reflects mandate's dual mechanism.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY MANDATE INFRASTRUCTURE / POST-THREAT DECAY (PITON) — Once a pathogen's threat recedes below proportionality thresholds, mandate infrastructure persists through institutional inertia: vaccination requirements remain on employment forms, school enrollment, travel documents despite no longer meeting justification criteria. Theater ratio is high — compliance appears necessary but the functional justification has evaporated. Mandate survives because dismantling requires institutional action; persistence is path-dependent, not functional.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PROPORTIONALITY AS NATURAL LIMIT (MOUNTAIN) — From civilizational scope, proportionality thresholds appear as immutable constraints on legitimate state authority: you cannot mandate intervention without proportional justification; the relationship between threat severity and response scale is a law-like feature of constitutional order, not a contingent policy choice. This reading risks naturalizing what is actually a foundational axiom of THIS reading's commitment system — the proportionality principle itself. Engine's false summit detector will flag this as normative naturalization.
constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_balance__proportionality_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, conditional on pathogen severity. The value reflects mandate conditions for high-severity pathogens (smallpox-equivalent). For low-severity pathogens, extractiveness drops to 0.25–0.30 (rope level). This is the defining feature of the proportionality reading: mandate extractiveness is not a fixed property but a function of disease parameters. When parameters change, the constraint's type changes. Suppression (0.48): Reflects administrative barriers to exemption (documentation requirements, proof timelines, narrowed approval criteria) plus social/employment consequences of non-compliance. This reading constrains suppression to cases where thresholds are met; unjustified suppression is itself a violation of proportionality. Theater ratio (0.35): Low because proportionality logic rejects performative mandate theater. Decisions are based on explicit threat assessment and safety data, not bureaucratic ritual. The theater that does exist (exemption documentation, risk communication processes) is functional rather than performative — it communicates thresholds and tracks compliance rationally. Measurement trajectory: Extractiveness and suppression rise from baseline to peak as enforcement mechanisms build (administrative barriers, employer/school requirements). Theater ratio rises slightly (communication and documentation overhead) but remains low. By time point 6, metrics plateau, indicating stabilized enforcement rather than continued ratcheting.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality reading generates a perspectival gap that is NOT present in the other siblings. The bodily_autonomy_primary reading sees mandates as always snare (categorical coercion). The public_health_primary reading sees mandates as always rope or tangled_rope (categorical coordination). But the proportionality_reading sees different types under different conditions: rope for low-severity pathogens (hesitant individuals face coordination without coercion), snare for high-severity pathogens (hesitant individuals face trapped exit), scaffold with sunset (mandate persists only while thresholds are met, then automatically contracts). The gap is not between observer positions but between pathogen parameters. This is the reading's structural signature: it makes mandate type a function of empirical facts (disease severity, transmission, safety), not abstract principles. This gap creates the constraint's distinctive analytic value — it explains why mandate legitimacy oscillates historically (justified for smallpox, debated for measles elimination, largely rejected for seasonal flu).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's structural position relative to THIS specific constraint. For vaccine-hesitant individuals: when proportionality thresholds are met, they are full targets of extraction (d→1.0, f(d)→1.40); when thresholds are unmet, they are near-symmetric (d→0.5, f(d)→0.65). For public health authority: they are beneficiaries with high arbitrage capacity (d→0.05, f(d)→-0.12). For vulnerable populations: they are mixed (benefit from herd immunity but extract cost via exemption denial for others) (d→0.50, f(d)→0.65). The engine derives d automatically from beneficiary/victim declarations and exit options; no override is needed because the structural data is clean. The key insight: d is NOT fixed across pathogen contexts — the same agent has different d values under different disease parameters. This is captured through multiple perspectives representing different pathogen conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint avoids mandatrophy because it explicitly embeds asymmetry resolution. The proportionality reading contains a sunset clause built into its logic: mandates persist only while thresholds remain met. As pathogen virulence declines (endemic drift), transmission falls (behavioral adaptation), or vaccine safety signals emerge, mandate legitimacy erodes structurally. This is not a hope that mandates will be repealed; it is a logical entailment of the reading's own framework. The constraint thus avoids the trap of indefinite extraction justified by past threats. Mandatrophy was historically real for smallpox mandates (continued 20+ years post-eradication in some jurisdictions), but the proportionality reading contains the mechanism to detect and correct this: once CFR→0 and endemic status achieved, the threshold no longer holds, and mandate legitimacy collapses. The piton perspective (legacy infrastructure) acknowledges that institutional inertia may sustain mandates post-threshold-failure, but the reading itself treats this as a violation of its own logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_calibration,
    'What CFR, R₀, and vaccine safety margins constitute ''strict proportionality'' — where does smallpox-level severity end and seasonal-flu-level severity begin?',
    'Meta-analysis of historical mandate justifications (smallpox eradication, polio campaigns, measles elimination) and failed justifications (seasonal influenza, varicella); consensus-building on numerical thresholds across epidemiology, ethics, constitutional law communities',
    'If thresholds are sharp (CFR >5%, R₀ >8): most real-world pathogens fall below, mandates are rare, bodily_autonomy_primary reading gains structural ground. If thresholds are diffuse (CFR >0.1%, R₀ >2): most respiratory pathogens qualify, proportionality_reading and public_health_primary converge, mandate grounds broaden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_calibration, empirical, 'Numerical calibration of proportionality thresholds for mandate legitimacy').

omega_variable(
    exemption_robustness_definition,
    'What makes an exemption ''robust'' versus performative — how much burden can proof requirements, administrative delay, and narrowed criteria impose before exemption becomes illusory?',
    'Comparative analysis of exemption frameworks (strict medical-only vs. broad philosophical); tracking of granted vs. denied rates; post-exemption health outcomes for grantees; case law on constitutionality of exemption criteria',
    'If exemptions require minimal documentation and swift approval: proportionality reading acknowledges robust exit, suppression is low. If exemptions face high documentation burden and slow processing: suppression approaches snare-level coercion, reading risks devolving into extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_definition, empirical, 'Operational definition and measurement of exemption robustness').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the proportionality_reading''s core logic (conditional mandate legitimacy) logically foreclose the bodily_autonomy_primary reading (unconditional mandate illegitimacy), or do they coexist as distinct normative frameworks?',
    'Conceptual analysis: if a framework can simultaneously hold (a) ''proportionality makes mandates legitimate when thresholds are met'' AND (b) ''no proportionality justifies overriding individual consent as a matter of principle,'' then they coexist. If (a) entails the negation of (b)''s core premise, then foreclosure holds.',
    'If coexist: both readings are live positions in constitutional debate; constraint families should be modeled as separate. If foreclose: one reading''s victory implies the other''s loss; the kernel contest becomes zero-sum. Current assessment: coexist (proportionality reading does not claim unconditional mandate legitimacy; bodily_autonomy_primary does not deny proportional hazards exist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Logical relationship between proportionality_reading and bodily_autonomy_primary axioms').

omega_variable(
    vaccine_safety_signal_emergence,
    'If novel safety signals emerge (serious adverse event frequency >1 in 100,000), does the proportionality threshold shift or does mandate legitimacy collapse entirely?',
    'Risk-benefit calculation under different safety profiles; testing whether the reading''s logic accommodates ''mandate no longer proportional'' as a live conclusion or treats safety signals as immaterial to the threshold',
    'If threshold shifts: proportionality reading is responsive, mandates persist under revised justification. If legitimacy collapses: vaccine_mandate_balance constraint dissolves, victims escape, extractiveness drops to near-zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vaccine_safety_signal_emergence, empirical, 'Sensitivity of proportionality thresholds to vaccine safety signals').

omega_variable(
    temporal_scope_of_mandate,
    'Does this reading''s proportionality logic apply only to emergency declarations (crisis-driven temporary mandates) or to endemic/endemic phases (routine maintenance of immunity thresholds)?',
    'Historical analysis of mandate sustainability post-emergency (smallpox continued even after endemic phase; seasonal flu mandates are discussed but rare); clarification of whether proportionality thresholds are fixed or phase-dependent',
    'If emergency-only: mandate automatically contracts as crisis resolves, piton perspective is temporary. If endemic-phase applicable: mandates persist indefinitely as long as vulnerability thresholds remain met, piton perspective becomes stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_of_mandate, conceptual, 'Temporal scope of proportionality mandate logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmp_theater_initial, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vmp_theater_mid, vaccine_mandate_balance__proportionality_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(vmp_theater_stable, vaccine_mandate_balance__proportionality_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(vmp_extractiveness_baseline, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vmp_extractiveness_mid_enforcement, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(vmp_extractiveness_peak_mandate, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vmp_suppression_early, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vmp_suppression_enforcement_buildup, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(vmp_suppression_stable, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, medical_exemption_access).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_safety_monitoring_infrastructure).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel is realized as three separate constraint stories, one per reading. Each reading has a different ε value (bodily_autonomy_primary likely ≈0.70 snare; proportionality_reading ≈0.52 tangled_rope context-dependent; public_health_primary likely ≈0.30 rope). The ε-invariance principle applies: the three readings are structurally distinct constraints, not measurements of the same constraint. They are linked via network.affects_constraints to show the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
