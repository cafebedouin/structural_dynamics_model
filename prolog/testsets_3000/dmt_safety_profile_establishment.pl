% ============================================================================
% CONSTRAINT STORY: dmt_safety_profile_establishment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dmt_safety_profile_establishment, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dmt_safety_profile_establishment
 *   human_readable: DMT Safety Profile Establishment in Drug Regulatory Classification
 *   domain: pharmacology/drug_policy/neuroscience
 *
 * SUMMARY:
 *   The DMT safety profile establishment creates a structural constraint
 *   spanning pharmacology, drug policy, neuroscience research, and patient
 *   medical autonomy. DMT (dimethyltryptamine) is simultaneously an
 *   endogenous human neurochemical, a naturally occurring compound in plant
 *   medicine traditions, and a Schedule I controlled substance with severe
 *   research restrictions. The constraint operates at the intersection of
 *   legitimate safety coordination (clinical trial standards, toxicology
 *   assessment) and institutional power preservation (regulatory gatekeeping,
 *   market protection for approved alternatives). This generates multiple
 *   genuine classifications from different structural positions. The original
 *   1971 Controlled Substances Act scheduling predates modern neuroscience
 *   findings on DMT's mechanism, receptor pharmacology, and comparative
 *   toxicity profile. The intervening 55+ years have produced peer-reviewed
 *   research on endogenous DMT synthesis, cerebral distribution, and
 *   neuroprotective potential — yet the regulatory classification has not
 *   updated proportionally. This gap between current scientific knowledge and
 *   regulatory framework creates the constraint's hybrid nature: it genuinely
 *   coordinates some safety functions while extracting from patient access,
 *   research capacity, and medical autonomy through mechanisms unrelated to
 *   demonstrated safety risk.
 *
 * KEY AGENTS:
 *   - Patient populations with treatment-resistant conditions: Primary victim (powerless/trapped) — cannot access investigational therapeutic pathways; bears medical burden without exit option
 *   - Independent neuroscience researchers: Primary victim (powerless/trapped) — face prohibitive licensing barriers unrelated to actual safety risk; constrained from mechanistic inquiry into endogenous neurotransmitter systems
 *   - Pharmaceutical corporate investors: Primary beneficiary (institutional/arbitrage) — benefit from Schedule I protection of market position for approved alternatives; benefit from investment risk management via restricted competition
 *   - Regulatory institutions (DEA/FDA): Primary beneficiary (institutional/arbitrage) — derive institutional legitimacy, budgetary continuity, and authority structure from scheduling system; coordinate their own power through the constraint
 *   - Clinical research institutions: Secondary actor (moderate/constrained) — genuinely coordinate patient safety through IRBs but also bear regulatory overhead that creates capital barriers favoring large institutions
 *   - Psychedelic research renaissance coalition: Organized agent (organized/mobile) — building alternative capacity through breakthrough designations and institutional coordination; has exit capacity via jurisdictional arbitrage
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional scheduling as inherent pharmacological principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dmt_safety_profile_establishment, 0.58).
domain_priors:suppression_score(dmt_safety_profile_establishment, 0.68).
domain_priors:theater_ratio(dmt_safety_profile_establishment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dmt_safety_profile_establishment, extractiveness, 0.58).
narrative_ontology:constraint_metric(dmt_safety_profile_establishment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dmt_safety_profile_establishment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dmt_safety_profile_establishment, tangled_rope).
narrative_ontology:human_readable(dmt_safety_profile_establishment, "DMT Safety Profile Establishment in Drug Regulatory Classification").
narrative_ontology:topic_domain(dmt_safety_profile_establishment, "pharmacology/drug_policy/neuroscience").

domain_priors:requires_active_enforcement(dmt_safety_profile_establishment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dmt_safety_profile_establishment, pharmaceutical_industry_investors).
narrative_ontology:constraint_beneficiary(dmt_safety_profile_establishment, regulatory_institutional_power).
narrative_ontology:constraint_victim(dmt_safety_profile_establishment, patient_access_to_research).
narrative_ontology:constraint_victim(dmt_safety_profile_establishment, independent_neuroscience_research).
narrative_ontology:constraint_victim(dmt_safety_profile_establishment, clinical_investigation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT ACCESS (SNARE) — Patients with treatment-resistant conditions have no legal exit from constraint. Cannot access investigational therapeutic pathways despite potential medical benefit. Bears full cost: loss of medical options, disease burden continuation, no voice in research prioritization. Trapped by legal classification and DEA scheduling. Maximum experienced extraction.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT RESEARCH (SNARE) — Academic neuroscientists studying endogenous psychoactive compounds face prohibitive licensing barriers, federal permits, and institutional review complexity unrelated to actual safety profile. Cannot conduct mechanistic research without DEA compliance overhead. Trapped by regulatory infrastructure. Bears cost of constrained inquiry without corresponding safety benefit.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CLINICAL RESEARCH INSTITUTIONS (TANGLED ROPE) — Universities and hospitals genuinely coordinate patient protection through IRB review and safety protocols — this coordination function is real. But the constraint also extracts: regulatory overhead creates barriers to entry favoring large-capital institutions, creates funding bottlenecks, and creates time delays on potentially beneficial research. Some agency but significant asymmetric cost.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE PHARMACEUTICAL SECTOR (ROPE) — Institutional investors benefit from Schedule I constraints that protect market position of approved alternatives and create patent-licensing arbitrage opportunities. The safety profile establishment functions as coordination: it coordinates capital toward approved drug pathways and coordinates investment risk management. Net beneficiary with exit via relabeling or alternative product development. Experiences constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY INSTITUTIONS (ROPE) — DEA and FDA derive institutional legitimacy and budgetary continuity from scheduling system. The safety profile constraint coordinates their authority structure and resource allocation. From inside the institution, the constraint appears as pure coordination: it enables them to manage drug policy in a structured manner. Institutional power experiences the constraint as a coordination platform for their own functions.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SCHEDULING LEGACY (PITON) — The 1971 Controlled Substances Act scheduling for DMT was based on pre-neuroscience pharmacology, cultural panic narratives, and minimal safety data. The original functional justification has largely atrophied: modern neuroscience has advanced beyond the 1971 knowledge base; the theater ratio has risen as the original rationale is no longer scientifically current. The system persists through institutional inertia and path dependency rather than functional safety coordination. Theater ratio 0.65 reflects that much of the regulatory activity is now maintenance of historical classification rather than novel safety assessment.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: RESEARCH COALITION (TANGLED ROPE) — Organized research community (Johns Hopkins, NYU, UCSF programs) has coordinated around breakthrough therapy designations and compassionate-use pathways, building alternative institutional capacity. This coalition experiences genuine mixed coordination (IRBs, safety protocols coordinate patient protection and research ethics) alongside extraction (regulatory overhead creates capital barriers, timeline delays, institutional gatekeeping). Mobile exit capacity — coalition can shift to jurisdictions with lower overhead or build alternative pathways. Moderate experienced extraction.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely toxicological lens, some safety assessment is intrinsic to compound introduction — this is a structural feature of how pharmacology works. The constraint could naturalize as a mountain: 'Safety profile establishment is inherent to drug regulation.' But structural data reveals this as false summit. The current constraint is not about safety assessment; it is about legal scheduling and institutional power preservation. True safety assessment would adapt to updated neuroscience; this constraint does not.
constraint_indexing:constraint_classification(dmt_safety_profile_establishment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dmt_safety_profile_establishment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dmt_safety_profile_establishment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dmt_safety_profile_establishment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dmt_safety_profile_establishment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dmt_safety_profile_establishment, TR),
    TR >= 0.70.

:- end_tests(dmt_safety_profile_establishment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from powerless agents (patients, independent researchers) through legal restriction mechanisms unrelated to demonstrated safety superiority. However, extraction is not maximal (0.80+) because genuine safety coordination functions do exist (IRB review, clinical protocols, toxicology assessment). The extraction flows toward institutional beneficiaries (pharma investors, regulatory power) through market protection and authority preservation. Base extractiveness has increased over the 30-year interval (0.35 → 0.58) as neuroscience has advanced beyond the constraint's original justification without updating the constraint itself. Suppression (0.68): High. Multiple suppression mechanisms operate in concert: legal prohibition, federal licensing requirements, institutional inertia, patient knowledge constraints (lack of accessible research literature), international treaty coordination. The suppression is multilayered and not easily bypassed. However, suppression is not total (0.85+) — breakthrough designations, international variance (some jurisdictions have rescheduled or decriminalized), and underground research networks create exit pathways for determined actors. Theater ratio (0.65): Moderate-high. The regulatory activity around DMT safety profiles involves substantial performative elements: scheduling reviews that reaffirm existing decisions without updating to current science; IRB processes that apply general pharmaceutical caution to a compound with different risk profile; grant denial practices that cite legal status rather than scientific grounds. However, some genuine functional activity persists: toxicology assessment, clinical protocol development, patient safety monitoring in breakthrough trials. The theater ratio's increase (0.40 → 0.65) reflects that the original coordination function (safety assessment) has atrophied while the institutional ritual (scheduling maintenance) has become the primary function.
 *
 * PERSPECTIVAL GAP:
 *   The snare vs rope polarity is sharpest between patients/researchers and pharma/regulatory institutions. Patients cannot perceive the coordination benefits because the constraint prevents them from accessing the coordination mechanism (research programs, therapeutic exploration). Pharma and regulatory institutions perceive only coordination and authority preservation because they benefit from the constraint's existence. The tangled rope perspectives (clinical institutions, research coalition) experience both: they participate in genuine safety coordination while also bearing costs from regulatory overhead and institutional gatekeeping. The piton perspective reveals that the constraint's original function (safety assessment proportional to actual risk) has been replaced by institutional ritual (scheduling maintenance independent of updated science). The mountain perspective risks naturalizing this institutional arrangement as inherent to pharmacology — a false summit that obscures the contingent institutional origins.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: beneficiaries have low d (constraint extracts from others toward them); victims have high d (constraint extracts from them). Powerless trapped agents experience maximum d (vulnerable, no exit). Institutional beneficiaries experience low d (benefit from constraint, arbitrage exits available). The pharma investor's d derives from beneficiary status + arbitrage exit → low d → low/negative χ (experiences constraint as beneficial coordination). The patient's d derives from victim status + trapped exit → high d → high χ (experiences constraint as extractive maximum coercion). Regulatory institutions' d derives from beneficiary status + arbitrage exit → low d → institutional power preservation. Clinical institutions' d derives from mixed victim (regulatory overhead) and beneficiary (safety coordination) roles + constrained exit → moderate d → moderate χ. The research coalition's d derives from victim status (research constraints) + mobile exit (breakthrough designations, jurisdictional arbitrage) → lower moderate d. The directional flow is clear: extraction moves from powerless/constrained agents toward institutional beneficiaries, mediated through legal mechanisms and market protection.
 *
 * MANDATROPHY ANALYSIS:
 *   Resolving mandatrophy requires identifying which perspectives are measuring the same constraint vs different constraints. The core structural question: Is the DMT safety profile constraint primarily a safety coordination mechanism or primarily an institutional power preservation mechanism? The answer determines which classifications are correct vs which represent capture. Evidence: (1) If the constraint functioned as pure safety coordination (rope), updating to current neuroscience would be expected. The absence of such updating despite 55+ years of research suggests institutional inertia. (2) If the constraint functioned as pure safety coordination, Schedule I status would be unique to compounds with demonstrated higher risk than Schedule II (opioids, amphetamines). DMT's comparative toxicity profile does not justify differential scheduling, suggesting the constraint functions to protect market position rather than coordinate safety. (3) The theater ratio increase (0.40 → 0.65) indicates that performative institutional activity is replacing functional safety assessment. (4) The extractiveness increase (0.35 → 0.58) indicates that the constraint is progressively becoming less about safety and more about institutional power. Conclusion: The constraint is primarily institutional power preservation disguised as safety coordination — it is tangled rope with strong snare characteristics, not rope. The rope classifications from beneficiary perspectives represent capture: the beneficiaries experience the constraint as beneficial, but their perception reflects their position within an extractive system, not the system's actual function. The mountain classification from the analytical observer is a false summit: naturalizing a contingent institutional arrangement as inherent to pharmacology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_current_safety_data,
    'How much of the current safety profile restriction reflects 1971 pharmacological knowledge vs modern neuroscience findings?',
    'Systematic comparison of original scheduling rationale against current peer-reviewed neurotoxicity literature, cellular mechanism studies, and longitudinal user safety epidemiology',
    'If historical knowledge gap > 70%: constraint is primarily institutional inertia (piton/snare hybrid). If gap < 30%: constraint retains genuine safety coordination function (rope/tangled rope). Gap determines whether classification is false summit or legitimate coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vs_current_safety_data, empirical, 'Proportion of current restriction based on outdated vs current safety data').

omega_variable(
    endogenous_compound_precedent,
    'Does DMT''s status as endogenous to human neurochemistry establish structural precedent for different regulatory category than synthetic drugs?',
    'Comparison of regulatory treatment: endogenous compounds (melatonin, GABA, dopamine analogues) vs synthetic scheduled compounds; systematic review of regulatory frameworks in jurisdictions with alternative scheduling categories',
    'If endogenous compounds treated differently: DMT schedule reflects institutional path dependency not pharmacological principle (snare/piton). If treated identically: institutional uniformity has rationale (tangled rope). Determines whether constraint reflects genuine safety principle or institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_compound_precedent, empirical, 'Whether endogenous compound status justifies alternative regulatory category').

omega_variable(
    breakthrough_therapy_capacity,
    'Do breakthrough therapy designations and compassionate use pathways genuinely reduce the extraction experienced by patient and research populations, or do they provide theater without material capacity expansion?',
    'Longitudinal tracking of breakthrough designation timelines, IRB approval rates, patient access numbers; comparison of actual research capacity before/after designation vs regulatory overhead maintained',
    'If pathways substantively expand capacity: constraint is transitioning toward scaffold with real sunset (organized coalition perspective confirmed). If pathways are theater: extraction persists and constraint remains snare/tangled rope despite procedural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breakthrough_therapy_capacity, empirical, 'Whether breakthrough pathways substantively expand research access or remain procedural theater').

omega_variable(
    institutional_capture_depth,
    'To what extent do pharmaceutical industry incentives actively maintain DMT scheduling to protect market position of approved alternatives vs passive institutional inertia?',
    'Historical document analysis of lobbying records, patent filings, and pharmaceutical industry positioning on psychedelic rescheduling; comparison of industry posture across compounds where Schedule I status protects profitability vs compounds where it does not',
    'If active capture > passive inertia: constraint is strategically maintained snare/tangled rope (beneficiary actively enforces extraction). If passive inertia dominates: constraint is piton (institutional theater with atrophied function). Determines whether extraction mechanism is active enforcement or path-dependent momentum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_depth, empirical, 'Proportion of scheduling maintenance driven by active industry capture vs institutional inertia').

omega_variable(
    cognitive_liberty_framework,
    'Does the constraint violate autonomy principles by preventing informed choice of non-addictive, non-lethal compounds for research and therapeutic exploration?',
    'Normative analysis of cognitive liberty frameworks; comparison of constraint to other pharmaceutical autonomy precedents (off-label use, patient choice in terminal illness); empirical documentation of user populations'' self-reported autonomy loss',
    'If autonomy violation is primary: constraint classification shifts toward snare (extraction from powerless agent''s decision-making capacity). If autonomy is secondary: constraint remains tangled rope (mixed coordination and extraction). Determines preference-based vs empirical classification gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_liberty_framework, preference, 'Whether constraint violates cognitive liberty and personal autonomy principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dmt_safety_profile_establishment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmt_safety_tr_t0, dmt_safety_profile_establishment, theater_ratio, 0, 0.4).
narrative_ontology:measurement(dmt_safety_tr_t15, dmt_safety_profile_establishment, theater_ratio, 15, 0.58).
narrative_ontology:measurement(dmt_safety_tr_t30, dmt_safety_profile_establishment, theater_ratio, 30, 0.65).
narrative_ontology:measurement(dmt_safety_tr_t8, dmt_safety_profile_establishment, theater_ratio, 8, 0.5).

% Extraction over time
narrative_ontology:measurement(dmt_safety_be_t0, dmt_safety_profile_establishment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dmt_safety_be_t15, dmt_safety_profile_establishment, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dmt_safety_be_t30, dmt_safety_profile_establishment, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(dmt_safety_be_t8, dmt_safety_profile_establishment, base_extractiveness, 8, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dmt_safety_profile_establishment, enforcement_mechanism).
narrative_ontology:affects_constraint(dmt_safety_profile_establishment, psychedelic_research_access).
narrative_ontology:affects_constraint(dmt_safety_profile_establishment, treatment_resistant_condition_therapeutics).
narrative_ontology:affects_constraint(dmt_safety_profile_establishment, endogenous_neurochemistry_research).

% DUAL FORMULATION NOTE:
% The DMT safety profile constraint decomposes into multiple structurally distinct mechanisms: (1) legitimate toxicology and safety assessment (genuine coordination) with baseline extractiveness ~0.15-0.25, and (2) institutional scheduling maintenance and market protection (pure institutional power) with extractiveness ~0.60-0.75. The tangled rope classification at ε=0.58 represents the empirical blend; decomposition would show separate stories for safety_coordination (lower ε, rope) vs power_preservation (higher ε, snare). The current story models the empirical constraint as experienced by agents, which combines both mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dmt_safety_profile_establishment, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
