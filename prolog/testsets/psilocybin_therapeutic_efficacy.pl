% ============================================================================
% CONSTRAINT STORY: psilocybin_therapeutic_efficacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_psilocybin_therapeutic_efficacy, []).

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
 *   constraint_id: psilocybin_therapeutic_efficacy
 *   human_readable: Psilocybin Therapeutic Efficacy Claims and Evidence Bottleneck
 *   domain: psychiatry/neuroscience/regulatory_governance
 *
 * SUMMARY:
 *   Psilocybin's Schedule I designation creates a constraint that
 *   simultaneously gates access to a potentially therapeutic compound and
 *   generates competing institutional interests that maintain the gate. The
 *   constraint exhibits classic tangled rope structure: genuine safety
 *   verification coordination coexists with asymmetric extraction of benefits
 *   toward gatekeeping institutions and pharmaceutical incumbents, while
 *   harms fall on treatment-seeking populations. The constraint's theater
 *   ratio (0.68) reflects that enforcement of Schedule I for psilocybin
 *   increasingly appears performative — the scientific and safety evidence
 *   supporting rescheduling has accumulated significantly over the past
 *   decade, yet the legal status remains static. This gap between evidence
 *   and designation indicates institutional inertia (piton layer) overlaid on
 *   regulatory capture mechanisms (institutional perspective extraction). The
 *   constraint shows measurable degradation over time: theater ratio has
 *   increased as the gap between evidence and policy widens, and
 *   extractiveness has grown as clinical demand for psilocybin treatment
 *   exceeds supply while gatekeepers accumulate approval authority and market
 *   protection for competing pharmaceutical interests.
 *
 * KEY AGENTS:
 *   - Treatment Seekers: Primary victims (powerless/trapped) — lack access to potentially effective treatment due to Schedule I designation; cannot exit through legal alternatives for many conditions
 *   - Clinical Researchers: Mixed agents (moderate/constrained) — benefit from legitimacy and career advancement via rare approvals but constrained by DEA licensing and FDA trial barriers
 *   - Pharmaceutical Companies: Primary beneficiaries (institutional/arbitrage) — benefit from patent exclusivity and market protection from psilocybin competition; can arbitrage to other compounds or geographies
 *   - Psychiatric Professional Community: Organized actors (organized/constrained) — receive genuine coordination benefit from evidence standards but extracted from by delayed adoption of potentially superior treatments
 *   - Regulatory Pathway Coalition: Organized agents (organized/constrained) — building exit pathways (breakthrough designation, expanded access, compassionate use) with visible sunset logic
 *   - War on Drugs Institutional Legacy: Institutional inertia (institutional/arbitrage) — maintains Schedule I enforcement through budgetary and jurisdictional commitments despite evidence misalignment (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory choice as pharmacological fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(psilocybin_therapeutic_efficacy, 0.58).
domain_priors:suppression_score(psilocybin_therapeutic_efficacy, 0.65).
domain_priors:theater_ratio(psilocybin_therapeutic_efficacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(psilocybin_therapeutic_efficacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(psilocybin_therapeutic_efficacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(psilocybin_therapeutic_efficacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(psilocybin_therapeutic_efficacy, tangled_rope).
narrative_ontology:human_readable(psilocybin_therapeutic_efficacy, "Psilocybin Therapeutic Efficacy Claims and Evidence Bottleneck").
narrative_ontology:topic_domain(psilocybin_therapeutic_efficacy, "psychiatry/neuroscience/regulatory_governance").

domain_priors:requires_active_enforcement(psilocybin_therapeutic_efficacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(psilocybin_therapeutic_efficacy, therapy_research_institutions).
narrative_ontology:constraint_beneficiary(psilocybin_therapeutic_efficacy, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(psilocybin_therapeutic_efficacy, regulatory_pathway_gatekeepers).
narrative_ontology:constraint_victim(psilocybin_therapeutic_efficacy, treatment_seekers_with_access_barriers).
narrative_ontology:constraint_victim(psilocybin_therapeutic_efficacy, field_empirical_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESPERATE PATIENT (SNARE) — Trapped by FDA Schedule I designation and therapeutic scarcity despite compelling evidence of efficacy for treatment-resistant depression. Cannot access treatment; bears full cost of bureaucratic gatekeeping. No alternative therapies available for some conditions. Maximum experienced extraction — the patient has structural mobility (could theoretically emigrate or participate in underground therapy) but exit is prohibitively costly.
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLINICAL RESEARCHER (TANGLED ROPE) — Constrained by DEA licensing requirements, FDA trial protocols, and funding scarcity, but also benefits from the legitimacy that the regulatory system provides to their research. Genuine coordination of safety verification exists alongside asymmetric extraction (researcher careers are advanced by the restriction while patients are harmed). Significant agency but real costs.
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL COMPANY (ROPE) — Benefits from patent exclusivity and regulatory pathway clarity enabled by Schedule I status. Experiences constraint as coordination: the regulatory bottleneck prevents competitors from accessing the therapeutic space until compounds are approved, protecting first-mover advantage. Net beneficiary with arbitrage options (can choose other compounds, geographies, timelines).
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PSYCHIATRIC COMMUNITY (TANGLED ROPE) — Organized through professional societies (APA, neuropsychiatry associations). Experiences genuine coordination problem: evidence-based treatment guidelines require robust efficacy data, and the regulatory system enforces data standards. But also extracted from: the constraint delays adoption of potentially effective treatments and forces profession-wide compliance with restrictive licensing that may exceed actual safety needs.
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY PATHWAY COALITION (SCAFFOLD) — Organized agents (FDA, breakthrough designation programs, expanded access protocols) are building pathways to bypass full Schedule I restrictions through compassionate use, Section 505(b)(2) expedited pathways, and psychedelic medicine track-and-trace programs. These represent genuine coordination solutions with sunset logic: as psilocybin moves through clinical trials, the restrictive schedule will be modified or terminated. Low effective extraction because exit pathways are visible and expanding.
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WAR ON DRUGS INSTITUTIONAL LEGACY (PITON) — Schedule I designation for psilocybin is largely performative institutional inertia from 1970 Controlled Substances Act. The original categorization was based on recreational abuse potential and political momentum, not on pharmacological evidence. Enforcement persists through budgetary and jurisdictional commitments despite evidence that the schedule is misaligned with actual therapeutic potential. Theater ratio high because the constraint maintains ritual criminalization of a substance whose harm profile is lower than legal alternatives (alcohol, tobacco).
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHARMACOLOGICAL NATURALISM VIEW (MOUNTAIN) — From a civilizational/universal perspective, psilocybin's schedule status reflects immutable biological realities: it is a Schedule I compound, full stop. This perspective naturalizes the legal designation as a constraint embedded in law of nature. However, the structural data reveals this as false naturalization — the schedule is a contingent regulatory choice, not a discovered natural law. The engine's false summit detection identifies this perspectival error.
constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(psilocybin_therapeutic_efficacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(psilocybin_therapeutic_efficacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(psilocybin_therapeutic_efficacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(psilocybin_therapeutic_efficacy, TR),
    TR >= 0.70.

:- end_tests(psilocybin_therapeutic_efficacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint generates significant extraction toward pharmaceutical incumbents and regulatory gatekeepers while blocking patients from access. However, the extraction is partially justified by legitimate safety verification coordination — psilocybin's pharmacology is complex and required rigorous trial design. The 0.58 value reflects that both extraction and coordination are genuinely present. Suppression (0.65): High. Barriers to accessing psilocybin therapy include legal criminalization, DEA licensing requirements, limited research slots, geographic concentration of approved trial sites, and social stigma. Treatment-seeking patients face arrest risk in most jurisdictions and prohibitive travel/cost barriers for jurisdictions where therapy is available (Oregon, Canada, Netherlands). Suppression increased from 0.60 to 0.65 over the interval as demand for psilocybin-assisted therapy increased faster than legal supply pathways. Theater ratio (0.68): High. Enforcement of Schedule I for psilocybin has become increasingly performative. The original 1970 scheduling was based on abuse potential and political momentum; current enforcement continues despite clinical evidence that psilocybin has lower abuse potential and harm profile than schedule II opioids, schedule III sedatives, and unscheduled alcohol/tobacco. The theater gap (between evidence supporting rescheduling and continued Schedule I status) has widened measurably, with the gap contributing to the increase in theater ratio over the interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same base metrics produce six different classifications depending on structural position. Treatment-seeking patients (trapped, powerless) perceive a snare — pure extraction with no exit. Clinical researchers (constrained, moderate power) perceive tangled rope — genuine safety coordination mixed with asymmetric extraction. Pharmaceutical companies (arbitrage, institutional) perceive rope — coordination that protects their market position. The psychiatric community (organized, constrained) perceives tangled rope — professional coordination benefits mixed with delayed treatment adoption costs. The regulatory pathway coalition (organized, constrained) perceives scaffold — a temporary restriction being actively bypassed through legal pathways with visible sunset. The War on Drugs institutional legacy (arbitrage, institutional) perceives piton — the schedule persists through inertia and jurisdictional commitments despite evidence misalignment. The civilizational analytical observer risks perceiving mountain — scheduling as an immutable legal fact — but the structural data reveals this as false naturalization: the schedule is a contingent regulatory choice subject to change through political process.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from the asymmetry in who benefits and who bears costs. Pharmaceutical companies as institutional beneficiaries with arbitrage options derive low d (around 0.18) — they benefit from Schedule I market protection and experience negative effective extraction (the constraint subsidizes them). Treatment-seeking patients as powerless agents trapped by legal and access barriers derive high d (around 0.92) — they bear maximum extraction with no exit options. Clinical researchers occupy the middle (d around 0.55) — they are constrained but gain career legitimacy from rare research approvals. The psychiatric community as organized agents with moderate exit (can use alternative treatments, though suboptimal) derives moderate d (around 0.58). The regulatory coalition as organized agents building exit pathways derives lower d (around 0.42) — their exit agency reduces experienced extraction even though the institutional framework imposes barriers. The derived d values feed the sigmoid f(d) to compute effective extractiveness chi, which explains why the same base extractiveness (0.58) produces snare classification from the powerless perspective (high chi) and rope classification from the institutional pharmaceutical perspective (negative chi).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by demonstrating that the six types are legitimate perspectival readings of genuine structural asymmetries. The question 'Is psilocybin scheduling a snare or rope or mountain?' has no single answer because different agents have structurally different relationships to the same institutional arrangement. The mandate (Schedule I status) produces: snare for trapped patients, tangled rope for researchers and psychiatric professionals, rope for pharmaceutical companies, scaffold for regulatory reformers, piton for institutional legacy systems, and false mountain for observers who naturalize regulation as law. The mandatrophy is resolved by recognizing that the perspectival presheaf itself is the answer — the constraint's type depends on which structural position is being analyzed. However, if forced to choose a single systemwide classification for regulatory policy, the analytical observer's perspective (civilizational/universal/analytical) indicates tangled rope as the canonical type: the constraint provides genuine coordination (safety verification, evidence standards) while enabling asymmetric extraction (market protection for incumbents, access barriers for patients, career advancement for gatekeepers). The classification as tangled rope is strengthened by the presence of both beneficiaries and victims in base properties and the requirement for active enforcement (regulatory machinery maintains Schedule I status).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_threshold_ambiguity,
    'What magnitude of clinical efficacy and safety profile justifies rescheduling or compassionate use exemption?',
    'Comparative efficacy meta-analysis against existing depression/PTSD treatments; dose-response safety profiling in diverse populations; long-term follow-up data on sustained remission and adverse events',
    'If threshold is met by current evidence: reclassify constraint as scaffold or rope (regulatory pathway opening). If threshold requires higher bar: constraint remains snare/tangled rope (gatekeeping justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_threshold_ambiguity, empirical, 'Clinical efficacy threshold for rescheduling justification').

omega_variable(
    schedule_enforcement_mechanism,
    'Is Schedule I enforcement driven by pharmacological risk assessment or by institutional inertia and political capture by competing pharmaceutical interests?',
    'Historical document analysis of scheduling rationale vs current evidence; comparative scheduling of structurally similar or pharmacologically equivalent compounds; analysis of pharmaceutical industry influence on DEA scheduling decisions',
    'If driven by legitimate risk: snare classification is accurate (gatekeeping protects patients). If driven by inertia/capture: piton classification confirmed (performative enforcement); constraint should degrade faster.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schedule_enforcement_mechanism, empirical, 'Mechanism driving Schedule I enforcement').

omega_variable(
    regulatory_capture_by_pharmaceutical_incumbents,
    'Do incumbent pharmaceutical companies (SSRIs, psychiatric medications) benefit from psilocybin remaining Schedule I, and does this influence FDA approval timelines or regulatory standards?',
    'Patent analysis and market competition data; comparison of psilocybin approval timelines with other breakthrough designations; analysis of FDA reviewer funding sources and pharmaceutical company connections; behavioral economics of incumbent company positions on psilocybin rescheduling',
    'If capture exists: pharmaceutical company perspective shifts from rope to institutional identity_locked (captured within status quo). Directionality overrides needed for pharmaceutical institutions. Constraint becomes pure snare from patient perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_pharmaceutical_incumbents, empirical, 'Regulatory capture by incumbent pharmaceutical interests').

omega_variable(
    placebo_effect_contamination,
    'How much of psilocybin''s therapeutic efficacy is genuine pharmacological effect vs placebo/expectancy/set-and-setting effects?',
    'High-quality randomized controlled trials with inert placebo controls; open-label vs blinded trial comparison; neuroimaging studies of mechanism of action; response rates across culturally different cohorts with different expectancy priors',
    'If mostly placebo: efficacy claims weaken; constraint becomes snare without justification (pure gatekeeping). If genuine pharmacological: efficacy claims strengthen; constraint becomes snare with partial justification (some caution warranted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placebo_effect_contamination, empirical, 'Pharmacological vs placebo contribution to efficacy').

omega_variable(
    therapy_modality_requirement_gate,
    'Does psilocybin require specialized therapy modality (psychotherapy integration, guided sessions) to achieve efficacy, or can efficacy be achieved with minimal supportive intervention?',
    'Comparison of psilocybin efficacy across minimal-support vs intensive-therapy trials; analysis of dose-response and supportive care interaction effects; real-world outcome data from jurisdictions where psilocybin is decriminalized',
    'If modality is essential: creates new constraint (therapy capacity bottleneck) but enables decriminalization with safety valve. If efficacy independent of modality: stronger case for rescheduling; constraint becomes pure institutional inertia (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapy_modality_requirement_gate, empirical, 'Whether therapy integration is essential for efficacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(psilocybin_therapeutic_efficacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psilo_tr_t0, psilocybin_therapeutic_efficacy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(psilo_tr_t10, psilocybin_therapeutic_efficacy, theater_ratio, 10, 0.62).
narrative_ontology:measurement(psilo_tr_t20, psilocybin_therapeutic_efficacy, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(psilo_be_t0, psilocybin_therapeutic_efficacy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(psilo_be_t10, psilocybin_therapeutic_efficacy, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(psilo_be_t20, psilocybin_therapeutic_efficacy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(psilocybin_therapeutic_efficacy, enforcement_mechanism).
narrative_ontology:affects_constraint(psilocybin_therapeutic_efficacy, psychedelic_research_access_bottleneck).
narrative_ontology:affects_constraint(psilocybin_therapeutic_efficacy, psychiatric_treatment_alternative_pathways).

% DUAL FORMULATION NOTE:
% Psilocybin's Schedule I status instantiates two structurally distinct constraints: the pharmacological efficacy verification coordination problem (genuine scientific gatekeeping) and the institutional inertia/capture problem (performative enforcement maintaining restrictive designation despite evidence). These are decomposed as separate stories with different ε values. The efficacy verification is upstream (higher empirical confidence); the institutional inertia is downstream (more contested, more extractive). Both stories link via network.affects_constraints to recognize their causal coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(psilocybin_therapeutic_efficacy, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
