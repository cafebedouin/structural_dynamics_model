% ============================================================================
% CONSTRAINT STORY: schedule_i_regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_schedule_i_regulatory_capture, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: schedule_i_regulatory_capture
 *   human_readable: Schedule I Regulatory Capture in Drug Policy
 *   domain: policy/regulatory_capture/criminal_justice
 *
 * SUMMARY:
 *   Schedule I regulatory capture represents a structural constraint where
 *   legitimate coordination (preventing uncontrolled distribution of untested
 *   substances) is layered with severe asymmetric extraction (suppressing
 *   medical research, criminalizing patient access, blocking therapeutic
 *   development). The constraint emerged from 1970s drug policy consolidation
 *   and persists through a coalition of law enforcement agencies (budget
 *   justification), pharmaceutical incumbents (market protection), and
 *   international treaty frameworks (diplomatic path dependency). The
 *   constraint exhibits all classic regulatory capture hallmarks: agencies
 *   tasked with neutral administration instead protect incumbent interests;
 *   entry barriers (legal prohibition of research) prevent competitive
 *   challenge; cost-shifting to powerless groups (patients, researchers)
 *   while benefits concentrate on institutional actors. The theater ratio
 *   (0.64) reflects that much Schedule I enforcement is performative: drug
 *   interdiction produces elaborate ritual and criminal justice processing
 *   without reducing substance availability or harms. The effective
 *   extractiveness (0.68) reflects that the constraint simultaneously
 *   coordinates (prevents market flooding) and extracts (suppresses
 *   alternatives and criminalizes populations). The measurement trajectory
 *   shows extractiveness rising from 0.42 to 0.68 over 30 years, driven by
 *   escalating enforcement spending, research suppression, and
 *   criminalization accumulation — the constraint is actively extracting more
 *   value as it matures. Theater ratio rising from 0.52 to 0.64 indicates
 *   that the constraint is increasingly performative — enforcement activity
 *   increases while outcome metrics (substance availability, harms) remain
 *   flat or worsen.
 *
 * KEY AGENTS:
 *   - Patient Populations: Primary victim (powerless/trapped) — criminalized for seeking substances that may have therapeutic value; bears extraction without coordination benefit
 *   - Medical Research Communities: Secondary victim (moderate/constrained) — prohibited from studying therapeutic mechanisms; face career risk and legal barriers for pursuing research; collectively trapped by Schedule I regime
 *   - DEA and Regulatory Agencies: Institutional beneficiary and coordinator (institutional/constrained) — benefit from budget justification and enforcement authority; provide genuine coordination function (preventing uncontrolled distribution) but also extract institutional power
 *   - Pharmaceutical Industry: Institutional beneficiary (institutional/arbitrage) — protected from competition by Schedule I designation; benefits from foreclosure of lower-cost botanical and alternative treatment pathways; maintains status via regulatory arbitrage
 *   - Law Enforcement Agencies: Institutional beneficiary (institutional/arbitrage) — benefit from ongoing drug war mandate, funding allocation, and enforcement authority; arbitrage federal resources for local operations
 *   - Reform Coalition: Organized challengers (organized/mobile) — medical associations, patient advocates, state legislatures building alternative pathways (medical exceptions, state rescheduling, harm reduction); seeing Schedule I as temporary with sunset clause
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees both genuine coordination function and severe extraction asymmetry; observes that beneficiaries are organized and institutional while victims are dispersed and powerless
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(schedule_i_regulatory_capture, 0.68).
domain_priors:suppression_score(schedule_i_regulatory_capture, 0.72).
domain_priors:theater_ratio(schedule_i_regulatory_capture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(schedule_i_regulatory_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(schedule_i_regulatory_capture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(schedule_i_regulatory_capture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(schedule_i_regulatory_capture, tangled_rope).
narrative_ontology:human_readable(schedule_i_regulatory_capture, "Schedule I Regulatory Capture in Drug Policy").
narrative_ontology:topic_domain(schedule_i_regulatory_capture, "policy/regulatory_capture/criminal_justice").

domain_priors:requires_active_enforcement(schedule_i_regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(schedule_i_regulatory_capture, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(schedule_i_regulatory_capture, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(schedule_i_regulatory_capture, incumbent_regulatory_apparatus).
narrative_ontology:constraint_victim(schedule_i_regulatory_capture, medical_research_communities).
narrative_ontology:constraint_victim(schedule_i_regulatory_capture, patient_populations).
narrative_ontology:constraint_victim(schedule_i_regulatory_capture, criminal_justice_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT POPULATION (SNARE) — Trapped within a regime that prohibits access to substances that medical evidence suggests may be therapeutically beneficial. Exit is literal: emigration to jurisdictions with different scheduling. Suppression operates through criminalization of patients and prescribers alike. Bears extraction without coordination benefit.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL RESEARCH COMMUNITY (SNARE) — Constrained by Schedule I designation which legally prohibits research into therapeutic mechanisms, safety profiles, and dosing protocols. Exit options exist but are costly: researchers can relocate, change fields, or seek expensive DEA waivers. The constraint extracts research attention and talent from areas of genuine medical need.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEA AND REGULATORY AGENCIES (TANGLED ROPE) — These institutions have a genuine coordination function: preventing distribution of harmful substances and maintaining drug supply chain integrity. However, the Schedule I regime also extracts institutional power and budget justification. Agencies benefit from maintaining Schedule I status (budget allocation, enforcement authority) while also bearing coordination costs. Constrained by political pressure and congressional mandates, not freely mobile.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (ROPE) — Benefits from Schedule I status via reduced competition from lower-cost botanical and synthetic alternatives. The constraint serves a genuine coordination function for these firms: it stabilizes market structure and prevents disruptive entry. Pharmaceuticals can exit via lobbying, but maintain Status Quo through arbitrage (capturing regulatory processes). Net beneficiary experiencing constraint as coordination mechanism.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized actors (medical associations, patient advocacy groups, state legislatures) are building alternative pathways: state-level rescheduling, medical exceptions, harm reduction frameworks. These groups see Schedule I as a temporary coordination failure with a sunset clause. Oregon's drug decriminalization, multiple states' medical cannabis exceptions, and ongoing FDA fast-track designations for psilocybin and MDMA research represent exit pathways maturing. Coalition has agency and sees time-bound transformation.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL DRUG CONTROL TREATIES (PITON) — The Single Convention on Narcotic Drugs (1961) and successors maintained Schedule I globally through institutional inertia rather than functional necessity. These treaties persist through diplomatic convention and path dependency despite mounting evidence of their counterproductivity. Theater ratio is high: treaty compliance produces elaborate administrative apparatus and enforcement rituals with minimal actual coordination benefit. The treaties are degraded (their primary stated goal—preventing harms—is not achieved) but persist because no coalition has yet replaced them institutionally.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical stance, Schedule I exhibits both genuine coordination (preventing market flooding with untested substances) and severe asymmetric extraction (suppressing research, criminalizing patients, blocking therapeutic access). The constraint persists because the coordination function is real enough to justify the apparatus, while the extraction mechanism is distributed enough (affects multiple powerless groups) that no single coalition can organize against it. Effective extraction chi remains elevated across time horizons.
constraint_indexing:constraint_classification(schedule_i_regulatory_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(schedule_i_regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(schedule_i_regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(schedule_i_regulatory_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(schedule_i_regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(schedule_i_regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(schedule_i_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting asymmetric extraction. The Schedule I regime provides pharmaceutical incumbents with market protection (estimated value: billions in prevented competition) while imposing costs on patients (denied therapeutic access), researchers (prohibited inquiry), and public health (reduced understanding of harms/benefits). The value is not maximal (0.72+) because some genuine coordination exists: preventing completely uncontrolled distribution of novel substances has real public health rationale. But the asymmetry is severe — benefits concentrate on institutional actors with lobbying capacity while costs distribute across powerless, dispersed groups. Suppression (0.72): Very high. Multiple suppression mechanisms operate simultaneously: legal prohibition on research, criminal penalties for possession/distribution, international treaty frameworks, budget-funded enforcement apparatus, and informational suppression (official harms narratives preventing accurate risk assessment). Agents cannot exit without extraordinary cost (emigration, career change, legal jeopardy). Theater ratio (0.64): Moderate-high, indicating substantial performative activity. Drug enforcement and interdiction produce elaborate administrative and criminal justice processing without proportionate reduction in substance availability or population-level harms. The constraint maintains legitimacy through enforcement ritual rather than outcome delivery. The theater ratio increased from 0.52 to 0.64 as enforcement spending escalated without corresponding outcome improvements — classic theater drift indicating degradation of primary function (harm prevention) and replacement with secondary function (maintaining institutional authority).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is extreme and reveals the capture mechanism. The pharmaceutical industry sees Rope (coordination mechanism protecting market structure); the DEA sees Tangled Rope (mixed coordination and power extraction); patients see Snare (pure extraction with no exit). The analytical observer sees the same structural data and classifies as Tangled Rope — both coordination and extraction are present and real, but the extraction falls overwhelmingly on powerless agents while coordination serves institutional interests. The reform coalition sees a Scaffold with sunset (state-level alternatives maturing, international consensus shifting) — they correctly identify that the constraint is temporary and being replaced. The international treaty framework operates as a Piton — maintains itself through diplomatic inertia despite degraded function and mounting evidence of failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each actor's structural position. Patients as victims with trapped exit: d ≈ 0.95, producing maximum experienced extraction via f(d) ≈ 1.42. Researchers as moderate victims with constrained exit: d ≈ 0.72, producing high experienced extraction via f(d) ≈ 1.13. DEA as institutional coordinator with constrained exit: d ≈ 0.55 (both coordinator and beneficiary; extraction flows toward them but not maximal), producing moderate-high f(d) ≈ 0.75. Pharmaceutical industry as institutional beneficiary with arbitrage: d ≈ 0.15 (full beneficiary), producing low/negative f(d) ≈ -0.01. Scope modifier σ(national) = 1.0 leaves χ unmodified from base scaling. The chi formula χ = ε × f(d) × σ(S) thus produces: patients experience χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.97 (severe extraction); pharma experiences χ ≈ 0.68 × (-0.01) × 1.0 ≈ -0.01 (subsidized, beneficiary position). This directionality differential is the core diagnostic signal of regulatory capture.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Schedule I exhibits mandatrophy (extractiveness 0.68 > 0.70 threshold). The resolution is that the constraint is not a false positive for 'coordination' — it genuinely coordinates (prevents uncontrolled distribution of untested substances, maintains supply chain integrity, prevents market flooding). However, the coordination function is real AND subordinate to the extraction mechanism. The constraint persists not because coordination would fail without it, but because institutional actors benefit from maintaining the extraction. This is the canonical mandatrophy resolution: the constraint is Tangled Rope, not Snare in disguise. The coordination function is sufficient to distinguish it from pure extraction (which would require near-zero beneficiary advantage from coordination) but asymmetric enough to classify as extraction-dominated hybrid. The analytical observer correctly sees Tangled Rope at the civilizational horizon — the constraint will persist as long as institutional beneficiaries (pharma, law enforcement, DEA) maintain capture over the rescheduling process. The scaffold perspective (reform coalition seeing sunset) is perspectival, not structural — the sunset requires breaking the institutional capture coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_evidence_threshold,
    'What standard of therapeutic evidence should trigger rescheduling or research authorization?',
    'International consensus on evidence thresholds via WHO expert committees; comparative analysis of approval standards across jurisdictions with different scheduling regimes',
    'Low threshold: many substances reclassified, research expands, extraction mechanism weakens. High threshold: Schedule I persists despite clinical data, extraction sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_evidence_threshold, preference, 'Evidence threshold for therapeutic reclassification').

omega_variable(
    regulatory_capture_mechanism_identification,
    'Is Schedule I maintained primarily by pharmaceutical industry lobbying, bureaucratic inertia, political risk aversion, or genuine public health concerns?',
    'Empirical analysis of lobbying expenditure correlations, regulatory agency budget trends, legislative voting patterns conditional on industry donations, comparative outcomes in high-capture vs. low-capture jurisdictions',
    'If primarily lobbying: targeted enforcement against capture mechanisms. If primarily inertia: reform requires international treaty renegotiation. If primarily political risk: educational campaigns may shift calculus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism_identification, empirical, 'Primary mechanism sustaining Schedule I designation').

omega_variable(
    medical_research_necessity_vs_extraction,
    'Does Schedule I prohibition on research represent legitimate precaution against dangerous research or illegitimate suppression of valid inquiry?',
    'Comparison of research safety outcomes in jurisdictions with unrestricted research access vs. Schedule I jurisdictions; analysis of harm curves and adverse event rates; cross-national policy diffusion patterns',
    'If legitimate precaution: constraint classification remains Snare (suppression justified). If illegitimate suppression: classification shifts to pure extraction, Snare persists but justification collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_research_necessity_vs_extraction, empirical, 'Whether research prohibition serves safety or suppression').

omega_variable(
    international_coordination_dependency,
    'Can Schedule I be effectively rescheduled at national or state levels, or does global treaty obligation create hard coordination requirement?',
    'Legal analysis of treaty exit mechanisms; empirical examination of countries/regions that have unilaterally broken with conventions (Netherlands cannabis, Canada psychedelics); cost-benefit analysis of treaty exit vs. renegotiation',
    'If national exit is viable: scaffold and reform pathways are real, sunset clause is operative. If treaty-locked: rescheduling requires global consensus, sunset extends decades, extraction mechanism locks in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_coordination_dependency, conceptual, 'Whether Schedule I can be rescheduled at national/state level').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(schedule_i_regulatory_capture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sched_i_tr_t0, schedule_i_regulatory_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sched_i_tr_t15, schedule_i_regulatory_capture, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sched_i_tr_t30, schedule_i_regulatory_capture, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(sched_i_be_t0, schedule_i_regulatory_capture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sched_i_be_t15, schedule_i_regulatory_capture, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(sched_i_be_t30, schedule_i_regulatory_capture, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(schedule_i_regulatory_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(schedule_i_regulatory_capture, 0.12).
narrative_ontology:affects_constraint(schedule_i_regulatory_capture, pharmaceutical_market_concentration).
narrative_ontology:affects_constraint(schedule_i_regulatory_capture, criminal_justice_system_expansion).
narrative_ontology:affects_constraint(schedule_i_regulatory_capture, medical_research_access_restrictions).

% DUAL FORMULATION NOTE:
% Schedule I regulatory capture is downstream of and reinforces broader constraints: pharmaceutical market concentration (Schedule I forecloses competition), criminal justice expansion (enables mass incarceration), and medical research restrictions (suppresses knowledge production). These constraints form a causal family where Schedule I serves as the institutional mechanism linking market power, enforcement authority, and epistemic suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(schedule_i_regulatory_capture, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
