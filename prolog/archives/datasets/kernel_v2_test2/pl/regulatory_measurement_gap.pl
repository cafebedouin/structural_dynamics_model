% ============================================================================
% CONSTRAINT STORY: regulatory_measurement_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_measurement_gap, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_measurement_gap
 *   human_readable: Regulatory Measurement Gap in Aging Intervention Assessment
 *   domain: biomedical_research/regulatory_science/technology_governance
 *
 * SUMMARY:
 *   The regulatory measurement gap for aging interventions reflects a genuine
 *   coordination problem at the intersection of biological complexity,
 *   regulatory science, and therapeutic development. No FDA-accepted
 *   biomarkers exist for aging or age reversal, creating uncertainty for
 *   clinical trial design and drug approval pathways. Multiple stakeholders
 *   are working to close this gap: the TAME trial is testing metformin with
 *   composite endpoints, the XPrize Healthspan competition is developing
 *   standardized measurement protocols, the Biomarkers Consortium is pursuing
 *   FDA biomarker qualification, and academic researchers are validating
 *   candidate markers in longitudinal cohorts. The constraint coordinates
 *   research effort toward rigorous validation while preventing premature
 *   lock-in on inadequate measures. Extraction is low to moderate: the gap
 *   creates timeline uncertainty and capital inefficiency for therapeutic
 *   developers, but this reflects genuine scientific uncertainty rather than
 *   institutional rent-seeking. The measurement gap is downstream of the
 *   reprogramming safety toxicity constraint: even if cellular reprogramming
 *   interventions prove safe, they cannot achieve FDA approval without
 *   validated endpoints to demonstrate efficacy. The theater ratio (0.42)
 *   reflects moderate performative content: some biomarker qualification
 *   submissions are strategic positioning rather than scientifically mature
 *   proposals, and some regulatory conservatism is institutional caution
 *   rather than evidence-based gatekeeping. But the core function — ensuring
 *   that aging endpoints are scientifically valid before approving
 *   therapeutics — is genuine coordination, not theater.
 *
 * KEY AGENTS:
 *   - FDA Regulatory Authority: Institutional actor (institutional/constrained) — statutory mandate to require validated endpoints; benefits from coordination function; bears cost of resource allocation to qualification reviews
 *   - Aging Research Community: Organized beneficiary (organized/mobile) — needs shared standards to evaluate interventions; benefits from coordination as standards emerge; mobile exit through alternative research pathways
 *   - Longevity Biotech Sector: Powerful actor with mixed experience (powerful/arbitrage) — benefits from coordination function but bears extraction from regulatory uncertainty; arbitrage exit through wellness markets and international trials
 *   - Biomarker Development Consortia: Organized actors building sunset (organized/mobile) — TAME trial, XPrize, Biomarkers Consortium explicitly working to close the gap; sees constraint as temporary
 *   - Clinical Trial Sponsors: Moderate actors (moderate/constrained) — constrained by regulatory requirements but not trapped; benefits from coordination function; bears moderate extraction from timeline uncertainty
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees measurement gap as coordination problem inherent to translating complex biology into regulatory endpoints; confirms rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_measurement_gap, 0.28).
domain_priors:suppression_score(regulatory_measurement_gap, 0.35).
domain_priors:theater_ratio(regulatory_measurement_gap, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_measurement_gap, extractiveness, 0.28).
narrative_ontology:constraint_metric(regulatory_measurement_gap, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(regulatory_measurement_gap, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(regulatory_measurement_gap, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(regulatory_measurement_gap, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_measurement_gap, rope).
narrative_ontology:human_readable(regulatory_measurement_gap, "Regulatory Measurement Gap in Aging Intervention Assessment").
narrative_ontology:topic_domain(regulatory_measurement_gap, "biomedical_research/regulatory_science/technology_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_measurement_gap, aging_research_community).
narrative_ontology:constraint_beneficiary(regulatory_measurement_gap, biomarker_development_consortia).
narrative_ontology:constraint_beneficiary(regulatory_measurement_gap, longevity_biotech_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(regulatory_measurement_gap, longevity_biotech_sector).
narrative_ontology:constraint_victim(regulatory_measurement_gap, clinical_trial_sponsors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Statutory mandate to require validated endpoints for drug approval. Operates biomarker qualification program to evaluate candidate aging biomarkers. Bears cost of resource allocation to qualification reviews and criticism for regulatory conservatism. Benefits from coordination function: rigorous biomarker validation serves agency mission of ensuring drug safety and efficacy. Constrained by statutory authority and scientific uncertainty about which biomarkers are predictive.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, fda_regulatory_authority, agenda_setter,
    institutional, biographical, constrained, national).

% Organized through professional societies, consortia, and collaborative networks. Needs shared measurement standards to evaluate interventions, compare results across studies, and communicate findings. Benefits from coordination function as standards emerge. Mobile exit through alternative research pathways: basic science, animal models, international collaborations. Low extraction — the constraint enables rather than extracts from research progress.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, aging_research_community, beneficiary,
    organized, biographical, mobile, global).

% Developing aging interventions for FDA approval or consumer wellness markets. Benefits from coordination function: validated biomarkers would enable FDA approval pathways and increase investor confidence. Bears extraction: measurement gap creates regulatory uncertainty that increases capital costs, extends development timelines, and concentrates risk on early-stage companies. Arbitrage exit through wellness market positioning, international clinical trials, and direct-to-consumer offerings. Mixed experience: the gap both enables alternative business models and blocks the most lucrative pathway.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, longevity_biotech_sector, beneficiary,
    powerful, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(regulatory_measurement_gap, longevity_biotech_sector, payer).

% TAME trial, XPrize Healthspan, Biomarkers Consortium, and academic research groups building measurement infrastructure to close the gap. Organized actors with multiple funding sources and research pathways. Explicitly building toward a sunset: once validated biomarkers achieve FDA qualification, the measurement gap dissolves. Low extraction because the constraint is the problem they are organized to solve, and they have agency and resources to solve it.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, biomarker_development_consortia, agenda_setter,
    organized, generational, mobile, global).

% Pharmaceutical and biotech companies designing clinical trials for aging interventions. Constrained by regulatory requirements and capital availability but not trapped: can pursue surrogate endpoints, accelerated approval pathways, or international trials. Bears cost of timeline uncertainty and capital inefficiency from measurement gap. Benefits from coordination function: needs FDA-accepted endpoints to design approvable trials. Moderate extraction from regulatory uncertainty, but constraint reflects genuine scientific uncertainty about what to measure.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, clinical_trial_sponsors, payer,
    moderate, biographical, constrained, national).

% Patients with age-related diseases and advocates for faster access to interventions. Largely excluded from biomarker qualification process, which is technical and dominated by researchers and regulators. Would object to prolonged measurement gap if present in the conversation: demand faster access to potentially beneficial interventions. Constrained exit: can pursue wellness market products or international trials, but cannot access FDA-approved aging therapeutics until biomarkers are qualified.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, patient_advocacy_groups, excluded,
    moderate, immediate, constrained, national).

% Views the measurement gap as a coordination problem inherent to translating complex biological phenomena into regulatory endpoints. Aging is multidimensional and no single biomarker captures the full construct. The gap reflects genuine scientific uncertainty, not institutional extraction. The constraint coordinates research effort toward rigorous validation and prevents premature lock-in on inadequate measures. Confirms rope classification: this is a coordination mechanism with minimal extractive overhead.
narrative_ontology:constraint_stakeholder(regulatory_measurement_gap, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(regulatory_measurement_gap, diffuse).
narrative_ontology:fixing_cost_class(regulatory_measurement_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The measurement gap coordinates research effort toward rigorous validation of aging biomarkers and prevents premature regulatory lock-in on inadequate measures. It ensures that therapeutic claims about aging or age reversal are evaluated against scientifically valid endpoints rather than surrogate markers of unknown predictive validity.
% TRANSFER_FUNCTION: Timeline uncertainty and capital inefficiency flow from therapeutic developers (particularly early-stage biotech) to the regulatory system and to the scientific community pursuing biomarker validation. Regulatory authority and scientific credibility flow from the FDA and research consortia to the field. The gap transfers risk from patients (who would be exposed to interventions approved on inadequate endpoints) to developers (who bear the cost of regulatory uncertainty).
% ABSENT_VOICES: Patient advocacy groups are largely excluded from the biomarker qualification process, which is technical and dominated by researchers and regulators. Patients with age-related diseases would demand faster access to potentially beneficial interventions if they were more present in the conversation. Their absence means the coordination function (rigorous validation) is weighted more heavily than the access function (faster availability of interventions).
% DISAPPEARANCE_RATIONALE: If the measurement gap disappeared overnight (FDA suddenly accepted multiple aging biomarkers without validation), the world would rearrange significantly: clinical trials would proliferate using the newly accepted endpoints, capital would flow into longevity biotech, therapeutic development timelines would compress, and FDA approval pathways would open. However, the quality of evidence would decline: interventions might be approved based on biomarkers of uncertain predictive validity, increasing the risk of ineffective or harmful therapeutics reaching patients. The constraint's coordination function would be lost.
% FOUNDING_PROBLEM: The measurement gap was not 'built' — it emerged from the absence of validated biomarkers for a complex, multidimensional phenotype (aging) that was not historically a therapeutic target. The founding problem is: how do we evaluate whether an intervention slows, stops, or reverses aging when aging itself is not a single measurable process but a collection of functional declines, disease risks, and molecular changes that unfold over decades?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by the FDA (which operates a biomarker qualification program specifically to address this gap), by the National Institute on Aging (which funds longitudinal studies to validate candidate biomarkers), by the research community (which has not reached consensus on which biomarkers are most predictive), and by the longevity biotech sector (which faces regulatory uncertainty precisely because no validated endpoints exist). The problem is live: aging remains multidimensional, longitudinal validation is ongoing, and scientific uncertainty about which biomarkers are predictive persists.
narrative_ontology:disappearance_verdict(regulatory_measurement_gap, world_rearranges).
narrative_ontology:founding_problem_status(regulatory_measurement_gap, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FDA REGULATORY AUTHORITY (ROPE) — Constrained by statutory mandate to require validated endpoints but benefits from the coordination function: the measurement gap creates pressure for rigorous biomarker qualification, which serves the agency's mission of ensuring drug safety and efficacy. The absence of accepted aging biomarkers is a coordination problem the agency is actively working to solve through the biomarker qualification program. Moderate extraction because the agency bears some cost (resource allocation to qualification reviews, criticism for conservatism) but primarily experiences this as a coordination challenge with genuine public health stakes.
constraint_indexing:constraint_classification(regulatory_measurement_gap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: AGING RESEARCH COMMUNITY (ROPE) — Mobile exit options through alternative research pathways (basic science, animal models, international collaborations) and organized through professional societies, consortia, and collaborative networks. The measurement gap is a genuine coordination problem: the field needs shared standards to evaluate interventions, compare results across studies, and communicate findings. Benefits from the coordination function as standards emerge. Low extraction — the constraint enables rather than extracts from research progress.
constraint_indexing:constraint_classification(regulatory_measurement_gap, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: LONGEVITY BIOTECH SECTOR (TANGLED ROPE) — Arbitrage exit through consumer wellness markets, international clinical trials, and direct-to-consumer offerings. Benefits from the coordination function (validated biomarkers would enable FDA approval pathways) but also bears extraction: the measurement gap creates regulatory uncertainty that increases capital costs, extends development timelines, and concentrates risk on early-stage companies. Mixed experience: the gap both enables alternative business models (wellness market) and blocks the most lucrative pathway (FDA-approved therapeutics). Requires active enforcement through FDA's statutory authority over drug claims.
constraint_indexing:constraint_classification(regulatory_measurement_gap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BIOMARKER DEVELOPMENT CONSORTIA (SCAFFOLD) — Organized actors (TAME trial, XPrize Healthspan, Biomarkers Consortium) building the measurement infrastructure that will close the gap. Mobile exit through multiple funding sources and research pathways. Sees the constraint as temporary: the gap exists because aging biology is complex and measurement science is catching up, not because of structural barriers. The consortia are explicitly building toward a sunset — once validated biomarkers achieve FDA qualification, the measurement gap dissolves. Low extraction because the constraint is the problem they are organized to solve, and they have agency and resources to solve it.
constraint_indexing:constraint_classification(regulatory_measurement_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLINICAL TRIAL SPONSORS (ROPE) — Constrained by regulatory requirements and capital availability but not trapped: can pursue surrogate endpoints, accelerated approval pathways, or international trials. The measurement gap is a coordination problem: sponsors need FDA-accepted endpoints to design approvable trials. Benefits from the coordination function as biomarker qualification proceeds. Moderate extraction from timeline uncertainty and capital inefficiency, but the constraint is not primarily extractive — it reflects genuine scientific uncertainty about what to measure.
constraint_indexing:constraint_classification(regulatory_measurement_gap, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the measurement gap is a coordination problem inherent to translating complex biological phenomena into regulatory endpoints. Aging is multidimensional (functional decline, disease incidence, molecular markers, subjective health) and no single biomarker captures the full construct. The gap reflects genuine scientific uncertainty, not institutional extraction. The constraint coordinates research effort toward rigorous validation and prevents premature lock-in on inadequate measures. Low extraction — the constraint serves its coordination function. The analytical perspective confirms the rope classification: this is a coordination mechanism with minimal extractive overhead, not a naturalized extraction mechanism.
constraint_indexing:constraint_classification(regulatory_measurement_gap, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_measurement_gap_tests).
:- end_tests(regulatory_measurement_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low to moderate. The measurement gap creates timeline uncertainty and capital inefficiency for therapeutic developers, particularly early-stage biotech companies pursuing aging interventions. However, much of this 'extraction' is actually the cost of genuine scientific uncertainty — aging is multidimensional and no consensus exists on what to measure. The gap is not primarily a rent-seeking mechanism; it reflects the difficulty of validating biomarkers for a complex, slowly-progressing phenotype. Extraction has increased modestly over the interval (0.15 → 0.28) as the longevity biotech sector has grown and more capital is exposed to regulatory uncertainty, but remains well below the threshold for snare or tangled_rope classification. Suppression (0.35): Low to moderate. Barriers to closing the gap include the cost and duration of longitudinal validation studies, FDA's statutory conservatism on novel endpoints, and scientific disagreement about which biomarkers are most predictive. But suppression is not high: multiple pathways exist (surrogate endpoints, accelerated approval, international trials, wellness market), and active efforts are underway to qualify biomarkers through the FDA's established process. Suppression has decreased slightly over the interval (0.40 → 0.35) as biomarker qualification submissions have advanced and regulatory guidance has clarified. Theater ratio (0.42): Moderate. Some performative content exists: biomarker qualification submissions that are strategic positioning rather than scientifically mature, regulatory conservatism that is institutional caution rather than evidence-based, and industry claims about 'aging reversal' that outpace measurement capability. But the core function is genuine: the FDA's requirement for validated endpoints prevents approval of interventions based on inadequate measures, and the research community's effort to develop rigorous biomarkers serves a real coordination need. Theater has increased modestly over the interval (0.30 → 0.42) as commercial interest in longevity has grown faster than measurement science. Accessibility collapse (0.40): Moderate. Once the measurement gap is understood, some alternatives collapse: you cannot get FDA approval for an aging intervention without accepted endpoints, and you cannot validate endpoints without longitudinal data. But many alternatives remain accessible: basic research, animal models, surrogate endpoints, international trials, wellness market positioning. The gap constrains but does not eliminate pathways. Resistance (0.45): Moderate. The constraint meets real resistance from therapeutic developers frustrated by regulatory uncertainty, researchers advocating for specific biomarkers, and patient advocates demanding faster access to interventions. But resistance is not overwhelming: most stakeholders accept that rigorous validation is necessary, and active collaboration exists between FDA, academia, and industry to close the gap.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as rope or scaffold, with one tangled_rope perspective (longevity biotech sector). This narrow perspectival range confirms that the constraint is primarily a coordination mechanism rather than an extraction mechanism. The FDA sees a coordination problem it is actively working to solve through biomarker qualification. The research community sees a genuine need for shared standards. The biomarker consortia see a temporary problem with a sunset — they are building the infrastructure to close the gap. Clinical trial sponsors see a coordination challenge that creates timeline uncertainty but serves a legitimate regulatory function. The longevity biotech sector has the most extractive experience (tangled_rope) because the gap creates regulatory uncertainty that concentrates risk on early-stage companies, but even this perspective benefits from the coordination function and has arbitrage exit options. The analytical observer confirms the rope classification: the measurement gap coordinates research effort toward rigorous validation and prevents premature lock-in on inadequate measures. No perspective sees a snare or mountain — the constraint is neither pure extraction nor an immutable natural law. The gap is a coordination problem that multiple stakeholders are actively working to solve, with genuine scientific uncertainty about what to measure and how to validate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The FDA, aging research community, biomarker consortia, and clinical trial sponsors are all declared beneficiaries — they benefit from the coordination function as measurement standards emerge. No victims are declared because the constraint is not primarily extractive: the timeline uncertainty and capital inefficiency are costs of genuine scientific uncertainty, not asymmetric extraction. The longevity biotech sector has mixed experience (beneficiary with extraction) because the gap both enables alternative business models and blocks FDA approval pathways. Exit options differentiate experienced extraction: institutional actors with constrained exit (FDA, clinical trial sponsors) experience moderate extraction from resource allocation and timeline uncertainty; organized actors with mobile exit (research community, consortia) experience low extraction because they have alternative pathways; powerful actors with arbitrage exit (biotech sector) experience low extraction despite bearing costs because they can route around the constraint through wellness markets and international trials. The analytical observer confirms the rope classification: from a civilizational perspective, the measurement gap is a coordination mechanism with minimal extractive overhead, serving the genuine function of ensuring that aging endpoints are scientifically valid before therapeutics are approved.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit mandatrophy. The measurement gap's coordination function (ensuring validated endpoints before therapeutic approval) remains live and serves its intended purpose. The gap exists because aging biology is complex and measurement science is catching up, not because the original mandate has been captured or outlived. The scaffold perspective (biomarker consortia) shows that active efforts are underway to close the gap, with a realistic sunset timeline as validated biomarkers achieve FDA qualification. The modest increase in extraction over the interval (0.15 → 0.28) reflects growth in the longevity biotech sector and increased capital exposure to regulatory uncertainty, not mandate drift or institutional capture. The theater ratio (0.42) is moderate and reflects genuine scientific uncertainty and strategic positioning, not a degraded coordination function maintained for rent extraction. Mandatrophy is not resolved because it was never present: the constraint is functioning as intended, coordinating research effort toward rigorous biomarker validation while bearing the costs of genuine scientific uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_sufficiency_threshold,
    'What level of predictive validity is sufficient for a biomarker to qualify as an aging endpoint? Is correlation with lifespan in model organisms enough, or is human longitudinal data required?',
    'FDA biomarker qualification decisions; comparison of qualification standards across therapeutic areas; analysis of which evidence packages succeed vs fail in the qualification process',
    'If threshold is low (model organism data sufficient): gap closes rapidly, multiple biomarkers qualify, rope classification confirmed. If threshold is high (human longitudinal data required): gap persists for decades, extraction increases, classification shifts toward tangled_rope for some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomarker_sufficiency_threshold, empirical, 'Evidence threshold for biomarker qualification as aging endpoint').

omega_variable(
    composite_endpoint_acceptance,
    'Will FDA accept composite endpoints (combining multiple biomarkers, functional measures, and disease incidence) for aging interventions, or require single validated biomarkers?',
    'FDA guidance documents on aging endpoints; precedent from other therapeutic areas using composite endpoints; stakeholder feedback from pre-IND meetings',
    'If composite endpoints accepted: measurement gap narrows significantly, multiple pathways to approval open, scaffold sunset accelerates. If single biomarkers required: gap persists longer, concentration of research effort on a few candidate biomarkers, potential for premature lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_endpoint_acceptance, preference, 'Whether composite endpoints are acceptable for aging intervention approval').

omega_variable(
    international_harmonization,
    'Will international regulatory agencies (EMA, PMDA, Health Canada) converge on aging biomarker standards, or will divergent requirements fragment the development pathway?',
    'ICH guideline development; bilateral regulatory agreements; analysis of biomarker qualification submissions across jurisdictions',
    'If harmonized: global coordination function strengthens, rope classification robust across spatial scopes. If fragmented: regulatory arbitrage increases, extraction rises for companies navigating multiple standards, classification shifts toward tangled_rope at global scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_harmonization, empirical, 'Whether international regulatory standards for aging biomarkers will harmonize').

omega_variable(
    xprize_protocol_adoption,
    'Will the XPrize Healthspan measurement protocol achieve de facto standard status in the research community before or independent of FDA qualification?',
    'Adoption rate in published studies; citation patterns; use in clinical trial designs; industry consortium endorsements',
    'If widely adopted pre-FDA: creates parallel standard that may pressure FDA acceptance, scaffold sunset accelerates. If adoption remains limited: FDA qualification remains the bottleneck, measurement gap persists at current extraction level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xprize_protocol_adoption, empirical, 'Whether XPrize protocol becomes de facto standard before FDA qualification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_measurement_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reg_meas_tr_t0, regulatory_measurement_gap, theater_ratio, 0, 0.3).
narrative_ontology:measurement(reg_meas_tr_t3, regulatory_measurement_gap, theater_ratio, 3, 0.35).
narrative_ontology:measurement(reg_meas_tr_t6, regulatory_measurement_gap, theater_ratio, 6, 0.4).
narrative_ontology:measurement(reg_meas_tr_t10, regulatory_measurement_gap, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(reg_meas_be_t0, regulatory_measurement_gap, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(reg_meas_be_t3, regulatory_measurement_gap, base_extractiveness, 3, 0.2).
narrative_ontology:measurement(reg_meas_be_t6, regulatory_measurement_gap, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(reg_meas_be_t10, regulatory_measurement_gap, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(reg_meas_su_t0, regulatory_measurement_gap, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(reg_meas_su_t3, regulatory_measurement_gap, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(reg_meas_su_t6, regulatory_measurement_gap, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(reg_meas_su_t10, regulatory_measurement_gap, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_measurement_gap, information_standard).

% DUAL FORMULATION NOTE:
% The regulatory measurement gap is downstream of reprogramming_safety_toxicity: even if cellular reprogramming interventions prove safe, they cannot achieve FDA approval without validated endpoints to demonstrate efficacy. The measurement gap is a distinct constraint with its own coordination function and extractiveness profile, not merely a consequence of safety concerns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
