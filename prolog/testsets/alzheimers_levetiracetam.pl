% ============================================================================
% CONSTRAINT STORY: alzheimers_levetiracetam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alzheimers_levetiracetam, []).

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
 *   constraint_id: alzheimers_levetiracetam
 *   human_readable: Levetiracetam as Alzheimer's Preventative
 *   domain: social/medical/pharmaceutical
 *
 * SUMMARY:
 *   Levetiracetam (Keppra), an established anti-seizure medication, has been
 *   proposed and increasingly used off-label to prevent or slow cognitive
 *   decline in Alzheimer's disease and mild cognitive impairment. The
 *   scientific basis is plausible but unproven: seizure-like
 *   hyperexcitability is observed in Alzheimer's brains, and levetiracetam
 *   modulates calcium channels and synaptic vesicle proteins. However, no
 *   Phase III randomized controlled trial has validated levetiracetam's
 *   efficacy for Alzheimer's prevention. Despite this evidence gap, off-label
 *   use is growing in clinical practice, recommended by some neurologists,
 *   advocacy organizations, and online patient communities. The constraint
 *   exhibits classic tangled rope structure: genuine coordination function
 *   (providing desperate patients access to a plausible intervention for a
 *   disease with no approved preventatives) combined with genuine extraction
 *   (pharmaceutical benefit without evidence liability, patient exposure to
 *   drug side effects for unproven indication, suppression of uncertainty
 *   about efficacy and harms). The theater_ratio reflects that clinical
 *   judgment appears to validate the practice, but actually rests on
 *   mechanistic plausibility and case reports rather than controlled
 *   evidence. The constraint has a regulatory sunset logic: if levetiracetam
 *   demonstrates efficacy in rigorous trials, it should transition to
 *   approved indication; if harm emerges, off-label use should cease. But the
 *   timeline is uncertain, creating indefinite extraction risk.
 *
 * KEY AGENTS:
 *   - Patients with Alzheimer's or MCI: Primary victims (powerless/trapped) — seek prevention but cannot evaluate evidence; exposed to drug side effects and false hope without clear informed consent
 *   - Cautious Clinicians: Mixed position (moderate/constrained) — provide coordination by offering plausible option but bear career and liability risk
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from off-label adoption without conducting expensive trials
 *   - Patient Advocacy Organizations: Organized amplifiers (organized/constrained) — represent desperate patients but may suppress uncertainty if funded by pharmaceutical interests
 *   - Regulatory Framework: Institutional referee (organized/constrained) — permits off-label use as temporary mechanism but has sunset logic requiring either trial approval or contraindication
 *   - Evidence Standards as Institutional Ritual: Structural actor (institutional/arbitrage) — maintains appearance of evidence-based medicine while functional verification degrades
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and genuine extraction intertwined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alzheimers_levetiracetam, 0.52).
domain_priors:suppression_score(alzheimers_levetiracetam, 0.65).
domain_priors:theater_ratio(alzheimers_levetiracetam, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alzheimers_levetiracetam, extractiveness, 0.52).
narrative_ontology:constraint_metric(alzheimers_levetiracetam, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(alzheimers_levetiracetam, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alzheimers_levetiracetam, tangled_rope).
narrative_ontology:human_readable(alzheimers_levetiracetam, "Levetiracetam as Alzheimer's Preventative").
narrative_ontology:topic_domain(alzheimers_levetiracetam, "social/medical/pharmaceutical").

domain_priors:requires_active_enforcement(alzheimers_levetiracetam).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alzheimers_levetiracetam, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(alzheimers_levetiracetam, off_label_prescribers).
narrative_ontology:constraint_beneficiary(alzheimers_levetiracetam, early_adopter_clinicians).
narrative_ontology:constraint_victim(alzheimers_levetiracetam, regulatory_integrity).
narrative_ontology:constraint_victim(alzheimers_levetiracetam, patient_safety_epistemic_commons).
narrative_ontology:constraint_victim(alzheimers_levetiracetam, evidence_standard_adherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT BEARING UNKNOWN RISK (SNARE) — Cognitively declining patients or those with family history of Alzheimer's cannot evaluate risk-benefit tradeoffs for off-label levetiracetam use. Trapped by cognitive decline, information asymmetry, and physician authority. Bears full extraction: exposure to drug with unvalidated Alzheimer's efficacy and known seizure-drug side effects (cognitive dulling, mood changes, risk of dependence) with minimal informed consent about off-label status or evidence gaps.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAUTIOUS CLINICIAN (TANGLED ROPE) — Sees coordination benefit (access to promising intervention for desperate patients with no FDA-approved preventatives) but also bears extraction: career risk if adverse events emerge, liability exposure, requirement to stay current with off-label literature vs approved protocols. Can constrain prescribing but cannot fully exit the tension between evidence standards and patient desperation. Mixed extraction and coordination.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from off-label adoption without conducting the expensive Phase III trials required for Alzheimer's indication approval. Coordination function: drug repurposing mechanism solves access problem for desperate patients. Exit via arbitrage: can market drug for seizures while patients and clinicians coordinate its Alzheimer's use independently. Net beneficiary.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (SCAFFOLD) — FDA permits off-label prescribing as temporary coordination mechanism for unmet medical needs, but sunset logic applies: if levetiracetam demonstrates efficacy in rigorous Phase III trials, the off-label use should transition to approved indication with mandatory labeling, pharmacovigilance, and informed consent requirements. Off-label use is structurally temporary — either evidence accumulates (trial approval pathway) or harm emerges (contraindication). Theater is moderate: off-label use appears to be clinical judgment but is actually a regulatory workaround with a built-in expiration date.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EVIDENCE STANDARD AS RITUAL (PITON) — Clinical practice guidelines and evidence hierarchies (RCT > observational > case report) are theoretically the mechanism for validating off-label use. But in practice, levetiracetam for Alzheimer's persists in clinical recommendations with theater_ratio increasing: citations to mechanistic plausibility (seizure-related excitotoxicity hypothesis) and small open-label studies substitute for controlled trial data. The ritual of evidence evaluation continues (literature reviews, case conferences) but the functional distinction between weak evidence and strong evidence has atrophied — off-label use is recommended with the same confidence as approved interventions. Institutional inertia maintains the appearance of evidence-based decision-making while the actual verification has degraded.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PATIENT ADVOCACY ORGANIZATION (TANGLED ROPE) — Coordination benefit: advocacy groups represent desperate patients with no approved Alzheimer's preventatives and can mobilize access to promising off-label options. Extraction: advocacy organizations may be funded by pharmaceutical interests or may amplify promising anecdotes beyond evidence, suppressing informed uncertainty and driving adoption of interventions with unproven efficacy. Constrained exit: organizations that emphasize caution and evidence gaps risk losing support from desperate members seeking hope.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the levetiracetam-Alzheimer's constraint exhibits both genuine coordination (off-label use for unmet medical needs) and genuine extraction (pharmaceutical benefit without evidence liability, patient exposure to side effects and false hope, regulatory capture of evidence standards). The constraint persists because the coordination function is real — patients desperately need options — but the extraction is concealed by the appearance of clinical judgment. The analytical observer sees this as a classic tangled rope: coordination and extraction structurally intertwined, not separable.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alzheimers_levetiracetam_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alzheimers_levetiracetam, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alzheimers_levetiracetam, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alzheimers_levetiracetam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alzheimers_levetiracetam, TR),
    TR >= 0.70.

:- end_tests(alzheimers_levetiracetam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value for pharmaceutical manufacturers (drug sales without approval costs) and early-adopter clinicians (reputation for innovation) while transferring risk to patients (exposure to side effects, false hope, opportunity cost of delaying approved treatments). The value is not higher because the coordination function is genuinely real — no approved Alzheimer's preventatives exist — so off-label use does solve an access problem, not merely create false demand. Suppression (0.65): High. Multiple mechanisms suppress uncertainty: (1) mechanistic plausibility is emphasized over clinical trial absence; (2) pharmaceutical funding of advocacy groups and CME programs influences recommendations; (3) patients with cognitive decline cannot fully evaluate risk-benefit tradeoffs; (4) clinical communities adopt recommendations before evidence accumulates; (5) negative or null case reports are less likely to be published or shared than positive anecdotes. Theater ratio (0.68): Moderate-high. Clinical judgment appears to validate off-label use, but the actual decision-making rests on plausible mechanism, case reports, and advocacy messaging rather than controlled evidence. The appearance of evidence-based practice has increased over time as levetiracetam recommendations spread despite stable evidence base.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence on the Snare-Rope axis. The patient sees pure extraction (snare) because they are trapped with no exit and bear full risk. The manufacturer sees pure coordination (rope) because they exit via arbitrage and benefit cleanly. The clinician is genuinely between these poles, experiencing both coordination (solving patient access problem) and extraction (bearing liability and reputational risk). The regulatory framework and evidence standard are institutional actors experiencing degradation over time — they maintain the appearance of rigor while actual verification decays. The analytical observer at civilizational scale sees the constraint as a permanent feature of Alzheimer's desperateness: off-label use will persist as long as (1) approved preventatives remain absent, and (2) the mechanism is plausible enough to justify hope. This is a tangled rope, not a snare or rope alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality from beneficiary/victim declarations and exit options. Patients (trapped, no exit) experience maximum directionality toward victimhood (high d → high f(d) → high experienced extraction). Manufacturers (arbitrage options, can exit if approval fails) experience low directionality (low d → negative f(d) → net benefit). Clinicians (constrained exit, mixed benefits and risks) experience mid-range directionality. The engine computes f(d) from these structural parameters, automatically producing the observed perspectival gap without additional axes or measurement bases. Suppression (0.65) is a raw structural property — not scaled by context — reflecting the multiple mechanisms that hide uncertainty: pharmaceutical marketing, advocacy messaging, cognitive decline in patients, publication bias, and institutional inertia in evidence standards.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy (false classification as pure extraction when coordination is present, or vice versa) by declaring both components explicitly. The coordination function is real: levetiracetam addresses an unmet medical need. The extraction is real: patients bear risk without evidence, manufacturers benefit without approval costs, and evidence standards degrade. These are not competing labels — both are structural. The mandatrophy is resolved by requiring beneficiary AND victim declarations, plus active enforcement (off-label prescribing persists through clinical choice, not law). The alternative misclassifications would be: (1) Rope alone — erasing patient risk and evidence gaps (false positivity). (2) Snare alone — erasing the genuine absence of approved alternatives and valid clinical need (false negation). (3) Mountain — naturalizing off-label use as inherent to how medicine works (false summit). The tangled_rope classification is correct because both coordination and extraction are measurable and necessary to explain the constraint's persistence. If approved preventatives emerge, constraint transitions to snare (extraction without coordination). If trial shows harm, constraint transitions to snare. If trial shows benefit, constraint transitions to rope (pure coordination via approved indication). The currently observed mixed state is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seizure_excitotoxicity_mechanism,
    'Does levetiracetam''s mechanism of action against seizures (calcium channel modulation, synaptic vesicle protein binding) provide actual neuroprotection against Alzheimer''s excitotoxic cascade, or is the mechanistic plausibility merely rhetorical?',
    'Controlled Phase III trials testing levetiracetam vs placebo in mild cognitive impairment or preclinical Alzheimer''s populations with biomarker endpoints (CSF tau, amyloid-beta, neuroinflammation markers). In vivo imaging of synaptic density and calcium dynamics in treated vs untreated populations.',
    'If mechanism validated: levetiracetam moves from off-label extraction to approved indication. If mechanism absent or insufficient: constraint shifts from tangled_rope to snare — patients exposed to drug with side effects for unproven prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seizure_excitotoxicity_mechanism, empirical, 'Whether levetiracetam''s seizure mechanism translates to Alzheimer''s neuroprotection').

omega_variable(
    adverse_event_monitoring_gap,
    'What is the true rate of cognitive decline, mood changes, dependence potential, and long-term neurological sequelae in cognitively normal or mildly impaired patients taking levetiracetam chronically off-label?',
    'Prospective cohort study tracking adverse events in off-label levetiracetam users vs matched controls; analysis of pharmacovigilance reports (FDA FAERS) for levetiracetam use outside seizure indication; long-term follow-up of patients who discontinued off-label use.',
    'If adverse event rate is low: constraint is justified as temporary scaffold with low net harm. If adverse event rate is high or reveals delayed cognitive effects: constraint shifts fully to snare — patients exposed to harm without informed consent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adverse_event_monitoring_gap, empirical, 'Rate of adverse events in chronic off-label levetiracetam use').

omega_variable(
    trial_feasibility_timeline,
    'What is the realistic timeline and cost for conducting a Phase III levetiracetam trial in Alzheimer''s prevention? Is the trial feasible or does the cost-benefit ratio favor indefinite off-label use over formal approval?',
    'Health economics analysis of trial cost vs expected patient population and pharmaceutical market opportunity; regulatory pathway analysis for repurposed drugs; comparison to timelines for novel Alzheimer''s drug development.',
    'If trial is feasible and funded: scaffold sunset is real — constraint has exit date. If trial is prohibitively expensive or market opportunity insufficient: off-label use becomes indefinite — constraint shifts toward snare (permanent extraction with no approval pathway).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trial_feasibility_timeline, empirical, 'Feasibility and timeline for Phase III levetiracetam trial').

omega_variable(
    informed_consent_capacity,
    'Can cognitively impaired patients or those with early Alzheimer''s symptoms genuinely provide informed consent to off-label levetiracetam use? What capacity assessments are actually being conducted in clinical practice?',
    'Audit of informed consent documentation in clinics prescribing off-label levetiracetam for Alzheimer''s prevention; assessment of whether consent forms disclose off-label status, evidence limitations, and side effect profile; capacity evaluation of patient cohort.',
    'If informed consent is inadequate: constraint is extraction-dominant (snare) despite coordination framing. If consent process is robust: constraint remains tangled_rope with better transparency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informed_consent_capacity, empirical, 'Whether informed consent practices meet ethical standards').

omega_variable(
    regulatory_capture_mechanism,
    'Is pharmaceutical funding of Alzheimer''s advocacy organizations, clinical societies, and CME programs influencing off-label levetiracetam recommendations beyond what the evidence base supports?',
    'Funding source analysis of organizations promoting levetiracetam for Alzheimer''s; comparison of recommendation strength to evidence base quality; tracking of CME speakers and conflicts of interest; analysis of whether advocacy organizations disclose pharmaceutical funding when promoting off-label use.',
    'If capture is significant: constraint shifts toward snare — extraction is institutional and suppressed by lack of transparency. If capture is minimal: constraint remains tangled_rope with genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Degree of pharmaceutical influence on off-label recommendations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alzheimers_levetiracetam, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alz_lev_tr_t0, alzheimers_levetiracetam, theater_ratio, 0, 0.45).
narrative_ontology:measurement(alz_lev_tr_t5, alzheimers_levetiracetam, theater_ratio, 5, 0.58).
narrative_ontology:measurement(alz_lev_tr_t10, alzheimers_levetiracetam, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(alz_lev_be_t0, alzheimers_levetiracetam, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(alz_lev_be_t5, alzheimers_levetiracetam, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(alz_lev_be_t10, alzheimers_levetiracetam, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alzheimers_levetiracetam, resource_allocation).
narrative_ontology:affects_constraint(alzheimers_levetiracetam, alzheimers_drug_approval_timeline).
narrative_ontology:affects_constraint(alzheimers_levetiracetam, patient_informed_consent_capacity).

% DUAL FORMULATION NOTE:
% This constraint exists at the intersection of pharmaceutical policy (drug repurposing, off-label use), medical practice (clinical evidence standards), and patient access (unmet need for Alzheimer's prevention). The levetiracetam-Alzheimer's claim is downstream of the broader constraint that FDA-approved Alzheimer's preventatives do not exist — the off-label use fills a gap created by approval timeline failures. Separate story (constraint_id: alzheimers_drug_approval_timeline) models the upstream pharmaceutical development constraint; this story models the intermediate coordination-extraction hybrid that emerges when plausible drugs are used off-label due to approval gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alzheimers_levetiracetam, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
