% ============================================================================
% CONSTRAINT STORY: ad_fus_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ad_fus_coordination, []).

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
 *   constraint_id: ad_fus_coordination
 *   human_readable: Focused Ultrasound Alzheimer's Intervention Coordination
 *   domain: medical/neurological
 *
 * SUMMARY:
 *   Focused Ultrasound (FUS) has emerged as a promising neuromodulation tool
 *   for Alzheimer's disease intervention, with proposed mechanisms including
 *   mechanical disruption of amyloid-beta aggregates, enhancement of
 *   blood-brain barrier permeability for drug delivery, and modulation of
 *   glial responses. However, the FUS-AD ecosystem exhibits a structural
 *   tension between genuine coordination (multi-site trials, standardized
 *   protocols, shared evidence generation) and institutional extraction
 *   (device manufacturer control, licensing gatekeeping, restricted patient
 *   access during clinical development phases). This constraint demonstrates
 *   how a potentially beneficial technology can simultaneously function as a
 *   coordination mechanism for clinical validation AND as an extraction
 *   apparatus that maintains patient desperation and geographic/economic
 *   inequity during the extended development phase. The theater_ratio (0.68)
 *   reflects the gap between performative multi-site trial infrastructure and
 *   the limited evidence for clinical cognitive benefit — institutions
 *   perform rigorous trial design and regulatory compliance while mechanistic
 *   understanding remains contested and long-term safety data is absent.
 *
 * KEY AGENTS:
 *   - Early-Stage Alzheimer's Patients: Primary victim (powerless/trapped) — desperate for options, trapped in disease progression, dependent on trial access controlled by others
 *   - Device Manufacturers (Insightec, Philips, etc.): Primary beneficiary (institutional/arbitrage) — control device production, licensing, and distribution; capture monopoly rents during development phase
 *   - Neurology Clinicians and Community Hospitals: Secondary actor (moderate/constrained) — gain prestige and research participation benefits but constrained by equipment costs and liability; extract some benefit from coordination
 *   - Regulatory Bodies (FDA, EMA) and Academic Medical Centers: Organized coordinator (organized/constrained) — structure trial pathways, set safety standards, possess sunset visibility to eventual approval/integration
 *   - Established Pharma (Biogen, Eli Lilly, Roche): Competing beneficiary (institutional/arbitrage) — maintain drug-centric narrative while performing support for multimodal approaches; preserve existing market dominance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks misinterpreting institutional trial complexity as immutable scientific necessity rather than contingent governance structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ad_fus_coordination, 0.38).
domain_priors:suppression_score(ad_fus_coordination, 0.52).
domain_priors:theater_ratio(ad_fus_coordination, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ad_fus_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(ad_fus_coordination, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ad_fus_coordination, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ad_fus_coordination, tangled_rope).
narrative_ontology:human_readable(ad_fus_coordination, "Focused Ultrasound Alzheimer's Intervention Coordination").
narrative_ontology:topic_domain(ad_fus_coordination, "medical/neurological").

domain_priors:requires_active_enforcement(ad_fus_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ad_fus_coordination, device_manufacturers).
narrative_ontology:constraint_beneficiary(ad_fus_coordination, neuromodulation_research_groups).
narrative_ontology:constraint_victim(ad_fus_coordination, patient_access_equity).
narrative_ontology:constraint_victim(ad_fus_coordination, long_term_safety_evidence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-STAGE PATIENTS (SNARE) — Trapped in disease progression with limited therapeutic options. FUS clinical trial participation or access is restricted by device availability, treatment center location, and trial enrollment criteria. No meaningful exit option; bears full risk of experimental intervention without guarantee of benefit. Maximum experienced extraction — desperation creates dependence on institutions controlling access.
constraint_indexing:constraint_classification(ad_fus_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICIANS AND COMMUNITY HOSPITALS (TANGLED ROPE) — Constrained by regulatory requirements, device certification timelines, and liability exposure. Benefits from coordination incentive: FUS access increases their institutional prestige and enables participation in multi-site trials. Moderate extraction due to training requirements and equipment costs; moderate coordination benefit from shared research infrastructure.
constraint_indexing:constraint_classification(ad_fus_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVICE MANUFACTURERS (ROPE) — Primary beneficiary with arbitrage options. Controls device access, patent protections, and commercial licensing. Experiences FUS coordination as enabling market creation. Net extraction runs toward this actor — they coordinate the intervention pathway while capturing licensing revenue and first-mover advantage.
constraint_indexing:constraint_classification(ad_fus_coordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND ACADEMIC COORDINATION (SCAFFOLD) — FDA, EMA, and academic medical centers see FUS as a temporary coordination mechanism: structured trials, outcome registries, and standardized safety protocols are building the evidence base toward eventual regulatory approval or market expansion. Low effective extraction because organized agents see sunset pathway: once efficacy/safety is established, FUS moves from experimental coordination to standard clinical practice, reducing the friction of access control.
constraint_indexing:constraint_classification(ad_fus_coordination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTABLISHED PHARMA (PITON) — Large pharmaceutical companies have invested heavily in amyloid-targeting drugs (monoclonal antibodies, secretase inhibitors) approved or in late trials. FUS coordination creates competitive pressure but also reputational theater: endorsing multimodal approaches (drug + device) maintains institutional legitimacy while preserving existing product revenue. Theater ratio high because pharma support for FUS is performative — they benefit more from monopolizing the treatment narrative than from genuine adoption.
constraint_indexing:constraint_classification(ad_fus_coordination, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the coordination bottleneck in neurotechnology development appears inherent: complex interventions targeting the brain require extensive safety validation before scaling, and the clinical trial apparatus represents an immutable constraint imposed by epistemic and regulatory necessity. However, structural data contradicts this — the bottleneck is enforced institutional arrangement, not natural law.
constraint_indexing:constraint_classification(ad_fus_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ad_fus_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ad_fus_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ad_fus_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ad_fus_coordination, TR),
    TR >= 0.70.

:- end_tests(ad_fus_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Device manufacturers capture licensing revenue and market control during clinical development, but extraction is tempered by genuine R&D costs, regulatory compliance burden, and uncertainty over commercialization success. The 'extraction' reflects information asymmetry (patients cannot assess device efficacy) and access gatekeeping (limited trial slots, geographic concentration), not pure predation. Suppression (0.52): Moderate-high. Barriers to independent verification include proprietary device specifications, restricted trial data access, need for specialized equipment and training, and regulatory requirements limiting off-label use. But suppression is not total — academic medical centers participate in trials, published results appear, and alternative ultrasound hardware exists (though not optimized for FUS-AD). Theater ratio (0.68): Moderately high, increasing over interval. Early FUS development (t=0) emphasized mechanistic plausibility and proof-of-concept; by t=8, the ecosystem has built elaborate trial infrastructure, regulatory engagement, and multimodal treatment narratives, but evidence for cognitive benefit remains limited and mechanistic understanding contested. Theater has accumulated as institutional stakeholders perform clinical legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (device manufacturer) sees this constraint as a pure coordination mechanism enabling clinical development and market creation — they emphasize the shared trial infrastructure, regulatory collaboration, and standardized safety protocols. The victim (early-stage patients) sees extraction — restricted access, high treatment costs (if commercialized), and desperation-driven enrollment in trials with uncertain benefit. Clinicians see a mixed picture (tangled rope) — coordination benefits (prestige, research participation, access to novel technology) coupled with extraction (equipment costs, liability, training burden). Regulatory bodies see a temporary coordination problem with sunset (scaffold) — trials are building evidence toward approval, after which FUS transitions from restricted coordination mechanism to standard clinical practice. Established pharma sees competitive threat but maintains performative support (piton) — endorsing multimodal treatment preserves institutional legitimacy while defending existing drug revenue. The analytical observer risks seeing an immutable natural law of neurotechnology development (mountain) — the elaborate trial apparatus appears necessary for safety validation — but structural data reveals this as naturalization of a contingent institutional arrangement: manufacturer control, patent-driven exclusivity, and regulatory gatekeeping are policy choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Device manufacturers occupy the strongest beneficiary position (institutional power, arbitrage exit) — they set device specifications, licensing terms, and treatment protocols with limited external constraint. Early-stage patients occupy the weakest position (powerless, trapped exit) — disease progression creates desperation that overwhelms alternatives; they cannot arbitrage to other Alzheimer's treatments with demonstrated efficacy. Clinicians and hospitals occupy a middle position (moderate power, constrained exit) — they gain from institutional prestige and research participation but cannot easily exit due to equipment sunk costs and training investment. Regulatory bodies and academic centers operate as organized coordinators (constrained exit, sunset visibility) — they structure the trial pathway but are also constrained by the manufacturer's technology and intellectual property. Pharma occupies a strategic position (institutional power, but with competitive threat from FUS) — they perform support for multimodal approaches while protecting existing drug revenue streams. The directionality gradient from powerless patients to institutional manufacturers drives the tangled_rope classification: genuine coordination function (multi-site trials, standardized protocols) coexists with asymmetric extraction (access control, information asymmetry, licensing rents).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying which coordination function is genuine vs. performative. GENUINE COORDINATION: Multi-site trials, standardized protocols, safety monitoring, and regulatory alignment create real collective action benefits — they reduce duplicate effort, enable statistical power, and generate trustworthy evidence. These functions would survive scrutiny from any perspective. EXTRACTIVE LAYERING: Device manufacturer monopoly on hardware production, licensing gatekeeping, patent-protected treatment protocols, and restricted trial access create barriers that persist even after the genuine coordination function is complete. If FUS efficacy is confirmed, the scaffold sunset should transition FUS to commodity neuroimaging with open-source hardware development and low licensing friction. The theater ratio (0.68) and rising trajectory indicates that institutional stakeholders are investing in performative legitimacy (elaborate trial infrastructure, regulatory engagement theater) to maintain the extraction apparatus beyond the point where genuine coordination is necessary. The mandatrophy is resolved by distinguishing: (a) coordination that would continue because it solves a real problem (multi-site evidence generation), and (b) extraction that depends on suppressing alternatives and maintaining information asymmetry (device monopoly, patent licensing, restricted access). The constraint exhibits tangled rope, not pure snare, because the genuine coordination function is substantial and participants actually benefit from it — clinicians gain prestige, patients gain access to a novel therapy, manufacturers gain market opportunity. But the extraction layer (licensing rents, access control) is not necessary to the coordination function and would fall away under open-source hardware and nonexclusive licensing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fus_mechanism_clarity,
    'Does FUS modulate Alzheimer''s pathology through mechanical disruption of protein aggregates, or through modulation of neuroimmune responses and blood-brain barrier permeability?',
    'Multi-modal imaging studies (PET, MRI, ultrasound backscatter); biomarker tracking (CSF/blood phospho-tau, amyloid clearance); mechanistic intervention studies (FUS alone vs FUS + BBB opening inhibitors)',
    'If mechanism is direct mechanical: FUS is a device coordination tool with predictable dose-response. If mechanism is neuroimmune modulation: FUS requires personalized timing relative to immune state, increasing complexity and extraction opacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fus_mechanism_clarity, empirical, 'Mechanism of FUS action on Alzheimer''s pathology').

omega_variable(
    long_term_safety_trajectory,
    'What is the long-term neurological safety profile of repetitive FUS exposure over years? Are there delayed adverse effects, cumulative damage, or off-target effects not visible in short-term trials?',
    'Long-term follow-up of Phase 2/3 trial participants (5-10 year cohorts); neuroimaging for white matter changes, microhemorrhages, or network disruption; comparison to device-naive Alzheimer''s cohorts matched on disease stage',
    'If safe: FUS enters standard care pathway rapidly (scaffold sunset accelerates). If significant delayed risks: victim extraction persists indefinitely (snare perspective confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_safety_trajectory, empirical, 'Long-term safety of repeated FUS treatment').

omega_variable(
    access_equity_constraint,
    'Is FUS equipment cost and geographic concentration inherently unequal (mountain-like property of technology capital), or a contingent market extraction enforced by patent and licensing?',
    'Comparative health economics: cost analysis of FUS vs. established AD treatments (monoclonal antibodies, cholinesterase inhibitors); geographic availability modeling; licensing terms analysis; investigation of generic/open-source ultrasound hardware feasibility',
    'If inherent to technology: inequality is persistent (structural property). If contingent: different licensing and manufacturing models could reduce access barriers (scaffold sunset feasible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_equity_constraint, conceptual, 'Whether access inequality is inherent or contingent').

omega_variable(
    trial_completion_feasibility,
    'Can adequately powered Phase 3 trials demonstrating cognitive benefit or disease modification be completed within 5-10 years, or does the heterogeneity of Alzheimer''s pathology and slow progression rates make rapid trial closure infeasible?',
    'Analysis of ongoing trial enrollment rates, dropout rates, and cognitive decline slopes; biomarker-stratified subgroup analysis; feasibility of surrogate endpoints (amyloid/tau biomarkers) vs. cognitive outcomes',
    'If trials can complete on standard timeline: regulatory approval pathway visible, scaffold sunset real. If trials require >10 years: coordination mechanism persists as extraction structure, piton classification deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trial_completion_feasibility, empirical, 'Feasibility of Phase 3 trial completion timelines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ad_fus_coordination, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adfus_tr_t0, ad_fus_coordination, theater_ratio, 0, 0.42).
narrative_ontology:measurement(adfus_tr_t4, ad_fus_coordination, theater_ratio, 4, 0.58).
narrative_ontology:measurement(adfus_tr_t8, ad_fus_coordination, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(adfus_be_t0, ad_fus_coordination, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(adfus_be_t4, ad_fus_coordination, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(adfus_be_t8, ad_fus_coordination, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ad_fus_coordination, enforcement_mechanism).
narrative_ontology:affects_constraint(ad_fus_coordination, amyloid_targeting_drug_approval).
narrative_ontology:affects_constraint(ad_fus_coordination, neuroimaging_standardization).
narrative_ontology:affects_constraint(ad_fus_coordination, clinical_trial_patient_recruitment).

% DUAL FORMULATION NOTE:
% The FUS-AD constraint overlaps with three structural mechanisms: (1) amyloid-targeting drug approval (upstream pharmacological competitor), (2) neuroimaging standardization (shared infrastructure dependency), and (3) clinical trial patient recruitment (shared victim population). FUS represents an alternative therapeutic pathway that could displace or integrate with existing approaches. The constraint family requires separate stories for each mechanism with different epsilon values reflecting their distinct empirical status and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ad_fus_coordination, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
