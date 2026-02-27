% ============================================================================
% CONSTRAINT STORY: coffee_cardiovascular_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coffee_cardiovascular_2026, []).

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
 *   constraint_id: coffee_cardiovascular_2026
 *   human_readable: The Caffeine Paradox Realignment
 *   domain: medical/health/cardiology
 *
 * SUMMARY:
 *   The caffeine paradox realignment exemplifies how medical guidance can
 *   become extractive when it persists despite contradictory evidence and
 *   lacks mechanistic justification. For approximately 15 years (2005-2020),
 *   cardiologists cautioned atrial fibrillation patients to avoid or severely
 *   limit coffee consumption based on mechanistic reasoning (caffeine as a
 *   sympathomimetic stimulant and presumed arrhythmia trigger) rather than
 *   clinical evidence. Beginning around 2010, large epidemiological studies
 *   and meta-analyses consistently documented that coffee consumption was not
 *   associated with increased AF risk and might confer protective effects.
 *   Yet the restrictive guidance remained embedded in clinical practice,
 *   patient education materials, and cardiologist communication well into the
 *   2020s. The constraint exhibits all six DR types from different
 *   perspectives: it is an immutable medical conservatism (mountain), a
 *   performative risk-aversion ritual (piton), a temporary guideline lag
 *   being corrected by evidence-based medicine (scaffold), a coordination
 *   mechanism for conservative practice (rope), a mixed
 *   extraction-coordination mechanism for pharmaceutical interests (tangled
 *   rope), and a pure extraction mechanism for patients who complied with
 *   unnecessary restrictions (snare). The constraint's theater ratio
 *   increased from 0.35 to 0.65 over the interval as the gap between evidence
 *   and guidance widened, indicating increasing performativity — the caution
 *   persisted not because evidence supported it but because institutional
 *   structures (liability concern, guideline inertia, professional authority)
 *   maintained it.
 *
 * KEY AGENTS:
 *   - Atrial Fibrillation Patients: Primary victims (powerless/trapped) — forced to eliminate or restrict coffee despite accumulating evidence of safety; no organized coalition to challenge guidance until meta-analyses shifted consensus
 *   - Clinical Evidence Generators: Secondary actors (moderate/constrained) — conducted epidemiological studies and trials showing safety/benefit; benefited from research funding and publications but bore career risk of contradicting established guidance
 *   - Conservative Cardiologists: Primary beneficiaries (institutional/arbitrage) — maintained authority through caution-first approach; reduced medico-legal exposure by documenting conservative advice; could update recommendations anytime but had no incentive to do so
 *   - Pharmaceutical Manufacturers (Antiarrhythmics, Beta-Blockers): Secondary beneficiaries (institutional/constrained) — benefited from sustained patient medication adherence when lifestyle restrictions reinforced pharmacological approach; constrained by regulatory oversight and evidence accumulation forcing repositioning
 *   - Evidence-Based Medicine Coalition: Organized actors (organized/mobile) — developed systematic reviews, meta-analyses, and preregistered trials that built alternative verification pathways; successfully shifted guideline consensus from 2015-2026
 *   - Medical Institutional Default for Risk-Aversion: Theater maintainer (institutional/arbitrage) — broader tendency to issue restrictive guidance absent definitive harm evidence; sustained the caution through inertia, liability considerations, and change resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coffee_cardiovascular_2026, 0.38).
domain_priors:suppression_score(coffee_cardiovascular_2026, 0.48).
domain_priors:theater_ratio(coffee_cardiovascular_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coffee_cardiovascular_2026, tangled_rope).
narrative_ontology:human_readable(coffee_cardiovascular_2026, "The Caffeine Paradox Realignment").
narrative_ontology:topic_domain(coffee_cardiovascular_2026, "medical/health/cardiology").

domain_priors:requires_active_enforcement(coffee_cardiovascular_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, conservative_cardiologists).
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, coffee_consuming_patients).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, clinical_evidence_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATRIAL FIBRILLATION PATIENT (SNARE) — For 15+ years, patients were told to eliminate coffee despite accumulating evidence of safety and possible cardioprotection. Exit options are minimal: compliance means lifestyle restriction; non-compliance means confronting medical authority while managing arrhythmia risk. The constraint extracts years of avoided pleasure and social participation. No organized patient coalition could challenge the guidance until meta-analyses shifted consensus.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICAL EVIDENCE GENERATOR (TANGLED ROPE) — Research groups conducting epidemiological studies and trials both benefited from and bore costs of the constraint. Benefits: funding for caffeine-arrhythmia research, publication in high-impact journals, discovery of paradoxical protective effects. Costs: replication burden, career risk of contradicting established guidance, data access limitations. Constrained by institutional review boards, funding availability, and established research hierarchies.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSERVATIVE CARDIOLOGISTS (ROPE) — Beneficiaries of the restrictive guidance. The caution-first approach coordinated clinical practice, reduced medico-legal exposure (documented conservative advice), and maintained professional authority. Arbitrage options abundant: could cite accumulated evidence anytime; maintained optionality. Net beneficiary during the entire period — the constraint subsidized their position through institutional momentum.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURERS (TANGLED ROPE) — Benefited from restricted coffee consumption because patient compliance with beta-blockers and other arrhythmia medications improved when lifestyle restrictions reinforced pharmacological approach. Extraction mechanism: kept patients on higher medication doses or extended treatment durations by reducing a low-cost competing intervention (coffee). Constrained by regulatory oversight and clinical evidence accumulation forcing repositioning. Active enforcement required: marketing, continuing medical education, clinical guideline influence.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EVIDENCE-BASED MEDICINE COALITION (SCAFFOLD) — Organized groups (systematic review networks, guideline developers, open-data advocates) built alternative verification pathways. Meta-analyses and preregistered trials created mechanisms to challenge established guidance. The constraint has a sunset: as consensus shifted (2015-2026), organized actors developed exit routes. Guidelines updated, patient autonomy increased. Theater declined as evidence moved from caution-first rhetoric to risk-stratified personalized approaches. This is a real sunset — the restrictive guidance is being actively replaced by evidence-informed alternatives.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: RISK-AVERSION INSTITUTIONAL DEFAULT (PITON) — The broader medical institutional tendency to issue restrictive guidance absent definitive harm evidence. This became theater: guidelines that persisted not because evidence supported them but because institutional inertia, liability considerations, and change resistance sustained the caution. The degraded function is evident: by 2020, the guidance no longer reflected the evidence base, yet continued circulating in patient education materials and clinical conversations. Theater ratio reflects that the 'coffee causes AF' framing became performative — a ritualized caution maintained despite contradictory meta-analyses.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some level of medical conservatism regarding novel substances is inherent to clinical practice: physicians must balance benefit, harm, and uncertainty. The constraint appears as an immutable feature of how medicine proceeds under incomplete information. However, the structural data reveals false naturalization: the specific 15-year persistence of restrictive caffeine guidance despite accumulating reassuring evidence is not inherent to medicine but reflects institutional constraints (liability, authority structures, guideline inertia) that are contingent and changeable.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coffee_cardiovascular_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(coffee_cardiovascular_2026, TR),
    TR >= 0.70.

:- end_tests(coffee_cardiovascular_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts value from patients through unnecessary lifestyle restrictions, but the extraction is not maximal because the harm is inconvenience rather than direct injury, patients can access information to challenge the guidance, and the window of extraction is finite (guidelines are updating). The initial value (0.22) reflects that early caution was justified by mechanistic plausibility; the increase to 0.38 reflects accumulation of contradictory evidence without guideline updating, indicating growing extractiveness as the gap widens. Suppression (0.48): Moderate. Patients face barriers to challenging guidance (medical authority, complexity of interpreting meta-analyses, risk aversion regarding personal cardiac health decisions) but are not completely trapped (internet access to evidence, ability to consult multiple cardiologists, eventual guideline updates). Theater ratio (0.65): Moderately high. The ritualized caution persists despite contradictory evidence — patient education materials continue warning against coffee, clinician conversations maintain the caution, and guideline updates lag evidence by years. The increase from 0.35 to 0.65 reveals growing performativity: the guidance functions primarily to maintain institutional authority and reduce liability exposure rather than to protect patients.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic. Patients see extraction (snare): years of lifestyle restriction for unproven benefit. Conservative cardiologists see coordination (rope): maintaining cautious practice standards. Pharmaceutical manufacturers see mixed extraction-coordination (tangled rope): using restrictive lifestyle guidance to reinforce pharmacological treatment adherence. The evidence-based medicine coalition sees a temporary problem with a sunset (scaffold): meta-analyses and guideline updates are creating exit pathways and replacing the restrictive guidance with evidence-informed personalization. The institutional risk-aversion default sees its own degraded function (piton): the caution persists through inertia rather than because it works. The civilizational analytical observer risks seeing an immutable natural law (mountain): medical conservatism regarding novel substances is inherent to clinical practice under uncertainty. But the structural data reveals false naturalization — the specific 15-year persistence despite contradictory evidence reflects contingent institutional structures (liability, authority, guideline inertia) rather than inherent properties of medicine.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation differs across perspectives. Patients (powerless + trapped) experience high d, producing high chi and snare classification — they bear the full cost of unnecessary restriction with no exit. Evidence generators (moderate + constrained) experience moderate d — they benefit from publication and funding but bear career risk. Conservative cardiologists (institutional + arbitrage) experience low d — they benefit from maintained authority and easily exit when convenient (arbitrage option). Pharmaceutical manufacturers (institutional + constrained) experience moderate d — they benefit from prolonged patient medication adherence but face increasing constraint from evidence accumulation forcing repositioning. The open science coalition (organized + mobile) experience low d through their exit option — they can and do exit by building alternative verification systems. The institutional risk-aversion default experiences near-zero d — it benefits from maintenance of authority and reduces liability exposure with no meaningful exit cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The caffeine paradox resolves the mandatrophy by showing how institutional structures can convert legitimate medical conservatism (mountain-like, justified by mechanistic plausibility) into extractive guidance persisting despite contradictory evidence (snare-like). The key insight is that the constraint's type depends on the temporal reference: at t=0 (early 2000s), the guidance was justified conservatism (rope: coordination around plausible mechanism). As evidence accumulated, the constraint either would transition to rope-with-sunset (scaffold: temporary caution while evidence accumulates) OR persist as snare-like extraction (institutional inertia overriding evidence). The actual historical outcome shows snare-like persistence: the constraint extracted for 8+ years despite mounting evidence because institutional structures (guideline development inertia, liability concern, professional authority) prioritized caution over responsiveness to evidence. The mandatrophy is resolved by recognizing that the same base properties (moderate extractiveness, theater, suppression) produce snare classification for patients (trapped), scaffold classification for evidence generators (constrained but mobile), and rope classification for beneficiary institutions (institutional with arbitrage). The constraint is not 'really' one type — it is a hybrid that exhibits snare extraction FOR patients while functioning as a coordination mechanism FOR physicians and as a theater FOR institutions maintaining authority. The perspectival gap IS the answer: understanding which agents benefit and which bear costs reveals the constraint's actual structure better than forcing it into a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caffeine_dose_response_threshold,
    'Does a dose-response threshold exist below which caffeine actually reduces AF risk, or is the relationship monotonically protective across all consumption levels?',
    'High-resolution prospective studies with standardized caffeine dosing and AF event monitoring; subgroup analysis by baseline arrhythmia burden and medication use',
    'If threshold exists at low doses: current guidance to eliminate all coffee is over-conservative. If monotonic: even high-dose coffee may be protective, drastically changing risk calculus for AF patients.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caffeine_dose_response_threshold, empirical, 'Dose-response relationship between caffeine and AF outcomes').

omega_variable(
    mechanism_of_paradoxical_protection,
    'Does the apparent cardioprotective effect of caffeine operate through sympathomimetic preconditioning, adenosine receptor antagonism conferring protection, or unmeasured confounding (coffee drinkers differ in unmeasured ways)?',
    'Mechanistic studies in cardiac tissue; investigation of adenosine signaling pathways; careful confounding adjustment for diet quality, exercise, socioeconomic status, and genetic polymorphisms in caffeine metabolism',
    'If mechanism confirmed and not confounded: reverses entire therapeutic guidance from restriction to conditional recommendation. If strong confounding suspected: paradox may be observational artifact and guidelines remain justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_of_paradoxical_protection, empirical, 'Biological mechanism underlying apparent cardioprotective effect').

omega_variable(
    guideline_authority_asymmetry,
    'Why did conservative cardiology societies'' restrictive guidance persist in clinical practice and patient education for 8+ years after major systematic reviews documented safety, rather than updating within 1-2 years as evidence standards would suggest?',
    'Analysis of guideline development timelines, publication lags between meta-analyses and guideline updates, citation patterns in clinical literature, survey of cardiologists'' reasons for recommendation persistence',
    'If institutional inertia/liability dominates: reveals snare mechanism (patients trapped by delayed guideline response). If evidence standards were genuinely satisfied: guideline committees behaved rationally and the constraint is less extractive than it appears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guideline_authority_asymmetry, conceptual, 'Why restrictive guidance persisted despite accumulating evidence of safety').

omega_variable(
    patient_harm_from_caffeine_avoidance,
    'Did years of unnecessary caffeine restriction contribute to depression, reduced cognitive function, or other harms from foregone coffee consumption in AF patients?',
    'Retrospective case studies; surveys of AF patients asking about quality-of-life impact of restriction; psychiatric comorbidity analysis in restricted vs unrestricted cohorts during the guidance period',
    'If significant harm quantified: transforms the constraint from low-harm guideline caution to extractive guidance causing actual damage. If minimal harm: suggests the cost was inconvenience rather than health damage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_harm_from_caffeine_avoidance, empirical, 'Potential harms from prolonged unnecessary caffeine restriction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coffee_cardiovascular_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caff_tr_t0, coffee_cardiovascular_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(caff_tr_t5, coffee_cardiovascular_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(caff_tr_t10, coffee_cardiovascular_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(caff_be_t0, coffee_cardiovascular_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(caff_be_t5, coffee_cardiovascular_2026, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(caff_be_t10, coffee_cardiovascular_2026, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coffee_cardiovascular_2026, information_standard).
narrative_ontology:affects_constraint(coffee_cardiovascular_2026, antiarrhythmic_guideline_compliance).
narrative_ontology:affects_constraint(coffee_cardiovascular_2026, patient_medication_adherence_paradox).

% DUAL FORMULATION NOTE:
% The caffeine paradox decomposes into two constraint families: (1) the cardiovascular evidence base (caffeine effects on AF risk, dose-response relationships), and (2) the institutional guidance response (why restrictive guidance persists despite evidence). This story focuses on the institutional guidance constraint (coffee_cardiovascular_2026). The upstream cardiovascular evidence constraint has its own extractiveness reflecting empirical uncertainty; the downstream institutional constraint reflects guideline response inertia. Both are linked because evidence-based guideline updates are slowly replacing restrictive guidance, creating a sunset mechanism that makes the institutional constraint eventually disappear as alternatives establish themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coffee_cardiovascular_2026, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
