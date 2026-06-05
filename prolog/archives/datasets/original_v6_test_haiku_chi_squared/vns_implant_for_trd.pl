% ============================================================================
% CONSTRAINT STORY: vns_implant_for_trd
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vns_implant_for_trd, []).

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
 *   constraint_id: vns_implant_for_trd
 *   human_readable: Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression
 *   domain: biomedical/psychiatric_treatment
 *
 * SUMMARY:
 *   Vagus Nerve Stimulation (VNS) for treatment-resistant depression
 *   represents a surgical intervention for patients who have failed multiple
 *   pharmacological and psychological treatments. The constraint operates as
 *   a tangled rope with severe asymmetric extraction: VNS provides genuine
 *   coordination benefit (enabling treatment of otherwise untreatable severe
 *   depression) while simultaneously creating structural lock-in that
 *   benefits manufacturers, surgeons, and the psychiatric establishment at
 *   the expense of patient autonomy and alternative treatment pathways. The
 *   device requires permanent implantation, long-term follow-up, periodic
 *   battery replacements, and generates irreversible dependency. Theater
 *   ratio (0.61) reflects that informed consent processes, outcome
 *   statistics, and clinical protocols contain substantial performative
 *   elements that obscure the true costs and alternatives. The constraint
 *   exhibits all six types from different structural positions: a natural law
 *   perspective (depression is intractable, therefore intervention is
 *   inevitable), a manufacturer/surgeon perspective (coordination enabling
 *   profit), an establishment perspective (mixed coordination and
 *   professional extraction), a patient perspective (structural snare), and a
 *   medical autonomy perspective (degraded ritual). The core mandatrophy
 *   tension: Is VNS a coordination mechanism enabling treatment of the
 *   severely disabled (genuine benefit) or pure extraction exploiting
 *   desperation (structural predation)? The answer depends critically on what
 *   alternatives would be available if VNS were not approved and funded,
 *   which is counterfactual and suppressed by current institutional choices.
 *
 * KEY AGENTS:
 *   - Treatment-Resistant Depression Patient: Primary victim (powerless/trapped) — exhausted all standard treatments; faces binary choice between surgical implant or continued illness
 *   - Device Manufacturer (LivaNova/Entellus): Primary beneficiary (institutional/arbitrage) — captures approval pathway, long-term revenue from device replacements and clinical optimization
 *   - Implanting Surgeon: Secondary beneficiary (moderate/constrained) — derives procedural revenue and device-dependent follow-up income; professional reputation tied to adoption
 *   - Psychiatric Establishment: Mixed beneficiary/coordinator (organized/constrained) — benefits from validated biomedical intervention but constrained by professional commitments to neurobiological models
 *   - Alternative Treatment Pathways: Secondary victim (moderate/constrained) — psychedelic-assisted therapy, intensive psychotherapy, and neurofeedback face suppression from VNS institutional dominance
 *   - Medical Autonomy Ideal: Abstract victim (powerful/mobile) — informed consent processes become performative; patient decision-making capacity is substantially constrained by desperation and information asymmetry
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent institutional choice as immutable law of psychiatric medicine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vns_implant_for_trd, 0.52).
domain_priors:suppression_score(vns_implant_for_trd, 0.68).
domain_priors:theater_ratio(vns_implant_for_trd, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vns_implant_for_trd, extractiveness, 0.52).
narrative_ontology:constraint_metric(vns_implant_for_trd, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vns_implant_for_trd, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vns_implant_for_trd, tangled_rope).
narrative_ontology:human_readable(vns_implant_for_trd, "Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression").
narrative_ontology:topic_domain(vns_implant_for_trd, "biomedical/psychiatric_treatment").

domain_priors:requires_active_enforcement(vns_implant_for_trd).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, device_manufacturer).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, implanting_surgeons).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, psychiatric_establishment).
narrative_ontology:constraint_victim(vns_implant_for_trd, trd_patients).
narrative_ontology:constraint_victim(vns_implant_for_trd, medical_autonomy).
narrative_ontology:constraint_victim(vns_implant_for_trd, alternative_treatment_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRD PATIENT (SNARE) — Exhausted all standard psychiatric treatments (multiple SSRIs, psychotherapy, ECT). Faces binary: accept surgical implant with permanent device-dependence or remain untreated. Cannot exit the constraint without accepting depression. Suppression is extreme: information asymmetry, surgical lock-in, device manufacturer dependency. d≈0.93, f(d)≈1.39, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(vns_implant_for_trd, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVICE MANUFACTURER (ROPE) — Benefits from approval pathway, patient lock-in (replacement batteries, settings optimization, long-term follow-up revenue). Experiences constraint as coordination: FDA approval creates predictable market, clinical protocols standardize use. Has full exit option (arbitrage) — can shift to other neurotech products. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; effective extraction is negative (subsidy).
constraint_indexing:constraint_classification(vns_implant_for_trd, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PSYCHIATRIC ESTABLISHMENT (TANGLED ROPE) — Coordination function: VNS provides validated intervention for TRD, enabling treatment of previously untreatable patients (genuine coordination benefit). Extraction mechanism: VNS promotes neurobiological reductionism, pathologizes depression as device-fixable disease, marginalizes psychosocial approaches (structural extraction). Constrained exit: institutional commitments to pharmacological/biomedical models make exit costly. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPLANTING SURGEON (TANGLED ROPE) — Coordination: surgical intervention enables treatment for desperate patients (genuine benefit). Extraction: captures procedural revenue, device-dependent income stream, creates patient lock-in that drives follow-up appointments and optimization consultations. Constrained exit: professional reputation tied to adoption of FDA-approved intervention. d≈0.52, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE TREATMENT PATHWAYS (SNARE) — Psychedelic-assisted therapy, intensive psychotherapy, neurofeedback, and other non-surgical approaches face suppression by VNS approval and reimbursement capture. Medical establishment endorsement of VNS as 'the' validated solution for TRD suppresses research funding and clinical development of alternatives. Constrained exit: these pathways exist but lack institutional backing. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(vns_implant_for_trd, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDICAL AUTONOMY / BIOPOLITICAL AUTONOMY (PITON) — The ideal of patient autonomy in medical decision-making is substantially performative within the VNS system. Theater ratio (0.61) reflects: informed consent rituals that obscure irreversible surgical commitment, outcome statistics that inflate efficacy estimates, and procedural complexity that prevents meaningful patient evaluation. The autonomy ideal persists as institutional ritual while actual decision-making capacity is severely constrained by information asymmetry and desperation. theater_ratio≥0.70 not quite met, but approaching degradation.
constraint_indexing:constraint_classification(vns_implant_for_trd, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risk of naturalizing as immutable: 'Severe treatment-resistant depression is by definition untreatable by standard means; therefore any available intervention becomes acceptable.' This invokes a natural law framing (depression is intractable, therefore VNS is inevitable), but the structural data (ε=0.52, suppression=0.68, theater=0.61) contradicts true mountainhood. The constraint is contingent on institutional choices (reimbursement, professional adoption, alternative pathway suppression), not on natural limits. Engine will flag as false summit.
constraint_indexing:constraint_classification(vns_implant_for_trd, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vns_implant_for_trd_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vns_implant_for_trd, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vns_implant_for_trd, TR),
    TR >= 0.70.

:- end_tests(vns_implant_for_trd_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. VNS creates substantial extraction from patients through device lock-in (permanent implant, battery replacements, MRI restrictions, explantation difficulty), long-term clinical dependency, and information asymmetry about true efficacy vs. ritual/placebo contribution. However, extractiveness is not maximal because the intervention does provide genuine therapeutic benefit for some patients (coordination function exists). The manufacturing and surgical revenue streams amplify extraction. Suppression (0.68): Moderate-high. Patients face multiple suppression mechanisms: (1) information asymmetry — complex neuroscience framing obscures lack of understanding of mechanism; (2) desperation — exhausted standard treatments create psychological coercion; (3) procedural complexity — surgical irreversibility and device management prevent easy exit; (4) institutional barrier — psychiatric establishment endorsement marginalizes alternative pathways; (5) financial capture — insurance reimbursement preferentially funds VNS over alternatives. Suppression is not total (some patients do decline, alternatives do exist) but is severe. Theater ratio (0.61): Moderate-high. The clinical validation process contains substantial theatrical elements: (1) efficacy statistics conflate device effect with therapeutic ritual, (2) informed consent rituals create appearance of autonomous choice while desperation constrains actual agency, (3) outcome reporting emphasizes responder rates while suppressing non-responder experiences, (4) follow-up optimization appointments are presented as personalized care while serving revenue capture. Theater has increased over the 20-year interval as institutional adoption has normalized the procedure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests dramatically different classification across structural positions. The manufacturer sees coordination (Rope/pure benefit) — VNS solves a genuine medical need while generating stable revenue. The surgeon sees mixed coordination-extraction (Tangled Rope) — patients benefit from treatment access; surgeon benefits from procedural revenue. The psychiatric establishment sees mixed coordination-extraction (Tangled Rope) — VNS enables treatment of severe TRD while reinforcing neurobiological reductionism. The TRD patient sees extraction (Snare) — offers only binary choice (implant or remain ill), creates permanent device dependence, and obscures true mechanisms through medical theater. Alternative treatment pathways see suppression (Snare) — VNS FDA approval and insurance capture suppress development of non-surgical approaches. Medical autonomy sees degraded ritual (Piton) — informed consent becomes performative. The analytical observer risks seeing natural law (false Mountain) — depression is intractable, therefore VNS becomes inevitable — but the structural data reveals this as a contingent institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   TRD Patient: Victim + trapped → d≈0.93, f(d)≈1.39. Maximum extraction. Patient is the primary target of extraction; has no exit option within the constraint (accept implant or remain untreated; cannot exit the constraint itself). Device Manufacturer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction (net subsidy from constraint). Has full exit option — can shift to competing neurotech products. Implanting Surgeon: Beneficiary + constrained → d≈0.55, f(d)≈0.75. Moderate-high extraction. Surgeon benefits from procedural revenue and device-dependent follow-up, but has constrained exit (professional reputation tied to adoption of FDA-approved intervention). Psychiatric Establishment: Mixed + constrained → d≈0.48, f(d)≈0.62. Moderate extraction. Both coordinates (enables treatment) and extracts (reinforces professional power); constrained exit (institutional commitments). Alternative Pathways: Victim + constrained → d≈0.88, f(d)≈1.28. High extraction. Suppressed by VNS dominance; have constrained exit (exist but lack institutional backing). Medical Autonomy: Victim + mobile → d≈0.85, f(d)≈1.15. High extraction. Ideal is nominal mobile in other contexts, but within medical decision-making process is structurally trapped by information asymmetry and desperation.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The VNS constraint sits at the boundary between Tangled Rope (coordination + extraction) and Snare (pure extraction). The mandatrophy is unresolved because the empirical facts are ambiguous: (1) True efficacy attribution is contested — how much of VNS benefit is device stimulation vs. therapeutic ritual/placebo? (2) Alternative pathway suppression is real but not quantified — does VNS approval actively suppress other treatments or do they coexist? (3) Patient autonomy is contested — is desperation-driven consent genuine autonomous choice or structural coercion? The claimed_type (tangled_rope) reflects that genuine coordination function exists (VNS does enable treatment of severe TRD), but the suppression metrics (0.68) and patient victim classification (snare from powerless perspective) indicate extraction mechanisms are severe. RESOLUTION PATH: Three experiments would resolve this: (A) Long-term outcome studies comparing device-on vs. sham stimulation to isolate true efficacy from ritual/placebo — if true efficacy is >60% device effect, tangled rope is correct. (B) Historical analysis of alternative treatment funding pre- vs. post-VNS approval — if suppression is active and significant, snare classification is amplified. (C) Counterfactual scenario modeling what patients would choose if alternative treatments (psychedelic-assisted therapy, intensive psychotherapy) were equally funded and available — if alternatives would be chosen by >40% of current VNS candidates, patient desperation coercion is real. Until these empirical questions are resolved, the constraint cannot be definitively classified. The current tangled_rope classification is conservative (recognizes genuine benefit) but acknowledges unresolved snare risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_response_rate_attribution,
    'What portion of VNS efficacy is attributable to device stimulation vs. therapeutic ritual, placebo response, intensive follow-up care, or selection bias toward motivated patients?',
    'Long-term follow-up data comparing device-on vs. sham stimulation; patient-reported outcomes; correlation with objective depression biomarkers; comparison of efficacy in blinded vs. unblinded populations',
    'If efficacy is >60% ritual/placebo: extraction mechanism is amplified (patients pay for theater). If efficacy is <30% ritual/placebo: tangled rope classification is correct (genuine coordination benefit exists). This determines whether the constraint is primarily Snare or Tangled Rope from patient perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_response_rate_attribution, empirical, 'Attribution of VNS efficacy to device vs. ritual/placebo').

omega_variable(
    alternative_pathway_suppression_mechanism,
    'Does VNS FDA approval and insurance reimbursement actively suppress development of alternative non-surgical TRD treatments, or do they coexist independently?',
    'Historical analysis of funding allocation for psychedelic-assisted therapy, intensive psychotherapy, and neurofeedback trials pre- vs. post-VNS approval; correlation between insurance reimbursement expansion and research grant distribution; interviews with alternative treatment researchers on funding pressures',
    'If suppression is active and significant: VNS is a Snare from the perspective of alternative pathways (victim classification confirmed). If suppression is minimal: VNS is neutral to competing approaches. This determines whether ''medical autonomy'' victim classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_suppression_mechanism, empirical, 'Whether VNS approval suppresses alternative treatment development').

omega_variable(
    device_dependence_irreversibility_burden,
    'What is the true lifetime cost and burden of device dependence (battery replacements, device malfunctions, infection risk, MRI restrictions, explantation difficulty)?',
    'Longitudinal patient cohort tracking device-related complications, explantation rates, quality-of-life impact from device-related restrictions; cost analysis of lifetime device management vs. one-time surgical investment; patient regret rates at 5, 10, and 20 year follow-up',
    'If lifetime burden is high: extraction mechanism is amplified (patient lock-in cost is real). If burden is manageable: coordin function is amplified (intervention enables ongoing treatment). This directly affects χ calculation and whether the constraint should be classified as pure Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(device_dependence_irreversibility_burden, empirical, 'Lifetime burden of device dependence and irreversibility').

omega_variable(
    patient_desperation_boundary,
    'At what threshold of treatment failure does patient desperation override informed consent? Is surgical implant acceptance a genuine autonomous choice or structural coercion?',
    'Psychological assessment of decision-making capacity in TRD patients pre-VNS; comparison of autonomy measures between VNS candidates and general population; analysis of counterfactual alternatives (what would patients choose if alternative treatments were equally available and funded?)',
    'If desperation-based coercion is primary: patient agency is severely constrained, d→0.98, and Snare classification is unambiguous. If autonomy is substantial: tangled rope classification is more accurate. This is conceptual and preference-dependent, not purely empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_desperation_boundary, preference, 'Whether patient desperation overrides informed consent capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vns_implant_for_trd, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vns_tr_t0, vns_implant_for_trd, theater_ratio, 0, 0.48).
narrative_ontology:measurement(vns_tr_t10, vns_implant_for_trd, theater_ratio, 10, 0.55).
narrative_ontology:measurement(vns_tr_t20, vns_implant_for_trd, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(vns_be_t0, vns_implant_for_trd, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vns_be_t10, vns_implant_for_trd, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(vns_be_t20, vns_implant_for_trd, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vns_implant_for_trd, enforcement_mechanism).
narrative_ontology:affects_constraint(vns_implant_for_trd, psychiatric_pharmaceutical_extraction).
narrative_ontology:affects_constraint(vns_implant_for_trd, biomedical_model_dominance).
narrative_ontology:affects_constraint(vns_implant_for_trd, medical_desperation_coercion).

% DUAL FORMULATION NOTE:
% VNS for TRD is downstream of pharmaceutical treatment failure (patients reach VNS only after exhausting SSRIs, SNRIs, ECT) and upstream of biomedical model hegemony (VNS legitimizes neurobiological reductionism in depression treatment). The constraint family includes (1) psychiatric_pharmaceutical_extraction (the broader pharma-dependent patient lock-in), (2) biomedical_model_dominance (institutional marginalization of psychosocial approaches), and (3) medical_desperation_coercion (structural use of patient desperation to override autonomous decision-making). Each has distinct ε value reflecting different aspects of the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vns_implant_for_trd, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
