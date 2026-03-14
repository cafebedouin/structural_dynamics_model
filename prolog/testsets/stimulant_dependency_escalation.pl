% ============================================================================
% CONSTRAINT STORY: stimulant_dependency_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stimulant_dependency_escalation, []).

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
 *   constraint_id: stimulant_dependency_escalation
 *   human_readable: Stimulant Dependency Escalation Trap
 *   domain: neurobiology/pharmacology/addiction
 *
 * SUMMARY:
 *   Stimulant dependency escalation is a neurobiological-institutional
 *   constraint that traps individuals through a combination of
 *   pharmacological mechanism (dopamine receptor tolerance) and institutional
 *   structures (prescription practices, clinical identity, regulatory
 *   theater) that amplify and legitimize the escalation. The constraint
 *   exhibits multiple structural forms depending on the observer's position:
 *   a natural law of pharmacodynamics (mountain, false summit), a regulatory
 *   performance ritual (piton), pure extraction from the dependent user
 *   (snare from powerless perspective), mixed coordination and extraction
 *   from the prescriber (tangled rope), identity-locked professional
 *   commitment (snare from clinical perspective), and institutional arbitrage
 *   for manufacturers (rope). The escalation dynamic is not purely biological
 *   — it is amplified by prescribing norms that treat dose increases as
 *   evidence of therapeutic appropriateness rather than warning signs of
 *   tolerance. The constraint's theater ratio (0.58) reflects that clinical
 *   symptom checklists, dosage titration protocols, and regulatory forms
 *   perform significant compliance theater without effectively preventing
 *   dependency-driven escalation. The measurements show extractiveness and
 *   theater rising together from year 0 to year 10, indicating that as the
 *   institutional apparatus for managing ADHD matured (more prescribers,
 *   refined diagnostic criteria, expanded clinical networks), the
 *   constraint's extractive grip actually tightened rather than loosened.
 *
 * KEY AGENTS:
 *   - Dependent User: Primary victim (powerless/trapped) — bears full cost of escalation through neurotoxicity, financial depletion, and withdrawal suppression
 *   - Neurobiological Self: Secondary victim (powerless/trapped at generational horizon) — dopaminergic substrate is rewired, making lower-dose preference and abstinence unattainable without months of recovery
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — captures escalation benefit through increased dosage volumes and lifetime customer value
 *   - Prescribing Physician: Mixed actor (moderate/constrained) — both coordinates genuine symptom treatment AND extracts via insufficient dependency monitoring
 *   - ADHD Clinician Profession: Secondary actor (moderate/identity_locked) — professionally constituted through stimulant-first paradigm; exit to behavioral approaches is structurally possible but identity-locked
 *   - FDA/DEA Regulatory Apparatus: Institutional actor (institutional/arbitrage) — maintains performative control structures with minimal enforcement capacity; benefits from appearance of regulation without bearing costs of actual prevention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stimulant_dependency_escalation, 0.68).
domain_priors:suppression_score(stimulant_dependency_escalation, 0.72).
domain_priors:theater_ratio(stimulant_dependency_escalation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stimulant_dependency_escalation, extractiveness, 0.68).
narrative_ontology:constraint_metric(stimulant_dependency_escalation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(stimulant_dependency_escalation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stimulant_dependency_escalation, snare).
narrative_ontology:human_readable(stimulant_dependency_escalation, "Stimulant Dependency Escalation Trap").
narrative_ontology:topic_domain(stimulant_dependency_escalation, "neurobiology/pharmacology/addiction").

domain_priors:requires_active_enforcement(stimulant_dependency_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stimulant_dependency_escalation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(stimulant_dependency_escalation, prescribing_physicians).
narrative_ontology:constraint_victim(stimulant_dependency_escalation, dependent_users).
narrative_ontology:constraint_victim(stimulant_dependency_escalation, neurobiological_self).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT USER (SNARE) — Trapped by neurobiological changes that make abstinence acutely aversive. Initial use was often voluntary (prescribed or self-administered), but the constraint mechanism ensures escalating doses are required to avoid withdrawal dysphoria. The user bears full cost: neurotoxicity, financial depletion, social isolation, organ damage. Zero coordination benefit. Maximum suppression — the body's own altered reward chemistry becomes the enforcement mechanism.
constraint_indexing:constraint_classification(stimulant_dependency_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NEUROBIOLOGICAL SELF (SNARE, GENERATIONAL) — At longer timescales, the constraint operates on the user's dopaminergic and glutamatergic systems. Chronic stimulant use produces D1/D2 receptor downregulation, blunted baseline reward capacity, and enhanced sensitization to drug-associated cues. The neurobiological 'self' that would prefer lower doses or abstinence is structurally suppressed by the altered neural substrate. From this perspective, the user is not choosing escalation — the constraint enforces it through rewiring. Exit requires rebuilding reward circuitry, which takes months to years (and feels like anhedonia during that interval).
constraint_indexing:constraint_classification(stimulant_dependency_escalation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Manufactures prescription stimulants (methylphenidate, amphetamine salts, lisdexamfetamine) nominally for ADHD treatment. The escalation dynamic is a coordinating feature: dosage titration reflects genuine therapeutic adjustment AND creates increasing dependence. Manufacturer benefits from the constraint: higher lifetime dosages = higher sales volume. The constraint appears to them as efficient market coordination — matching dose to need. Extraction runs toward them; they have full arbitrage optionality (exit by stopping production, but that is not salient because they benefit).
constraint_indexing:constraint_classification(stimulant_dependency_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRESCRIBING PHYSICIAN (TANGLED ROPE) — Experiences the constraint as mixed coordination and extraction. Genuine coordination function: matching stimulant dosage to ADHD symptom severity benefits both physician (successful treatment) and patient (symptom relief). But escalation mechanism creates extraction: the physician's liability exposure if the patient develops dependence is limited (off-label responsibility is diffuse); their reputation and income are tied to patient satisfaction and outcome metrics that reward symptom control regardless of long-term dependency trajectory. Constrained by institutional incentives (medical liability, practice revenue, time pressure during appointments). The physician both benefits (positive outcome ratings, satisfied patients during the therapeutic window) and extracts from the patient (insufficient monitoring for dependence signs, insufficient tapering protocols).
constraint_indexing:constraint_classification(stimulant_dependency_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ADHD CLINICIAN PROFESSION (SNARE, identity_locked) — The professional identity of ADHD clinicians is partially constituted through the stimulant-centered treatment model. Decades of training, research productivity, and professional reputation have been built on pharmacological intervention (titration protocols, dosage adjustments, symptom checklists tied to pharmaceutical response). Clinical psychology and psychiatry have internalized stimulants as THE validated approach. Exit from stimulant-first paradigms would require abandoning not just a treatment modality but the identity through which these professionals see their own expertise and success. The constraint is identity-locked: the profession *could* transition to behavioral interventions, mindfulness training, or neurofeedback-first approaches (structurally mobile), but the cognitive frame that constitutes clinical credibility makes this shift unthinkable from within. The victim here is the profession's own capacity for innovation — locked into a single-mechanism solution space.
constraint_indexing:constraint_classification(stimulant_dependency_escalation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY APPARATUS (PITON) — The FDA/DEA regulatory framework for stimulant scheduling and prescription oversight performs a theatrical compliance function rather than an effective protection mechanism. Schedule II classification theoretically prevents escalation through non-refillable prescriptions and limited quantity thresholds. But: (1) prescribers routinely circumvent limits via new prescriptions before prior ones expire, (2) the regulatory apparatus has capacity to monitor only aggregate prescribing trends, not individual dependency trajectories, (3) DEA enforcement is sparse relative to the volume of prescriptions issued. Theater ratio is high because the regulatory ritual (prescription forms, DEA numbers, periodic audits) persists despite marginal effectiveness at preventing dependency. The constraint maintains itself through institutional inertia — alternatives (real-time prescription databases, mandatory tapering protocols, addiction screening at each visit) exist but would require regulatory restructuring. The piton persists because exiting the current framework requires admitting that the existing apparatus is largely performative.
constraint_indexing:constraint_classification(stimulant_dependency_escalation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, stimulant escalation reflects immutable properties of dopaminergic pharmacology: repeated activation of D1/D2 receptors produces tolerance (receptors downregulate), requiring higher doses to achieve equivalent neural effect. This escalation is inherent to the molecule's mechanism, not contingent on institutional arrangements. The 'constraint' appears as a natural law — a mathematical consequence of receptor kinetics and adaptation, not a social extraction mechanism. However, the structural data contradicts this classification. The escalation magnitude and trajectory are heavily mediated by prescribing norms, psychiatric framing (disease model vs behavioral model), regulatory laxity, and pharmaceutical marketing. Non-human animals self-administering stimulants show escalation, but human clinical escalation is significantly faster and more severe — suggesting institutional/social amplification of the biological baseline. The analytical mountain perspective is a false summit: naturalization of what is partially contingent institutional arrangement.
constraint_indexing:constraint_classification(stimulant_dependency_escalation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stimulant_dependency_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stimulant_dependency_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stimulant_dependency_escalation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stimulant_dependency_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stimulant_dependency_escalation, TR),
    TR >= 0.70.

:- end_tests(stimulant_dependency_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The dependent user experiences severe extraction — escalating doses are required to avoid withdrawal dysphoria, meaning the user must continuously increase consumption to maintain baseline functionality. The pharmacological mechanism is non-negotiable: dopamine receptors downregulate with chronic stimulation. But the extractiveness magnitude is amplified by institutional factors: prescribers are incentivized to titrate doses upward (patient satisfaction metrics, symptom-response attribution), pharmaceutical marketing normalizes high-dose treatment, and regulatory oversight is insufficient to detect and prevent individual dependency trajectories. The value reflects that roughly 30-40% of people initiating prescription stimulants develop escalation patterns exceeding clinical guidelines, and for those who do, the extraction is nearly absolute (they cannot access lower doses without severe withdrawal). Suppression (0.72): High. Multiple suppression layers: (1) Neurobiological — withdrawal dysphoria, anhedonia, and fatigue make abstinence acutely unbearable, (2) Institutional — medical models frame escalation as symptom-responsive titration rather than dependency, (3) Social — ADHD diagnosis is often necessary for academic/occupational accommodation, creating identity tie to stimulant treatment, (4) Economic — alternative treatments (behavioral therapy, coaching) are expensive and uninsured while prescriptions are subsidized. Suppression is highest at the dependent-user perspective (trapped, not merely constrained). Theater ratio (0.58): Moderate-high. Significant performative content in the clinical apparatus: symptom rating scales (ADHD Rating Scale, Connor Scale) are designed to detect symptom severity but are easily gamed by patients seeking dose increases; dosage titration protocols perform the function of therapeutic matching while actually enabling escalation; prescriber documentation often attributes dose increases to 'diagnostic confirmation' (ADHD was more severe than initially assessed) rather than to tolerance. Regulatory forms (DEA triplicate prescriptions, state prescription databases) perform control theater with minimal actual prevention — prescribers circumvent them through rapid prescription cycling or multi-provider strategies.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification (perspectives 1-2) reflects the dependent user's and neurobiological substrate's experience: maximum suppression, zero coordination benefit, no exit option. The rope classification (perspective 3) reflects the manufacturer's experience: pure coordination benefit with low suppression cost. The tangled rope classification (perspective 4) reflects the prescriber's mixed experience: genuine symptom coordination alongside asymmetric extraction via insufficient monitoring. The snare with identity lock (perspective 5) reflects the profession's experience: the constraint is experienced as snare (pure extraction of alternative treatment pathways from the profession's cognitive toolkit) because exit is identity-locked, not because the structural barriers are materially insurmountable. The piton classification (perspective 6) reflects the regulatory apparatus's experience: the constraint persists through theatrical compliance despite functional ineffectiveness. The mountain classification (perspective 7) is a false summit — it naturalizes the escalation as pharmacological law while ignoring institutional amplification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps each agent's structural position — their power level, exit options, and beneficiary/victim status — to their experienced extractiveness. The dependent user (powerless/trapped/victim) derives d ≈ 0.95, yielding maximum f(d) ≈ 1.42, so they experience χ = 0.68 × 1.42 × (scope modifier) — very high effective extraction. The manufacturer (institutional/arbitrage/beneficiary) derives d ≈ 0.05, yielding f(d) ≈ -0.12, so they experience χ = 0.68 × (-0.12) × 1.0 — negative effective extraction (the constraint subsidizes them). The prescriber (moderate/constrained, mixed beneficiary-victim) derives d ≈ 0.50-0.55, yielding moderate f(d) ≈ 0.65-0.75, so they experience χ ≈ 0.44-0.51 — moderate extraction (they both benefit and pay). The regulatory apparatus (institutional/arbitrage despite performative function) derives d ≈ 0.00-0.10, yielding f(d) ≈ -0.12 to 0.05, so they experience χ ≈ -0.08 to 0.03 — near-zero or negative (their institutional position is maintained without bearing extraction costs).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE RESOLUTION WITHOUT MANDATROPHY PARADOX: The constraint is classified as snare from multiple perspectives, particularly from the dependent user and profession perspectives. The mandatrophy question is: does this high-extraction constraint actually coordinate something genuine, or is it pure extraction? The answer is that it coordinates different things at different perspectives. From the manufacturer's view, it coordinates product-customer loyalty (rope). From the prescriber's view, it coordinates symptom-response matching with problematic escalation (tangled rope). From the user's view, it coordinates withdrawal prevention with increasing dependency (snare — no genuine coordination benefit to the victim). The mandatrophy is resolved by accepting that coordination and extraction are NOT inverses at the constraint level — the same mechanism (dose escalation) can be genuine therapeutic coordination from one perspective and pure extraction from another. The constraint is legitimately snare from the powerless user perspective and legitimately rope from the institutional manufacturer perspective. This is not a paradox; it is the normal state of asymmetric constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_vs_dependency_dosage_threshold,
    'Where is the threshold between therapeutic dosage adjustment and dependency-driven escalation? Is the same dose increase justified by ADHD symptom progression or by neurobiological tolerance?',
    'Longitudinal tracking of ADHD symptom improvement vs dosage increase rates; comparison of symptom severity in patients stabilized at lower doses vs escalating patients; biomarkers of tolerance (dopamine transporter density via PET) vs therapeutic response (behavioral improvement via standardized scales)',
    'If threshold is identifiable: escalation can be distinguished from appropriate titration, enabling prescriber oversight. If threshold is invisible: ''therapeutic need'' becomes unfalsifiable cover for dependency-driven escalation, and snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_vs_dependency_dosage_threshold, empirical, 'Distinction between therapeutic dosage adjustment and tolerance-driven escalation').

omega_variable(
    prescriber_knowledge_of_dependency_mechanism,
    'Do prescribing physicians understand the neurobiological escalation mechanism, or do they interpret dose increases as evidence of treatment effectiveness?',
    'Survey of prescriber knowledge of dopamine receptor downregulation, tolerance timelines, and dependency risk; comparison of prescribing behavior between providers trained on tolerance mechanisms vs those trained on symptom-responsive dosing; analysis of whether prescribers document escalation explicitly or attribute it to symptom severity',
    'If prescribers understand the mechanism: tangled rope classification is correct (they knowingly extract via escalation). If prescribers genuinely believe escalation reflects therapeutic need: they are unwitting conduits for the snare (captured by false framing), and the constraint has an additional layer of cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prescriber_knowledge_of_dependency_mechanism, empirical, 'Prescriber understanding of pharmacological escalation mechanisms').

omega_variable(
    exit_possibility_without_medical_supervision,
    'Can dependent users achieve stable tapering and abstinence without medical intervention? Is medical support necessary to exit, or does the ''need for tapering protocol'' constitute additional artificial suppression?',
    'Comparative analysis of self-directed tapering success rates vs medically-supervised tapering; tracking of withdrawal severity and anhedonia duration in self-tapered vs supervised groups; assessment of whether medical involvement increases tapering success or merely lengthens the process',
    'If self-directed tapering is viable: the suppression mechanism is primarily biological (withdrawal), and the constraint is high but potentially escapable. If self-directed tapering fails catastrophically: medical dependence becomes structural, and suppression is amplified by institutional gating of exit protocols.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_possibility_without_medical_supervision, empirical, 'Feasibility of self-directed versus medically-supervised dependency exit').

omega_variable(
    identity_lock_persistence_post_profession_change,
    'If ADHD clinicians transition to non-pharmacological treatment modalities, does their professional identity require recovery, or do they adapt to new models while maintaining clinical coherence?',
    'Longitudinal interviews with clinicians who transitioned from pharmacology-first to behavioral/cognitive approaches; assessment of identity continuity, professional satisfaction, and self-perceived expertise before and after transition',
    'If identity recovery is required: the profession is genuinely identity-locked, and perspective 5 classification is validated. If identity adapts smoothly: the lock is weaker than perceived, and the snare may be partially optional (constrained rather than identity-locked).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_profession_change, empirical, 'Identity persistence through professional reorientation from pharmacological to behavioral paradigms').

omega_variable(
    regulatory_apparatus_actual_deterrence_capacity,
    'Do the Schedule II controls (non-refillable prescriptions, quantity limits, DEA monitoring) actually prevent escalation, or are they purely performative?',
    'Analysis of prescription patterns before and after regulatory tightening (e.g., DEA scheduling changes); comparison of escalation rates in jurisdictions with tight vs loose prescription database monitoring; assessment of whether prescribers'' circumvention strategies (rapid prescription cycling, multi-provider prescribing) defeat the intended control mechanism',
    'If controls are effective: piton classification is incorrect, and the apparatus performs real constraint function (reclassify as rope or tangled rope). If controls are defeated: piton classification is validated, and the theater ratio remains high despite regulatory activity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_apparatus_actual_deterrence_capacity, empirical, 'Effectiveness of Schedule II controls in preventing escalation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stimulant_dependency_escalation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stim_dep_tr_t0, stimulant_dependency_escalation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stim_dep_tr_t2, stimulant_dependency_escalation, theater_ratio, 2, 0.5).
narrative_ontology:measurement(stim_dep_tr_t5, stimulant_dependency_escalation, theater_ratio, 5, 0.56).
narrative_ontology:measurement(stim_dep_tr_t10, stimulant_dependency_escalation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(stim_dep_be_t0, stimulant_dependency_escalation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stim_dep_be_t2, stimulant_dependency_escalation, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(stim_dep_be_t5, stimulant_dependency_escalation, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(stim_dep_be_t10, stimulant_dependency_escalation, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stimulant_dependency_escalation, resource_allocation).
narrative_ontology:affects_constraint(stimulant_dependency_escalation, adhd_diagnostic_criterion_expansion).
narrative_ontology:affects_constraint(stimulant_dependency_escalation, pharmaceutical_marketing_directtoconsumer).
narrative_ontology:affects_constraint(stimulant_dependency_escalation, prescription_database_oversight).

% DUAL FORMULATION NOTE:
% Stimulant dependency escalation is downstream of ADHD diagnostic criterion expansion (broader eligibility pool) and upstream of prescription database oversight (detection and prevention mechanisms). The three stories in this family have different ε values reflecting different observables: diagnostic expansion (ε=0.45, tangled rope — genuine need coordination + asymptomatic prescribing), escalation mechanism (ε=0.68, snare — neurobiological lock-in), and oversight failure (ε=0.38, piton — performative regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stimulant_dependency_escalation, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
