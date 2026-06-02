% ============================================================================
% CONSTRAINT STORY: mental_health_treatment_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mental_health_treatment_legitimacy, []).

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
 *   constraint_id: mental_health_treatment_legitimacy
 *   human_readable: Mental Health Treatment Legitimacy Constraint
 *   domain: healthcare/psychology/institutional_power
 *
 * SUMMARY:
 *   Mental health treatment legitimacy operates through a credentialing and
 *   reimbursement system that creates a dual structure: genuine coordination
 *   (preventing unqualified practitioners from causing harm) layered over
 *   institutional extraction (protecting psychiatric and pharmaceutical
 *   market share, suppressing effective alternative modalities, creating
 *   identity locks that prevent agents from seeking non-credentialed care).
 *   The constraint exhibits high perspectival variation because different
 *   agents experience the legitimacy requirement at radically different
 *   positions in the extraction pipeline. For powerless patients in acute
 *   crisis, the constraint appears as snare — forced into credentialed
 *   treatments with limited alternatives and maximum risk. For the
 *   psychiatric establishment, it appears as rope — pure coordination of
 *   standardized diagnoses and treatment protocols that they control. For
 *   alternative healers, it appears as identity-locked snare — legally
 *   prohibited from practicing and internalized as deserved disqualification.
 *   For the open mental health movement, it appears as scaffold — legitimate
 *   alternative pathways are building slowly with eventual sunset of
 *   psychiatric monopoly. The theater ratio (0.68) reflects that insurance
 *   prior authorization, DSM diagnostic categorization, and licensing review
 *   are substantially performative: they maintain the appearance of quality
 *   control while often delaying effective care or suppressing effective
 *   alternatives. The rising extractiveness trajectory (0.42 → 0.58 over 20
 *   years) reflects increasing pharmaceutical marketing, diagnostic
 *   inflation, insurance bureaucratization, and costs, suggesting the
 *   coordination floor is being steadily exceeded by new extraction layers.
 *
 * KEY AGENTS:
 *   - Patients Seeking Treatment: Primary victims (powerless/trapped) — face acute need with limited exit options; bear costs of treatment failures and delayed access
 *   - Psychiatric Establishment: Primary beneficiary (institutional/arbitrage) — controls legitimacy definition; captures income, prestige, and regulatory protection
 *   - Pharmaceutical Industry: Secondary beneficiary (powerful/mobile) — creates markets through diagnostic expansion; captures high margins; benefits from psychiatric legitimacy
 *   - Therapy Practitioners: Mixed victim/beneficiary (moderate/constrained) — constrained by licensing requirements but benefit from scarcity rents; genuine role in coordination
 *   - Alternative Healers: Victims (powerless/identity_locked) — legally suppressed and internalized as unqualified; prevented from practicing despite potential efficacy
 *   - Insurance Systems: Institutional actor (institutional/constrained) — maintains performative gatekeeping through prior authorization theater
 *   - Open Mental Health Movement: Organized actor (organized/constrained) — building alternative legitimacy pathways; exit path visible but constrained by institutional pressures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether legitimacy constraint solves coordination or extracts above coordination floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mental_health_treatment_legitimacy, 0.58).
domain_priors:suppression_score(mental_health_treatment_legitimacy, 0.65).
domain_priors:theater_ratio(mental_health_treatment_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mental_health_treatment_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(mental_health_treatment_legitimacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mental_health_treatment_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mental_health_treatment_legitimacy, tangled_rope).
narrative_ontology:human_readable(mental_health_treatment_legitimacy, "Mental Health Treatment Legitimacy Constraint").
narrative_ontology:topic_domain(mental_health_treatment_legitimacy, "healthcare/psychology/institutional_power").

domain_priors:requires_active_enforcement(mental_health_treatment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mental_health_treatment_legitimacy, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(mental_health_treatment_legitimacy, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(mental_health_treatment_legitimacy, medical_credentialing_systems).
narrative_ontology:constraint_victim(mental_health_treatment_legitimacy, patients_seeking_effective_treatment).
narrative_ontology:constraint_victim(mental_health_treatment_legitimacy, alternative_treatment_modalities).
narrative_ontology:constraint_victim(mental_health_treatment_legitimacy, community_mental_health_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT IN CRISIS (SNARE) — Faces acute mental health symptoms and limited exit options. Cannot arbitrate treatment legitimacy claims while experiencing cognitive distress. Forced to accept credentialed treatments or face legal/social consequences if self-treating. High suppression: licensing laws, insurance barriers, involuntary commitment mechanisms. Maximum experienced extraction — patient bears all risk of failed treatment while institutional actors capture fees and liability protection.
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THERAPY PRACTITIONER (TANGLED ROPE) — Constrained by licensing requirements, liability exposure, and insurance networks. Must align practice with established psychiatric models or lose access to funding/patients. However, also benefits from the legitimacy constraint — licensing creates barrier to entry that protects practitioners' market share and income stability. Genuine coordination function (standardization prevents dangerous unqualified providers) exists alongside asymmetric extraction (practitioners extract rents from licensing-created scarcity).
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PSYCHIATRIC ESTABLISHMENT (ROPE) — Primary beneficiary of the legitimacy constraint. Controls definition of what counts as 'real' treatment via licensing, diagnostic manuals (DSM), and insurance reimbursement. Experiences constraint as pure coordination: standardized diagnoses enable communication between providers and institutions. Arbitrage exit — can shift to alternative modalities if profitable but faces no barriers. Net flow of extraction runs toward this actor.
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL CORPORATION (TANGLED ROPE) — Powerful actor with high mobility (can shift markets, develop new drugs, lobby for indications). Benefits from legitimacy constraint — psychiatric diagnoses create markets for psychiatric medications. Genuine coordination role (medications provide real treatment options for some patients) exists alongside high extraction (drugs priced at monopoly levels, diagnostic criteria shaped to expand markets, long-term dependency created). Mobile exit option means lower experienced extraction than practitioners, but still snare-adjacent.
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSURANCE REIMBURSEMENT SYSTEM (PITON) — Maintains theater-heavy legitimacy checking (prior authorization, treatment codes, diagnostic justification) that creates performative gatekeeping. Theater ratio (0.68) reflects that most insurance review of mental health claims is administrative ritual: reviewers assess diagnoses against DSM code categories and approved treatment pathways, but do not evaluate actual treatment efficacy or appropriateness for individual patients. System persists through regulatory requirement and inertia even as empirical evidence suggests prior authorization delays harm outcomes. Constrained exit (cannot abandon legitimacy verification without regulatory change).
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE HEALER (SNARE / IDENTITY_LOCKED) — Faces structural trapping through licensing laws (cannot legally practice many healing modalities without credentials), financial barriers (no insurance reimbursement), and legal liability exposure (unlicensed practice charges). But also experiences identity lock: has internalized the delegitimization narrative. Sees self as 'unqualified' and psychiatric establishment as legitimate authority. Could exit through underground practice or lobby for legal reform, but identity frame makes exit unthinkable — would require abandoning professional identity. Combines trapped exit (legal barriers) with identity_locked mechanism (internalized delegitimization).
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: OPEN MENTAL HEALTH MOVEMENT (SCAFFOLD) — Organized actors (peer support networks, digital mental health platforms, community-based programs, harm reduction advocates) building alternative legitimacy pathways outside credentialing system. Genuine coordination function (peer support, community care) exists with reduced extraction relative to institutional psychiatry. Sunset clause: as digital and community modalities accumulate outcome data and social legitimacy, the psychiatric establishment's monopoly on legitimacy definition weakens. Constrained exit (still operate within legal/social pressure to validate against psychiatric standards, but exit path visible).
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED_ROPE) — From civilizational scope, the legitimacy constraint solves a genuine coordination problem: mental health treatment is genuinely complex, and unqualified practitioners create real risks. Some standardization is necessary and beneficial (sanitation in psychiatric facilities, medication safety, trauma-informed protocols). However, the constraint has degenerated into pure extraction above the necessary coordination floor. Psychiatric monopoly on legitimacy suppresses effective alternative modalities, inflates treatment costs, and creates identity locks that prevent agents from exploring treatments outside credentialed systems. The observer classifies this as tangled rope with high effective extraction (chi): genuine coordination below suppression floor, but substantial excess extraction above it.
constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mental_health_treatment_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mental_health_treatment_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mental_health_treatment_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mental_health_treatment_legitimacy, TR),
    TR >= 0.70.

:- end_tests(mental_health_treatment_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting multiple extraction layers. Psychiatric diagnoses create pharmaceutical markets (DSM diagnostic inflation); licensing creates practitioner scarcity rents; insurance prior authorization delays care (extraction of time/outcome); credentialing suppresses alternative modalities. However, not as extreme as pure snare (0.66+) because genuine coordination benefits exist: some standardization prevents unqualified practitioners from causing clear harms, psychiatric medications provide real benefits for some patients, treatment protocols do improve safety. The 0.58 reflects measured extraction above genuine coordination floor. Suppression (0.65): High. Legal barriers (unlicensed practice prohibitions), economic barriers (insurance reimbursement restricted to credentialed providers), social barriers (delegitimization of alternatives), and cognitive barriers (identity lock preventing agents from seeking non-credentialed care). Suppression increased over interval as credentialing requirements expanded and insurance bureaucratization deepened. Theater ratio (0.68): Moderately high. Insurance prior authorization is substantially ritual (reviewers match diagnoses to approved codes without assessing individual appropriateness). DSM categorization is partially performative (categories reflect institutional consensus more than neurobiological reality). Licensing exams are somewhat performative (test memorization of established views more than clinical judgment). However, not 100% theater — genuine safety testing and outcome tracking do occur at measurable levels.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival divergence because agents occupy radically different structural positions. For powerless patients, legitimacy requirement is an unbreakable constraint preventing alternatives (snare). For institutional psychiatry, it is a coordination mechanism they control and benefit from (rope). For alternative healers, it combines legal trap with internalized delegitimization (identity_locked snare). For organized open health advocates, it is a temporary obstacle with visible sunset (scaffold). For the analytical observer at civilizational scope, the gap reveals that the legitimacy constraint has degenerated beyond its coordination floor — it now extracts more than it coordinates (tangled rope with high chi). The pharmaceutical perspective shows how coordination functions (medications for severe illness) can be leveraged as cover for extraction (diagnostic inflation, dependency creation, monopoly pricing).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim position + exit options. Patients with trapped exit + victim status have high d (0.95) → high f(d) → high χ. Psychiatric establishment with arbitrage exit + beneficiary status have low d (0.05) → negative f(d) → negative χ (experienced as benefit, not extraction). Practitioners with constrained exit + mixed status have moderate-high d (0.55) → moderate f(d) → moderate χ. Alternative healers with trapped exit + victim status + identity_locked mechanism have high d (0.89) → high f(d) but offset by identity_lock internalization → paradoxically moderate experienced χ (identity lock reduces the felt extraction because the agent has internalized the constraint as deserved). Pharmaceutical corporations with powerful status and mobile exit have moderate d (0.60) derived from victim creation (patients in dependency) but balanced against genuine coordination function. Open health movement with organized/constrained has moderate d (0.50) with exit path visible. The scope modifier σ(S) = 1.0 (national scope), so χ = ε × f(d) × 1.0. National scope prevents amplification that would occur at global scope (σ=1.2) or dampening at local scope (σ=0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that it operates as tangled rope with degenerated coordination-to-extraction ratio. Genuine coordination function: preventing unqualified practitioners, standardizing safety protocols, creating diagnostic communication standards. Genuine coordination floor: probably 15-25% of the institutional overhead (licensing requirements, diagnostic standardization, some treatment protocols). Excess extraction: approximately 33-43% of the constraint's operation (pharmaceutical diagnostic inflation, insurance prior authorization delays, suppression of effective alternatives, identity locks preventing agents from exploring non-credentialed care). The remaining 25-35%: uncertain (could be either justified coordination costs or disguised extraction). The manifesto declares the constraint as tangled rope with χ ≈ 0.50-0.65 (depending on agent perspective and scope), confirming this is not pure coordination (rope) nor pure extraction (snare), but genuine hybrid with excess extraction above coordination floor. The rising theater_ratio (0.52 → 0.68 over 20 years) suggests the ratio is shifting toward performance and away from function — possible indicator of degradation toward snare classification if trend continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_measurement_ambiguity,
    'What counts as ''effective'' mental health treatment, and who gets to decide the measurement criteria?',
    'Comparison of outcome metrics: psychiatric establishment (remission of DSM symptoms) vs patient-centered outcomes (functional capacity, subjective wellbeing, side effect burden) vs alternative modalities (spiritual growth, relational healing, community integration). Track divergence in rankings.',
    'If psychiatric metrics are truly objective: legitimacy constraint may be justified (specialists know best). If metrics are socially constructed and exclude alternative measures: constraint is revealed as self-serving gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_measurement_ambiguity, conceptual, 'Ambiguity in efficacy measurement criteria and decision-making authority').

omega_variable(
    identity_lock_mechanism,
    'Is the alternative healer''s acceptance of delegitimization structural (genuine legal/economic barriers) or internalized (identity fusion with the ''unqualified'' frame)?',
    'Ethnographic study of unlicensed practitioners: track those who frame illegality as unjust constraint vs those who frame it as deserved gatekeeping. Analyze licensing exam pass rates and career transition patterns.',
    'If primarily structural: reclassify exit_options to trapped (not identity_locked). Remedy is legal/economic reform. If primarily internalized: identity-lock classification confirmed. Remedy requires identity-frame breaking (consciousness raising, peer community building).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether alternative healer suppression is structural or internalized').

omega_variable(
    pharmaceutical_coordination_floor,
    'What proportion of psychiatric medication use represents genuine therapeutic benefit vs dependency creation vs profit-driven overprescription?',
    'Longitudinal outcome tracking: patients on medications vs off medications (controlling for severity) over 5-10 year horizons. Compare long-term recovery rates (not symptom suppression) across modalities.',
    'If coordination floor > 70%: pharmaceutical extraction is moderate, constraint is justified. If < 40%: pharmaceutical component is primarily extractive, not coordinating — reclassify to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_coordination_floor, empirical, 'Proportion of pharmaceutical use that represents genuine coordination vs extraction').

omega_variable(
    alternative_modality_suppression_mechanism,
    'Is the suppression of alternative modalities (acupuncture, meditation, community healing, peer support) based on genuine lack of efficacy evidence or on institutional gatekeeping?',
    'Meta-analysis of outcome studies: control for publication bias and funding source. Compare success rates of alternative modalities in contexts where they have institutional support vs where they are suppressed.',
    'If efficacy genuinely poor: suppression is protective. If efficacy comparable or superior: suppression is pure gatekeeping. Determines whether legitimacy constraint is mountain (genuine danger) or snare (institutional extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_modality_suppression_mechanism, empirical, 'Whether alternative modality suppression is evidence-based or gatekeeping-based').

omega_variable(
    insurance_prior_authorization_efficacy,
    'Does the insurance prior authorization ritual (theater_ratio 0.68) actually prevent harmful treatments or just delay access to beneficial care?',
    'Outcome tracking: compare patients approved on first authorization vs denied/delayed. Track harms: suicide during waiting period, crisis escalation, treatment abandonment.',
    'If prior auth prevents net harm: theater is justified coordination cost. If delays cause more harm than prevented: ritual is pure extraction (snare from patient perspective). Theater ratio may actually be underestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insurance_prior_authorization_efficacy, empirical, 'Whether insurance prior authorization prevents harm or just delays care').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mental_health_treatment_legitimacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mhtl_tr_t0, mental_health_treatment_legitimacy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mhtl_tr_t10, mental_health_treatment_legitimacy, theater_ratio, 10, 0.6).
narrative_ontology:measurement(mhtl_tr_t20, mental_health_treatment_legitimacy, theater_ratio, 20, 0.68).
narrative_ontology:measurement(mhtl_tr_t5, mental_health_treatment_legitimacy, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(mhtl_be_t0, mental_health_treatment_legitimacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mhtl_be_t10, mental_health_treatment_legitimacy, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(mhtl_be_t20, mental_health_treatment_legitimacy, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mhtl_be_t5, mental_health_treatment_legitimacy, base_extractiveness, 5, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mental_health_treatment_legitimacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mental_health_treatment_legitimacy, 0.22).
narrative_ontology:affects_constraint(mental_health_treatment_legitimacy, pharmaceutical_market_gatekeeping).
narrative_ontology:affects_constraint(mental_health_treatment_legitimacy, psychiatric_diagnostic_inflation).
narrative_ontology:affects_constraint(mental_health_treatment_legitimacy, healthcare_insurance_access_barriers).

% DUAL FORMULATION NOTE:
% Mental health legitimacy decomposes into multiple structurally distinct constraints: (1) practitioner safety standards (genuine coordination, low ε ≈ 0.15), (2) pharmaceutical marketing through diagnostic expansion (pure extraction, high ε ≈ 0.68), (3) insurance administrative gatekeeping (mixed, ε ≈ 0.45). This story treats the unified constraint at institutional level. Decomposed stories track each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mental_health_treatment_legitimacy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
