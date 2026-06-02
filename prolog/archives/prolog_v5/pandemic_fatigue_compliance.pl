% ============================================================================
% CONSTRAINT STORY: pandemic_fatigue_compliance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pandemic_fatigue_compliance, []).

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
 *   constraint_id: pandemic_fatigue_compliance
 *   human_readable: Pandemic Fatigue Compliance Constraint
 *   domain: public_health/behavioral_dynamics
 *
 * SUMMARY:
 *   Pandemic fatigue compliance represents a structural constraint where
 *   institutional authority maintains behavioral mandates (mask-wearing,
 *   vaccination, isolation, testing protocols) beyond the point where the
 *   epidemiological justification has degraded. The constraint operates
 *   through suppression mechanisms (legal enforcement, employment
 *   discrimination, social stigma, internalized guilt) that sustain
 *   compliance even as the target population's willingness to comply has
 *   diminished. Over the pandemic's lifecycle, the extractiveness increased
 *   from moderate (0.35, where genuine coordination with public health
 *   existed) to high (0.62, where behavioral compliance serves institutional
 *   authority interests rather than population health). Theater ratio
 *   similarly rose from 0.42 (protocols had clear epidemiological function)
 *   to 0.75 (performative maintenance of rituals disconnected from outcome).
 *   The constraint exhibits all six classification types depending on
 *   observational position: powerless citizens experience snare (trapped in
 *   compliance loop), non-compliant holdouts experience tangled rope (mixed
 *   extraction and coordination), authorities experience rope (pure
 *   coordination benefit), the scientific consensus experiences scaffold
 *   (temporary with sunset), the compliance apparatus itself experiences
 *   piton (degraded ritual), and the analytical observer risks mountain
 *   classification (naturalizing social choices as biological
 *   inevitabilities).
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) — faces suppression through legal/social enforcement; cannot exit without cost; bears extraction through behavioral compliance with declining epidemiological justification
 *   - Non-Compliant Minority: Secondary victim (moderate/constrained) — faces discrimination, social stigma, employment barriers; also benefits from coordination when others comply; asymmetric extraction with embedded coordination
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — maintains institutional legitimacy and control through mandate enforcement; can pivot policy without loss; experiences constraint as coordination function
 *   - Scientific/Medical Consensus: Organized actor (organized/mobile) — sees compliance as temporary with explicit sunset criteria; has exit pathway as population immunity accumulates; provides rationale for scaffold classification
 *   - Compliance Apparatus (employers, public venues, enforcement agencies): Secondary beneficiary (institutional/arbitrage) — benefits from simple compliance rules that don't require real-time epidemiological judgment; maintains institutional routines
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks confusing biological constraints with social policy choices; sees apparent immutability that is actually institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pandemic_fatigue_compliance, 0.58).
domain_priors:suppression_score(pandemic_fatigue_compliance, 0.65).
domain_priors:theater_ratio(pandemic_fatigue_compliance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pandemic_fatigue_compliance, extractiveness, 0.58).
narrative_ontology:constraint_metric(pandemic_fatigue_compliance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pandemic_fatigue_compliance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pandemic_fatigue_compliance, snare).
narrative_ontology:human_readable(pandemic_fatigue_compliance, "Pandemic Fatigue Compliance Constraint").
narrative_ontology:topic_domain(pandemic_fatigue_compliance, "public_health/behavioral_dynamics").

domain_priors:requires_active_enforcement(pandemic_fatigue_compliance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pandemic_fatigue_compliance, institutional_authority).
narrative_ontology:constraint_beneficiary(pandemic_fatigue_compliance, compliance_theater_actors).
narrative_ontology:constraint_victim(pandemic_fatigue_compliance, general_population).
narrative_ontology:constraint_victim(pandemic_fatigue_compliance, public_health_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPLIANT CITIZEN (SNARE) — Faces maximum suppression through institutional mandates, peer pressure, and internalized guilt. Cannot exit compliance without social/professional cost. Trapped between pandemic threat perception and compliance fatigue. Extraction runs toward institutional authority; citizen experiences high chi through f(d) multiplication of trapped exit + victim status.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-COMPLIANT HOLDOUT (TANGLED ROPE) — Bears extraction (social stigma, potential employment/housing discrimination) but also benefits from genuine coordination (when others comply, ambient risk environment changes). Constrained by costs of defection but can exercise some agency. Asymmetric extraction with embedded coordination function.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences constraint as pure coordination problem: communicating clear guidance, establishing norms, ensuring population compliance with evidence-based measures. Net beneficiary with arbitrage exit (can pivot policy without loss). Extraction runs toward this agent; they perceive genuine coordination function.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SCIENTIFIC MEDICAL CONSENSUS (SCAFFOLD) — Organized epistemic community sees compliance as temporary coordination with explicit sunset: vaccine effectiveness, variant evolution, population immunity threshold, and transition to endemic phase provide exit criteria. Declining suppression as population gains immunity and behavioral alternatives emerge. Chi compressed by mobile exit options and sunset framing.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE THEATER APPARATUS (PITON) — Performative rituals (mask mandates when baseline population immunity is sufficient, quarantine protocols with minimal epidemiological benefit, vaccine passports after widespread natural immunity) persist through institutional inertia despite degraded functional justification. Theater ratio elevated by mismatch between maintained protocols and changed epidemiological context. Benefits some institutional actors but no longer solves the original coordination problem.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational view, some epidemiological reality is immutable: respiratory pathogens spread through human contact, vaccination confers protection, immunity accumulates over time. These are structural facts. However, this perspective risks naturalizing contingent policy choices (duration of mandates, stringency levels, compliance enforcement mechanisms) as biological inevitabilities. The engine will flag this as a false summit — the extractive constraint is social, not biological.
constraint_indexing:constraint_classification(pandemic_fatigue_compliance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pandemic_fatigue_compliance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pandemic_fatigue_compliance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pandemic_fatigue_compliance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pandemic_fatigue_compliance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pandemic_fatigue_compliance, TR),
    TR >= 0.70.

:- end_tests(pandemic_fatigue_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Initial phase (0.35) involved genuine coordination — population and authorities aligned on disease control. Mid-phase (0.48) showed extraction mounting as fatigue accumulated but mandates persisted. Final measurement (0.62) reflects high asymmetry: compliance required despite weakened epidemiological justification. Suppression (0.65): Moderately high and durable. Structural barriers include employment discrimination, educational/travel restrictions, and legal penalties. Internalized suppression (guilt, identity fusion with compliance role) may constitute 30-40% of measured suppression. Theater ratio (0.75 at endpoint): Elevated. Early protocols (masking in high-transmission contexts) had clear function. Late protocols (masking in low-transmission settings, vaccine passports after widespread natural immunity, quarantines with minimal epidemiological impact) are substantially performative — maintained through institutional inertia and risk-aversion rather than epidemiological evidence. The rise from 0.42 to 0.75 tracks the decoupling of policy from epidemiological conditions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The compliant citizen sees a snare (high suppression, no exit, extraction clear). The non-compliant sees tangled rope (extraction real but coordination benefit exists for them when others comply). Authorities see rope (pure coordination value, they are not extracting — in their framing). The scientific community sees scaffold (temporary, with sunset criteria like herd immunity or variant evolution). The compliance apparatus sees piton (the ritual persists even though its function has degraded). The analytical observer risks mountain (confusing 'respiratory viruses exist' with 'these specific mandates are natural law'). The perspectival gaps reveal structural asymmetries: citizens and authorities have radically different access to mandate reversal options, victims and beneficiaries have inverted directionality, and the transition from coordination to extraction is measurable but denied by those who benefit from the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values derive from structural position and exit capacity. Powerless citizens with trapped exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42, generating high chi. They are victims with no exit pathway. Non-compliant holdouts with constrained exit (d ≈ 0.65) experience f(d) ≈ 1.00, moderate chi — they have some agency but face penalties. Public health authorities with arbitrage exit (d ≈ 0.15) experience f(d) ≈ -0.01, negative chi — they are net beneficiaries with complete policy flexibility. The scientific consensus with mobile exit options (d ≈ 0.50) experiences f(d) ≈ 0.65, compressed chi — they can exit via changed criteria. The directionality pipeline automatically computes these from beneficiary/victim declarations plus power and exit values; no override necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that all six types coexist in temporal progression. EARLY PHASE (T=0-4): Rope-dominant — genuine coordination between population and authorities around disease control, low extraction, high function. MID PHASE (T=4-12): Tangled Rope-dominant — coordination function persists (reducing baseline population risk) but extraction rises as fatigue accumulates and mandates continue despite declining threat. LATE PHASE (T=12-24): Snare-dominant for powerless agents — extraction maximizes as suppression mechanisms lock in despite epidemiological justification having degraded. Scaffold describes the theoretical endpoint — authorities and scientific consensus frame mandates as temporary with clear exit criteria (endemic phase, herd immunity threshold, vaccine effectiveness validation). Piton describes the institutional reality — compliance apparatus maintains rituals even after functional justification has degraded, suggesting the sunset is aspirational rather than real. Mountain describes the false summit — the analytical observer risks naturalizing social choices as biological necessity. No single type is 'correct' — the constraint's type changes as conditions change and power asymmetries shift. This is a diagnostic exemplar of mandatrophy resolution through temporal decomposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fatigue_accumulation_mechanism,
    'Is pandemic fatigue a discrete behavioral threshold (compliance suddenly collapses) or a continuous decay process (adherence monotonically declines)?',
    'Longitudinal compliance data at weekly granularity; statistical testing for change-point detection vs linear decline; behavioral economic experiments on fatigue accumulation',
    'If threshold-based: compliance enforcement can be strategically timed to prevent cliff. If continuous decay: extraction mechanisms must adapt or face inevitable mass non-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fatigue_accumulation_mechanism, empirical, 'Fatigue as threshold vs continuous decay process').

omega_variable(
    suppression_internalization_ambiguity,
    'What proportion of measured suppression is structural (external barriers to non-compliance) vs internalized (agent has accepted compliance frame)?',
    'Post-mandate compliance tracking (do compliant agents maintain behavior after institutional enforcement ends?); attitudinal surveys controlling for social desirability bias; longitudinal identity-fusion measures',
    'If mostly structural: suppression will drop sharply when mandates end. If mostly internalized: compliance persists and becomes identity-locked even after external pressure ceases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural vs internalized suppression balance').

omega_variable(
    endemic_transition_criteria_ambiguity,
    'What epidemiological/immunological thresholds justify end of mandate-based compliance enforcement?',
    'Expert panel consensus on threshold values; international comparative analysis of different transition timelines; post-transition outcome tracking (did healthcare capacity remain adequate, did mortality surge)',
    'If thresholds are objective: scaffold sunset is real and compliance extraction declines predictably. If subjective/political: sunset is arbitrary and extraction persists as pure coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endemic_transition_criteria_ambiguity, conceptual, 'Definition of endemic phase completion criteria').

omega_variable(
    identity_lock_vs_constrained_distinction,
    'For agents maintaining compliance post-pandemic: Is continued adherence identity-locked (self-concept fused with compliance role) or constrained (external costs of defection remain)?',
    'Qualitative interviews: can agents articulate reasons for compliance independent of identity? Do they see themselves differently post-pandemic? Behavioral switching tests (do they maintain compliance in contexts with zero social observation)?',
    'If identity-locked: compliance becomes intergenerational norm even when suppression evaporates. If constrained: compliance collapses when external enforcement ends.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_distinction, empirical, 'Identity lock vs constrained compliance in post-pandemic period').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pandemic_fatigue_compliance, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(panfat_tr_t0, pandemic_fatigue_compliance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(panfat_tr_t6, pandemic_fatigue_compliance, theater_ratio, 6, 0.58).
narrative_ontology:measurement(panfat_tr_t12, pandemic_fatigue_compliance, theater_ratio, 12, 0.68).
narrative_ontology:measurement(panfat_tr_t18, pandemic_fatigue_compliance, theater_ratio, 18, 0.75).

% Extraction over time
narrative_ontology:measurement(panfat_be_t0, pandemic_fatigue_compliance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(panfat_be_t6, pandemic_fatigue_compliance, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(panfat_be_t12, pandemic_fatigue_compliance, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(panfat_be_t18, pandemic_fatigue_compliance, base_extractiveness, 18, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pandemic_fatigue_compliance, resource_allocation).
narrative_ontology:affects_constraint(pandemic_fatigue_compliance, vaccine_mandate_compliance).
narrative_ontology:affects_constraint(pandemic_fatigue_compliance, institutional_authority_legitimacy_erosion).
narrative_ontology:affects_constraint(pandemic_fatigue_compliance, population_trust_in_public_health).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pandemic_fatigue_compliance, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
