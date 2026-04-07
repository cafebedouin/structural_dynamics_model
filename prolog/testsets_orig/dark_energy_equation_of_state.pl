% ============================================================================
% CONSTRAINT STORY: dark_energy_equation_of_state
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_energy_equation_of_state, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dark_energy_equation_of_state
 *   human_readable: Dark Energy Equation of State Constraint
 *   domain: cosmology/observational_astrophysics
 *
 * SUMMARY:
 *   The dark energy equation of state constraint exemplifies how
 *   observational consensus in fundamental physics can become extractive when
 *   a specific theoretical assumption (w=-1 for the cosmological constant)
 *   becomes operationalized into observational infrastructure before
 *   empirical justification is established. The constraint arises from a
 *   structural asymmetry: the Lambda-CDM framework's computational simplicity
 *   and empirical fit incentivize treating w=-1 as a parameter rather than a
 *   hypothesis to test. This choice is embedded into survey design, data
 *   analysis pipelines, and theoretical model construction. Alternative
 *   gravity theories and dynamical dark energy models face systematic
 *   suppression through publication bias, funding allocation patterns, and
 *   observational channel control. The theater_ratio (0.81) reflects that
 *   much cosmological discourse treats w=-1 as natural law (immutable
 *   consequence of observations) when it is actually a contingent
 *   institutional choice. The extractiveness value (0.68) captures the
 *   asymmetric cost imposed on researchers pursuing alternatives: they must
 *   explain existing data within frameworks pre-optimized for Lambda-CDM
 *   parameters, while beneficiaries (the Lambda-CDM establishment) face no
 *   requirement to test their core assumptions.
 *
 * KEY AGENTS:
 *   - Lambda-CDM Establishment: Primary beneficiary (institutional/arbitrage) — controls observational infrastructure, funding, journals; w=-1 assumption simplifies their work while imposing costs on alternatives
 *   - Alternative Gravity Theorists: Primary victim (powerless/trapped) — must explain observations within observational frameworks controlled by Lambda-CDM; cannot develop competing theories without defeating the w=-1 assumption first
 *   - Observational Cosmologists: Secondary victim (moderate/constrained) — funding and career constraints tied to Lambda-CDM consensus; benefits from access to massive surveys but limited in framing questions outside consensus
 *   - Fundamental Physics Foundations: Diffuse victim (institutional/analytical) — theoretical understanding of dark energy/vacuum energy stalls due to w=-1 default masking the problem; fine-tuning issue (120 orders of magnitude discrepancy) remains unresolved
 *   - Phenomenological Dark Energy Programs: Organized actors (organized/constrained) — building empirical w(z) measurements that can eventually bypass the w=-1 assumption with sufficient precision
 *   - Cosmological Constant Theory: Theoretical component (institutional/arbitrage) — benefits from institutional momentum; theoretically unmotivated yet resistant to change due to lack of superior alternative and institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_energy_equation_of_state, 0.68).
domain_priors:suppression_score(dark_energy_equation_of_state, 0.72).
domain_priors:theater_ratio(dark_energy_equation_of_state, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_energy_equation_of_state, extractiveness, 0.68).
narrative_ontology:constraint_metric(dark_energy_equation_of_state, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dark_energy_equation_of_state, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_energy_equation_of_state, snare).
narrative_ontology:human_readable(dark_energy_equation_of_state, "Dark Energy Equation of State Constraint").
narrative_ontology:topic_domain(dark_energy_equation_of_state, "cosmology/observational_astrophysics").

domain_priors:requires_active_enforcement(dark_energy_equation_of_state).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dark_energy_equation_of_state, standard_lambda_cdm_framework).
narrative_ontology:constraint_victim(dark_energy_equation_of_state, alternative_gravity_theories).
narrative_ontology:constraint_victim(dark_energy_equation_of_state, observational_cosmologists).
narrative_ontology:constraint_victim(dark_energy_equation_of_state, fundamental_physics_foundations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE GRAVITY THEORIST (SNARE) — Trapped by observational dominance of Lambda-CDM. Cannot exit: alternative theories must explain the same data within existing observational frameworks controlled by Lambda-CDM assumptions. Suppression is structural: redshift surveys, type Ia supernovae, CMB measurements all pre-filtered through Lambda-CDM priors. Maximum extraction with no exit path.
constraint_indexing:constraint_classification(dark_energy_equation_of_state, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL COSMOLOGIST (TANGLED ROPE) — Constrained by funding and publication channels tied to Lambda-CDM consensus, but benefits from access to massive surveys (Planck, SDSS, DES) designed within the framework. Career costs to questioning w=-1 assumption are high but not insurmountable; some agencies fund alternative probes (21cm cosmology, void surveys). Mixed coordination and extraction: the framework enables their work while constraining their framing.
constraint_indexing:constraint_classification(dark_energy_equation_of_state, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LAMBDA-CDM ESTABLISHMENT (ROPE) — Institutional beneficiary with full arbitrage capacity. Controls observational infrastructure, funding allocation, journal editorial boards. Experiences the equation-of-state constraint as pure coordination: the w=-1 assumption enables their research program and simplifies theoretical scaffolding. Net beneficiary with escape velocity — can always shift to alternative frameworks if needed, but has no incentive.
constraint_indexing:constraint_classification(dark_energy_equation_of_state, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COSMOLOGICAL CONSTANT THEORY (PITON) — The equation w=-1 (cosmological constant behavior) is theoretically unmotivated: nobody understands why the vacuum energy density should equal the critical density to 120 decimal places (fine-tuning problem), yet the assumption persists through institutional inertia. Fundamentally degraded — high theater ratio reflects that the theory is maintained despite lack of theoretical understanding. Replaced functionally by dark energy phenomenology (measuring w empirically without explaining why), but the w=-1 default persists in model construction.
constraint_indexing:constraint_classification(dark_energy_equation_of_state, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHENOMENOLOGICAL DARK ENERGY PROGRAM (SCAFFOLD) — Organized agents (Planck, DES, Euclid collaborations) building empirical measurements of the equation-of-state parameter w(z) without requiring theoretical understanding. This represents a temporary structured bypass of the w=-1 assumption: measure first, interpret later. Sunset clause: as redshift surveys mature (Vera Rubin Observatory, CMB-S4), direct w(z) measurements will either confirm w=-1 or reveal evolution, enabling theoretical progress. Constrained by need to work within existing observational infrastructure, but has clear exit mechanism (empirical precision replaces assumption).
constraint_indexing:constraint_classification(dark_energy_equation_of_state, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At the civilizational scale, the acceleration of cosmic expansion appears immutable: the universe exhibits acceleration, and any theoretical framework must accommodate this observation. From this perspective, SOME constraint on dark energy properties is natural law — but the specific equation w=-1 is not. The false summit is the claim that Lambda-CDM's particular choice is inherent rather than contingent. The constraint is not the observational fact (acceleration), but the institutional imposition of a specific equation of state without empirical justification.
constraint_indexing:constraint_classification(dark_energy_equation_of_state, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_energy_equation_of_state_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_energy_equation_of_state, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_energy_equation_of_state, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dark_energy_equation_of_state, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dark_energy_equation_of_state, TR),
    TR >= 0.70.

:- end_tests(dark_energy_equation_of_state_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The w=-1 assumption provides computational and conceptual simplicity to the Lambda-CDM framework that benefits institutional actors, while imposing empirical testing barriers on alternatives. The extraction is not intentional conspiracy but structural: the framework's success in fitting data is purchased partly through parameter reduction (w fixed rather than measured as a function of redshift). Suppression (0.72): High. Alternative theories face systematic barriers: (1) Observational dominance — major surveys (Planck, SDSS, DES) operate within Lambda-CDM parameter spaces, making it costly to extract constraints on w(z) or modified gravity parameters; (2) Publication bias — journals favor papers that assume w=-1; (3) Funding patterns — dark energy research funding heavily concentrates in Lambda-CDM infrastructure; (4) Prior embedding — cosmological parameter extraction codes use w=-1 as default or strongly weighted prior. Theater ratio (0.81): High and rising. The fine-tuning problem (120-order-of-magnitude discrepancy between vacuum energy density and critical density) is solved by assumption (w=-1), not by theory. Yet this is treated as a solved problem in much cosmological literature. The theater consists of: (a) treating w=-1 as observed consequence when it is empirical choice; (b) fine-tuning discussions that acknowledge the problem but leave it unresolved; (c) theoretical papers on quintessence/dynamical dark energy that rarely compare quantitatively with w=-1 at accessible redshifts. Theater has increased over the interval (1998-2025) as the w=-1 default became more entrenched despite lacking theoretical foundation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gap between institutional beneficiaries and trapped theoretical alternatives. The Lambda-CDM establishment sees Rope — the equation of state is a useful coordination mechanism for interpreting observations. Alternative theorists see Snare — they cannot develop competing theories without defeating w=-1 first, and the observational channels they would use are already optimized for the dominant framework. The observational cosmologist sees Tangled Rope — benefits from access to massive surveys but constrained by consensus assumptions. The phenomenological dark energy program sees Scaffold — empirical w(z) measurements will eventually provide an exit path from the w=-1 default. The cosmological constant theory itself sees Piton — the theory lacks fundamental justification (nobody explains the fine-tuning) and persists through institutional momentum. The analytical observer risks seeing Mountain (expansion acceleration is immutable) but the structural data reveals this as false summit: the observational fact of acceleration is natural law; the specific equation w=-1 is not.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are driven by structural relationships to the w=-1 constraint. The Lambda-CDM establishment (institutional/arbitrage) benefits from simplicity and faces no cost to maintaining the assumption — derived d approaches 0.0 (full beneficiary). Alternative gravity theorists (powerless/trapped) face maximum empirical barriers and no exit options — derived d approaches 1.0 (full target). Observational cosmologists (moderate/constrained) are somewhere in the middle: they benefit from survey access and infrastructure (lowering their d) but face career costs to questioning consensus (raising their d). The phenomenological dark energy program (organized/constrained) has higher agency and clear exit mechanisms via improved measurements (d~0.45), shifting them toward rope/scaffold perception. Directionality overrides are not needed here: the derivation chain naturally produces the perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH INSTITUTIONAL INERTIA: This constraint resolves mandatrophy by separating the underlying physics (acceleration is real; explanation is needed) from the institutional imposition (w=-1 is assumed without sufficient justification). The snare classification is correct because: (1) high extractiveness — the framework's success imposes costs on alternatives; (2) high suppression — observational channels are controlled by dominant theory; (3) trapped victims — alternative theories cannot develop independently. The false summit risk is real: civilization-scale acceleration IS immutable, but w=-1 is contingent. The constraint is neither natural law nor pure coordination — it is institutional extraction masquerading as natural law. The mandatrophy is resolved by noting that the phenomenological dark energy perspective (Scaffold) provides a structural exit: as redshift surveys mature, direct w(z) measurements will either confirm w=-1 (reducing it to rope/coordination) or reveal evolution (breaking the constraint entirely). The timeline for this resolution is generational (10-20 years), making the Snare classification accurate for the biographical horizon but potentially temporary at the generational scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fine_tuning_theoretical_status,
    'Does the cosmological constant fine-tuning problem reflect a genuine theoretical crisis (suggesting w=-1 is incorrect) or is it a meta-theoretical issue (the theory works empirically despite lack of understanding)?',
    'Theoretical breakthroughs explaining vacuum energy from first principles; empirical measurement of w evolution via high-redshift supernovae, BAO, weak lensing precision; detection of w(z) deviation from -1 at >5-sigma',
    'If fine-tuning indicates physical error: w=-1 assumption is extractive, should be abandoned. If meta-theoretical only: fine-tuning persists but does not justify alternative frameworks. Classification shifts from Snare (extraction trap) to Rope (coordination with theoretical baggage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fine_tuning_theoretical_status, empirical, 'Whether fine-tuning indicates true theoretical crisis').

omega_variable(
    alternative_gravity_empirical_viability,
    'Can modified gravity theories (f(R), scalar-tensor, MOND) explain dark energy observations without invoking w=-1, or do they merely shift parameters and reproduce Lambda-CDM?',
    'Detailed comparison of modified gravity predictions with Planck CMB, SDSS BAO, DES weak lensing, SNe Ia measurements; test whether alternative theories reduce fine-tuning or merely hide it; assess whether modified gravity has independent predictive power beyond fitting existing data',
    'If alternatives are genuinely equivalent: w=-1 is neither extracted nor imposed, just a parametrization choice. If alternatives are suppressed: snare classification is correct. If alternatives have superior theoretical motivation but lower empirical support: tangled rope / scaffold dynamics revealed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_gravity_empirical_viability, empirical, 'Whether modified gravity theories offer genuine alternatives to Lambda-CDM').

omega_variable(
    observational_independence_from_priors,
    'How much of the current w=-1 consensus derives from observational data versus prior assumptions embedded in cosmological models (Planck polarization analysis, SNe distance modulus calculations, BAO inference)?',
    'Cosmological parameter extraction using model-independent methods (model-agnostic reconstruction of H(z), w(z) from data); comparison of Planck results using different model priors; blind reanalysis of legacy supernova and BAO data without Lambda-CDM templates',
    'If observational support is prior-independent: w=-1 is empirically justified. If support depends on Lambda-CDM assumptions: institutional extraction is structural, snare classification confirmed. If middle ground: tangled rope dynamics (coordination + bias).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_independence_from_priors, empirical, 'Model independence of dark energy equation-of-state constraints').

omega_variable(
    next_generation_survey_capability,
    'Will LSST/Vera Rubin, Euclid, CMB-S4 precision redshift measurements enable w(z) evolution detection with sufficient clarity to move beyond w=-1, or will they confirm the approximation holds at accessible scales?',
    'Five-year forecasts from Vera Rubin weak lensing and SNe (target: w precision ~0.05); Euclid BAO and galaxy clustering; CMB-S4 polarization and lensing; empirical w(z) reconstruction with minimal model assumptions',
    'If w(z) evolution is detected: scaffold perspective is confirmed, empirical dark energy science replaces theoretical constraint. If w=-1 persists: underlying theoretical/institutional constraint remains structural. The answer determines whether the snare is temporary (scaffold with sunset) or permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(next_generation_survey_capability, empirical, 'Whether next-generation surveys enable w(z) evolution detection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_energy_equation_of_state, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(darkene_tr_t0, dark_energy_equation_of_state, theater_ratio, 0, 0.55).
narrative_ontology:measurement(darkene_tr_t7, dark_energy_equation_of_state, theater_ratio, 7, 0.68).
narrative_ontology:measurement(darkene_tr_t15, dark_energy_equation_of_state, theater_ratio, 15, 0.81).
narrative_ontology:measurement(darkene_tr_t22, dark_energy_equation_of_state, theater_ratio, 22, 0.75).

% Extraction over time
narrative_ontology:measurement(darkene_be_t0, dark_energy_equation_of_state, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(darkene_be_t7, dark_energy_equation_of_state, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(darkene_be_t15, dark_energy_equation_of_state, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(darkene_be_t22, dark_energy_equation_of_state, base_extractiveness, 22, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_energy_equation_of_state, information_standard).
narrative_ontology:affects_constraint(dark_energy_equation_of_state, fine_tuning_vacuum_energy_problem).
narrative_ontology:affects_constraint(dark_energy_equation_of_state, modified_gravity_phenomenology).
narrative_ontology:affects_constraint(dark_energy_equation_of_state, cmb_tensions_hubble_constant).

% DUAL FORMULATION NOTE:
% The dark energy equation-of-state constraint is downstream of cosmological observations (acceleration) but upstream of theoretical frameworks attempting to explain it. The constraint decomposes into two structurally distinct stories: (1) observational-phenomenological (ε~0.25, Rope) — measuring w(z) without assuming w=-1; (2) institutional-theoretical (ε~0.68, Snare) — the w=-1 default enforcement in model construction. This story addresses the institutional constraint; the phenomenological side would be a separate story with lower extractiveness and clearer coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
