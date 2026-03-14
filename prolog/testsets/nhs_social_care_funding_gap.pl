% ============================================================================
% CONSTRAINT STORY: nhs_social_care_funding_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nhs_social_care_funding_gap, []).

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
 *   constraint_id: nhs_social_care_funding_gap
 *   human_readable: NHS Social Care Funding Gap
 *   domain: healthcare_policy/social_policy
 *
 * SUMMARY:
 *   The NHS social care funding gap represents a structural failure of
 *   coordination between health provision (NHS) and social care (local
 *   authorities, private providers), coupled with asymmetric extraction from
 *   the most vulnerable agents in the system. Since 2010, real-terms funding
 *   per service user has declined approximately 4% while demand has increased
 *   30-40% due to aging demographics. The constraint exhibits tangled_rope
 *   characteristics: genuine coordination problems (integrating health and
 *   social care, supporting informal carers) coexist with clear asymmetric
 *   extraction (means-testing that depletes recipient assets, wage
 *   suppression in the care worker sector, local authority spending caps that
 *   force rationing of eligibility). The theatrical reform cycles (2014 Care
 *   Act, 2017 cap-on-costs proposal, 2021 social care levy, 2023 CQC focus)
 *   promise structural solutions but defer implementation, substituting
 *   policy theater for actual funding increases. The extractiveness has
 *   increased from 0.35 to 0.58 over the 14-year interval, while
 *   theater_ratio has risen from 0.42 to 0.68, indicating systematic
 *   substitution of reform rituals for functional change. The suppression has
 *   remained high (0.70+) because the targets (care recipients, care workers,
 *   local authorities) have no effective exit option and cannot organize
 *   alternative provision.
 *
 * KEY AGENTS:
 *   - Care Recipients (Elderly, Disabled, Frail): Primary victims (powerless/trapped) — means-testing extracts assets; eligibility rationing delays care; dependency structure allows no exit
 *   - Care Workers: Secondary victims (moderate/constrained) — wage suppression £11-13/hour, high burnout, low professionalization, constrained exit through credential non-portability
 *   - Local Authorities (Care Commissioners): Organized victims (organized/constrained) — statutory duty to provide/commission care; funding formula doesn't match need; forced to raise eligibility thresholds
 *   - Central Treasury / Department of Health: Primary beneficiaries (institutional/arbitrage) — defers spending through underfunding; redirects resources to acute NHS; controls budget rules
 *   - Private Care Operators: Secondary beneficiaries (institutional/arbitrage) — benefit from market segmentation created by public underfunding; affluent self-funders flee to premium private
 *   - Informal Carers (Unpaid): Tertiary victims (moderate/constrained) — provide de facto provision filling gap; bear emotional/physical burden of unmet need; no recognition or support
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choices as inevitable constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nhs_social_care_funding_gap, 0.58).
domain_priors:suppression_score(nhs_social_care_funding_gap, 0.72).
domain_priors:theater_ratio(nhs_social_care_funding_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nhs_social_care_funding_gap, extractiveness, 0.58).
narrative_ontology:constraint_metric(nhs_social_care_funding_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nhs_social_care_funding_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nhs_social_care_funding_gap, tangled_rope).
narrative_ontology:human_readable(nhs_social_care_funding_gap, "NHS Social Care Funding Gap").
narrative_ontology:topic_domain(nhs_social_care_funding_gap, "healthcare_policy/social_policy").

domain_priors:requires_active_enforcement(nhs_social_care_funding_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nhs_social_care_funding_gap, central_treasury).
narrative_ontology:constraint_beneficiary(nhs_social_care_funding_gap, acute_nhs_providers).
narrative_ontology:constraint_beneficiary(nhs_social_care_funding_gap, private_care_operators).
narrative_ontology:constraint_victim(nhs_social_care_funding_gap, social_care_recipients).
narrative_ontology:constraint_victim(nhs_social_care_funding_gap, social_care_workers).
narrative_ontology:constraint_victim(nhs_social_care_funding_gap, local_authorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CARE RECIPIENT (SNARE) — Trapped by dependency on care services with no alternatives. Faces means-testing that extracts assets, delaying institutional care through underfunded home support, then forced institutional placement. No exit option; bears maximum cost of the funding gap.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE WORKER (TANGLED ROPE) — Constrained by low wages (£11-13/hour vs £10.42 minimum wage in many regions), no pension, no progression. High turnover and burnout. Genuine coordination function: care workers coordinate actual provision of services. But constrained exit (relocation burden, credential non-portability) and asymmetric extraction (bears wage suppression). Benefits from employment but trapped in low-wage sector.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOCAL AUTHORITY (TANGLED ROPE) — Organized actor (council budgets, statutory duties). Genuine coordination function: councils commission services, coordinate between health and social care. But constrained by central government funding formula that hasn't kept pace with demand, forcing rationing of eligibility thresholds. Extraction runs toward central treasury; councils bear cost of unmet need. Some agency through commissioning but bounded by spending caps.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL TREASURY / DHSC (ROPE) — Benefits from the funding gap through deferred spending (treating social care as lower priority than acute NHS). Experiences the constraint as coordination: balancing spending across multiple priorities. Net beneficiary through arbitrage — can redirect social care funding shortfalls to acute services. Low suppression experienced because they set the budget rules.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVATE CARE OPERATORS (ROPE) — Benefit from public underfunding through market segmentation: affluent self-funders flee underfunded public services, creating premium private market. Low effective suppression because private operators set own terms. Genuine coordination function in organizing care delivery, but net beneficiaries from the public funding gap.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY THEATER (PITON) — Repeated social care reform cycles (2014 Care Act, 2017 cap-on-costs proposals, 2021 social care levy, 2023 Care Quality Commission focus) promise structural solutions but defer implementation. Theater-ratio is high because reform rituals substitute for funding. The theatrical reforms maintain political legitimacy while suppressing structural solutions. Piton classification: former rope (genuine coordination attempts) has degraded into performative reform cycles.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The funding gap is structurally embedded: aging demographics increase need; per-capita funding per service user stagnates; means-testing creates asset extraction from recipients; low-wage structure suppresses sector professionalization. Real coordination function (health and social care integration, support for carers) coexists with clear asymmetric extraction (burden on powerless recipients and constrained workers; benefits concentrated in treasury and private operators). Chi computation: substantial extraction (chi ~0.65) from trapped/constrained agents; low chi for beneficiaries.
constraint_indexing:constraint_classification(nhs_social_care_funding_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nhs_social_care_funding_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nhs_social_care_funding_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nhs_social_care_funding_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nhs_social_care_funding_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nhs_social_care_funding_gap, TR),
    TR >= 0.70.

:- end_tests(nhs_social_care_funding_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The funding gap creates three distinct extraction flows: (1) care recipients via means-testing that depletes assets before state provision begins; (2) care workers via wage suppression relative to skill requirements and emotional labor; (3) local authorities via underfunded mandates. The 0.58 value reflects that extraction is substantial but not maximal — it is constrained by political legitimacy concerns and by the fact that some actual care is being provided. If recipients and workers were entirely abandoned, extractiveness would approach 0.85+ (pure snare). Suppression (0.72): High. Multiple barriers prevent exit: recipients depend on care services with no alternatives; workers face low wages but caring profession identity commitment and credential non-portability; councils have statutory duty to provide. However, some suppression is mitigated by informal care, private market options for wealthy, and partial political pressure for reform. Theater ratio (0.68): High. Reform cycles (2014, 2017, 2021, 2023) generate policy theater — consultations, proposals, announcements — that creates appearance of progress while deferring structural funding increases. Theater substitutes for functional change; it maintains political legitimacy without resolving the gap.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates clear perspectival divergence across the system. Care recipients and workers (trapped/constrained agents) perceive the funding gap as snare or tangled_rope — extraction is the primary experience, with suppression of alternatives. Local authorities (organized victims) perceive tangled_rope — genuine coordination function (commissioning care, integrating health/social) coexists with constrained spending and forced rationing. The treasury and private operators (institutional beneficiaries) perceive rope — they coordinate spending across priorities and benefit from the current allocation. The analytical observer perceives tangled_rope — the entire system is both coordinating care provision (real function) and extracting from vulnerable groups (real asymmetry). The piton perspective (policy theater) emerges from the chronological pattern: repeated reform cycles substitute for funding increases, maintaining theatrical legitimacy while suppressing structural change. This perspectival gap is diagnostic: if the constraint were a natural law or inevitable feature of aging, all perspectives would converge. The gap reveals that this is a policy choice, not a natural limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the extraction flow. Care recipients classified as powerless/trapped have maximum d (~0.95) because they are pure targets — bear costs with no exit. Care workers (moderate/constrained) have high d (~0.75) because they bear extraction costs (wage suppression) but retain some options (relocate, change sectors, though at high cost). Local authorities (organized/constrained) have moderate d (~0.60) because they coordinate genuine functions but are constrained by funding rules set by higher authority. Treasury/private operators (institutional/arbitrage) have low d (~0.15) because they are beneficiaries — arbitrage options allow them to shift resources. The f(d) sigmoid maps these d values to effective power modifiers, which feed into chi computation. High d agents experience high chi (experienced extraction); low d agents experience low or negative chi (experienced benefit). The perspectival gap in classification reflects this: high d agents see snare or tangled_rope; low d agents see rope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint is NOT subject to mandatrophy because the base properties and structural data support tangled_rope classification universally. The tension between coordination (genuine need for health/social care integration, care worker organization) and extraction (means-testing, wage suppression, eligibility rationing) is real and structural. No ambiguity about whether this is 'really' pure coordination (rope) or pure extraction (snare) — it is demonstrably both. The mandatrophy question 'Is this coordination or extraction?' is answered: it is both, in different proportions for different agents, which is exactly what tangled_rope captures. The piton perspective correctly identifies the reform theater as degraded — repeated policy cycles substitute for funding increases, maintaining theatrical legitimacy without functional improvement. This pattern is diagnostic of inertial institutional behavior, not of genuine coordination. The analytical perspective correctly identifies the constraint as tangled_rope with chi ~0.65, reflecting the substantial extraction balanced against partial coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_vs_policy_driver,
    'Is the funding gap primarily driven by demographic demand (aging population) or by policy choices (spending restraint, means-testing rules, wage suppression)?',
    'Comparative analysis: scenarios with stable demographics and policy change vs stable policy and demographic change. Cross-national comparison of countries with similar aging but different funding models (Germany, France).',
    'If demographic: funding gap is approaching mountain status (inherent structural limit). If policy: funding gap is remediable coordination problem (rope) or deliberate extraction (snare/tangled_rope). Current evidence suggests 70-80% policy, 20-30% demographic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_vs_policy_driver, empirical, 'Whether funding gap is driven by demographics or policy choices').

omega_variable(
    means_testing_asset_extraction,
    'Does means-testing extract sufficient assets from recipients to constitute the primary mechanism of cost suppression, or is low-wage employment suppression the primary extractor?',
    'Longitudinal tracking of care recipient asset depletion rates; comparison of lifetime costs under means-testing vs free provision models. Analysis of care worker earnings trajectories.',
    'If asset extraction is primary: tangled_rope classification holds. If wage suppression is primary: classification shifts toward snare (care workers are trapped targets). If both equally: confirms tangled_rope analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(means_testing_asset_extraction, empirical, 'Primary extraction mechanism: means-testing assets vs wage suppression').

omega_variable(
    private_market_segmentation_equilibrium,
    'Does the private care market depend structurally on public underfunding, or would it exist and thrive even with fully funded public provision?',
    'Analysis of private market size/quality in fully-funded public systems (Scandinavia, some Swiss cantons). Counterfactual: if NHS social care were fully funded, what private niche remains?',
    'If private depends on public gap: funding gap is structural requirement for private benefit (snare + rope in different segments). If private market is independent: public underfunding is policy choice not market equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_market_segmentation_equilibrium, empirical, 'Whether private care depends on public underfunding').

omega_variable(
    reform_cycle_functionality,
    'Do periodic reform cycles (2014, 2017, 2021, 2023) generate incremental improvements that justify the piton classification, or are they purely theatrical with zero functional change?',
    'Metric tracking: care recipient outcomes, worker wages, local authority funding adequacy before/after each reform. Identify any policy change that moved the extractiveness or suppression metrics.',
    'If reforms are functional: piton classification should downgrade to scaffold (temporary support with sunset). If purely theatrical: piton classification confirmed; reform cycles are the suppression mechanism itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_cycle_functionality, empirical, 'Whether reform cycles produce functional change or pure theater').

omega_variable(
    care_worker_identity_lock_depth,
    'Are care workers trapped by material constraints (low wages, credential non-portability) or by identity fusion (caring profession, commitment to vulnerable people making exit unthinkable)?',
    'Post-exit trajectory studies: care workers who leave sector — do barriers to re-entry reflect material/structural issues or identity/commitment issues? Analysis of career narratives.',
    'If material: exit_options should be ''constrained'' (high-cost external barriers). If identity: exit_options should be ''identity_locked'' (cognitive framing makes exit unthinkable). If both: separate stories by domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_worker_identity_lock_depth, empirical, 'Whether care worker exit barriers are material or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nhs_social_care_funding_gap, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nhsscare_tr_t0, nhs_social_care_funding_gap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nhsscare_tr_t7, nhs_social_care_funding_gap, theater_ratio, 7, 0.55).
narrative_ontology:measurement(nhsscare_tr_t14, nhs_social_care_funding_gap, theater_ratio, 14, 0.68).
narrative_ontology:measurement(nhsscare_tr_t21, nhs_social_care_funding_gap, theater_ratio, 21, 0.74).

% Extraction over time
narrative_ontology:measurement(nhsscare_be_t0, nhs_social_care_funding_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nhsscare_be_t7, nhs_social_care_funding_gap, base_extractiveness, 7, 0.47).
narrative_ontology:measurement(nhsscare_be_t14, nhs_social_care_funding_gap, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(nhsscare_be_t21, nhs_social_care_funding_gap, base_extractiveness, 21, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nhs_social_care_funding_gap, resource_allocation).
narrative_ontology:affects_constraint(nhs_social_care_funding_gap, nhs_acute_provider_squeeze).
narrative_ontology:affects_constraint(nhs_social_care_funding_gap, informal_caregiver_burden).
narrative_ontology:affects_constraint(nhs_social_care_funding_gap, residential_care_quality_decline).

% DUAL FORMULATION NOTE:
% The social care funding gap is structurally linked to three downstream constraints: (1) squeeze on acute NHS providers who absorb hospital bed blocking when community care is insufficient; (2) informal caregiver burden as family members substitute for underfunded services; (3) residential care quality decline as providers cut costs to remain solvent under low funding. Each has its own extractiveness value reflecting the specific mechanism. The funding gap story models the systemic constraint; the downstream stories model specific manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nhs_social_care_funding_gap, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
