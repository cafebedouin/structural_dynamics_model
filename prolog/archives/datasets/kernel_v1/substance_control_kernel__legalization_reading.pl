% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalization Reading: Substance Use as Individual Liberty with Externality Correction
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading of the substance control kernel asserts that
 *   individual liberty over one's own consumption is the primary moral
 *   constraint, with state intervention justified only to prevent third-party
 *   harm and internalize externality costs. This reading instantiates a
 *   specific structural claim: substance users are relocated from the victim
 *   set (as in prohibition reading) into the beneficiary set (via legal
 *   market access and choice), while third parties (DUI victims, secondhand
 *   exposure) are relocated into the victim set via negative externalities.
 *   The state's role transforms from enforcer of prohibition to revenue
 *   collector and externality-pricing mechanism. This reading is one of three
 *   competing framings of the substance control kernel; the others are the
 *   prohibition reading (substance use creates sufficient externality and
 *   addiction risk to justify preventive bans) and the harm reduction reading
 *   (state role is public health authority, neutral on legality, focused on
 *   minimizing consumption-related harms). The legalization reading coexists
 *   with prohibition and influences harm reduction — all three readings are
 *   live positions in contemporary policy discourse, each grounded in
 *   different empirical claims and normative axioms.
 *
 * KEY AGENTS:
 *   - Substance Users (Legal Market): Primary beneficiary (moderate/mobile) — exit criminal justice system; enter legal market with choice and consumer protections
 *   - Third-Party Harm Bearers: Primary victims (powerless/trapped) — DUI victims, secondhand exposure, workplace intoxication costs; bear uncompensated harms despite theoretical externality correction
 *   - Regulated Commercial Suppliers: Secondary beneficiary (institutional/arbitrage) — market access, brand establishment, but constrained by licensing, testing, regulatory compliance
 *   - Low-Income Users: Secondary victim (powerless/trapped) — price discrimination and product stratification; legalization transforms extraction from criminalization to market mechanisms
 *   - State (Revenue Collector and Regulator): Mixed institutional actor (institutional/constrained) — benefits via taxation and licensing; trapped in balancing revenue maximization against black market suppression and market stability
 *   - Treatment and Public Health Infrastructure: Institutional actor (institutional/constrained) — promises expanded access via tax funding; actual deployment lags; prevention messaging becomes theater
 *   - Analytical Observer (Liberty Emphasis): Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional claim (individual liberty as natural law) rather than understanding it as contestable normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.35).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.38).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization Reading: Substance Use as Individual Liberty with Externality Correction").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, 'ee708700-395d-4316-a39f-d85af6ec3460').
narrative_ontology:cs_kernel_codification('ee708700-395d-4316-a39f-d85af6ec3460', distributed).
narrative_ontology:cs_authority_grounding('ee708700-395d-4316-a39f-d85af6ec3460', distributed).
narrative_ontology:cs_reading_relation('ee708700-395d-4316-a39f-d85af6ec3460', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee708700-395d-4316-a39f-d85af6ec3460', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('ee708700-395d-4316-a39f-d85af6ec3460', foundational, self_regarding_action_liberty_immune).
narrative_ontology:cs_axiom_status(self_regarding_action_liberty_immune, holdable).
narrative_ontology:cs_axiom_grounding('ee708700-395d-4316-a39f-d85af6ec3460', self_regarding_action_liberty_immune, deontological).
narrative_ontology:cs_axiom('ee708700-395d-4316-a39f-d85af6ec3460', foundational, externality_correction_via_taxation_sufficient).
narrative_ontology:cs_axiom_status(externality_correction_via_taxation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ee708700-395d-4316-a39f-d85af6ec3460', externality_correction_via_taxation_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('ee708700-395d-4316-a39f-d85af6ec3460', individual_liberty_primacy_framework).
narrative_ontology:cs_drift_state('ee708700-395d-4316-a39f-d85af6ec3460', contemporary_regulatory_capture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee708700-395d-4316-a39f-d85af6ec3460', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users_legal_market).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, regulated_commercial_suppliers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_revenue).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, treatment_access_expansion).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_harm_bearers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, regulatory_capture_risk).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, low_income_users_price_discrimination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGAL SUBSTANCE USER (ROPE) — Individual liberty framework allows consumption choice; state coordination mechanism is regulatory infrastructure (purity standards, labeling, quality assurance) that enables market function. User experiences this as low extraction — the coordination benefits (safe supply, consistent dosing, legal status) outweigh regulatory costs. Moderately mobile within legal market.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD-PARTY HARM BEARER (TANGLED ROPE) — This reading moves users out of the victim set and places third parties in it. Innocent bystanders exposed to secondhand smoke, impaired driving incidents, or workplace intoxication bear costs they did not choose. The legalization reading 'corrects' this by asserting the state should capture externality costs via taxation/regulation, but actual capture is incomplete and asymmetric. Third parties remain partially trapped in bearing uncompensated harms despite the theoretical correction mechanism.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED COMMERCIAL SUPPLIERS (TANGLED ROPE) — Commercial suppliers benefit from legalization (market access, brand establishment, profit margins) but face compliance costs (licensing, testing, regulatory reporting, potency caps). The legalization reading enables extraction from users through pricing power while coordination occurs via market segmentation and quality differentiation. Institutional actor with exit flexibility — can adjust product mix, pricing, marketing within regulatory boundaries.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOW-INCOME USERS (SNARE) — Legalization does not eliminate class stratification; instead, it transforms the extraction mechanism from criminalization to price discrimination and product stratification. Low-income users face 'bargain' products with lower quality, more additives, or reduced potency guidance. Cannot exit because legal market pricing is uniform-binding. This perspective reveals that legalization reading's individual liberty claim applies primarily to affluent users who can afford choice; poor users face different extraction mechanics.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE AS REVENUE COLLECTOR AND REGULATOR (TANGLED ROPE) — Legalization transforms the state's role from enforcer of prohibition to beneficiary via taxation and licensing revenue. The state coordinates the legal market (purity, labeling, potency standards, treatment access funding) while extracting via taxation. Tax rates create black market incentives if set too high, requiring enforcement to protect legal market. The state is trapped in balancing revenue maximization against market stability — extraction increases coordination costs.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TREATMENT AND PUBLIC HEALTH INFRASTRUCTURE (PITON) — Legalization reading promises expanded treatment access via tax revenue and reduced criminal justice costs. However, actual infrastructure deployment lags; treatment is often underfunded relative to tax revenue collected; prevention messaging becomes theater (high-visibility campaigns with modest behavioral impact). The coordination function (evidence-based treatment) is real but increasingly performative as actual funding allocation reflects political rather than epidemiological priorities.
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective emphasizing individual autonomy as a fundamental right, some legalization advocates treat the liberty claim as a natural law: self-regarding action (use without third-party harm) must be immune from state prohibition, independent of empirical outcomes. This perspective sees prohibition itself as an immutable violation of individual sovereignty. However, the structural data reveals this as a false summit — the liberty principle depends on the empirical claim that most use is self-regarding, which is contestable (secondhand exposure, addiction effects on family, workplace incidents).
constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_kernel__legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_kernel__legalization_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The legalization reading represents a genuine structural shift in the extraction mechanism — users exit the criminal justice extraction system (high suppression, prohibition-era extractiveness ~0.65) and enter a legal market system with lower extractiveness. However, extraction does not eliminate; it transforms. Commercial suppliers extract via pricing power and product differentiation. The state extracts via taxation. Low-income users face price discrimination (trapped at lower quality/higher relative cost). Residual harms to third parties remain partially uncompensated despite the theory of externality pricing. Suppression (0.38): Moderate. Suppression requirements decline sharply compared to prohibition era (suppression ~0.75 under criminalization) because users are no longer legally prohibited. However, regulatory enforcement persists: purity testing, potency limits, licensing verification, and enforcement against unlicensed suppliers. Suppression is now 'light-touch' regulation rather than criminal prohibition, but still substantial enough to constrain behavior. Theater ratio (0.52): Moderate-high, rising. Initial theater is lower than prohibition system because legalization creates genuine functional markets (purity is testable, supply is reliable, quality standards are enforced). However, over time, theater rises as prevention campaigns, treatment access promises, and regulatory theatrics (visible enforcement against gray markets) become performative — the actual infrastructure lags the promised coordination function. Rising theater over the interval reflects the gap between legalization's promises (evidence-based treatment, harm reduction via regulation) and actual deployment (underfunded treatment, regulatory capture by commercial interests).
 *
 * PERSPECTIVAL GAP:
 *   This reading generates sharp perspectival divergence between beneficiaries and victims. The legal market user (rope) experiences the legalization reading as liberation and coordination — they gain choice, safety, and legal status. Commercial suppliers (tangled_rope) experience genuine market coordination alongside extraction opportunities. Low-income users (snare) experience transformation of extraction mechanism from criminalization to price discrimination — the reading does not eliminate extraction, it changes its form. Third-party harm bearers (tangled_rope) discover they are newly identified as victims, bearing costs (secondhand exposure, DUI incidents) that the reading's externality correction mechanism only partially addresses. The state (tangled_rope) is trapped between revenue maximization (encourage consumption, taxation) and public health (minimize harms, limit potency, support treatment). The treatment infrastructure (piton) promises expansion but delivers performance theater as actual funding lags. The analytical observer risks naturalizing the liberty principle as a natural law rather than as a contestable normative claim dependent on empirical assumptions about self-regarding action. The reading's internal coherence depends on the claim that most use remains self-regarding (impacting only the user), but this is precisely what the harm reduction and prohibition readings dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The legalization reading's directionality is determined by the structural repositioning of users (from victim to beneficiary), third parties (from external to victim), and the state (from prohibition enforcer to revenue collector). For users in the legal market, directionality d drops dramatically compared to prohibition era — they have mobile exit options (choice within legal market), reduced suppression (regulation rather than criminalization), and beneficiary status (legal access, consumer protections). Derived d ≈ 0.25 (moderate beneficiary, mobile) yields low/negative effective extraction. For third parties, directionality d rises — they are newly identified as bearing costs (DUI, secondhand exposure) without compensation, and they have trapped exit options (cannot opt out of being harmed). Derived d ≈ 0.85 (powerless/trapped victim) yields high f(d) ≈ 1.15. The state's directionality depends on whether taxation is viewed as legitimate externality pricing (d ≈ 0.50, symmetric) or revenue extraction (d ≈ 0.40, beneficiary). Commercial suppliers have d ≈ 0.30 (beneficiary with constrained exit due to licensing). Low-income users have d ≈ 0.90 (trapped victim of price discrimination). This perspectival variance is precisely what the tangled_rope classification captures — genuine coordination (legal market enables quality assurance, treatment access) coexists with asymmetric extraction (pricing power, regulatory capture risk, residual third-party harms).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clarifying that legalization reading is ONE institutional framing of substance control, not a universal classification. The reading is tangled_rope because it genuinely coordinates the legal market (purity, potency, access, treatment funding) while extracting via commercial pricing, state taxation, and price discrimination. The mountain classification at the analytical level is a false summit — it naturalizes the individual liberty principle as an immutable law of autonomy, but this principle is empirically contingent on whether actual substance use is genuinely self-regarding. The snare classification for low-income users reveals that legalization does not eliminate extraction; it transforms it. The rope classification for legal market users is their genuine experience — but not universal. The piton classification for treatment infrastructure reveals promise (expanded access) degrading into theater (underfunded prevention, visible but ineffective campaigns). Mandatrophy is resolved by recognizing that no single type describes the reading — it is a mixed structure that benefits some agents (legal users, suppliers) while transforming extraction mechanisms for others (low-income users via price discrimination, third parties via new victim set). The analytical task is to map the perspectival differences and identify which agents experience genuine coordination vs which experience extraction relabeled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_capture_completeness,
    'Does taxation and regulation actually capture the full externality costs of substance use, or does the legalization reading externalize residual harms?',
    'Comparative analysis of actual tax revenue vs measured third-party harm costs (healthcare, accident, lost productivity, secondhand exposure); jurisdiction-level comparison of tax rates and harm reduction outcomes',
    'If capture is substantial (>80%): tangled_rope classification confirmed — genuine coordination via externality pricing. If capture is partial (<50%): classification shifts toward snare (extraction without full correction); third-party harm bearers remain structural victims. If capture is inverse (revenue>harms): state is extracting rent from externality correction mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_completeness, empirical, 'Degree to which taxation captures externality costs vs leaves residual third-party harm uncompensated').

omega_variable(
    legalization_reading_core_premise,
    'Is the legalization reading''s core premise (substance use is individual liberty immune from state prohibition) logically compatible with the prohibition reading''s core premise (substance use creates externalities justifying preventive prohibition)?',
    'Clarify the empirical boundary: does the legalization reading define ''self-regarding'' as ''physically isolated'' (only impacts user''s own body) or ''rationally defensible'' (user accepts consequences including addiction and impaired judgment)? If physically isolated: legalization and prohibition forecloses each other on empirical grounds. If rationally defensible: coexistence depends on contested claims about user agency and addiction mechanisms.',
    'If forecloses: the readings are incompatible; any single framework must choose one. If coexists: disagreement is deeper (about autonomy, rationality, and addiction) and requires institutional/empirical resolution, not logical elimination. If influences: legalization''s empirical claims about externality capture shape prohibition''s residual legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legalization_reading_core_premise, conceptual, 'Whether legalization and prohibition readings logically foreclose each other or coexist as live positions').

omega_variable(
    class_stratification_under_legalization,
    'Does legalization eliminate the extraction mechanism toward users, or does it transform extraction from criminalization to price discrimination and unequal access?',
    'Empirical analysis of product pricing tiers, quality variation, and access barriers across income groups in legalized jurisdictions; comparison of low-income user outcomes under legalization vs prohibition; measurement of price elasticity and substitution patterns',
    'If extraction eliminates: legalization reading is genuine individual liberty recovery; low-income users exit victim set. If transforms: legalization reading benefits affluent users disproportionately; low-income users shift from criminal justice victims to market stratification victims. If amplifies: legalization creates new extraction mechanisms (targeted marketing, product design for addiction potential, pricing strategies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_stratification_under_legalization, empirical, 'Whether legalization eliminates extraction toward users or transforms it to class-based mechanisms').

omega_variable(
    reading_vs_prohibition_on_prevention_authority,
    'Does the legalization reading (individual liberty over state prevention) logically foreclose the harm reduction reading (state as public health authority deploying evidence-based interventions)?',
    'Clarify whether legalization reading''s individual liberty claim extends to prevention/treatment access. Does it require ''no state intervention'' (forecloses harm reduction) or ''state intervention only via incentive, not coercion'' (coexists with harm reduction)? Examine actual jurisdictions: do legalization regimes permit or prohibit mandatory treatment, involuntary intervention, or paternalistic prevention?',
    'If forecloses: legalization reading implies minimal state role even in treatment; harm reduction reading is logically eliminated. If coexists: legalization is compatible with robust treatment expansion; readings differ on enforcement mechanism, not state role. If influences: legalization changes harm reduction''s empirical operating environment (who seeks treatment, funding availability, stigma effects) without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_prohibition_on_prevention_authority, conceptual, 'Whether legalization reading forecloses harm reduction reading on prevention/treatment authority').

omega_variable(
    regulatory_capture_risk_under_legalization,
    'Does legalization reading''s reliance on state taxation and regulation create structural conditions for regulatory capture by commercial suppliers?',
    'Analysis of lobbying influence, licensing board composition, and regulatory agency funding in legalized jurisdictions; tracking of potency limits, product restrictions, and marketing rules over time; comparison of regulatory outcomes across jurisdictions with different supplier concentration levels',
    'If high capture risk: legalization reading''s externality correction mechanism is structurally vulnerable to subversion; state becomes beneficiary of commercial interests rather than public health. If low: regulatory independence can maintain externality pricing. If captures over time: legalization creates perverse incentive (state revenue-dependent on high consumption) that undermines prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk_under_legalization, empirical, 'Structural vulnerability of legalization''s regulatory mechanism to capture by commercial interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subleg_theater_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(subleg_theater_t3, substance_control_kernel__legalization_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(subleg_theater_t6, substance_control_kernel__legalization_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(subleg_extract_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(subleg_extract_t3, substance_control_kernel__legalization_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(subleg_extract_t6, substance_control_kernel__legalization_reading, base_extractiveness, 6, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(subleg_suppress_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(subleg_suppress_t3, substance_control_kernel__legalization_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(subleg_suppress_t6, substance_control_kernel__legalization_reading, suppression_requirement, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, taxation_as_externality_pricing).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, regulatory_capture_in_legal_markets).

% DUAL FORMULATION NOTE:
% The legalization reading is one of three structurally distinct constraints sharing the substance_control_kernel. Each reading instantiates a different ε and different beneficiary/victim map. Prohibition reading emphasizes prevention authority (ε_prohibition ≈ 0.55, snare for users). Harm reduction reading emphasizes public health optimization (ε_harm_reduction ≈ 0.42, rope for all agents under evidence-based interventions). Legalization reading emphasizes individual liberty (ε_legalization = 0.35, tangled_rope). The three readings are NOT observational variants of one constraint — they have genuinely different extractiveness values and different structural assumptions. They are linked via network.affects_constraints as a constraint family, with the kernel as the unifying element.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, moderate, 0.25).
constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, powerless, 0.9).
constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
