% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Substance Control Legitimacy (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'substance_control_legitimacy' — specifically, the legalization reading.
 *   Under this reading, state authority over substance use is legitimated
 *   ONLY by preventing third-party harm; competent adults retain autonomy to
 *   use substances. This is distinct from the harm-reduction reading (which
 *   treats use as a public health issue and legitimates state intervention to
 *   minimize harm without criminalization) and the prohibition reading (which
 *   treats use as inherently harmful and legitimates criminalization as moral
 *   duty). The legalization reading produces a specific structural
 *   configuration: adult users exit the victim set (they gain autonomy),
 *   third-party harm bearers become the primary victim group (traffic risk,
 *   secondhand exposure, workplace safety), and the legal market structure
 *   embeds corporate extraction alongside coordination functions. The
 *   measurement trajectory (rising extractiveness and theater ratio over the
 *   interval, falling suppression) reflects the empirical dynamics of
 *   legalization regimes: initial restraint (careful regulation) gives way to
 *   market pressure (expanding product lines, aggressive marketing,
 *   regulatory capture risk). The suppression metrics show that legalization
 *   reduces state coercion (criminalization is replaced by market
 *   participation), but the extraction mechanism shifts from legal penalties
 *   to market-embedded incentives (profit-driven consumption maximization,
 *   vulnerable population targeting).
 *
 * KEY AGENTS:
 *   - Adult Users: Primary beneficiary (moderate/mobile) — gain autonomy protection and legal access; exit victim set; moderate experience of remaining regulation as coordination
 *   - Legal Market Producers: Institutional beneficiary (institutional/arbitrage) — profit from legal sales; arbitrage options enable regulatory pressure; embedded extraction through consumption maximization
 *   - Third-Party Harm Bearers: Primary victim (moderate/constrained) — face traffic risk, secondhand exposure, workplace disruption; constrained exit (geographic relocation, social isolation)
 *   - Vulnerable Populations (Adolescents, Addiction-Prone): Trapped victim (powerless/trapped) — fall outside autonomy protection; targeted by market incentives; no genuine exit
 *   - Public Health Regulation Coalition: Organized actor (organized/mobile) — sees legalization as solvable through robust regulation; scaffold perspective; conditional sunset on regulatory capacity
 *   - Population-Level Public Health: Collective victim (powerless/trapped) — cannot exit legalization regime; bears aggregate health outcome regardless of individual choice; snare perspective
 *   - Analytical Observer: Cross-perspective analyst (analytical/analytical) — sees tangled rope structure: genuine coordination (user autonomy, market transparency, tax revenue, harm regulation) alongside embedded extraction (corporate profit incentive, vulnerable population exposure, regulatory capture risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.48).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Substance Control Legitimacy (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '79dbc99e-06f0-472a-b312-94849d2f4f2c').
narrative_ontology:cs_kernel_codification('79dbc99e-06f0-472a-b312-94849d2f4f2c', formalized).
narrative_ontology:cs_authority_grounding('79dbc99e-06f0-472a-b312-94849d2f4f2c', extraction).
narrative_ontology:cs_reading_relation('79dbc99e-06f0-472a-b312-94849d2f4f2c', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('79dbc99e-06f0-472a-b312-94849d2f4f2c', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('79dbc99e-06f0-472a-b312-94849d2f4f2c', foundational, adult_autonomy_over_substance_choice).
narrative_ontology:cs_axiom_status(adult_autonomy_over_substance_choice, holdable).
narrative_ontology:cs_axiom_grounding('79dbc99e-06f0-472a-b312-94849d2f4f2c', adult_autonomy_over_substance_choice, deontological).
narrative_ontology:cs_axiom('79dbc99e-06f0-472a-b312-94849d2f4f2c', foundational, third_party_harm_as_sole_legitimacy_ground).
narrative_ontology:cs_axiom_status(third_party_harm_as_sole_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('79dbc99e-06f0-472a-b312-94849d2f4f2c', third_party_harm_as_sole_legitimacy_ground, conventional).
narrative_ontology:cs_reference_frame('79dbc99e-06f0-472a-b312-94849d2f4f2c', autonomous_adult_governance_framework).
narrative_ontology:cs_drift_state('79dbc99e-06f0-472a-b312-94849d2f4f2c', contemporary_regulatory_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('79dbc99e-06f0-472a-b312-94849d2f4f2c', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_users_within_jurisdiction).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_market_producers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_revenue_recipients).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_bearers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, vulnerable_populations_access).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, public_health_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADULT USER (ROPE) — Under legalization, the adult user is released from victim status. Exit is now real and low-cost: use the legal product openly. The constraint shifts from 'criminal penalty for possession' to 'regulation of production and marketing.' The user experiences the remaining constraint (product quality standards, advertising restrictions, age gates) as coordination: legitimate rules that enable safe market function. No extraction from the user's structural position — they are mobile, can choose participation, and benefit from legal status.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD-PARTY HARM BEARER (TANGLED ROPE) — Experiences mixed coordination and extraction. Legitimate coordination function: state regulation of impaired driving, secondhand exposure, workplace safety. But extraction is embedded: the legal market's profit incentive creates pressure to maximize consumption, including into externality-bearing populations. The third-party harm bearer bears costs of market expansion while benefiting from regulation of the most extreme harms. Exit is constrained (geographic relocation, social isolation) — cannot fully escape secondhand exposure or traffic risk from legal substance users.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VULNERABLE POPULATIONS (SNARE) — Adolescents and addiction-prone individuals face the highest structural extraction under legalization. Market logic incentivizes aggressive marketing and product optimization for addictive potential. The 'autonomy' framing applies only to hypothetically rational adults; minors are trapped and lack genuine exit. Regulatory gates (age restrictions) are notoriously weak under market pressure. No coordination benefit accrues to this group — they bear extraction (addiction initiation, escalation) without the autonomy protection the reading extends to competent adults.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL MARKET PRODUCERS (TANGLED ROPE) — Genuine coordination function: standardization, quality control, tax collection, public health reporting. But extraction is structurally embedded: the profit motive aligns with maximizing consumption volume and potency. Market producers have arbitrage options (relocate to jurisdictions with laxer regulation, lobby for deregulation, product line expansion toward more addictive variants). They benefit from the legalization reading's core premise (autonomy removes criminal barriers) and bear some costs (regulation, taxation). Net beneficiary position — extraction runs toward this group.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH REGULATION COALITION (SCAFFOLD) — Sees legalization as a temporary coordination problem with a sunset: robust regulatory infrastructure (taxation, marketing restrictions, age gates, product potency limits, addiction-treatment funding) can decouple user autonomy from market extraction. This perspective assumes the extraction can be bounded through active enforcement — sunset clause is conditional on regulatory capacity holding. If regulatory capture occurs (corporate influence over limits, weakening of age enforcement), the scaffold becomes a snare. The coalition has organized capacity and exit pathways (advocacy, regulation tightening) that moderate their vulnerability.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: POPULATION-LEVEL PUBLIC HEALTH (SNARE) — The aggregate health outcome of a population cannot exit the legalization regime. If legalization produces net negative health outcomes (addiction prevalence, overdose mortality, secondhand harms exceed quality-of-life gains from user autonomy), the public health function is trapped in extraction. No exit options exist at the population level — individual users gain autonomy but the collective bears the cost. This perspective generates the strongest tension with the legalization reading's autonomy axiom: aggregate welfare may require restricting individual autonomy.
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The legalization reading instantiates a genuine coordination mechanism (user autonomy, market transparency, tax revenue, harm regulation) alongside embedded extraction (corporate profit-seeking, vulnerable population targeting, regulatory capture risk). The framework produces real benefits (removing criminalization, enabling quality control, funding treatment) and real harms (market incentive to expand consumption, age-gate weakening, addiction-treatment underfunding despite tax revenue). The framework is coherent but not benign — it trades user autonomy for market-embedded extraction. The observer sees tangled rope, not rope (which would deny extraction) and not snare (which would deny coordination).
constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_legitimacy__legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, increasing from 0.28 at t0): The legalization reading produces moderate-to-high extractiveness because the legal market structure creates profit incentives misaligned with public health. Corporate producers benefit from maximizing consumption (especially into vulnerable populations), while users gain autonomy. The trajectory shows rising extractiveness over time as market maturity increases pressure for deregulation and consumption expansion. At t0 (early legalization), extractiveness is lower because regulatory apparatus is fresh and enforcement is strong. By t10, market consolidation and regulatory fatigue have enabled extraction to rise. Suppression (0.48, falling from 0.62 at t0): Legalization reduces state coercion compared to prohibition (no criminal penalties), but substitutes market-embedded suppression: addictive product design, aggressive marketing to vulnerable populations, product placement and availability targeting. The falling trajectory reflects removal of criminal apparatus, but the underlying suppression mechanism (market incentive to maximize consumption) persists. Theater ratio (0.38, rising from 0.28 at t0): Legalization minimizes performative activity in user regulation (age checks, point-of-sale compliance) but increases theater in policy discourse (claims about 'responsible regulation,' 'harm reduction through taxation,' regulatory review boards that lack enforcement capacity). The rising trajectory reflects erosion of actual regulatory enforcement and substitution of symbolic compliance.
 *
 * PERSPECTIVAL GAP:
 *   The legalization reading produces stark perspectival divergence. The adult user experiences ROPE: autonomy, low exit cost, legitimate coordination function (product quality, safety standards). The legal market producer experiences TANGLED ROPE: profit opportunity (coordination function: market standardization, quality assurance) alongside extraction (regulatory compliance cost, taxation). The third-party harm bearer experiences TANGLED ROPE: coordination (state regulation of impaired driving, workplace safety) alongside extraction (secondhand harms, traffic risk). Vulnerable populations experience SNARE: no autonomy protection, no exit, pure extraction via addictive product design and aggressive marketing. Population-level public health experiences SNARE: trapped aggregate outcome, cannot exit regime, bears net health cost if legalization increases addiction prevalence. The public health coalition experiences SCAFFOLD: sees regulation as temporary coordination problem with sunset (robust regulatory infrastructure can decouple autonomy from extraction). The analytical observer experiences TANGLED ROPE: genuine coordination functions embedded with structural extraction mechanisms. The gap between user (rope) and vulnerable population (snare) is the crux — legalization's core premise (competent adult autonomy) does not apply to minors and addiction-prone individuals, who bear the extraction while users gain the benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d value) is determined by structural position in the extraction flow: (1) Adult user (d ≈ 0.15): beneficiary + mobile exit → low d → low experienced extraction (rope classification). (2) Legal market producer (d ≈ 0.20): beneficiary + arbitrage exit → low d → low experienced extraction (but tangled rope because victims exist). (3) Third-party harm bearer (d ≈ 0.65): mixed (some benefits from harm regulation, but bears net costs from market expansion) + constrained exit → moderate d → moderate experienced extraction. (4) Vulnerable population (d ≈ 0.92): victim + trapped → high d → high experienced extraction (snare). (5) Population health (d ≈ 1.00): victim + trapped (collective cannot exit) → maximum d → maximum experienced extraction. (6) Public health coalition (d ≈ 0.50): symmetric (both costs from regulation, benefits from risk reduction) + mobile exit (advocacy, regulation changes) → moderate d → moderate experienced extraction (scaffold). (7) Analytical observer (d ≈ 0.68): observational position, sees full structure, no exit from the framework itself → canonical analytical d. The directionality overrides (none declared here) would be used if, for example, a public health regulator was partially captured by industry — their d would be overridden upward from 0.50 to 0.70 to reflect the hidden extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading resolves mandatrophy by explicitly trading user autonomy for market-embedded extraction. The constraint is not 'is this good or bad?' but 'does this reading honestly declare what it trades away?' The mandatrophy is resolved when: (1) the beneficiary's coordination function is real (user autonomy, market transparency, tax revenue enabling treatment) and (2) the extraction mechanism is explicit (corporate profit-seeking misaligned with public health, vulnerable population targeting, regulatory capture risk). The tangled rope classification means 'both goods and harms are real, neither can be eliminated without losing the structure.' Prohibition reading eliminates extraction by eliminating market, but loses coordination (user autonomy). Harm-reduction reading attempts to preserve coordination while constraining extraction through active public health authority. The legalization reading chooses a different trade: accept extraction as the price of user autonomy. The framework is coherent and defensible, but not benign. Mandatrophy is resolved by refusing to naturalizes the extraction as inevitable or deny it exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_threshold_definition,
    'What makes a substance user''s decision ''competent'' enough to warrant autonomy protection, and who adjudicates this threshold?',
    'Empirical threshold analysis: at what point do neurodevelopmental maturity, information access, and addiction vulnerability prevent genuine autonomy? Comparative legal review of how different jurisdictions operationalize ''competent adult.''',
    'If threshold is low (age 18+, no screening): many incompetent decisions are autonomy-protected, extraction increases. If threshold is high (neuroscience-based maturity, addiction screening): autonomy is narrow, and the legalization reading collapses toward harm-reduction framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_threshold_definition, empirical, 'Definition and adjudication of competence threshold for autonomy protection').

omega_variable(
    third_party_harm_boundary,
    'Which harms count as ''third-party harms'' justifying state restriction, and which count as individual risk that autonomy should permit?',
    'Causal tracing: secondhand smoke exposure, impaired driving, fetal exposure, workplace performance — each can be framed as third-party harm or as individual risk. Legal case analysis and epidemiological studies tracing attribution and causality.',
    'If boundary is narrow (only direct physical injury counts): extraction increases (many harms are externalized). If boundary is broad (includes productivity loss, social outcomes, family disruption): the legalization reading approaches harm-reduction framing, and state authority expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_boundary, conceptual, 'Boundary between third-party harms and individual risks').

omega_variable(
    market_extraction_vs_tax_benefit,
    'Does legal market taxation and regulation produce net public benefit sufficient to offset corporate profit-driven consumption maximization?',
    'Cost-benefit analysis: tax revenue and treatment funding vs. addiction externalities, emergency healthcare, productivity loss, overdose mortality. Comparative jurisdictional study of legal vs. prohibition regimes.',
    'If taxation/regulation exceeds extraction costs: tangled rope classification holds. If extraction costs exceed benefits: constraint reclassifies toward snare. This is the pivot point where the legalization reading''s empirical viability depends on regulatory capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_extraction_vs_tax_benefit, empirical, 'Net public benefit of legal market versus extraction costs').

omega_variable(
    regulatory_capture_trajectory,
    'Does the legal market structure inevitably produce regulatory capture, weakening consumption limits and age enforcement over time?',
    'Historical pattern analysis: tobacco, alcohol, pharmaceutical industries and their lobbying trajectories. Predictive modeling of capture likelihood under different regulatory architectures.',
    'If capture is inevitable: the scaffold perspective is false — regulation will degrade toward snare. If capture is avoidable: scaffold remains viable with strong institutions. This determines whether legalization is a temporary coordination (scaffold) or a durable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_trajectory, empirical, 'Inevitability and trajectory of regulatory capture in legal markets').

omega_variable(
    sibling_reading_empirical_delta,
    'What empirical outcomes distinguish legalization, harm-reduction, and prohibition readings? Under what data would each reading''s core premise be falsified?',
    'Longitudinal comparative study: jurisdictions under each reading''s policy regime. Track addiction prevalence, overdose mortality, criminal justice burden, tax revenue, vulnerable population outcomes, and third-party harm indices.',
    'If legalization produces lower overall harm than prohibition: legalization axiom holds. If harm-reduction produces better outcomes: axiom is overridden. If empirical data favors prohibition on harm measures: legalization reading''s grounding_type (autonomy axiom is deontological, not empirical) protects it from foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_delta, empirical, 'Comparative empirical outcomes of legalization vs. harm-reduction vs. prohibition').

omega_variable(
    kernel_coexistence_vs_foreclosure,
    'Do legalization, harm-reduction, and prohibition readings genuinely coexist as coherent frameworks, or does one''s adoption logically foreclose the others?',
    'Philosophical reconstruction: trace the core normative premises of each reading and identify points of logical incompatibility. Assess whether disagreement is about empirical consequences (coexistence) or foundational values (foreclosure).',
    'If coexistence: readings are held by different parties without logical mutual exclusion (most likely outcome). If foreclosure: one reading''s axioms logically rule out sibling frameworks, enabling the engine to compute terminal attractor states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_coexistence_vs_foreclosure, conceptual, 'Logical relationships among sibling readings of the substance control kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scl_leg_theater_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(scl_leg_theater_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(scl_leg_theater_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(scl_leg_extractiveness_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(scl_leg_extractiveness_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(scl_leg_extractiveness_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(scl_leg_suppression_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(scl_leg_suppression_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(scl_leg_suppression_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).

% DUAL FORMULATION NOTE:
% The kernel 'substance_control_legitimacy' decomposes into three constraint stories, one per reading. Each reading instantiates a different ε value reflecting different policy structures: prohibition_reading (ε ≈ 0.68, snare-heavy) criminalizes use and embeds enforcement extraction; legalization_reading (ε ≈ 0.52, tangled rope) legalizes use and shifts extraction to market mechanisms; harm_reduction_reading (ε ≈ 0.40, tangled rope) treats use as public health issue and emphasizes coordination over extraction. The three stories are linked via network.affects_constraints — legalization influences both siblings by shifting resource distribution and changing enforcement capacity. Do not try to fold these into one story with observable-dependent ε; they are genuinely different structural configurations grounded in different normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
