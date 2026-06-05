% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading of the substance_control_authority kernel frames
 *   state authority as the regulator of drug markets operating as legal
 *   commerce with quality and access controls. This reading instantiates one
 *   core commitment: that unregulated supply and criminal enforcement create
 *   greater total harm than regulated legal markets, and that state capacity
 *   to set quality standards, track supply chains, conduct epidemiological
 *   surveillance, and link users to treatment services produces net public
 *   benefit. The legalization reading directly opposes the prohibition
 *   reading (which holds that legal status inherently increases use and harm)
 *   and coexists with the harm_reduction reading (which accepts use while
 *   minimizing harms through public health without necessarily legalizing
 *   commerce). The constraint exhibits tangled_rope classification across
 *   most perspectives: genuine coordination functions (quality assurance, tax
 *   collection, epidemiological tracking, demand management) combined with
 *   asymmetric extraction (users pay regulated prices, suppliers face
 *   compliance costs, communities bear externalized consumption harms). The
 *   extractiveness value (0.38) reflects moderate conversion of criminal
 *   extraction into regulatory transaction costs — substantial cost reduction
 *   compared to prohibition but not zero-cost coordination. The theater ratio
 *   (0.35) remains modest because legalization preserves the state's monopoly
 *   on market legitimacy (unlike harm reduction's public health framing). The
 *   suppression requirement shows decline over the measurement interval (0.55
 *   → 0.42), modeling the reduction in coercive enforcement as users
 *   transition from criminal status to regulatory status, and reflecting the
 *   historical trajectory in legalization jurisdictions.
 *
 * KEY AGENTS:
 *   - Users Exiting Criminal Markets: Primary beneficiaries (moderate/mobile) — exit from criminalization and black-market arbitrage; gain access to quality-assured supply and harm reduction services
 *   - Regulated Commercial Suppliers: Secondary beneficiaries (powerful/mobile) — gain legal market access and state enforcement of property rights; constrained by compliance costs and licensing restrictions
 *   - Public Health / Regulatory Agencies: Institutional beneficiary (institutional/constrained) — gain authority to set standards and conduct surveillance; face resource pressures and political vulnerability
 *   - State Revenue / Law Enforcement: Institutional beneficiary (institutional/arbitrage) — capture tax revenue and redeploy enforcement capacity; extract coordination rent from monopoly authority
 *   - Illegal Market Participants / Unregulated Suppliers: Primary victims (powerless/trapped) — criminalized or displaced; face market collapse and criminal liability with no exit option
 *   - Communities Experiencing Increased Use: Secondary victims (powerless/trapped) — bear externalized consumption harms; lack voice in licensing decisions
 *   - International Prohibition Regime: Degraded institutional actor (institutional/arbitrage) — loses enforcement capacity as states unilaterally exit; persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.38).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '4aeeebcc-e387-4583-9202-386a2d600461').
narrative_ontology:cs_kernel_codification('4aeeebcc-e387-4583-9202-386a2d600461', formalized).
narrative_ontology:cs_authority_grounding('4aeeebcc-e387-4583-9202-386a2d600461', extraction).
narrative_ontology:cs_interpretation_layer_present('4aeeebcc-e387-4583-9202-386a2d600461').
narrative_ontology:cs_reading_relation('4aeeebcc-e387-4583-9202-386a2d600461', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4aeeebcc-e387-4583-9202-386a2d600461', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('4aeeebcc-e387-4583-9202-386a2d600461', foundational, markets_can_be_regulated_for_public_benefit).
narrative_ontology:cs_axiom_status(markets_can_be_regulated_for_public_benefit, holdable).
narrative_ontology:cs_axiom_grounding('4aeeebcc-e387-4583-9202-386a2d600461', markets_can_be_regulated_for_public_benefit, empirically_contingent).
narrative_ontology:cs_axiom('4aeeebcc-e387-4583-9202-386a2d600461', foundational, state_monopoly_on_legitimate_supply_enables_public_health_authority).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_supply_enables_public_health_authority, holdable).
narrative_ontology:cs_axiom_grounding('4aeeebcc-e387-4583-9202-386a2d600461', state_monopoly_on_legitimate_supply_enables_public_health_authority, instrumental).
narrative_ontology:cs_reference_frame('4aeeebcc-e387-4583-9202-386a2d600461', regulated_legal_commerce_framework).
narrative_ontology:cs_drift_state('4aeeebcc-e387-4583-9202-386a2d600461', contemporary_global_prohibition_treaty_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4aeeebcc-e387-4583-9202-386a2d600461', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, users_exiting_criminal_markets).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_suppliers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_revenue_collectors).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, illegal_market_participants).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unregulated_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_experiencing_increased_use).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: USERS EXITING CRIMINAL MARKETS (ROPE) — Primary beneficiaries. Exit option from criminal prosecution (trapped → mobile) and access to regulated supply reduces harm and eliminates black-market arbitrage. Experiences the constraint as genuine coordination: legal status enables safe access, quality assurance, and harm reduction services. No systematic extraction — the user's exit option and beneficiary status produce low directionality.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATED COMMERCIAL SUPPLIERS (TANGLED ROPE) — Secondary beneficiaries with structural extraction. Gain access to a legal market and state enforcement of their property rights, but face regulatory compliance costs, tax obligations, and market-share caps or licensing restrictions that reduce profit potential versus unregulated competitors. Genuine coordination function (tax revenue, product quality standards, chain-of-custody tracking) combined with asymmetric extraction of regulatory rent. Mobile exit option reduces experienced extraction — suppliers can exit by operating in other jurisdictions or sectors — but regulatory barrier-to-entry extracts from new entrants.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ILLEGAL MARKET PARTICIPANTS / UNREGULATED SUPPLIERS (SNARE) — Primary victims. Criminalized or displaced by legalization. No exit from suppression: criminal liability persists for continued illegal operation, market share collapses as legal alternatives capture demand, and opportunity cost (cannot transition to legal market if licensing restricted to established operators or capital-wealthy firms). Trapped by combination of legal prohibition on their operations and market displacement — asymmetric extraction with no coordination function.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PUBLIC HEALTH / REGULATORY AGENCIES (TANGLED ROPE) — Institutional beneficiaries with constrained exit. Gain authority to set quality standards, track supply chains, conduct harm surveillance, and link users to treatment services. But face resource pressures, demand variability, and political vulnerability to public opinion on use rates or related harms. Genuine coordination function (epidemiological tracking, overdose response, quality assurance) combined with asymmetric extraction (agencies extract monopoly authority over market legitimacy, which becomes politically contested if harms rise).
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE REVENUE / LAW ENFORCEMENT APPARATUS (TANGLED ROPE) — Institutional beneficiaries with arbitrage exit (can recapture enforcement resources and redeploy to other domains). Gain tax revenue from legal sales and redeploy law enforcement capacity from drug enforcement to other crimes. But face political pressure to prevent use escalation and must maintain enforcement apparatus against illegal importation. Genuine coordination function (capturing tax base, redirecting enforcement) combined with asymmetric extraction of monopoly authority over legitimate supply — state extracts the coordination rent that enforcement makes possible.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: COMMUNITIES EXPERIENCING INCREASED USE (SNARE) — Potential secondary victims. If legalization increases use prevalence or concentrates availability in certain neighborhoods, communities bear externalized costs (public consumption, environmental damage, health infrastructure strain) without participation in benefit distribution or regulatory decision-making. Trapped by geography and inability to opt out of regional market dynamics. Suppression comes from lack of voice in licensing decisions and inability to exit the local market environment.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 7: INTERNATIONAL PROHIBITION REGIME (PITON) — Degraded institutional framework. UN drug control conventions and bilateral treaties codified 20th-century prohibition — now partially orphaned as states unilaterally legalize. The regime persists through treaty inertia, diplomatic face-saving, and bureaucratic entrenchment (UNODC, DEA, INTERPOL apparatus) rather than functional legitimacy. Extractiveness is low (regime has lost enforcement capacity as states exit) and theater is high (continued declarations of prohibition without enforcement mechanism). Piton classification reflects the institutional degradation of the prohibition regime as individual states adopt legalization.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / LEGALIZATION READING (ROPE) — The analytical position on legalization as pure coordination mechanism. From a civilizational timescale and global scope, legalization converts an extractive criminal enforcement system (prohibition) into a regulatory coordination system (quality standards, tax collection, demand management). The mechanism shift is the core claim: extraction moves from illegal market rents and law enforcement profiteering to regulatory compliance costs and tax obligations. If the shift is genuine, the constraint becomes a coordination mechanism with inherent transaction costs (regulation, compliance monitoring) — classic rope structure. This perspective risks collapsing into false coordination if regulatory capture occurs (law enforcement maintains extraction under new regime) — see mandatrophy analysis.
constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_authority__legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_authority__legalization_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting genuine conversion of extraction mechanisms from criminal to regulatory. In prohibition regimes, extraction flows through criminal enforcement profiteering, user vulnerability to arrest, and supplier rents from illegality. In legalization, extraction flows through regulatory compliance costs, tax obligations, and state monopoly on legitimate supply authority. The total extractiveness decreases because regulatory overhead is substantially lower than enforcement + criminal supply costs, but does not reach rope-level (0.45 ceiling for rope base extraction) because regulatory capacity creates new extraction vectors: licensing barriers, compliance asymmetries favoring capital-wealthy firms, and community harm concentration. Theater ratio (0.35): Moderate-low. Legalization reduces the performative content compared to prohibition (which requires elaborate enforcement theater and incarceration rituals) because regulatory legitimacy rests on technical standards and epidemiological data rather than crime control narratives. However, it remains above rope baseline (0.15–0.20) because regulatory agencies must maintain public legitimacy around their quality certifications and their role in preventing use escalation — this requires institutional theater that pure coordination mechanisms (e.g., technical standards bodies) do not require. Suppression (0.42): Moderate, and declining. At t0, suppression is high (0.55) because the constraint operates during transition, when criminal enforcement structures persist alongside emerging legality, creating uncertainty and dual-market dynamics. By t10, suppression declines to 0.42 as criminal enforcement fades and regulatory frameworks stabilize, but suppression does not reach rope-level (0.15–0.20) because regulatory apparatus maintains meaningful coercive capacity (product seizure, licensee sanctions, enforcement against illegal suppliers) and because users exiting criminal status enter regulatory status — they trade criminal suppression for regulatory compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between prohibition and legalization readings manifests most sharply in the powerless/trapped classification. In prohibition, users are trapped by criminalization and black-market exploitation simultaneously — snare with high extractiveness. In legalization, users are mobile (exit from criminalization) — rope with low extractiveness. The institutional perspective also shows a dramatic gap: in prohibition, law enforcement benefits from the constraint (institutional/arbitrage sees rope); in legalization, law enforcement must redeploy (institutional/arbitrage sees tangled_rope with constrained exit because resources are redefined). The analytical observer's perspective represents the key mandatrophy decision: legalization can be framed as rope (state capacity to regulate is genuine coordination) or tangled_rope (regulatory capture risk and asymmetric extraction persist). The choice depends on empirical resolution of regulatory capture omega and demand elasticity omega — if capture occurs and use surges, snare classification may prevail even from the analytical position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from beneficiary/victim status and exit options. Users exiting criminal markets: victim in prohibition (trapped, d ≈ 0.95) → beneficiary in legalization (mobile, d ≈ 0.15) — directionality shifts dramatically. Regulated suppliers: beneficiary status (institutional arbitrage in legal market) but constrained by licensing barriers — d ≈ 0.30 (beneficiary with partial constraints). Public health agencies: beneficiary + constrained exit → d ≈ 0.40 (mixed). State revenue: beneficiary + arbitrage → d ≈ 0.20. Illegal market participants: victim + trapped → d ≈ 0.95. Communities with increased use: victim + trapped → d ≈ 0.90. International regime: institutional arbitrage (can redeploy apparatus) but degraded → d ≈ 0.35. Analytical position derives d from the balance of beneficiaries and victims: multiple beneficiary groups (users, agencies, state) vs. multiple victim groups (illegal operators, affected communities, constrained suppliers) → d ≈ 0.55 (balanced but with organized beneficiaries and powerless victims, so slight directionality toward extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The mandatrophy for substance_control_authority is resolved by decomposing the kernel into three distinct constraint stories with different ε values and different structural positions. This file (legalization_reading) instantiates one: state as regulator of legal markets. The prohibition_reading (separate file) instantiates state as enforcer of criminalization. The harm_reduction_reading (separate file) instantiates state as minimizer of health harms without necessarily legalizing commerce. Each reading has its own ε because the observable (state authority type) produces genuinely different constraints when measured along the legalization vs. prohibition vs. harm-reduction axes. This is not a case of the same constraint seen from multiple perspectives — the constraint itself is different. The mandatrophy dissolves when the framework recognizes that 'substance control authority' is not a single constraint but a kernel (a contested commitment claim) that admits multiple readings. Each reading is a well-defined constraint. The tangled_rope classification across most perspectives (rather than mountain, rope, or snare) reflects the balanced presence of coordination (quality standards, tax collection, surveillance, treatment linkage) and extraction (regulatory compliance costs, license barriers, monopoly authority, use increase harms). This balance is the reading's signature — it is neither coordination-only (rope) nor pure extraction (snare) nor immutable (mountain). The presence of significant omegas reflects the reading's dependence on empirical outcomes (regulatory capture, demand elasticity, supply-chain integrity) that are not determined by the legalization commitment itself but by implementation quality and international coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_permanence,
    'Will legalization regimes prevent law enforcement and allied bureaucracies from reconstructing extraction mechanisms under regulatory framing (licensure capture, enforcement rent-seeking, compliance cost asymmetries)?',
    'Longitudinal comparison of actual regulatory costs, enforcement budgets, and arrest patterns in legalized jurisdictions vs. baseline; identification of institutional actors who benefit from regulatory apparatus and their influence on policy design',
    'If capture occurs: legalization is tangled_rope or snare with persistent extraction, not rope; the beneficiary class shifts from criminal suppliers to regulatory bureaucracies. If capture is prevented: rope classification confirmed; extraction genuinely converts to coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_permanence, empirical, 'Whether regulatory apparatus reconstitutes extraction mechanisms under new regime').

omega_variable(
    demand_elasticity_and_use_volume_increase,
    'Does legalization produce measurable increases in use prevalence? What magnitude of increase would constitute a structural harm offset that undermines the beneficiary framing?',
    'Comparative epidemiology across legalized jurisdictions (Colorado, Canada, Oregon) with matched controls; time-series analysis of prevalence before and after legalization, controlling for secular trends and sample design',
    'If use increases substantially (>20% prevalence increase): secondary victim category (communities experiencing increased use) becomes primary; snare classification for non-users strengthens. If increase is minimal or offset by reduced harms: rope/tangled_rope from user and public health perspectives confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_elasticity_and_use_volume_increase, empirical, 'Magnitude of use prevalence increase attributable to legalization').

omega_variable(
    kernel_foreclosure_vs_coexistence,
    'Does the legalization reading logically foreclose prohibition within a single framework, or do prohibition and legalization coexist as live alternatives held by different parties?',
    'Examination of foundational claims: does legalization''s core claim (markets can be regulated for public benefit) contradict prohibition''s core claim (legal status increases use volume and social harm) such that no framework can hold both? Or are these empirically contingent disagreements about elasticity, regulability, and enforcement efficacy?',
    'If forecloses: only one reading can be correct. If coexists: both are structurally live options in a contested domain, and the kernel remains open. Classification determines cs_structure.reading_relations choice (forecloses vs coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_vs_coexistence, conceptual, 'Whether legalization logically forecloses prohibition or both coexist as live alternatives').

omega_variable(
    supply_chain_internationalization_leakage,
    'Do legalized domestic markets remain intact, or does international prohibition (remaining in most other jurisdictions) create arbitrage incentives that leak legal supply into illegal international markets?',
    'Forensic tracking of product traceability, source attribution of seized cannabis in prohibition-state jurisdictions; comparison of quantity supplied to legal market vs. estimated total demand (domestic + leakage)',
    'If significant leakage: legalization in one jurisdiction does not eliminate illegal markets — it transforms them into re-export operations. Snare classification for international victims and transnational criminal networks becomes relevant. Tangled_rope classification for regulated suppliers shifts if they face pressure to supply illegal export demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_internationalization_leakage, empirical, 'Extent of supply-chain leakage from legal to illegal international markets').

omega_variable(
    regimes_differ_along_kernel_reading_axis,
    'This constraint (legalization reading) is one of three readings of the substance_control_authority kernel. The prohibition_reading and harm_reduction_reading are structurally distinct constraints with different ε values, different beneficiary/victim sets, and different authority framings. Omega variable: have the sibling readings been decomposed as separate constraint stories and linked via network.affects_constraints, or are they being confounded?',
    'Verification that three separate JSON files exist: substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading, and this file (legalization_reading). Each has its own ε, its own base_properties, its own perspectives. All three are linked via network.affects_constraints.',
    'If decomposed: ε-invariance principle satisfied; each reading is a clean constraint. If confounded: analytical error — the legalization reading''s ε may be artificially low if it is averaged with harm_reduction''s ε or prohibition''s ε. Recompute ε for legalization-only (state as regulator of legal markets for quality/access) without averaging across sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regimes_differ_along_kernel_reading_axis, conceptual, 'Kernel readings properly decomposed as distinct constraint stories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scal_theater_ratio_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(scal_theater_ratio_t5, substance_control_authority__legalization_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(scal_theater_ratio_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(scal_base_extractiveness_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(scal_base_extractiveness_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(scal_base_extractiveness_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(scal_suppression_requirement_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(scal_suppression_requirement_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(scal_suppression_requirement_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, illegal_supply_market_rents).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, criminal_enforcement_apparatus).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, regulatory_capture_extraction).

% DUAL FORMULATION NOTE:
% Legalization reading is one of three kernel readings of substance_control_authority. The prohibition_reading and harm_reduction_reading are separate constraint stories with different ε values and different structural positions. This reading's extractiveness (0.38) reflects regulatory coordination with asymmetric cost distribution — distinct from prohibition's extractiveness (state-criminal enforcement profiteering, estimated 0.55–0.65) and harm_reduction's extractiveness (public health provision without commerce, estimated 0.15–0.25). The three readings are linked as members of a constraint family via network.affects_constraints. Each reading's commentary addresses how it influences and is influenced by its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
