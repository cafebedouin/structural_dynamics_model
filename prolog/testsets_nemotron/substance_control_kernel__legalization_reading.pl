% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalized Substance Market with Externality Taxation
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the legalization reading of the
 *   substance_control_kernel: substance use is an individual liberty issue
 *   where the state intervenes only to prevent third-party harm and capture
 *   externality costs via taxation. Users are no longer victims of state
 *   enforcement; instead, a legal industry emerges as beneficiary, the state
 *   collects tax revenue, and third parties exposed to externalities (DUI,
 *   secondhand smoke, healthcare costs) enter the victim set. The black
 *   market either collapses or persists in gray areas (high-tax arbitrage,
 *   underage supply). The interval T=0 represents the
 *   prohibition-to-legalization transition; T=20 represents a mature legal
 *   market. Extraction declines from prohibition levels as state enforcement
 *   against users ceases, but stabilizes above zero due to tax extraction
 *   from consumers and industry profit margins. Suppression requirement drops
 *   sharply as criminal penalties for possession are removed, leaving only
 *   regulatory enforcement (licensing, age verification, DUI). Theater ratio
 *   declines as performative 'war on drugs' apparatus is dismantled, though
 *   some regulatory theater persists.
 *
 * KEY AGENTS:
 *   - consumer_adults: Primary beneficiary — gains legal access, bears tax cost (powerful/arbitrage)
 *   - legal_substance_industry: Primary beneficiary — captures market revenue, shapes regulation (institutional/arbitrage)
 *   - state_tax_authority: Beneficiary/agenda_setter — collects externality taxes, sets regulatory framework (institutional/analytical)
 *   - third_parties_exposed_to_externalities: Victim — bears DUI risk, secondhand exposure, healthcare externalities (powerless/constrained)
 *   - problematic_users_post_legalization: Victim — may face commercial predation via dependence-engineered products (moderate/identity_locked)
 *   - public_health_authorities: Observer — monitor population health outcomes, advise regulation (institutional/analytical)
 *   - criminal_justice_system_former: Excluded — loses enforcement role, budget, institutional purpose (institutional/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.22).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.18).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalized Substance Market with Externality Taxation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '89197bfe-1043-4a41-98f5-c5b01957f65a').
narrative_ontology:cs_kernel_codification('89197bfe-1043-4a41-98f5-c5b01957f65a', distributed).
narrative_ontology:cs_authority_grounding('89197bfe-1043-4a41-98f5-c5b01957f65a', distributed).
narrative_ontology:cs_reading_relation('89197bfe-1043-4a41-98f5-c5b01957f65a', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('89197bfe-1043-4a41-98f5-c5b01957f65a', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('89197bfe-1043-4a41-98f5-c5b01957f65a', foundational, bodily_autonomy_includes_substance_choice).
narrative_ontology:cs_axiom_status(bodily_autonomy_includes_substance_choice, holdable).
narrative_ontology:cs_axiom_grounding('89197bfe-1043-4a41-98f5-c5b01957f65a', bodily_autonomy_includes_substance_choice, deontological).
narrative_ontology:cs_axiom('89197bfe-1043-4a41-98f5-c5b01957f65a', foundational, state_legitimacy_requires_harm_principle).
narrative_ontology:cs_axiom_status(state_legitimacy_requires_harm_principle, holdable).
narrative_ontology:cs_axiom_grounding('89197bfe-1043-4a41-98f5-c5b01957f65a', state_legitimacy_requires_harm_principle, deontological).
narrative_ontology:cs_axiom('89197bfe-1043-4a41-98f5-c5b01957f65a', secondary, externality_taxation_sufficient_for_harm_internalization).
narrative_ontology:cs_axiom_status(externality_taxation_sufficient_for_harm_internalization, holdable).
narrative_ontology:cs_axiom_grounding('89197bfe-1043-4a41-98f5-c5b01957f65a', externality_taxation_sufficient_for_harm_internalization, instrumental).
narrative_ontology:cs_reference_frame('89197bfe-1043-4a41-98f5-c5b01957f65a', prohibition_regime_collapse).
narrative_ontology:cs_drift_state('89197bfe-1043-4a41-98f5-c5b01957f65a', mature_legal_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89197bfe-1043-4a41-98f5-c5b01957f65a', '2026-08-15T14:22:00Z').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_authority).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, consumer_adults).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, problematic_users_post_legalization).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, harm_principle_mill).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, prohibition_failure_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults who choose to use legalized substances. They gain legal access, product safety, quality consistency, and reliable supply. They pay excise taxes that approximate marginal external cost. They can exit by not purchasing, substituting other goods, or (where permitted) home production. Their choice set is broad and their power to shape the market through demand is significant.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, consumer_adults, beneficiary,
    powerful, biographical, arbitrage, national).

% Licensed producers, processors, distributors, and retailers of legalized substances. They capture the market revenue previously going to illicit markets. They shape regulation through lobbying, campaign contributions, and regulatory capture risk. They design products, set prices, and control marketing. Their exit options are strong — they can diversify, relocate, or shift product lines. They are the primary organized beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, legal_substance_industry, agenda_setter).

% Government agencies that design and collect excise taxes on legalized substances, allocate revenue to externality mitigation (DUI enforcement, treatment, healthcare), and enforce regulatory compliance (licensing, age verification, product standards). They benefit from a new revenue stream but bear administrative and enforcement costs. They set the regulatory agenda. Their exit is analytical — they cannot exit the policy domain but can reform the tax/regulatory structure.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_tax_authority, beneficiary).

% Individuals and communities bearing the spillover costs of legalized substance use: victims of impaired driving, household members exposed to secondhand smoke/vapor, neighbors of retail outlets, taxpayers covering residual healthcare costs not fully offset by tax revenue. They did not consent to the legal market and have limited exit — they cannot avoid public roads, shared air, or fiscal exposure. Their power to influence the constraint is minimal.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_parties_exposed_to_externalities, payer,
    powerless, biographical, constrained, local).

% Users who develop dependence or use disorders in the legal market. They benefit from safer supply and reduced criminal risk but may face commercial predation: products engineered for dependence potential, marketing targeting heavy users, price structures that exploit inelastic demand. Their exit is identity-locked — dependence physiology fuses their self-concept with the substance, and commercial design exploits this. They bear the cost of their own consumption plus any industry-extracted surplus.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, problematic_users_post_legalization, payer,
    moderate, biographical, identity_locked, national).

% Health agencies monitoring population-level outcomes: use prevalence, use disorder rates, overdose mortality, youth initiation, health equity impacts. They advise regulatory adjustments, allocate treatment resources, and evaluate whether the legal market's externality capture is adequate. They neither collect nor pay the constraint's primary flows but shape its evolution through evidence.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% Police, prosecutors, courts, and correctional institutions that previously enforced prohibition. They lose a major enforcement mission, associated budgets, asset forfeiture revenue, and institutional justification. They are structurally excluded from the legal market's governance but remain in the policy arena advocating for continued enforcement or expanded regulatory roles. Their exit is trapped — institutional identity and budget dependencies bind them to the drug policy domain.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, criminal_justice_system_former, excluded,
    institutional, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of safe, reliable substance supply for consenting adults: quality control, dose consistency, contamination prevention, and a legal market that displaces violent illicit trade. The state coordinates externality pricing through taxation rather than prohibition.
% TRANSFER_FUNCTION: Moves money from consumer adults (via retail price including tax and industry markup) to: (1) legal substance industry (profit margin above marginal cost), (2) state tax authority (excise tax revenue for externality mitigation and general fund), (3) regulatory compliance costs. Third parties bear non-monetary externalities (DUI risk, secondhand exposure) not fully captured by the tax.
% ABSENT_VOICES: Future generations who will bear long-term public health consequences of normalized commercial substance markets; children of problematic users who have no voice in market design; communities disproportionately targeted by industry marketing (low-income, minority) who are underrepresented in regulatory capture analysis. These voices are absent because they are not organized, not yet born, or structurally excluded from the policy negotiation.
% DISAPPEARANCE_RATIONALE: If the legal market and its tax/regulatory framework vanished overnight, the substance market would not disappear — it would revert to illicit or gray-market structures with higher violence, no quality control, no externality capture, and renewed criminalization of users. The world rearranges because the constraint actively structures the market's form; its removal changes the equilibrium.
% FOUNDING_PROBLEM: Prohibition's catastrophic failure: mass incarceration for possession, violent black markets corrupting institutions, racialized enforcement disparities, zero quality control causing overdose deaths, billions spent on enforcement with no reduction in availability, erosion of civil liberties through search/seizure expansion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (prohibition's failure) is corroborated by: (1) independent criminological consensus (National Research Council 2014, multiple meta-analyses) that prohibition fails to reduce supply/use and generates net social harm; (2) law enforcement leadership (Law Enforcement Action Partnership, former prosecutors) attesting to prohibition's institutional corruption; (3) public health data showing prohibition-era overdose mortality driven by supply adulteration; (4) fiscal analyses showing enforcement costs exceeding any measurable benefit. No credible source outside the prohibition-benefiting coalition claims prohibition succeeded.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) reflects the mature legal market: tax incidence on consumers plus industry markup above marginal cost, but no state extraction from users via criminal penalties. Suppression (0.18) is low — only regulatory compliance (age gates, licensing, DUI enforcement) remains. Theater ratio (0.12) is low — the constraint performs its stated function (externality capture via tax) with minimal performative excess. Accessibility collapse (0.15) is low — alternatives (not using, using illegally, home production) remain accessible. Resistance (0.08) is minimal — the constraint is broadly accepted by its primary subjects (consumer adults) and its agenda setters (state, industry). The claimed type is rope: genuine coordination problem (safe supply, quality control, externality pricing) solved with minimal coercive overhead and net benefit to participants.
 *
 * PERSPECTIVAL GAP:
 *   From the consumer adult seat, this is a rope — genuine coordination gain (safe, legal access) with fair cost (tax ≈ externality). From the third-party victim seat, it is a snare — they bear externalities with no consent and no exit from spillover harm. From the problematic user seat, it may be a tangled rope — coordination benefit (safer supply) coexists with commercial extraction (dependence-engineered products). The engine computes this divergence from the structural data; the claimed type (rope) reflects the dominant coordination function from the agenda-setter's design intent.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumer adults are structural beneficiaries (d ≈ 0.15) — they gain legal access and quality assurance, paying tax that approximates their externality cost. Legal substance industry is a strong beneficiary (d ≈ 0.05) — captures market surplus, influences regulation via lobbying, faces minimal exit constraints. State tax authority sits near symmetric (d ≈ 0.45) — collects revenue but bears enforcement and regulatory costs. Third parties exposed to externalities are targets (d ≈ 0.85) — bear DUI risk, secondhand harm, and healthcare cost externalities with no direct exit from the legal market's spillovers. Problematic users post-legalization are partial targets (d ≈ 0.65) — if industry exploits dependence, they bear commercial extraction; identity_locked exit due to dependence physiology and commercial targeting. Criminal justice system former actors are excluded (d ≈ 0.75) — lose institutional role but cannot exit the policy domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's failure: criminalization of users, black market violence, enforcement corruption) is substantially resolved — prohibition's extraction from users is eliminated. However, a new mandatrophy risk emerges if the legal industry's commercial incentives create a new extraction vector from dependent users that mirrors the old prohibition extraction. The constraint resolves the original mandatrophy but must be monitored for successor mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the substance_control_kernel — does the legalization reading''s structural profile (users exit victim set, industry emerges, state taxes) instantiate a genuinely different constraint from prohibition and harm reduction readings?',
    'Cross-reading epsilon comparison: if prohibition_reading ε ≈ 0.75 (users as victims, state as enforcer) and harm_reduction_reading ε ≈ 0.35 (users as patients, state as service provider) while this reading ε = 0.22, the kernel decomposes into three structurally distinct constraints per ε-invariance principle.',
    'Confirms the ε-invariance decomposition; if ε values cluster, the kernel may be one constraint with observational variance rather than three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Kernel decomposition into three ε-distinct constraints vs. observational variance on one constraint').

omega_variable(
    externality_capture_completeness,
    'Does the state''s taxation regime actually capture the full marginal external cost of legalized substance use (DUI, secondhand exposure, healthcare externalities, productivity loss), or does legalization create a new extraction vector where industry profits exceed externality capture?',
    'Longitudinal fiscal incidence study comparing tax revenue against quantified external costs over 10+ years in jurisdictions that legalized; industry profit margin analysis vs. externality cost curves.',
    'If externality capture is incomplete, the constraint drifts from rope toward tangled_rope (industry as beneficiary extracting from third-party victims). If capture exceeds costs, the state becomes net extractor from users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_completeness, empirical, 'Whether legalization''s fiscal design matches its stated externality-capture justification').

omega_variable(
    gray_market_persistence,
    'Does the black market collapse entirely post-legalization, or does it persist in gray areas (high-tax arbitrage, underage supply, unregulated potency) creating a dual-market structure that sustains extraction?',
    'Market share tracking of legal vs. illicit channels over time; price gap analysis between legal and gray-market products; enforcement intensity correlation with gray market share.',
    'Persistent gray market reintroduces prohibition-like extraction on residual illicit transactions while legal market extracts via taxation — potential tangled_rope dual structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_market_persistence, empirical, 'Whether legalization eliminates or displaces illicit market extraction').

omega_variable(
    problematic_user_victim_status,
    'Post-legalization, are problematic users (addiction, dependence) genuinely liberated from victim status, or does commercial industry targeting recreate a victim relationship through engineered dependence?',
    'Compare industry marketing spend, product design for dependence potential, and prevalence of use disorder in legal vs. prohibition regimes; longitudinal cohort studies of initiation and escalation.',
    'If industry exploits dependence commercially, problematic_users_post_legalization remain victims — the constraint extracts from them via commercial predation, not state punishment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(problematic_user_victim_status, empirical, 'Whether commercial legalization replaces state victimization with market victimization for dependent users').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sclr_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sclr_tr_t0, observed).
narrative_ontology:measurement(sclr_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(sclr_tr_t5, observed).
narrative_ontology:measurement(sclr_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(sclr_tr_t10, observed).
narrative_ontology:measurement(sclr_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(sclr_tr_t15, observed).
narrative_ontology:measurement(sclr_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(sclr_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(sclr_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sclr_be_t0, observed).
narrative_ontology:measurement(sclr_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(sclr_be_t5, observed).
narrative_ontology:measurement(sclr_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(sclr_be_t10, observed).
narrative_ontology:measurement(sclr_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement_basis(sclr_be_t15, observed).
narrative_ontology:measurement(sclr_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(sclr_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(sclr_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(sclr_su_t0, observed).
narrative_ontology:measurement(sclr_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(sclr_su_t5, observed).
narrative_ontology:measurement(sclr_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(sclr_su_t10, observed).
narrative_ontology:measurement(sclr_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement_basis(sclr_su_t15, observed).
narrative_ontology:measurement(sclr_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(sclr_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.15).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, traffic_safety_regulation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, public_health_funding).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, commercial_regulation).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three ε-distinct constraints per the ε-invariance principle. Prohibition_reading (ε ≈ 0.75) extracts from users via criminal enforcement. Harm_reduction_reading (ε ≈ 0.35) extracts from users via medicalized coercion and from taxpayers via service provision. Legalization_reading (ε ≈ 0.22) extracts from consumers via taxation and from third parties via externalities, while industry captures market surplus. All three readings are live in contemporary policy discourse; jurisdictions instantiate different readings or hybrids.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, moderate, 0.65).
constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
