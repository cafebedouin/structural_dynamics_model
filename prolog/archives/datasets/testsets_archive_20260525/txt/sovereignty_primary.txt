% ============================================================================
% CONSTRAINT STORY: sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_primary, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading: Free Movement Conditional on State Consent
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This is the sovereignty-primary reading of the contested federation
 *   treaty kernel. In this reading, member state governments retain primary
 *   authority to regulate labor market access, and federal free movement
 *   rights are conditional on state consent. Free movement is subordinate to
 *   state prerogatives to protect national welfare systems, labor market
 *   stability, and domestic constituencies. The constraint exhibits tangled
 *   rope structure: member states coordinate labor supply management (genuine
 *   coordination of welfare system boundaries and labor scarcity protection)
 *   while simultaneously extracting rents through access restriction and wage
 *   premium protection. Mobile workers experience this as snare (trapped by
 *   consent requirements with high suppression). The federation itself
 *   experiences the constraint as mixed (coordinating state authority while
 *   suffering from labor market fragmentation). Organized transnational
 *   actors perceive structural sunset (as worker cohorts normalize
 *   cross-border mobility, states' political capacity to enforce restrictions
 *   declines). The analytical observer risks false summitry — treating state
 *   sovereignty as an unchangeable law rather than a contingent institutional
 *   choice.
 *
 * KEY AGENTS:
 *   - Member State Governments: Primary beneficiary (institutional/arbitrage) — retain regulatory authority, can selectively open borders for domestic political advantage, extract rents through access scarcity
 *   - Domestic Labor Unions: Secondary beneficiary (moderate/constrained) — benefit from constrained labor supply and wage floor protection; also face constraint costs (restrict expansion into new sectors, reduce competitive pressure on wages)
 *   - Mobile Workers: Primary victim (powerless/trapped) — face irreversible barriers to labor market access, legal status contingent on state discretion, welfare benefits tied to citizenship
 *   - Foreign Labor Pool: Potential victim (powerless/trapped) — excluded from federation labor market access; bear full cost of wage differential between federation and exterior labor markets
 *   - Federation / Integration Project: Mixed (institutional/constrained) — genuinely coordinating labor mobility (benefit); constrained by state veto power (cost); experiences extraction through fragmented labor markets and regulatory harmonization failures
 *   - Transnational Worker Coalition: Organized (organized/mobile) — perceive structural sunset; building alternative pathways through political pressure and cross-border organizing
 *   - Treaty Compliance Apparatus: Institutional (institutional/arbitrage) — maintains performative bureaucracy; perpetuates itself through legitimizing paperwork while enabling workarounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_primary, 0.58).
domain_priors:suppression_score(sovereignty_primary, 0.65).
domain_priors:theater_ratio(sovereignty_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereignty_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sovereignty_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(sovereignty_primary, "Sovereignty-Primary Reading: Free Movement Conditional on State Consent").
narrative_ontology:topic_domain(sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(sovereignty_primary, fixed_text).
narrative_ontology:cs_authority_grounding(sovereignty_primary, lineage).
narrative_ontology:cs_interpretation_layer_present(sovereignty_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_primary, national_labor_unions).
narrative_ontology:constraint_beneficiary(sovereignty_primary, domestic_welfare_systems).
narrative_ontology:constraint_beneficiary(sovereignty_primary, member_state_governments).
narrative_ontology:constraint_victim(sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(sovereignty_primary, foreign_labor_pool).
narrative_ontology:constraint_victim(sovereignty_primary, federal_integration_project).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER (SNARE) — Faces irreversible barriers to labor market access across member states. Trapped by member state consent requirements; cannot exit the federation's jurisdiction without abandoning livelihood. Suppression is structural: regulatory authorization tied to member state discretion, legal status revocable at state border, welfare benefits tied to citizenship. No coordination benefit perceived — constraint exists solely to restrict and extract labor value differential.
constraint_indexing:constraint_classification(sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: DOMESTIC LABOR UNION (TANGLED ROPE) — Experiences mixed coordination and extraction. Benefits from labor supply discipline (constrained migration maintains wage floors in protected sectors); experiences constraint as coordination of labor scarcity and wage protection. Simultaneously extracts from mobile workers through negotiated access barriers and tiered employment standards. Has real exit options (can lobby for open borders, can cooperate with cross-border unions) but constrained by member state structural position and political economy of low-skill domestic constituencies.
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBER STATE GOVERNMENT (ROPE) — Experiences free movement conditionality as pure coordination of sovereignty preservation. Retains regulatory authority over labor market access; uses conditionality to coordinate with federal authority (treaty obligation) while protecting local constituencies. Benefits from the arrangement through political survival (can claim protection of domestic welfare system and workers against supranational pressure). Arbitrage option: can selectively open borders for specific sectors or workers when domestic labor shortage exceeds political cost.
constraint_indexing:constraint_classification(sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERATION / INTEGRATION PROJECT (TANGLED_ROPE) — Structurally coordinating labor mobility (genuine benefit: expanded market, wage convergence, productivity gains). Simultaneously experiencing extraction through member state withholding of consent and fragmentation of labor market. Constrained by treaty architecture that grants states veto power; cannot exit without dissolution. Both beneficiary (coordination logic) and victim (states exercise veto to extract rents). The constraint's enforcement requirement reflects this hybrid: states must actively police borders (enforcement costs) while federation must accommodate state discretion (integration costs).
constraint_indexing:constraint_classification(sovereignty_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRANSNATIONAL WORKER COALITION (SCAFFOLD) — Organized agents (cross-border unions, migrant rights networks) perceive the constraint as a temporary coordination failure with structural sunset logic. Movement restrictions are enforcement artifacts from an earlier federalism phase; as labor mobility increases and worker mobility becomes normal, states' political capacity to maintain borders declines (workers are constituents, employers lobby for access, brain drain becomes visible cost). Coalition mobilization and cross-border organizing is building exit pathway via political pressure. Extractiveness experienced as high but with finite lifetime — estimated sunset 15-25 years as cohort replacement makes restriction politically costly.
constraint_indexing:constraint_classification(sovereignty_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: TREATY COMPLIANCE APPARATUS (PITON) — The treaty's consent requirement generates a vast compliance bureaucracy (visa systems, work permit administration, border enforcement, harmonization committees) that is substantially performative. Most actual labor movement happens through exceptions, bilateral agreements, and structural workarounds (posted workers directives, self-employment loopholes, asylum stream recategorization). The apparatus persists through institutional inertia: it employs thousands, generates legitimizing paperwork, and makes restriction visible (politically appeasing domestic constituencies) while allowing substantial de facto mobility. Theater ratio (0.48) reflects this: roughly half the constraint's apparent force is genuine regulatory closure; half is performative compliance ritual.
constraint_indexing:constraint_classification(sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, state sovereignty and the right to control labor market access appear as unchangeable features of the international system: states are sovereign actors; sovereignty includes border control; no authority above states can mandate open borders. This perspective risks false summitry: state sovereignty is a contingent institutional arrangement, not a law of nature. The structural data reveals beneficiaries (member state governments, domestic unions) and victims (mobile workers, integration project), signaling that what appears 'natural' is actually a contingent institutional choice. Engine's false summit detector should flag this.
constraint_indexing:constraint_classification(sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The member state consent requirement extracts substantial value through access restriction and wage premium protection, but this is not pure extraction — genuine coordination of welfare system boundaries and labor scarcity does occur. The measured value reflects that roughly 55-65% of the constraint's force is extraction; 35-45% is legitimate coordination. The trajectory shows rising extractiveness over the interval (0.42 → 0.58) as states have increasingly used consent requirements to protect domestic constituencies against labor inflows, suggesting the extraction function has been prioritized over coordination. Suppression (0.65): High. Multiple layers of suppression: legal status contingency (visa requirements, work permits, revocable authorization), welfare benefit coupling to citizenship, regulatory harmonization barriers, and enforcement mechanisms (border control, employer verification). However, suppression is not absolute — informal channels (posted workers, self-employment, visa shopping) enable substantial de facto mobility. Theater ratio (0.48): Moderate. Treaty compliance bureaucracy is substantially performative (visa systems, work permits, border enforcement), but roughly equal to genuine regulatory closure. The ratio suggests that actual labor market integration is more open than formal rules suggest; the apparatus's main function is to make restriction visible (politically satisfying domestic constituencies) while enabling workarounds.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's dual nature. Member states see pure coordination (Rope) — they are solving the legitimate problem of protecting labor markets and welfare systems. Domestic unions see mixed coordination-extraction (Tangled Rope) — both benefit and cost. Mobile workers see pure extraction (Snare) — no coordination benefit, only suppression. The federation sees mixed (Tangled Rope) — coordinating labor mobility while constrained by state veto. Organized transnational actors see temporary problem with sunset (Scaffold) — political pressure will eventually force opening. The treaty compliance apparatus sees degraded ritual (Piton) — performative enforcement maintained through inertia. The analytical observer risks naturalizing contingent institutional choices as unchangeable state sovereignty (Mountain / false summit). The gap between member state's Rope and worker's Snare is maximal — the same mechanism appears as pure coordination from the beneficiary perspective and pure extraction from the victim perspective. The piton classification reveals that substantial bureaucratic machinery persists despite degraded function (actual mobility is much higher than formal rules suggest).
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments occupy institutional power with arbitrage exit options, enabling them to benefit from the constraint (can selectively open borders when domestic shortage exceeds political cost). Their d value is low (~0.10-0.20), producing low or negative effective extraction (f(d) ≈ -0.05 to 0.10). Domestic labor unions occupy moderate power with constrained exit; they benefit from wage protection but face constraint costs. Their d value is moderate (~0.50-0.55), producing moderate extraction (f(d) ≈ 0.65-0.75). Mobile workers occupy powerless position with trapped exit; they experience full cost of restriction with no compensation. Their d value is high (~0.92), producing maximum extraction (f(d) ≈ 1.38). The federation occupies institutional power with constrained exit (cannot unilaterally override member states without treaty violation); their d value is moderate-high (~0.55-0.65), producing substantial extraction (f(d) ≈ 0.75-0.90), reflecting mixed beneficiary/victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is legitimately tangled rope at the state level (genuine coordination of welfare boundaries + genuine extraction via access restriction) while simultaneously appearing as snare from the mobile worker perspective. The classification gap is not an error — it reflects the real structural asymmetry: member states genuinely coordinate welfare interests while genuinely extracting rents; mobile workers genuinely face suppression with no coordination benefit. The scaffold perspective's sunset logic (worker cohorts will make restriction politically costly) provides resolution pathway: as extractive function becomes less necessary and political costs rise, the coordination function should rebalance. The false summit (analytical observer treating sovereignty as natural law) should be flagged by the engine as conceptual capture — framing a contingent institutional choice as unchangeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this the sovereignty-primary reading of the federation treaty, or is the integration-primary reading a more accurate description of the actual treaty mechanism?',
    'Textual analysis of treaty articles: which framing (state consent as primary, or worker mobility as primary right with state exceptions) is codified? Historical analysis of treaty negotiation and amendment: which reading has driven policy direction over time?',
    'If sovereignty-primary is the canonical reading: this constraint''s high extractiveness and suppression are treaty-compliant and represent legitimate state authority. If integration-primary is canonical: this constraint represents a departure from treaty intent, and high suppression reflects extra-legal extraction by member states. Classification shifts from tangled_rope (high) to tangled_rope (low) and snare perspectives become dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the federation treaty is the canonical one').

omega_variable(
    consent_mechanism_extraction,
    'Does member state consent requirement function as coordinating mechanism (protecting legitimate welfare interests) or as extraction mechanism (protecting rents and wage floors)?',
    'Comparative analysis: states that use consent to protect welfare system integration vs states that use consent to protect low-skill wage premiums. If correlation with welfare risk is high, coordination function is genuine. If correlation with protectionist industry lobbying is high, extraction function dominates.',
    'If genuine coordination: extractiveness should be lower (~0.35, downgrade to lower tangled_rope). If extraction dominates: extractiveness should be higher (~0.70, upgrade to snare at institutional level). Changes the entire beneficiary/victim story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_extraction, empirical, 'Whether consent requirement is coordinating welfare systems or protecting rents').

omega_variable(
    actual_vs_formal_mobility,
    'What proportion of actual labor mobility in the federation occurs through formal consent channels vs informal/exception channels (posted workers, self-employment, visa shopping, asylum recategorization)?',
    'Labor flow data: track formal work permits vs estimated actual employment of citizens in other member states. If actual mobility is substantially higher than formal permits suggest, the theater ratio is accurate (~0.48). If formal permits track actual mobility, theater ratio is lower (~0.25) and constraint is more genuinely restrictive.',
    'If theater is high (formal restriction, informal permission): snare perspectives are accurate but constrained actors have more agency than classifications suggest — upgrade constrained agents toward mobile. If theater is low (formal equals actual): suppression is more absolute, snare is more severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_vs_formal_mobility, empirical, 'Ratio of formal to actual labor mobility across state boundaries').

omega_variable(
    welfare_system_convergence_trajectory,
    'Are welfare systems among member states converging or diverging? If converging, the member state consent rationale weakens over time.',
    'Multi-decade comparison of welfare spending, eligibility criteria, and benefit levels across member states. Correlation analysis: do states that harmonize welfare systems progressively relax labor movement restrictions?',
    'If converging: supports scaffold perspective (sunset logic is real — as welfare harmonizes, consent restrictions become obsolete). If diverging: supports snare perspective (welfare divergence justifies ongoing extraction). Structural prediction: if welfare is converging, expect coalition mobilization and political pressure to decline over next 15 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_convergence_trajectory, empirical, 'Welfare system convergence or divergence trajectory').

omega_variable(
    federal_authority_asymmetry,
    'Does the federation actually have authority to override member state consent, or is the treaty''s phrasing ''subject to member state consent'' a disguise for state veto?',
    'Treaty text analysis, case law analysis, and governance practice: has the federation ever attempted to force open borders against member state objection? Have member states ever been overruled by federal authority on labor mobility?',
    'If federation has real override authority: extractiveness lower (~0.40), classification shifts toward rope (federation can unilaterally open borders if it chooses to). If consent is de facto veto: extractiveness higher (~0.68), snare perspectives dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_authority_asymmetry, empirical, 'Whether federation has genuine authority to override state consent or if consent is veto').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_prim_tr_t0, sovereignty_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sov_prim_tr_t10, sovereignty_primary, theater_ratio, 10, 0.42).
narrative_ontology:measurement(sov_prim_tr_t20, sovereignty_primary, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(sov_prim_be_t0, sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sov_prim_be_t10, sovereignty_primary, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sov_prim_be_t20, sovereignty_primary, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(sovereignty_primary, integration_primary).
narrative_ontology:affects_constraint(sovereignty_primary, subsidiarity_balance).
narrative_ontology:affects_constraint(sovereignty_primary, welfare_state_vulnerability).
narrative_ontology:affects_constraint(sovereignty_primary, labor_market_fragmentation).

% DUAL FORMULATION NOTE:
% The federation membership treaty kernel admits three structurally distinct readings: sovereignty-primary (states control access, this constraint), integration-primary (worker mobility is primary right), and subsidiarity-balance (coequal principles, case-by-case). Each reading produces a different constraint with different ε, different beneficiary/victim structure, and different classification. This file documents the sovereignty-primary reading only. The sibling readings are separate constraint stories linked via network.affects_constraints. The three readings are not observational variants of one constraint — they are genuinely different institutional framings with different structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
