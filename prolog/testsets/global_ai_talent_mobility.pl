% ============================================================================
% CONSTRAINT STORY: global_ai_talent_mobility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_ai_talent_mobility, []).

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
 *   constraint_id: global_ai_talent_mobility
 *   human_readable: Global AI Talent Mobility Constraint
 *   domain: labor_economics/technology_policy
 *
 * SUMMARY:
 *   Global AI talent mobility exhibits a structural constraint where formal
 *   labor market openness coexists with deep barriers to movement for
 *   researchers from emerging markets. Developed-nation tech companies and
 *   research institutions benefit from access to a global talent pool while
 *   capturing disproportionate value through visa gatekeeping, credential
 *   non-recognition, and wage suppression enabled by geographic inequality.
 *   Emerging-market researchers face suppression through multiple mechanisms:
 *   visa visa policy, credential recognition barriers, work authorization
 *   restrictions tied to single employers, and brain-drain dynamics that
 *   concentrate rewards in developed nations. The constraint appears as pure
 *   coordination (rope) from the developed-nation employer perspective, mixed
 *   extraction-coordination (tangled rope) from the credentialed researcher
 *   and emigrating nation perspectives, pure extraction (snare) from the
 *   powerless researcher perspective, and a degraded bureaucratic apparatus
 *   (piton) from the historical perspective. The constraint has intensified
 *   over the decade as AI's economic value has increased, making talent
 *   acquisition more lucrative for developed-nation firms and creating
 *   stronger incentives for restrictive immigration policy. Theater ratio
 *   remains moderate because visa justifications (security, credential
 *   verification) have become more explicit, reducing theatrical performance.
 *   However, extractiveness has increased as the wage differentials and
 *   career opportunity gaps have widened.
 *
 * KEY AGENTS:
 *   - Emerging Market AI Researchers: Primary victim (powerless/trapped) — face visa restrictions, credential non-recognition, and brain-drain dynamics; cannot exit without abandoning research career
 *   - Developed-Nation Tech Companies: Primary beneficiary (institutional/arbitrage) — access global talent pools while suppressing wages through geographic gatekeeping; can arbitrage between talent markets
 *   - Visa Gatekeeping States: Beneficiary (powerful/mobile) — maintain visa policy tools that restrict labor competition and support domestic tech industry dominance
 *   - Credentialed Mobile Researchers: Secondary victim (moderate/constrained) — benefit from global collaboration but face work authorization restrictions and relocation costs
 *   - Emigrating Nation States: Victim (powerful/mobile) — experience talent drain and loss of trained capacity despite sovereignty over education systems
 *   - Capacity-Building Coalitions: Organized actors (organized/constrained) — building local AI research hubs as alternative to brain-drain; seeing sunset pathway through ecosystem development
 *   - Historical Visa Bureaucracy: Institutional apparatus (institutional/arbitrage) — maintains Cold War-era systems through inertia despite changed functional justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_ai_talent_mobility, 0.52).
domain_priors:suppression_score(global_ai_talent_mobility, 0.58).
domain_priors:theater_ratio(global_ai_talent_mobility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_ai_talent_mobility, extractiveness, 0.52).
narrative_ontology:constraint_metric(global_ai_talent_mobility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(global_ai_talent_mobility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_ai_talent_mobility, tangled_rope).
narrative_ontology:human_readable(global_ai_talent_mobility, "Global AI Talent Mobility Constraint").
narrative_ontology:topic_domain(global_ai_talent_mobility, "labor_economics/technology_policy").

domain_priors:requires_active_enforcement(global_ai_talent_mobility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_ai_talent_mobility, developed_nation_tech_companies).
narrative_ontology:constraint_beneficiary(global_ai_talent_mobility, visa_gatekeeping_states).
narrative_ontology:constraint_victim(global_ai_talent_mobility, emerging_market_researchers).
narrative_ontology:constraint_victim(global_ai_talent_mobility, global_ai_capability_distribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING MARKET RESEARCHER (SNARE) — Cannot exit the extraction mechanism without abandoning career prospects. Visa restrictions, credential recognition barriers, and brain-drain dynamics lock talent into low-wage markets while developed-nation firms capture value. Maximum suppression: structural barriers (visa policy, credential gatekeeping, language requirements) prevent mobility despite formal open labor markets.
constraint_indexing:constraint_classification(global_ai_talent_mobility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDENTIALED RESEARCHER (TANGLED ROPE) — Faces constrained mobility despite high credentials. Benefits from global knowledge-sharing and collaborative networks (coordination function). Simultaneously extracted: visa sponsorship ties them to single employer; relocation costs are high; credential recognition varies by destination. Mixed experience: genuine coordination of research plus asymmetric labor appropriation.
constraint_indexing:constraint_classification(global_ai_talent_mobility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPED-NATION TECH COMPANY (ROPE) — Experiences the constraint as pure coordination: global talent pools enable research scaling and geographic arbitrage. No extraction from this agent's perspective — they see the mechanism as enabling efficiency gains and capability access. Arbitrage exit: can shift talent acquisition between markets and jurisdictions freely.
constraint_indexing:constraint_classification(global_ai_talent_mobility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMIGRATING NATION STATE (TANGLED ROPE) — Powerful agent (sovereign control over borders/education) but experiencing structural extraction through talent drain. Coordinates own university and research ecosystem (genuine function). Simultaneously experiences loss of trained capacity to developed-nation employers. High suppression: cannot easily restrict emigration without damaging global standing; cannot force return without coercion. Mobile exit: can adjust visa policy, but faces retaliation from developed nations controlling foreign investment and markets.
constraint_indexing:constraint_classification(global_ai_talent_mobility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPACITY-BUILDING COALITION (SCAFFOLD) — Organized actors (Pan-African AI Initiative, ICTP physics networks, AI4ALL-style programs in developing regions) see the mobility constraint as temporary. Building local capacity, reversing brain-drain through return incentives, and creating high-wage research hubs in emerging markets. Sunset logic: as local research ecosystems mature and match developed-nation compensation, the extraction mechanism loses force. Constrained exit: limited funding and institutional capacity limit speed of alternative pathway construction.
constraint_indexing:constraint_classification(global_ai_talent_mobility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: VISA GATEKEEPING APPARATUS (PITON) — Historical visa and credential systems originated in Cold War security (keep foreign scientists out). Persist through bureaucratic inertia despite changed justification (now framed as 'national security' or 'protection of intellectual property'). Theater ratio high: extensive background checks, documentation requirements, and processing delays are largely performative security theater — actual spy detection rates are negligible. The apparatus maintains itself through institutional lock-in; few actors benefit from its continued existence, yet it persists.
constraint_indexing:constraint_classification(global_ai_talent_mobility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOGRAPHIC INEQUALITY VIEW (MOUNTAIN) — From civilizational/universal view, some talent concentration in wealthy hubs appears inevitable (wage differentials reflect genuine resource concentration, network effects are locally self-reinforcing). But structural data contradicts mountain classification: the constraint is maintained by policy choices (visa regimes, credential gatekeeping, work authorization restrictions), not immutable laws. False summit: naturalizes contingent institutional arrangements.
constraint_indexing:constraint_classification(global_ai_talent_mobility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_ai_talent_mobility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_ai_talent_mobility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_ai_talent_mobility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_ai_talent_mobility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_ai_talent_mobility, TR),
    TR >= 0.70.

:- end_tests(global_ai_talent_mobility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts moderate-high value from emerging-market researchers through suppressed wages, restricted mobility options, and brain-drain dynamics. The value is captured by developed-nation employers and visa-controlling states. However, extractiveness is not extreme because legitimate research collaboration and knowledge-sharing does occur globally, and some mobility pathways exist (H-1B, postdoc visas). The increase from 0.35 to 0.52 over the decade reflects intensification as AI economic value increased and developed nations adopted more explicit protectionist immigration policies. Suppression (0.58): High but not maximal. Structural barriers include visa policy (directly restrictive), credential non-recognition (reduces exit options), work authorization tied to sponsorship (traps researchers to single employers), and brain-drain dynamics (economic incentives strongly favor emigration). However, some suppression is reducible through policy change: visa regimes can be liberalized, credential recognition can be harmonized, work authorization can be decoupled from sponsorship. Suppression is not physical confinement (as it would be for 'trapped' in interpersonal contexts), but rather structural economic and legal barriers. Theater ratio (0.48): Moderate. Visa justifications invoke security and credential verification, but these are increasingly transparent cover stories for labor market protection. Security rationales are largely theater — actual counterintelligence value of visa screening is minimal. However, some theater has declined (less elaborate justification rhetoric) as protectionist intent has become more explicit in policy debates.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspective (developed-nation employer) sees rope-type coordination: matching talent with research opportunity. The victim perspective (emerging-market researcher) sees snare-type extraction: locked into low wages with no exit. This gap reveals the constraint's actual mechanics: what appears as coordination to the beneficiary is experienced as extraction by the victim. The gap also exposes the false mountain perspective (naturalizing geographic inequality) as contingent on policy choices rather than immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary structural relationship: Developed-nation tech companies and visa-controlling states benefit directly from the constraint's enforcement. They gain arbitrage capacity and labor supply control. Victim structural relationship: Emerging-market researchers and emigrating nation states bear direct extraction costs. Researchers lose wage opportunity and mobility; nations lose trained capacity. The directionality derivation prioritizes this beneficiary/victim structure. Exit modulation: beneficiaries have arbitrage exit (can move between labor markets and jurisdictions); victims have trapped or constrained exit (visa policy creates hard barriers; wage differentials create soft barriers). The combined effect (beneficiary + arbitrage → low d; victim + trapped → high d) produces the sharp perspectival gap observable in the classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED: Extractiveness 0.52 is below the mandatrophy gate (0.70), so mandatrophy resolution is not required by the schema. However, the constraint exhibits near-mandatrophy characteristics: it hovers between tangled_rope (mixed extraction and coordination) and snare (pure extraction) depending on perspective. The ambiguity arises from whether genuine research collaboration constitutes real coordination benefit or merely provides theatrical cover for wage suppression. The four omegas address the empirical resolution pathways: if credential barriers are performative (omega 3), if benefit distribution is asymmetric (omega 4), if policy dominates market dynamics (omega 1), and if capacity-building cannot reverse brain-drain (omega 2), the constraint slides toward pure snare classification. If these hypotheses resolve the opposite way, tangled_rope classification is sustained. The analytical observer's false summit (mountain perspective) must be explicitly flagged in compiled output as naturalization of contingent institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visa_policy_vs_market_dynamics,
    'What portion of talent mobility restriction is policy-driven (visa gatekeeping) vs market-driven (wage/research quality differentials)?',
    'Comparative analysis of visa policy changes and talent flow elasticity; examination of credential-matched researchers in high-visa-barrier vs low-visa-barrier corridors; controlled policy experiments (e.g., Estonia''s digital nomad visa impact)',
    'If policy-driven (>60%): constraint is tangled_rope with high policy-leverage ceiling. If market-driven (>60%): constraint may be legitimately rope-type coordination of real differentials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visa_policy_vs_market_dynamics, empirical, 'Decomposition of policy vs market drivers of talent concentration').

omega_variable(
    brain_drain_reversal_feasibility,
    'Can emerging-market research capacity-building reverse brain-drain to developed nations, or does talent mobility lock in permanent inequality?',
    'Longitudinal study of talent flows from countries that increased research funding and local opportunities (S. Korea, Taiwan, China in AI); correlation between local salary competitiveness and return rates; analysis of whether capacity-building reaches critical mass or plateaus',
    'If reversible: scaffold perspective is structural. If irreversible: the constraint functions as permanent extraction mechanism (snare from victim perspective). Determines whether sunset is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_reversal_feasibility, empirical, 'Reversibility of global talent concentration through local capacity-building').

omega_variable(
    credential_gatekeeping_necessity,
    'Are credential recognition barriers (degree accreditation, licensing reciprocity) necessary for quality assurance or performative gatekeeping?',
    'Comparison of outcomes for credential-matched researchers with vs without formal recognition; analysis of countries with mutual credential recognition (Schengen, APEC) vs strict gatekeeping; examination of whether credential barriers correlate with research quality verification or with protectionist labor market effects',
    'If necessary: suppression is justified overhead, reducing snare classification. If performative: suppression is pure gatekeeping, supporting snare classification for emerging-market researchers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_gatekeeping_necessity, empirical, 'Functional necessity of credential recognition barriers').

omega_variable(
    research_collaboration_benefit_distribution,
    'Do emerging-market researchers captured in remote collaboration (without immigration) actually receive proportional benefit from access to developed-nation knowledge networks?',
    'Analysis of citation impact for researchers in different geographic locations collaborating with developed-nation teams; correlation between remote collaboration participation and salary increases / research funding in emerging markets; examination of IP ownership in cross-border collaborations',
    'If benefits are proportional: constraint may be legitimate rope-type coordination. If benefits concentrate with developed-nation co-authors: constraint is extractive (tangled_rope or snare), using ''collaboration'' to justify wage differentials without reciprocal value transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_collaboration_benefit_distribution, empirical, 'Distribution of collaboration benefits across geographic boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_ai_talent_mobility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitm_tr_t0, global_ai_talent_mobility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aitm_tr_t5, global_ai_talent_mobility, theater_ratio, 5, 0.42).
narrative_ontology:measurement(aitm_tr_t10, global_ai_talent_mobility, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(aitm_be_t0, global_ai_talent_mobility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aitm_be_t5, global_ai_talent_mobility, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(aitm_be_t10, global_ai_talent_mobility, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_ai_talent_mobility, resource_allocation).
narrative_ontology:affects_constraint(global_ai_talent_mobility, semiconductor_supply_chain).
narrative_ontology:affects_constraint(global_ai_talent_mobility, global_research_equity).

% DUAL FORMULATION NOTE:
% Global AI talent mobility decomposes into two related constraints: (1) visa policy gatekeeping (policy artifact, strongly extractive), and (2) research capacity inequality (market outcome, partially extractive). This story treats them as a unified constraint but could be separated into policy-driven and market-driven stories with different ε values. Upstream: research quality differences between regions feed into credential gatekeeping. Downstream: talent concentration feeds into AI capability inequality between nations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_ai_talent_mobility, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
