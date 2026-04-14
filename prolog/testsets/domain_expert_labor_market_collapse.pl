% ============================================================================
% CONSTRAINT STORY: domain_expert_labor_market_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_expert_labor_market_collapse, []).

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
 *   constraint_id: domain_expert_labor_market_collapse
 *   human_readable: Domain Expert Labor Market Collapse
 *   domain: labor_economics/credentialism
 *
 * SUMMARY:
 *   Domain expert labor market collapse represents a structural constraint
 *   where declining demand for specialized knowledge, credential inflation,
 *   and outsourcing combine to suppress wages and eliminate career pathways
 *   for experts while simultaneously activating mechanisms that lock those
 *   experts in place. The constraint exhibits snare characteristics at the
 *   victim level (experts trapped by identity specialization and market
 *   collapse) while manifesting as extraction mechanisms enforced by
 *   credential gatekeepers and labor cost minimizers. The theater ratio rises
 *   over time as knowledge institutions persist in credentialing rituals even
 *   as the actual demand for expertise evaporates — universities continue
 *   awarding PhDs in fields with zero employment prospects, creating
 *   theatrical certification of expertise that no longer has market value.
 *   The constraint is not purely technological (expertise genuinely becomes
 *   obsolete in some domains) but is actively enforced through credential
 *   requirement escalation, outsourcing, and age discrimination. Experts face
 *   a double bind: their expertise is both 'obsolete' (no demand) and
 *   hyperspecialized (cannot transition without identity dissolution). The
 *   suppression is structural and internalized: material barriers (retraining
 *   costs, age discrimination, geographic immobility) combine with identity
 *   locks (decades-long professional identity investment, prestige status,
 *   intellectual community embededness) to trap experts despite theoretical
 *   exit options.
 *
 * KEY AGENTS:
 *   - Domain Experts: Primary victims (powerless/trapped) — face simultaneous credential devaluation and expertise obsolescence; cannot exit without identity dissolution
 *   - Mid-Career Professionals: Secondary victims (moderate/constrained) — retain some capital for transition but face high retraining costs and entry-level wage shock
 *   - Professional Associations: Secondary actors (organized/mobile) — coordinate knowledge standards but lose enforcement capacity as demand collapses
 *   - Credential Gatekeepers (Universities, Licensure Boards): Primary beneficiaries (institutional/arbitrage) — profit from credential inflation and maintained scarcity value
 *   - Labor Cost Minimizers (Firms, Investors): Primary beneficiaries (powerful/arbitrage) — actively extract value by suppressing expert wages and outsourcing expertise
 *   - Knowledge Institutions: Degraded actors (institutional/constrained) — maintain credentialing theater while losing knowledge transmission function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_expert_labor_market_collapse, 0.68).
domain_priors:suppression_score(domain_expert_labor_market_collapse, 0.72).
domain_priors:theater_ratio(domain_expert_labor_market_collapse, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_expert_labor_market_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(domain_expert_labor_market_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(domain_expert_labor_market_collapse, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_expert_labor_market_collapse, snare).
narrative_ontology:human_readable(domain_expert_labor_market_collapse, "Domain Expert Labor Market Collapse").
narrative_ontology:topic_domain(domain_expert_labor_market_collapse, "labor_economics/credentialism").

domain_priors:requires_active_enforcement(domain_expert_labor_market_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_expert_labor_market_collapse, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(domain_expert_labor_market_collapse, labor_cost_minimizers).
narrative_ontology:constraint_victim(domain_expert_labor_market_collapse, domain_experts).
narrative_ontology:constraint_victim(domain_expert_labor_market_collapse, knowledge_preservation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPECIALIST WITH OBSOLETE EXPERTISE (SNARE) — Domain experts face structural collapse: their expertise becomes 'obsolete' through demand destruction (outsourcing, automation, credential inflation), yet cannot exit without abandoning the identity they constructed over decades. Trapped by credential specialization, geographic immobility of expertise clusters, and age discrimination. Maximum experienced extraction with no alternatives.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PIVOT CANDIDATE (TANGLED ROPE) — Some domain experts retain enough capital (savings, network, residual prestige) to attempt field transition, but face significant barriers: retraining costs, entry-level wage shock, non-recognition of transferable skills. Experience both extraction (wage suppression in original domain) and coordination benefit (access to knowledge networks that support transition).
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROFESSIONAL ASSOCIATION (ROPE) — Organized defense of domain expertise: licensing boards, credentialing bodies, and professional societies maintain standards and create coordination benefits (knowledge sharing, reputation signaling). However, they face erosion of enforcement capacity as demand collapses. Rope classification reflects the genuine coordination function despite declining effectiveness.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIAL GATEKEEPER (ROPE) — Universities, certification bodies, and licensing boards benefit from continued credential inflation and market fragmentation. They experience the constraint as a coordination mechanism: maintaining credential value requires suppressing alternative pathways. Net beneficiary with low experienced extraction — they arbitrage the collapsing market.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR COST MINIMIZER (SNARE) — Firms and investors benefit from expert labor collapse: wage suppression, oversupply, and credential inflation allow extraction of expert cognitive labor at commodity prices. The constraint is actively enforced through hiring freezes, credential requirement escalation ('degree inflation'), and offshore outsourcing. Maximum extraction with active beneficiary enforcement.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: KNOWLEDGE PRESERVATION INSTITUTION (PITON) — Universities, research institutes, and professional guilds once coordinated knowledge transmission and expertise validation. These institutions now largely perform a degraded function: they continue credentialing rituals (theater_ratio=0.55) while losing capacity to actually cultivate or preserve domain expertise. Institutional inertia maintains the form while function atrophies.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint exhibits genuine coordination (knowledge standards, reputation systems) alongside asymmetric extraction (credential gatekeeping capturing rents, labor cost minimization suppressing wages). The system is not purely extractive — it does coordinate knowledge. But the coordination function is increasingly disconnected from the extraction mechanism, enabling the snare dynamic.
constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_expert_labor_market_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(domain_expert_labor_market_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(domain_expert_labor_market_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(domain_expert_labor_market_collapse, TR),
    TR >= 0.70.

:- end_tests(domain_expert_labor_market_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The domain expert labor market collapse extracts substantial value through wage suppression, credential inflation, and outsourcing. The value is captured by credential gatekeepers (who profit from required credentials despite declining job market) and labor cost minimizers (who pay commodity prices for expertise). The measurement trajectory shows acceleration: from 0.35 at t=0 (early stage, some demand remains) to 0.68 at t=20 (mature collapse, demand destruction complete, credential inflation entrenched). Suppression (0.72): High. Experts face compounded barriers: (1) material — retraining costs, age discrimination, geographic immobility of expertise clusters; (2) identity — decades of professional identity investment, prestige status, intellectual community embededness; (3) institutional — credential requirements escalate as labor cost minimizers use credentials as filtering mechanism. Theater ratio (0.55, rising): Moderate. Knowledge institutions continue credentialing rituals (dissertation, defense, degree) with diminishing connection to employability. The theater increases over time (0.30→0.55) as institutions persist in theatrical certification despite awareness that credentials no longer guarantee expertise or employment. This indicates piton characteristics in the knowledge institution perspective.
 *
 * PERSPECTIVAL GAP:
 *   The expert and the credential gatekeeper experience the same constraint completely differently. For the expert: snare — identity-locked by specialized expertise, trapped in credentials that have become simultaneously devalued and over-proliferated, unable to exit without identity dissolution. For the credential gatekeeper (university): rope — they coordinate knowledge standards and credential signaling; they experience the constraint as a coordination mechanism where credential inflation maintains value and scarcity. For the labor cost minimizer: snare or rope depending on whether they actively enforce the collapse or merely benefit from it passively. The analytical observer sees tangled rope: genuine coordination (knowledge standards) coupled with asymmetric extraction (credential gatekeeping rents, wage suppression). The piton perspective (knowledge institution) is crucial: universities maintain credentialing theater even as employment prospects evaporate, revealing that the institution's function has degraded from knowledge cultivation to credential manufacture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: trapped experts with no exit options experience maximum d ≈ 0.95, feeding high f(d) ≈ 1.42, producing high χ despite moderate-high ε. Credential gatekeepers (institutional/arbitrage) experience low d ≈ 0.10, feeding f(d) ≈ -0.08, producing negative χ (net beneficiaries). Labor cost minimizers experience moderate d ≈ 0.35-0.45 depending on how actively they enforce collapse vs. passively benefit. Mid-career professionals with some transition capital experience d ≈ 0.65, producing moderate χ. The analytical observer uses canonical d ≈ 0.73 for 'analytical' power, producing moderate χ that reflects the genuine mixed character of the constraint (both coordination and extraction). Professional associations experience constrained exit (they cannot abandon credentialing without destroying their institutional basis) but retain organized agency, producing d ≈ 0.40-0.50.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandate-atrophy paradox by distinguishing between the knowledge coordination function (which remains genuine but becomes increasingly ceremonial) and the extraction mechanism (which becomes the primary driver of the constraint's persistence). Universities genuinely coordinate knowledge standards and expertise validation — this is the 'mandate.' But as expert labor demand collapses, the mandate atrophies into theater: credentials continue being issued despite evaporating employment value, shifting the constraint's function from knowledge coordination to credential rent-seeking. The constraint is not a pure snare (which would have no coordination function) nor a pure rope (which would have minimal extraction). It is tangled rope at the civilizational analytical level: coordination persists in degraded form (piton theater) while extraction mechanisms (credential inflation, wage suppression, outsourcing) become dominant. The mandatrophy is the gap between the credentialing mandate (validate expertise and knowledge) and the actual function (manufacture credentials for rent extraction). Labor cost minimizers benefit by suppressing expert wages through oversupply induced by credential proliferation. The snare classification at the victim level is stable and analytically clean: trapped experts with no viable exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_obsolescence_mechanism,
    'Is expertise collapse driven by genuine technological substitution or by credential inflation as a rent-seeking mechanism?',
    'Cross-sectional analysis of wage suppression vs. measured productivity gains; skill obsolescence rates before/after credential requirement escalation; comparison of expert labor value in credentialed vs. non-credentialed markets',
    'If technological substitution: structural constraint is mountain-like (immutable), and market collapse is inevitable. If rent-seeking: constraint is snare (enforcement-dependent), and market could reset if credentialing mechanisms collapsed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_obsolescence_mechanism, empirical, 'Whether expertise collapse is due to technological change or credentialism').

omega_variable(
    identity_lock_depth,
    'How many trapped experts are unable to exit due to material barriers (age, family obligations, geographic immobility) vs. identity fusion with the expertise?',
    'Post-layoff longitudinal studies tracking career transitions; surveys measuring identity-fusion intensity and correlation with reemployment outcomes; analysis of barriers cited by non-transitioners vs. transitioners',
    'If primarily material barriers: exit_options should be ''constrained'' (high cost but possible) rather than ''trapped''. If primarily identity-locked: the constraint persists even if material barriers are removed, indicating cognitive capture deeper than structural mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Degree of identity fusion vs. material barriers in expert labor mobility').

omega_variable(
    knowledge_ecosystem_damage,
    'At what point does expert labor collapse damage the knowledge transmission infrastructure irreversibly?',
    'Measurement of knowledge transfer success rates (mentorship continuation, apprenticeship completion, publication quality); intergenerational expertise retention in domains experiencing collapse; comparison with historical profession-collapse cases (agriculture, manufacturing expertise)',
    'If damage is reversible within 20-30 years: constraint is snare with recoverability. If damage cascades (second-generation experts unavailable to mentor third): constraint approaches mountain status (expertise loss becomes irreversible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_ecosystem_damage, empirical, 'Reversibility of knowledge ecosystem damage from expert labor collapse').

omega_variable(
    credential_inflation_endogeneity,
    'Does credential requirement escalation drive expert supply collapse, or does collapsing demand cause credential requirement escalation as a filtering mechanism?',
    'Time-series analysis of credential requirements relative to job openings; comparison of credential timelines across labor markets with different demand trajectories; causal inference from policy shocks (licensure requirement changes)',
    'If inflation drives collapse: constraint is manufactured and could be dismantled by credential reform. If collapse drives inflation: credential escalation is a symptom, not a driver, and reform alone is insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_endogeneity, empirical, 'Causal direction in credential inflation vs. expert labor collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_expert_labor_market_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(delc_tr_t0, domain_expert_labor_market_collapse, theater_ratio, 0, 0.3).
narrative_ontology:measurement(delc_tr_t10, domain_expert_labor_market_collapse, theater_ratio, 10, 0.42).
narrative_ontology:measurement(delc_tr_t20, domain_expert_labor_market_collapse, theater_ratio, 20, 0.55).
narrative_ontology:measurement(delc_tr_t5, domain_expert_labor_market_collapse, theater_ratio, 5, 0.36).
narrative_ontology:measurement(delc_tr_t15, domain_expert_labor_market_collapse, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(delc_be_t0, domain_expert_labor_market_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(delc_be_t10, domain_expert_labor_market_collapse, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(delc_be_t20, domain_expert_labor_market_collapse, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(delc_be_t5, domain_expert_labor_market_collapse, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(delc_be_t15, domain_expert_labor_market_collapse, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_expert_labor_market_collapse, identity_coordination).
narrative_ontology:affects_constraint(domain_expert_labor_market_collapse, knowledge_transmission_degradation).
narrative_ontology:affects_constraint(domain_expert_labor_market_collapse, credential_inflation_spiral).
narrative_ontology:affects_constraint(domain_expert_labor_market_collapse, professional_guild_erosion).

% DUAL FORMULATION NOTE:
% Domain expert labor market collapse decomposes into three structurally related constraints: (1) knowledge_transmission_degradation (ε≈0.45) — universities degrading from knowledge institutions to credential factories; (2) credential_inflation_spiral (ε≈0.52) — feedback loop where falling expert salaries drive credential requirement escalation; (3) professional_guild_erosion (ε≈0.38) — loss of professional association enforcement capacity. Each has distinct measurement trajectories and omega variables. This story represents the unified constraint experienced by all three; the family members represent decomposed causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(domain_expert_labor_market_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
