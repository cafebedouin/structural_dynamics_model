% ============================================================================
% CONSTRAINT STORY: sotu_2006_bush_global_democracy_advancement_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2006_bush_global_democracy_advancement_doctrine, []).

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
 *   constraint_id: sotu_2006_bush_global_democracy_advancement_doctrine
 *   human_readable: Global Democracy Advancement Doctrine as Security Mechanism (2006 SOTU Frame)
 *   domain: foreign_policy/security_doctrine
 *
 * SUMMARY:
 *   The 2006 State of the Union address articulates a doctrine linking global
 *   democratic expansion directly to U.S. national security—premised on the
 *   empirical claim that democracies are less likely to generate terrorism,
 *   weapons proliferation, failed states, and therefore direct threats to
 *   American security. This constraint operates at the intersection of
 *   genuine security coordination and asymmetric geopolitical extraction. The
 *   doctrine provides institutional justification for sustained military and
 *   political engagement worldwide while distributing costs across multiple
 *   actors: authoritarian regimes lose geopolitical leverage; non-aligned
 *   states face pressure to align with U.S. security priorities;
 *   intervention-target populations experience coercive governance reform;
 *   allied donors bear resource burdens; the international development
 *   apparatus becomes functionally captured by security apparatus interests.
 *   The extractiveness has increased over the 2006-2016 interval (0.42 →
 *   0.58) as the doctrine's resource demands and scope expanded without
 *   proportional success in reducing terrorism or state failure. The theater
 *   ratio has also risen (0.48 → 0.64), indicating growing performative
 *   content: democracy promotion institutions increasingly engage in rituals
 *   (elections held, training programs completed, institutional reforms on
 *   paper) disconnected from functional democratic outcomes. The constraint
 *   exhibits all six classification types depending on perspective, making it
 *   a complex hybrid that combines genuine security coordination with
 *   structural extraction mechanisms.
 *
 * KEY AGENTS:
 *   - U.S. Security Apparatus (institutional/arbitrage): Primary beneficiary — gains predictability, threat reduction framing, expanded mandate for global engagement
 *   - Intervention-Target Populations (powerless/trapped): Primary victim — subject to coercive governance reform without genuine consent; bear direct costs of military intervention and structural adjustment
 *   - Authoritarian Regimes (powerful/constrained): Secondary actor — constrained by loss of autonomy and threat of intervention, but also benefit from clear negotiating parameters with major power
 *   - Allied Donor States (organized/constrained): Secondary victim — constrained by security alliance dependence; bear resource costs while U.S. captures geopolitical advantage
 *   - Democracy Promotion Institutions (institutional/arbitrage): Institutional apparatus increasingly captured by security apparatus; maintain themselves through performative activity and funding flows
 *   - International Development Community (organized/constrained): Sees doctrine as time-limited with eventual transition to development partnership; retains some agency through framing as temporary
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent security doctrine as immutable law of great-power competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2006_bush_global_democracy_advancement_doctrine, 0.58).
domain_priors:suppression_score(sotu_2006_bush_global_democracy_advancement_doctrine, 0.68).
domain_priors:theater_ratio(sotu_2006_bush_global_democracy_advancement_doctrine, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2006_bush_global_democracy_advancement_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2006_bush_global_democracy_advancement_doctrine, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_2006_bush_global_democracy_advancement_doctrine, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2006_bush_global_democracy_advancement_doctrine, tangled_rope).
narrative_ontology:human_readable(sotu_2006_bush_global_democracy_advancement_doctrine, "Global Democracy Advancement Doctrine as Security Mechanism (2006 SOTU Frame)").
narrative_ontology:topic_domain(sotu_2006_bush_global_democracy_advancement_doctrine, "foreign_policy/security_doctrine").

domain_priors:requires_active_enforcement(sotu_2006_bush_global_democracy_advancement_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2006_bush_global_democracy_advancement_doctrine, us_security_apparatus).
narrative_ontology:constraint_beneficiary(sotu_2006_bush_global_democracy_advancement_doctrine, us_geopolitical_leverage).
narrative_ontology:constraint_victim(sotu_2006_bush_global_democracy_advancement_doctrine, non_aligned_sovereignty).
narrative_ontology:constraint_victim(sotu_2006_bush_global_democracy_advancement_doctrine, resource_bearing_allies).
narrative_ontology:constraint_victim(sotu_2006_bush_global_democracy_advancement_doctrine, intervention_target_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERVENTION-TARGET POPULATION (SNARE) — Trapped in the receiving end of externally imposed governance reforms. High suppression through military presence, conditionality on aid, and structural adjustment. Minimal coordination benefit from the constraint — the 'democratic governance' being advanced is often chosen without local consent and implemented through coercive apparatus. Full extraction experienced.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AUTHORITARIAN REGIME (TANGLED ROPE) — Constrained by loss of geopolitical leverage and threat of sanctions/intervention, but also benefits from the constraint as a coordination mechanism: regime survival through demonstration of security partnership with major powers (even as a target of reform pressure), access to conditioned aid streams, and clear negotiating parameters. Extraction exists (loss of autonomy, threat of intervention) but mixed with coordination benefit.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. SECURITY APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination mechanism: democracies are claimed to produce fewer failed states, fewer terrorism sources, fewer WMD proliferation risks. The apparatus gains predictability, threat reduction, and expansive mandate for security engagement. Net beneficiary — extraction runs toward this agent. Arbitrage exit (can choose to pursue or deprioritize global democracy agenda based on shifting threat assessments).
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED DONOR STATES (TANGLED ROPE) — Organized but constrained by bilateral security dependence on the U.S. Faces pressure to contribute resources (military, development aid, diplomatic support) to the democracy advancement agenda. Mixed extraction and coordination: genuine interest in stability and threat reduction (coordination benefit) but also bear disproportionate resource costs while U.S. gains strategic positioning (extraction). Exit constrained by security alliance dependency and threat of geopolitical isolation.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NEIGHBORING NON-TARGET STATE (TANGLED ROPE) — Constrained by regional spillover (refugee flows, weapons proliferation, destabilization from intervention in neighbors). May also benefit from reduced terrorism threats and failed-state risks in the region. Experiences mixed extraction (bears costs of intervention spillover without direct input into decisions) and coordination (gains from regional stability if intervention succeeds). Exit constrained by geography and regional security dynamics.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: DEMOCRACY PROMOTION INSTITUTIONS (PITON) — NGOs, development banks, multilateral institutions tasked with implementing democracy advancement. Theater ratio high (0.64): much activity is performative legitimation of the security doctrine. The apparatus persists through institutional inertia and funding flows from security-aligned donors. Primary function (promoting genuine democratic participation) has atrophied; primary use (providing cover for geostrategic positioning) has become dominant. Maintains itself through reporting metrics (elections held, training completed) disconnected from functional democracy outcomes.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL DEVELOPMENT COMMUNITY (SCAFFOLD) — Organized agents (UNDP, World Bank, regional development banks) see the doctrine as temporary security justification with eventual sunset: as states develop economically, security rationale for intervention weakens and transitions to development partnership frame. Low effective extraction because the community has framed the constraint as time-limited and sees alternative models (economic interdependence, multilateral cooperation) as eventual replacements. Constrained exit because funding flows remain tied to security agenda, but sunset logic creates agency.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the constraint appears as an immutable law of international relations: great powers necessarily project power to secure their interests, and any security doctrine requires universal framing (democracy, human rights, rule of law) to motivate domestic constituencies and maintain alliance cohesion. The security-framed doctrine is presented as emerging naturally from structural anarchies and power competition. However, the beneficiary/victim structure reveals this as a false summit — specific institutional actors benefit systematically from the doctrine.
constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2006_bush_global_democracy_advancement_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2006_bush_global_democracy_advancement_doctrine, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2006_bush_global_democracy_advancement_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2006_bush_global_democracy_advancement_doctrine, TR),
    TR >= 0.70.

:- end_tests(sotu_2006_bush_global_democracy_advancement_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The doctrine generates measurable benefits for the U.S. security apparatus (expanded mandate, threat-reduction framing, geopolitical leverage) but faces significant empirical challenges to its core claims—post-2006 evidence increasingly questions whether democracies are demonstrably less likely sources of terrorism or state failure, or whether the relationship is spurious (development level, external support patterns confound the correlation). The extractiveness rise from 0.42 to 0.58 reflects growing gap between promised security outcomes and actual threat reduction. Suppression (0.68): High. Mechanisms include military coercion, economic conditionality on aid, diplomatic isolation of non-aligned states, institutional pressure on allied donors, and cognitive pressure (framing democratic expansion as universal good rather than geopolitical strategy). Suppression is not total—some states successfully resist, some populations mobilize resistance—but costs of non-compliance are steep. Theater ratio (0.64): Moderate-high. Much activity in the doctrine's institutional apparatus is performative: elections held without genuine contestation, civil society training programs teaching Western models disconnected from local governance capacity, institutional reforms on paper without functional change. Theater has increased over the interval as resource demands expanded without proportional outcome verification. The constraint is genuinely coordinating (democracies may indeed be more stable partners in security arrangements) but increasingly masked by performative institutional activity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is the exemplar for perspectival disagreement. From the security apparatus (institutional/arbitrage), the doctrine is Rope—genuine coordination mechanism for threat reduction. From intervention-target populations (powerless/trapped), it is Snare—coercive extraction with minimal coordination benefit. From authoritarian regimes (powerful/constrained), it is Tangled Rope—mixed loss of autonomy and clarity of alignment requirements. From allied donors (organized/constrained), it is Tangled Rope—shared security interest but resource burden. From development institutions (organized/constrained), it is Scaffold—temporary security justification with eventual sunset to development partnership. From democracy promotion apparatus (institutional/arbitrage), it is Piton—degraded institution maintained through inertia and security funding. From civilizational analyst (analytical/analytical), it risks appearing as Mountain—immutable law of great-power competition—but structural data reveals false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality flows from the U.S. security apparatus toward other actors. Primary extraction flows away from the apparatus (they are net beneficiaries); all other actors experience d > 0.5, meaning they bear net extraction. The magnitude of d is modulated by exit options: trapped actors experience maximum extraction; constrained actors experience moderate extraction; arbitrage-capable actors experience minimal extraction or negative extraction (benefit). The scope modifier σ(S) = 1.2 at global scope amplifies the effective extraction χ, making the constraint's global reach a mechanism for concealing extraction behind scale complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The doctrine classifies as Tangled Rope (ε=0.58, suppression=0.68, χ moderate-to-high) because it combines genuine security coordination (democracies may be more stable partners, threat reduction is a real concern) with systematic asymmetric extraction (U.S. security apparatus gains disproportionate benefit, costs are distributed across multiple constrained actors, intervention-target populations experience coercive governance reform). The doctrinal framing as universal good masks the asymmetric benefit distribution. The false summit (Mountain) appears from the analytical/civilizational perspective because great-power security projection appears as immutable law, but beneficiary/victim structure reveals contingent institutional arrangements. The piton classification arises from high theater ratio (0.64) and increasing performative content (rituals of democracy promotion disconnected from outcomes). The scaffold perspective is legitimately structured—development community sees genuine sunset logic as economic development eventually reduces security rationale for intervention. The snare perspective from intervention-target populations is structurally sound—trapped without consent, minimal coordination benefit, maximum extraction experienced. The mandate expansion omega captures the institutional incentive structure: is this genuinely about threat reduction, or does the doctrine function primarily as institutional mandate-expansion justification? If the latter, effective extraction is higher and the constraint approaches pure snare. The democratic peace hypothesis omega is critical: if the core empirical claim is invalidated, the entire coordination function evaporates and the constraint becomes pure extraction across all perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_peace_hypothesis_empirical_status,
    'Is the core empirical claim of the doctrine—that democracies are measurably less likely to generate terrorism, weapons proliferation, and failed states—validated by post-2006 evidence?',
    'Longitudinal analysis of terrorism sources, WMD proliferation paths, and state failure rates in democracies vs autocracies (2006-2025); control for development level, resource endowments, and external support patterns',
    'If validated: coordination function is genuine, tangled rope classification holds. If invalidated: constraint reduces to pure extraction mechanism, classification shifts to snare across most perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_peace_hypothesis_empirical_status, empirical, 'Empirical status of democratic peace hypothesis underlying the doctrine').

omega_variable(
    extraction_flow_attribution,
    'Are the security benefits flowing to the U.S. security apparatus proportional to the resource and political costs borne by other actors, or does the constraint function as structural rent extraction masked by security justification?',
    'Cost-benefit accounting: track military spending, aid flows, diplomatic capital, and resource commitments against documented threat reduction and security gains; examine counterfactual scenarios (what would threat posture be without the doctrine?)',
    'If proportional: primarily rope/tangled rope (genuine coordination with asymmetric benefit distribution). If grossly disproportionate: primarily snare (extraction mechanism with security framing as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_attribution, empirical, 'Whether extraction flow is proportional to security benefits').

omega_variable(
    institutional_capture_of_democracy_promotion,
    'Has the democracy promotion apparatus become captured by security apparatus interests, replacing genuine democratic development goals with security-aligned proxy outcomes?',
    'Institutional history analysis: track mission drift in USAID, State Department democracy programs, and allied donor agency mandates; examine correlation between democracy promotion disbursements and U.S. security interests (basing rights, intelligence access, UN voting alignment) vs independent measures of democratic institutional strength',
    'If captured: piton classification confirmed, theater ratio genuinely high. If independent: piton classification overstated, institutions retain functional democracy-promotion capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_democracy_promotion, empirical, 'Degree of security apparatus capture of democracy promotion institutions').

omega_variable(
    intervention_target_population_selection_bias,
    'Are populations receiving democracy advancement intervention systematically selected based on geopolitical utility to the U.S. rather than objective measures of democratic deficit or governance need?',
    'Geopolitical analysis: compare intervention frequency and intensity across regions with similar governance indices but different strategic importance; examine whether interventions cluster in areas of resource competition, basing rights importance, or alliance-building opportunity',
    'If utility-driven selection: confirms extraction mechanism (snare from target perspective), undermines universal-law framing. If need-driven: coordination function more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_target_population_selection_bias, empirical, 'Selection bias in democracy advancement intervention targeting').

omega_variable(
    suppression_mechanism_coercion_vs_incentive,
    'Is the measured suppression (0.68) primarily structural coercion (military capacity, sanctions threat, conditionality) or incentive-based (opportunity cost of non-alignment, access denial)?',
    'Counterfactual institutional analysis: study cases where U.S. military capacity was limited but doctrine promoted (diplomatic pressure only); study cases where coercive capacity existed but doctrine not applied (alliance with autocrats)',
    'If primarily coercive: suppression score valid, snare classification more defensible. If primarily incentive-based: suppression score may be overstated, rope classification more defensible for some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_coercion_vs_incentive, empirical, 'Whether suppression operates through coercion or incentive structure').

omega_variable(
    mandate_expansion_via_security_framing,
    'Does the security doctrine function primarily as genuine threat reduction mechanism, or as institutional mandate-expansion justification for the security apparatus?',
    'Organization theory analysis: track budget growth, personnel expansion, mission scope proliferation in State Department, DoD, intelligence community pre- and post-doctrine adoption; examine agency internal documents and Congressional testimony for stated rationales',
    'If threat-reduction primary: tangled rope with genuine coordination function. If mandate-expansion primary: tangled rope devolves toward snare, extraction mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_expansion_via_security_framing, conceptual, 'Whether doctrine serves threat reduction or institutional mandate expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2006_bush_global_democracy_advancement_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(democ_advance_tr_t0, sotu_2006_bush_global_democracy_advancement_doctrine, theater_ratio, 0, 0.48).
narrative_ontology:measurement(democ_advance_tr_t5, sotu_2006_bush_global_democracy_advancement_doctrine, theater_ratio, 5, 0.62).
narrative_ontology:measurement(democ_advance_tr_t10, sotu_2006_bush_global_democracy_advancement_doctrine, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(democ_advance_be_t0, sotu_2006_bush_global_democracy_advancement_doctrine, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(democ_advance_be_t5, sotu_2006_bush_global_democracy_advancement_doctrine, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(democ_advance_be_t10, sotu_2006_bush_global_democracy_advancement_doctrine, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2006_bush_global_democracy_advancement_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2006_bush_global_democracy_advancement_doctrine, military_basing_structure_middle_east).
narrative_ontology:affects_constraint(sotu_2006_bush_global_democracy_advancement_doctrine, intelligence_apparatus_global_surveillance).
narrative_ontology:affects_constraint(sotu_2006_bush_global_democracy_advancement_doctrine, development_aid_conditionality_system).
narrative_ontology:affects_constraint(sotu_2006_bush_global_democracy_advancement_doctrine, nato_alliance_burden_sharing).

% DUAL FORMULATION NOTE:
% The global democracy advancement doctrine is downstream of broader security strategy but represents a distinct constraint with its own extractiveness trajectory. Related constraints include specific regional security arrangements, intelligence apparatus expansion, development aid conditionality mechanisms, and NATO alliance politics. All are linked through the unified security doctrine framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2006_bush_global_democracy_advancement_doctrine, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
