% ============================================================================
% CONSTRAINT STORY: ua_mobilization_protector_cert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ua_mobilization_protector_cert, []).

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
 *   constraint_id: ua_mobilization_protector_cert
 *   human_readable: Ukrainian 'Protector' Certificate Mobilization System
 *   domain: political/technological/military_administration
 *
 * SUMMARY:
 *   The Ukrainian 'Protector' Certificate Mobilization System represents a
 *   mandatory digital credential framework designed to integrate military
 *   conscription, personnel tracking, and territorial movement control during
 *   existential conflict. Implemented in response to Russia's 2022 invasion
 *   and ongoing military pressure, the system creates a unified access permit
 *   that certifies mobilization status, exemption grounds, and regional
 *   assignment for draft-eligible population. Citizens require valid
 *   Protector certificates to cross regional boundaries, access certain
 *   services, and maintain legal status. The constraint embodies the
 *   structural tension between military necessity (centralized coordination
 *   of defense) and civilian costs (surveillance integration, suppression of
 *   exit options, concentration of allocative power in security apparatus).
 *   From the perspective of draft-eligible citizens, the system is a pure
 *   extraction mechanism with no meaningful alternative. From the perspective
 *   of the central mobilization authority, it is a necessary coordination
 *   tool solving problems of inter-agency communication, personnel
 *   verification, and resource allocation that would otherwise paralyze the
 *   defense effort. The constraint's theater_ratio reflects that the system
 *   has genuine logistical function (not pure theater) but also significant
 *   performative elements — formal appeals processes, humanitarian exemption
 *   pathways, and inter-agency review boards that have limited capacity to
 *   constrain central authority during wartime. The extractiveness trajectory
 *   (0.32 → 0.58 over the interval) reflects scope expansion: initial system
 *   design prioritized military logistics, but integration with internal
 *   migration controls, employment verification, and financial access
 *   expanded the extraction surface. As the system matures and becomes
 *   integrated with state infrastructure, the barriers to exit increase and
 *   the beneficiary's control over allocative decisions deepens.
 *
 * KEY AGENTS:
 *   - Draft-Eligible Population: Primary victims (powerless/trapped) — subject to mandatory certification, movement restrictions, and conscription obligation with no legal exit
 *   - Central Mobilization Authority: Primary beneficiary (institutional/arbitrage) — controls system design, certification criteria, and enforcement discretion; solves coordination problems across military, state security, and internal migration systems
 *   - Regional Administrators: Secondary actors (moderate/constrained) — enforce certification requirements, process exemptions, and report compliance metrics to central authority; face pressure to meet quotas
 *   - State Security Apparatus: Institutional beneficiary (institutional/arbitrage) — gains integrated surveillance access and coordination of internal security operations; benefits from unified population identity database
 *   - Military Command Structure: Mixed actor (powerful/mobile) — benefits from logistics coordination but bears enforcement costs and technological integration burden
 *   - Civil Society Monitoring Coalition: Organized victim (organized/constrained) — monitors system compliance, documents exemption denials, advocates for appeal accessibility; faces pressure and information restrictions
 *   - Allied Governments: Institutional observer (institutional/constrained) — benefit from Ukrainian military capability but cannot enforce system design changes without breaking alliance sovereignty norms
 *   - International Law Framework: Institutional observer (institutional/arbitrage) — nominally constrains enforcement but lacks enforcement mechanisms against allied wartime actions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_mobilization_protector_cert, 0.58).
domain_priors:suppression_score(ua_mobilization_protector_cert, 0.72).
domain_priors:theater_ratio(ua_mobilization_protector_cert, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, extractiveness, 0.58).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ua_mobilization_protector_cert, snare).
narrative_ontology:human_readable(ua_mobilization_protector_cert, "Ukrainian 'Protector' Certificate Mobilization System").
narrative_ontology:topic_domain(ua_mobilization_protector_cert, "political/technological/military_administration").

domain_priors:requires_active_enforcement(ua_mobilization_protector_cert).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, central_mobilization_authority).
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, state_security_apparatus).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, draft_eligible_population).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, regional_autonomy).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, economic_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRAFT-ELIGIBLE CITIZEN (SNARE) — Trapped within Ukrainian territory and conscription law. The Protector certificate creates a surveillance-integrated permit system with no genuine alternative. Cannot exit through legal channels; non-compliance triggers legal penalties, asset freezes, and mobility restrictions. Movement outside secure zones requires valid certificate. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. High effective extraction.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ADMINISTRATOR (SNARE) — Constrained by central authority directives but also benefits from resource allocation tied to compliance metrics. Faces pressure to enforce quotas and certify population. Has limited exit options (transfer, resignation risk career damage). Cannot refuse integration without losing administrative role. d≈0.68, f(d)≈0.98, σ=0.9 → χ≈0.57. Moderate-high extraction.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MILITARY COMMAND STRUCTURE (TANGLED ROPE) — Benefits from centralized personnel tracking and logistics coordination (rope function). Experiences enforcement burden and technological implementation costs (extraction component). Has some exit options through policy advocacy and resource negotiation. Sees both coordination necessity and asymmetric burden. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.40. Mixed extraction-coordination.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY MONITORING COALITION (SNARE) — Organized advocacy groups, legal NGOs, and human rights monitors are constrained by regime pressure and information access limitations. Cannot exit scrutiny role without abandoning mission. Face reputational and legal risks from enforcement apparatus. Have limited organizational exit options despite collective action capacity. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.64. High extraction relative to organized baseline.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL MOBILIZATION AUTHORITY (ROPE) — Primary beneficiary experiencing the constraint as coordination mechanism. Protector system solves real logistics, verification, and inter-agency coordination problems. Has significant exit options (policy revision, system redesign, alternative frameworks). Sees extraction component as minor cost of necessary coordination. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net negative extraction = coordination benefit.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALLIED OVERSIGHT APPARATUS (TANGLED ROPE) — NATO, EU, and allied governments benefit from improved Ukrainian military capability (rope coordination). Constrained by diplomatic necessity to accept Ukrainian sovereignty over system design (extraction burden they can observe but not control). Cannot exit without breaking alliance commitments. d≈0.58, f(d)≈0.73, σ=1.2 → χ≈0.47. Moderate mixed extraction-coordination.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL LAW FRAMEWORK (PITON) — Nominally constrained by humanitarian law, Geneva conventions, and protections for non-combatants. In practice, enforcement mechanisms are largely performative: no enforcement action against allies in active conflict. Theater of legal review persists despite inability to constrain state behavior during existential threat. theater_ratio=0.55 reflects partial functionality (some constraints honored) and partial performance (enforcement gaps). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (FALSE SUMMIT) — From a civilizational scale, centralized mobilization systems appear as immutable necessities of modern warfare: states at existential risk cannot coordinate defense without personnel tracking and integrated logistics. Risks naturalizing the contingent institutional design as a law of conflict. However, base properties (ε=0.58, suppression=0.72, theater=0.55) contradict the mountain classification. The constraint contains significant contingent policy choices (certification granularity, enforcement thresholds, appeal mechanisms) that differentiate it from physical laws. Engine classifies as false summit.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_mobilization_protector_cert_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ua_mobilization_protector_cert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ua_mobilization_protector_cert, TR),
    TR >= 0.70.

:- end_tests(ua_mobilization_protector_cert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The system extracts significant costs from draft-eligible population: surveillance overhead (certificate generation, data submission, proof of status), time costs (bureaucratic processing, appeals), and opportunity costs (geographic immobility prevents relocation for employment or family reasons). The extraction is not maximal (0.70+) because the system has some genuine coordination function and the draft-eligible population retains limited appeal pathways and exemption grounds. The trajectory from 0.32 → 0.58 reflects scope expansion from core military logistics toward integrated population control. Suppression (0.72): High. The system creates multiple enforcement levers with limited alternatives: without valid certificate, citizens face legal penalties (fines, asset seizures), movement restrictions (internal checkpoints), and exclusion from formal services. The suppression is not absolute (1.0) because some exemption grounds are honored in practice (medical, caregiver, occupational) and informal workarounds exist for localized movement. However, formal legal exit routes are genuinely constrained — there is no official 'opt-out' pathway that preserves legal status and geographic mobility. Theater ratio (0.55): Moderate. The system has real coordination function (logistics, inter-agency communication) but also significant performative elements: appeal boards exist but have limited discretion to overturn central authority decisions; humanitarian exemption criteria are published but applied inconsistently; inter-agency review processes occur but lack transparency. The theater is lower than pure surveillance systems (0.70+) because the coordination function is genuine; it is higher than systems with fully transparent and accountable decision-making (0.25-0.35).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival divergence between those who control the system and those subject to it. The central mobilization authority experiences the constraint as a coordination mechanism solving complex problems of military logistics, inter-agency synchronization, and personnel verification — they classify it as Rope and see only modest coordination overhead (f(d)≈-0.08). The draft-eligible citizen experiences the same system as a trap with no meaningful exit, comprehensive surveillance, and asymmetric allocative power concentrated in the hands of authorities — they classify it as pure Snare (χ≈0.80). The military command structure, though powerful, is constrained by the system's enforcement burdens and technological costs, classifying it as Tangled Rope with moderate extraction (χ≈0.40). The regional administrator is caught between central directives and local implementation pressures, experiencing both the coordination benefit (clearer reporting hierarchies) and the extraction cost (accountability for compliance). The civil society monitoring coalition, despite being organized, is structurally constrained by regime pressure and information asymmetries, experiencing the system as a snare that limits their exit options (cannot withdraw from monitoring role without betraying mission). The analytical observer risks naturalizing the constraint as an immutable feature of wartime governance, when in fact the specific design choices (certification granularity, appeal accessibility, surveillance integration scope) are contingent and could be substantially reformed without sacrificing core military coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Draft-eligible population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction directionality — these agents bear full cost of the constraint and have no legal exit options. Central mobilization authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Strong beneficiary directionality — primary designer and controller of system with multiple exit options (policy revision, system redesign, alternative frameworks). Regional administrator: Mixed position — victim of central directives (constrained exit) but also gatekeeper with some beneficiary function (resource allocation tied to compliance). Coded as d≈0.68 reflecting the constrained exit option. Military command structure: Victim of enforcement burden + mobile exit (policy advocacy, resource negotiation) → d≈0.52, f(d)≈0.68. Moderate extraction despite powerful structural position, because the mobile exit option (negotiation over resource allocation) prevents maximal victimization. Civil society coalition: Organized but constrained by regime pressure and information restrictions → d≈0.75, f(d)≈1.10. Despite being organized, these agents cannot exit their monitoring role without mission abandonment, and face reputational and legal risks from enforcement apparatus. Allied governments: Constrained by diplomatic necessity despite powerful position → d≈0.58, f(d)≈0.73. They benefit from Ukrainian military capability (rope function) but cannot enforce system design changes without violating alliance sovereignty norms. International law framework: d≈0.05, f(d)≈-0.12. Nominally the beneficiary (can articulate standards and generate legitimacy) but functionally a piton — enforces through normative pressure only, with gaps during wartime.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is 'Is the Protector system a legitimate military coordination mechanism (Rope/Tangled Rope) or a coercive extraction system (Snare)?' The resolution depends on whether the military necessity argument withstands scrutiny, and whether less-coercive alternatives exist. The indexical framework resolves this by showing that BOTH characterizations are structurally correct from different perspectives: the central authority's Rope classification (χ≈-0.05) and the draft-eligible citizen's Snare classification (χ≈0.80) are not contradictory — they represent different structural positions within the same constraint. The tension is not a classification error but a genuine distributional conflict. However, the system's extractiveness (0.58) and suppression (0.72) suggest it is not *purely* Rope-like (which would require ε ≤ 0.45 and suppression ≤ 0.55). The high suppression relative to coordination benefit indicates the system has crossed the threshold into genuine Snare territory from most perspectives. The mandatrophy resolves by accepting that the system is a *contingent* Snare — legitimate only insofar as the military threat is existential and no less-coercive alternatives exist. If either condition is violated (threat recedes, or alternatives become viable), the classification should shift toward Scaffold (temporary support with sunset clause) or remediated Tangled Rope (with meaningful appeal processes and scope constraints). The Analytical Observer's false mountain classification is caught by this analysis: the claim that 'centralized mobilization is a law of warfare' is contradicted by the existence of alternative coordination models (distributed logistics, volunteer-based systems, consent-based reporting) that peer democracies have historically used under non-existential threat. The Protector system's high suppression (0.72) is not inherent to mobilization logistics but reflects specific policy choices about centralization, surveillance integration, and enforcement discretion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_threshold,
    'At what level of military necessity does extraction become justified coordination, and how is that threshold measured?',
    'Comparative analysis of mobilization systems in peer democracies during similar threat levels; evaluation of alternative coordination mechanisms with lower suppression; forensic examination of specific certification denial cases and their justifications',
    'If threshold is low: constraint is justified snare (extraction licensed by context). If threshold requires demonstrating less-coercive alternatives: constraint may be reclassified as unjustified tangled_rope or snare with remediation pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_threat_threshold, conceptual, 'Whether suppression level is justified by existential military necessity').

omega_variable(
    certificate_scope_creep,
    'Will the Protector system''s purpose expand beyond military mobilization to general population surveillance and social control after the acute conflict phase?',
    'Historical tracking of similar systems post-conflict (Israeli security apparatus, Turkey''s ID system, Russia''s documentation controls); policy analysis of scope expansion mechanisms and incentive structures; civil society monitoring of actual system usage patterns over 3+ year periods',
    'If scope creep confirmed: the constraint''s effective extractiveness increases (χ approaches 1.0) and suppression becomes institutionalized across peacetime. If contained to military mobilization: extractiveness may decline post-conflict. Classification trajectory: snare (active conflict) → tangled_rope or piton (post-conflict).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certificate_scope_creep, empirical, 'Whether Protector system expands beyond military mobilization').

omega_variable(
    alternative_coordination_viability,
    'Could decentralized, voluntary, or consent-based mobilization mechanisms achieve comparable military logistical coordination with significantly lower suppression?',
    'Analysis of historical volunteer-based and conscription-free military systems during peer-level conflicts; game-theoretic modeling of incentive structures for voluntary reporting; comparative cost-benefit analysis of centralized vs distributed coordination approaches',
    'If viable alternatives exist: the snare classification is vulnerable to challenge (constraint is not structurally necessary). If alternatives prove infeasible under wartime pressure: snare classification is reinforced as a contingent necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether lower-suppression alternatives to centralized mobilization exist').

omega_variable(
    appeal_mechanism_functionality,
    'Do the formal appeal and exemption processes for the Protector certificate function as meaningful checks on authority, or are they performative (rubber-stamp operations)?',
    'Statistical analysis of appeal approval/denial rates and timelines; qualitative interviews with appeal applicants and reviewing officials; analysis of correlation between appeal outcomes and documented humanitarian, occupational, or health grounds',
    'If appeals are meaningful: suppression is lower than coded (0.72 → ~0.55), and the constraint is closer to tangled_rope boundary. If appeals are performative: suppression is correctly coded or understated, and classification as snare is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeal_mechanism_functionality, empirical, 'Whether appeal mechanisms provide functional checks on certificate denial').

omega_variable(
    technological_failure_cascade,
    'What happens to population mobility and legal status if the Protector system experiences significant technical failures (data loss, authentication server outages, database corruption)?',
    'System architecture review and failure mode analysis; examination of contingency protocols; stress testing of backup and recovery mechanisms; analysis of legal status definitions during system unavailability',
    'If system has robust contingency (certificates can be manually verified, alternative pathways exist): suppression risk is lower and theater_ratio reflects hybrid digital-analog process. If system lacks contingency (authentication unavailability = legal trap): suppression is actually higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_failure_cascade, empirical, 'System resilience to technical failures affecting population mobility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ua_mobilization_protector_cert, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(protector_tr_t0, ua_mobilization_protector_cert, theater_ratio, 0, 0.35).
narrative_ontology:measurement(protector_tr_t6, ua_mobilization_protector_cert, theater_ratio, 6, 0.48).
narrative_ontology:measurement(protector_tr_t12, ua_mobilization_protector_cert, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(protector_be_t0, ua_mobilization_protector_cert, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(protector_be_t6, ua_mobilization_protector_cert, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(protector_be_t12, ua_mobilization_protector_cert, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ua_mobilization_protector_cert, enforcement_mechanism).
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, internal_migration_control_system).
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, war_economy_labor_allocation).
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, state_emergency_powers_framework).

% DUAL FORMULATION NOTE:
% The Protector system decomposes into multiple structurally distinct constraints: (1) Military logistics coordination (ε≈0.25, Mountain or Rope), (2) Population movement control (ε≈0.55, Snare), (3) Surveillance integration (ε≈0.65, Snare). These are linked as constraint family members because the system conflates them into a single certificate. Future design separation (military transport pass vs. identity certification vs. surveillance query) would create three independent stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ua_mobilization_protector_cert, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
