% ============================================================================
% CONSTRAINT STORY: rn_proteus_adoption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rn_proteus_adoption, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: rn_proteus_adoption
 *   human_readable: Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter
 *   domain: defense/technological_capability
 *
 * SUMMARY:
 *   The Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter
 *   represents a technological transition that exhibits hybrid
 *   coordination-extraction dynamics. The constraint operates at multiple
 *   levels: Leonardo and UK defense procurement experience the integration as
 *   straightforward coordination (solving a capability gap); the helicopter
 *   pilot community experiences it as pure extraction (career disruption,
 *   status loss, trapped in seniority systems); training infrastructure
 *   experiences mixed coordination and extraction; and defense industrial
 *   strategy sees it as a temporary scaffold leading to full unmanned
 *   transition by ~2035. The theater ratio (0.58) reflects that much of the
 *   current discussion around Proteus adoption emphasizes modernization
 *   rhetoric ('next-generation ISR,' 'autonomous capability') while the
 *   underlying drivers are cost reduction and workforce optimization — the
 *   capability frame partially masks the extraction mechanism. The two-year
 *   trial is simultaneously a genuine operational test and a staged workforce
 *   transition that suppresses alternative voices in the pilot community
 *   through contractual lock-in.
 *
 * KEY AGENTS:
 *   - Helicopter Pilot Community: Primary victim (powerless/trapped) — career disruption, billet compression, status loss relative to emerging unmanned drone operators
 *   - Leonardo Spa: Primary beneficiary (institutional/arbitrage) — contract revenue, market dominance in UK/NATO unmanned helicopter segment, first-mover advantage
 *   - UK Defense Procurement (MoD): Secondary beneficiary (institutional/arbitrage) — capability acquisition at reduced lifecycle cost, modernization narrative
 *   - Helicopter Training Infrastructure: Mixed (moderate/constrained) — benefits from new curriculum; constrained by dual-track requirements and funding uncertainty
 *   - Strike Flotilla Operational Commanders: Mixed (powerful/mobile) — gains extended operational range and persistence; constrained by integration risks and regulatory ambiguity
 *   - UK Defense Industrial Strategy: Organized coordinator (organized/constrained) — drives transition; sees sunset when unmanned platforms mature (~2035)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rn_proteus_adoption, 0.35).
domain_priors:suppression_score(rn_proteus_adoption, 0.42).
domain_priors:theater_ratio(rn_proteus_adoption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rn_proteus_adoption, extractiveness, 0.35).
narrative_ontology:constraint_metric(rn_proteus_adoption, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rn_proteus_adoption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rn_proteus_adoption, tangled_rope).
narrative_ontology:human_readable(rn_proteus_adoption, "Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter").
narrative_ontology:topic_domain(rn_proteus_adoption, "defense/technological_capability").

domain_priors:requires_active_enforcement(rn_proteus_adoption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, leonardo_spa).
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, operational_commanders).
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, uk_defense_procurement).
narrative_ontology:constraint_victim(rn_proteus_adoption, crewed_helicopter_ecosystem).
narrative_ontology:constraint_victim(rn_proteus_adoption, carrier_air_group_billets).
narrative_ontology:constraint_victim(rn_proteus_adoption, helicopter_pilot_training_pipeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREWED HELICOPTER PILOT COMMUNITY (SNARE) — Trapped by contractual obligations and seniority systems; cannot exit the Royal Navy to preserve career continuity. The two-year Proteus trial creates uncertainty about future air group billets. Career path now bifurcates into manned/unmanned tracks with asymmetric prestige. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(rn_proteus_adoption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HELICOPTER TRAINING INFRASTRUCTURE (TANGLED ROPE) — Constrained by dual curriculum requirements (manned + unmanned pilots) and funding bottlenecks. Coordination benefit: Proteus training creates new pilot pipeline efficiencies. Extraction: uncertainty about resource allocation and curriculum evolution. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(rn_proteus_adoption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEONARDO SPA + UK DEFENSE PROCUREMENT (ROPE) — Experiences the constraint as coordination: Proteus adoption solves Royal Navy's requirement for extended-range ISR/strike capability without the crew costs of traditional helicopters. Leonardo captures contract revenue; procurement gains capability at reduced operational cost. First-mover advantage in NATO UUV market. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UK DEFENSE INDUSTRIAL STRATEGY (SCAFFOLD) — Organized agents (MoD, UKSA, industry roadmaps) see Proteus adoption as a temporary coordination bridge: shifting from crewed to autonomous helicopter platforms is inevitable, and the two-year trial is a structured transition mechanism with an implicit sunset (full transition to unmanned carrier air by ~2035). Suppression is tolerated because the coalition has visibility and agency. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.10. Low effective extraction relative to coordination benefit.
constraint_indexing:constraint_classification(rn_proteus_adoption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATO CREWED HELICOPTER DOCTRINE (PITON) — Traditional carrier air group organization (with crewed helicopters) persists through institutional inertia and interoperability requirements even as operational logic favors unmanned platforms. theater_ratio=0.58 indicates partial degradation: maritime doctrine still emphasizes crewed presence ('boots on deck') despite unmanned capability proving superior for many missions. The ritual of crewed carrier aviation maintains itself through allied training relationships and doctrine manuals even as the primary function (reconnaissance, targeting, strike) is migrating to unmanned platforms. d≈0.30, f(d)≈0.18, σ=1.2 → χ≈0.03.
constraint_indexing:constraint_classification(rn_proteus_adoption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPERATIONAL COMMANDER / STRIKE FLOTILLA (TANGLED ROPE) — Powerful actor with mobile exit (can deploy alternative ISR platforms). Experiences the constraint as mixed: Proteus offers significant operational benefit (longer endurance, persistent station-keeping, reduced crew risk) but introduces new suppression (regulatory complexity, contested airspace rules of engagement for unmanned systems, technical dependency on Leonardo supply chain). Benefits from capability expansion; constrained by integration unknowns. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.15.
constraint_indexing:constraint_classification(rn_proteus_adoption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNOLOGICAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, the shift from crewed to autonomous helicopters follows an irreversible technological trend: drone platforms are inherently more cost-effective, lower-risk, and better-suited to modern naval warfare than crewed helicopters. This constraint is therefore a natural consequence of technological evolution — the two-year trial is not an extraction mechanism but an adjustment to objective reality. However, the structural data (ε=0.35, suppression=0.42, theater=0.58) contradicts the mountain classification. The engine will identify this as a false summit: the transition is not inevitable by physics but contingent on institutional choices (defense budgets, workforce redeployment, doctrine evolution) and therefore amenable to collective decision-making.
constraint_indexing:constraint_classification(rn_proteus_adoption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rn_proteus_adoption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rn_proteus_adoption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rn_proteus_adoption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rn_proteus_adoption, TR),
    TR >= 0.70.

:- end_tests(rn_proteus_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts career value from the pilot community through billet compression and status realignment, but the extraction is not maximal because the Royal Navy is not deliberately destroying the pilot community — it is responding to genuine technological change that favors unmanned platforms. The extraction is real but bounded by the legitimacy of the underlying operational requirement. Suppression (0.42): Moderate. Significant barriers exist: contractual lock-in of pilots, institutional dependencies on existing crewed doctrine, training infrastructure dual-loading, and regulatory uncertainty around unmanned combat systems. However, suppression is not total — pilots retain options to leave the service (costly but possible), alternative voices (helicopter industry, allied navies) can articulate dissent, and the trial mechanism itself allows for performance-based course correction. Theater ratio (0.58): Moderate-high. The modernization narrative around Proteus emphasizes capability and autonomy, but a substantial portion of the discussion is performative: the primary driver is cost reduction (~25-30% per-flight-hour savings vs crewed platforms), which is less publicly emphasized because it implies workforce reduction. The technical integration challenges are real, but the theater reflects selective emphasis on capability gains over workforce displacement.
 *
 * PERSPECTIVAL GAP:
 *   The helicopter pilot community (snare) sees career loss and institutional abandonment. Leonardo and procurement (rope) see a straightforward capability upgrade with revenue benefit. The operational commander (tangled rope) sees genuine operational advantage mixed with integration risk. The defense industrial strategy (scaffold) sees a temporary transition with an endpoint (full unmanned by 2035). The NATO doctrine establishment (piton) sees its own traditional crewed-helicopter doctrine as degrading but persisting through institutional momentum. The civilizational analytical observer (mountain) risks naturalizing what is actually a contingent institutional choice as inevitable technological evolution. The perspectival gap is unusually wide here because the constraint affects a specific, bounded occupational community (helicopter pilots) while benefiting abstract entities (capability, cost reduction) and distributed stakeholders (Leonardo, procurement). The pilot perspective is structurally suppressed because pilots cannot organize collectively to block the transition without violating military hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Helicopter pilot community: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Pilots cannot exit without career destruction. Leonardo + procurement: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Free to exit (cancel contract) but highly incentivized to proceed. Training infrastructure: Mixed victim/beneficiary + constrained → d≈0.58, f(d)≈0.72. Moderate extraction balanced by coordination benefit. Operational commander: Mixed victim/beneficiary + mobile → d≈0.42, f(d)≈0.42. Can deploy alternatives; benefits from capability but constrained by integration complexity. Defense industrial strategy: Organized beneficiary + constrained → d≈0.35, f(d)≈0.30. Low extraction because the coalition has agency and vision of sunset. NATO doctrine: Institutional beneficiary + arbitrage → d≈0.30, f(d)≈0.18. Piton classification from theater gate, not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY DETECTION: The constraint does NOT resolve mandatrophy cleanly because it genuinely straddles coordination and extraction. The Proteus adoption simultaneously solves a real coordination problem (Royal Navy needs extended-endurance ISR in constrained-budget environment) and extracts value from the pilot community (career disruption, workforce compression, status loss). The key omega variable is crew redeployment capacity: if the Navy successfully redeployments disemployed pilots into equivalent or higher-status roles (emerging hypersonic, space, AI domains), the constraint becomes a true scaffold with sunset and the mandatrophy resolves. If redeployment fails and pilots are simply downsized, the constraint reveals itself as a snare with performative modernization language obscuring extraction. The theater ratio (0.58) is the diagnostic signal: it indicates partial degradation of the coordination narrative, suggesting that rhetorical emphasis on capability gains is masking the cost-reduction driver. The two-year trial is structured to delay mandatrophy resolution until after initial institutional commitment, which is itself an extraction mechanism (suppressing the crew redeployment question until the trial's success becomes organizational consensus).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_performance_parity,
    'Will Proteus performance in the two-year trial match or exceed crewed helicopter capabilities in contested maritime environments (electronic warfare, air defense, complex tactical scenarios)?',
    'Operational evaluation reports; comparative metrics on target detection, engagement accuracy, and platform survivability; classified assessment of performance against peer nation air defenses',
    'If parity achieved: transition to unmanned platforms is genuine capability upgrade (enables broader transition roadmap). If deficiencies persist: Proteus adoption is theater masking underlying capability shortfall — classification shifts toward piton (degraded ritual).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_performance_parity, empirical, 'Whether Proteus performance matches crewed helicopter capabilities in contested environments').

omega_variable(
    crew_redeployment_capacity,
    'Can helicopter pilots and crew disemployed by Proteus adoption be credibly redeployed into emerging roles (hypersonic defense, space warfare, AI system management) with compensation parity and psychological buy-in?',
    'Career transition success rates post-trial; retention rates for redeployed personnel; salary/status parity metrics; union/personnel committee feedback',
    'If successful redeployment: constraint is genuine scaffold with real sunset (temporary disruption, organized transition). If redeployment fails: constraint is snare disguised as modernization (permanent career loss, generational bitterness in naval culture).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crew_redeployment_capacity, empirical, 'Whether disemployed crews can be redeployed with parity and buy-in').

omega_variable(
    regulatory_airspace_clarity,
    'Will international maritime airspace and rules of engagement for unmanned combat systems be clarified sufficiently by 2028 to enable Proteus operational deployment, or will ambiguity persist, requiring crewed oversight for legal compliance?',
    'IMO/ICAO regulatory progress; legal precedent from trial incidents; NATO CONOPS revision documentation; clarity on unmanned platform targeting authority delegation',
    'If clarity achieved: Proteus enables true operational autonomy (rope/tangled_rope classification holds). If ambiguity persists: Proteus requires embedded crewed oversight or legal cover, increasing cost and degrading the efficiency argument (shifts toward piton or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_airspace_clarity, conceptual, 'Whether international regulatory frameworks will clarify for unmanned combat systems').

omega_variable(
    leonardo_supply_chain_resilience,
    'Can Leonardo Spa maintain supply chain integrity and geopolitical independence for Proteus component manufacturing and software updates across a 20+ year operational horizon given UK-EU-NATO supply dependencies and potential US export control escalation?',
    'Supply chain audit; geographic sourcing data; software update mechanisms; comparison to alternative vendor options; impact assessment of hypothetical US technology restrictions',
    'If resilient: Proteus adoption is robust modernization (coordination benefit justified). If vulnerable: adoption creates long-term dependency on single vendor subject to geopolitical risk — extraction mechanism hardens into structural lock-in (snare from UK defense perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leonardo_supply_chain_resilience, empirical, 'Whether Leonardo supply chain can sustain Proteus over 20+ year horizon').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rn_proteus_adoption, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proteus_tr_t0, rn_proteus_adoption, theater_ratio, 0, 0.38).
narrative_ontology:measurement(proteus_tr_t1, rn_proteus_adoption, theater_ratio, 1, 0.48).
narrative_ontology:measurement(proteus_tr_t2, rn_proteus_adoption, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(proteus_be_t0, rn_proteus_adoption, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(proteus_be_t1, rn_proteus_adoption, base_extractiveness, 1, 0.28).
narrative_ontology:measurement(proteus_be_t2, rn_proteus_adoption, base_extractiveness, 2, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rn_proteus_adoption, resource_allocation).
narrative_ontology:boltzmann_floor_override(rn_proteus_adoption, 0.25).
narrative_ontology:affects_constraint(rn_proteus_adoption, uk_naval_drone_doctrine).
narrative_ontology:affects_constraint(rn_proteus_adoption, nato_unmanned_air_integration).
narrative_ontology:affects_constraint(rn_proteus_adoption, defense_industrial_workforce_transition).

% DUAL FORMULATION NOTE:
% Proteus adoption is part of a broader constraint family around UK/NATO military roboticization and workforce transition. It is downstream of the abstract constraint 'defense_industrial_workforce_transition' (ε≈0.50, Snare at worker perspective) and upstream of 'uk_naval_drone_doctrine' (ε≈0.25, Rope, addressing integration and interoperability). The Proteus story focuses on the specific technological transition mechanism; the family's structure links workforce displacement to doctrinal evolution to capability modernization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rn_proteus_adoption, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
