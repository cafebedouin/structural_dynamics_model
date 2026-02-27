% ============================================================================
% CONSTRAINT STORY: ibm_shield_contract_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ibm_shield_contract_2026, []).

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
 *   constraint_id: ibm_shield_contract_2026
 *   human_readable: IBM SHIELD IDIQ Program Contract
 *   domain: technological/political
 *
 * SUMMARY:
 *   The IBM SHIELD IDIQ contract represents a structural constraint in which
 *   technological automation of military decision-making (the OODA loop) is
 *   coupled with vendor lock-in, contract indefiniteness, and degraded
 *   civilian oversight. The constraint exhibits all three core extraction
 *   mechanisms: (1) Temporal extraction — IBM captures sustained revenue
 *   stream via indefinite contract terms with minimal repricing triggers. (2)
 *   Competitive extraction — IDIQ structure creates winner-take-most market,
 *   eliminating competing contractors from the defense AI space. (3)
 *   Institutional extraction — automation of OODA loop reduces meaningful
 *   veto points for civilian and congressional oversight, concentrating
 *   decision authority in technical systems managed by a private corporation
 *   under national security classification. The constraint satisfies
 *   tangled_rope gates: it has a genuine coordination function (faster, more
 *   reliable OODA loops enable military effectiveness), active enforcement
 *   (DoD contracting and classification authority), beneficiaries (IBM,
 *   defense sector, military command), and victims (civilian oversight,
 *   competitors, adversarial systems facing accelerated opponent decision
 *   cycles). The theater ratio (0.55) reflects that standard procurement
 *   theater (competitive bidding, congressional hearings, audit requirements)
 *   persists but has become substantially decoupled from substantive
 *   decision-making — the contract structure was determined by technical and
 *   strategic factors; procurement process is performative.
 *
 * KEY AGENTS:
 *   - IBM Corporation: Primary beneficiary (institutional/arbitrage) — captures sustained IDIQ revenue, integration depth, strategic position in defense AI
 *   - US Military Command: Primary mixed actor (organized/constrained) — benefits from OODA loop automation; constrained by vendor dependency and reduced operator control
 *   - Civilian Oversight (Congress, DoD OCI, Defense Oversight Board): Primary victim (powerless/trapped) — lacks technical capacity to audit automated systems; oversight authority degraded without migration pathway
 *   - Competing Defense Contractors: Secondary victim (powerless/trapped) — excluded from market via IDIQ lock-in; cannot compete after integration depth established
 *   - Adversarial Intelligence Systems: Secondary victim (moderate/constrained) — face accelerated opponent decision cycles; constrained by asymmetric capability gap
 *   - Defense Procurement Theater (committees, review boards, audit functions): Institutional actor (institutional/arbitrage) — maintains performative role despite substantive irrelevance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ibm_shield_contract_2026, 0.58).
domain_priors:suppression_score(ibm_shield_contract_2026, 0.68).
domain_priors:theater_ratio(ibm_shield_contract_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ibm_shield_contract_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ibm_shield_contract_2026, tangled_rope).
narrative_ontology:human_readable(ibm_shield_contract_2026, "IBM SHIELD IDIQ Program Contract").
narrative_ontology:topic_domain(ibm_shield_contract_2026, "technological/political").

domain_priors:requires_active_enforcement(ibm_shield_contract_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, ibm_corporation).
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, defense_technology_sector).
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, us_military_command).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, civilian_oversight_mechanisms).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, competing_contractors).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, adversarial_intelligence_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN OVERSIGHT (SNARE) — Cannot exit the constraint; bears full cost of automated decision-making authority migration. Oversight mechanisms lack technical capacity to audit AI-enabled OODA loop. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.98. Trapped in degraded oversight capacity.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING CONTRACTORS (SNARE) — IDIQ structure creates winner-take-most dynamic. Competitors lack pathway to displace IBM once lock-in achieved through integration depth. d≈0.88, f(d)≈1.33, σ=1.1 → χ≈0.86. Trapped in technological dependency.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY COMMAND (TANGLED ROPE) — Benefits from automation of OODA loop (coordination function: faster decision cycles). Simultaneously victim of vendor lock-in and reduced human operator capacity. Constrained by contractual dependencies and operational continuity requirements. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.49. Mixed extraction/coordination.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IBM CORPORATION (ROPE) — Primary beneficiary. Experiences constraint as pure coordination: solving the technical problem of OODA loop automation. IDIQ terms enable continuous revenue, integration depth, and strategic position in defense technology. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERSARIAL SYSTEMS (TANGLED ROPE) — Victim of US OODA loop acceleration; also benefits (asymmetrically) from constraints it places on US oversight and decision authority. Constrained by inability to mirror technological capability. d≈0.68, f(d)≈1.03, σ=1.2 → χ≈0.63. Extraction masked as coordination challenge.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PROCUREMENT THEATER (PITON) — Congressional defense committee hearings, competitive bid processes, and audit protocols persist despite structural irrelevance. IDIQ contract signed with minimal substantive debate. Theater ratio=0.55 reflects performative compliance with acquisition regulations. d≈0.10, f(d)≈-0.04, σ=1.1 → χ≈-0.02. Theatrical maintenance of oversight authority that no longer functions.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, technological advantage in automation creates an immutable power asymmetry: the side with faster OODA loops wins. This appears as law of warfare. However, structural data (ε=0.58, suppression=0.68, theater=0.55) contradicts mountain gates. This is a false summit: contingent institutional arrangements (IDIQ contracting, vendor lock-in, oversight degradation) are naturalized as inevitable competition.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_contract_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ibm_shield_contract_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ibm_shield_contract_2026, TR),
    TR >= 0.70.

:- end_tests(ibm_shield_contract_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. IBM captures sustained economic rent (IDIQ indefinite delivery/indefinite quantity terms), integration rent (switching costs for military systems), and strategic rent (exclusive position in defense AI space). However, extraction is not maximal because military command does gain genuine operational benefit from OODA loop automation — the constraint is not pure rent-seeking. The trajectory shows rising extractiveness (0.42→0.58 over interval) as integration depth increases and switching costs accumulate. Suppression (0.68): High. Barriers to exit include: (a) classification and national security restrictions preventing public scrutiny, (b) technical switching costs and integration dependency, (c) temporal lock-in (IDIQ provides indefinite revenue reducing pressure to renegotiate), (d) asymmetric information (IBM controls system knowledge; military has limited independent audit capacity). Theater ratio (0.55): Moderate. Procurement process (competitive bidding, congressional testimony, audit committees) persists but is disconnected from actual contract determination. The SHIELD contract was awarded based on technical evaluation and strategic positioning, not procurement theater. Theater increased over interval (0.35→0.55) as oversight mechanisms became more performative relative to actual decision authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. IBM sees pure coordination (Rope) — a technical solution to a real problem. Military command sees tangled rope — real operational benefits coupled with vendor dependency. Civilian oversight sees snare — trapped in degraded audit capacity with no exit. Competitors see snare — permanently excluded via lock-in. Adversarial systems see tangled rope — face acceleration of opponent decision cycles (extraction) but also incur constraints on their own OODA loop advancement (coupling). Procurement theater sees piton — performative oversight ritual maintained despite irrelevance. The analytical observer risks seeing mountain (inevitable competition drives technological advancement) but the structural data reveals this as false summit — the constraint is contingent on IDIQ contracting terms, vendor lock-in mechanisms, and oversight degradation, not immutable competition law.
 *
 * DIRECTIONALITY LOGIC:
 *   IBM: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary with negative effective extraction. Military command: Mixed (beneficiary of coordination, victim of lock-in) + constrained → d≈0.62, f(d)≈0.85. Significant mixed position. Civilian oversight: Victim (oversight authority degraded) + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — no exit option and structural powerlessness. Competitors: Victim (excluded via lock-in) + trapped → d≈0.88, f(d)≈1.33. High extraction; trapped in permanent market exclusion. Adversarial systems: Victim (face accelerated opponent cycles) + constrained (cannot mirror capability) → d≈0.68, f(d)≈1.03. Significant extraction with structural constraints. Procurement theater: Institutional (maintains performative role) + arbitrage → d≈0.10, f(d)≈-0.04. Theatrical beneficiary; substantive irrelevance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled — it combines a coordination function (OODA loop automation solves real military effectiveness problem) with extraction mechanisms (vendor lock-in, indefinite contract terms, oversight degradation). The false mountain summit is the claim that 'technological competition inevitably leads to concentration and automation' — this naturalizes what is actually a contingent institutional arrangement (IDIQ contracting structure, classification barriers, vendor lock-in mechanism design). The tangled rope classification forces recognition that extraction and coordination are simultaneously present and structurally coupled. IBM's gain comes not merely from providing a useful system (that would be pure Rope) but from the institutional terms that prevent renegotiation, competitive replacement, or meaningful oversight — these are the extraction mechanisms. Conversely, the constraint is not pure Snare because military command genuinely benefits from the coordination function. The perspectival divergence between IBM (Rope) and oversight (Snare) is the diagnostic signature of tangled rope: beneficiary and victim experience the same structural phenomenon as opposite things.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ooda_loop_independence,
    'Is the SHIELD OODA loop automation a coordination function (faster decision-making benefits all stakeholders) or pure extraction (concentrated decisioning authority in automated systems bypasses civilian oversight)?',
    'Analysis of decision authority distribution before vs after SHIELD deployment; audit of veto points retained vs eliminated in automated chain',
    'If coordination dominant: constraint reclassifies toward Rope/Scaffold from military perspective. If extraction dominant: remains Tangled Rope/Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ooda_loop_independence, empirical, 'Whether OODA automation is coordination or extraction function').

omega_variable(
    vendor_lock_in_irreversibility,
    'Can the US military migrate away from IBM systems without catastrophic operational loss, or has integration depth created irreversible dependency?',
    'Technical audit of system interdependencies; cost-benefit analysis of alternative contractor transition; historical comparison with previous defense IT transitions',
    'If reversible: snare classification for competitors is avoidable. If irreversible: snare is structural and permanent for 15-25 year contract horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_in_irreversibility, empirical, 'Reversibility of IBM vendor lock-in').

omega_variable(
    civilian_oversight_technical_feasibility,
    'Can civilian and congressional oversight mechanisms be rebuilt to audit automated military OODA loops, or is the technical asymmetry insurmountable?',
    'Assessment of AI transparency/interpretability tools; analysis of what oversight data would be required vs what is technically extractable from black-box systems',
    'If feasible: oversight snare reverses. If infeasible: civilian oversight remains permanently trapped in degraded state for duration of SHIELD deployment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_oversight_technical_feasibility, empirical, 'Whether civilian oversight can be technically rebuilt').

omega_variable(
    idiq_indefinite_quantity_boundaries,
    'What prevents the SHIELD IDIQ from expanding to non-military domains (law enforcement, border control, intelligence) once integration and automation depth are established?',
    'Legal analysis of IDIQ scope clauses; historical review of IDIQ contract creep in prior DoD programs; political economy analysis of diffusion pressure',
    'If boundaries are firm: extraction limited to defense sector. If porous: constraint metastasizes to civilian policing and intelligence with further oversight degradation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(idiq_indefinite_quantity_boundaries, preference, 'IDIQ scope boundary enforceability and creep risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ibm_shield_contract_2026, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ibm__tr_t0, ibm_shield_contract_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ibm__tr_t4, ibm_shield_contract_2026, theater_ratio, 4, 0.48).
narrative_ontology:measurement(ibm__tr_t8, ibm_shield_contract_2026, theater_ratio, 8, 0.55).

% Extraction over time
narrative_ontology:measurement(ibm__be_t0, ibm_shield_contract_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ibm__be_t4, ibm_shield_contract_2026, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ibm__be_t8, ibm_shield_contract_2026, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ibm_shield_contract_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, us_military_ai_procurement_concentration).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, defense_technology_vendor_lock_in).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, civilian_oversight_technical_capacity).

% DUAL FORMULATION NOTE:
% SHIELD IDIQ is downstream of broader US military AI procurement strategy and upstream of specific operational vulnerabilities created by concentrated OODA loop automation. Related constraints include: (1) us_military_ai_procurement_concentration (ε≈0.45, structural tendency toward consolidation in defense tech), (2) defense_technology_vendor_lock_in (ε≈0.52, generic mechanism affecting all large defense contracts), (3) civilian_oversight_technical_capacity (ε≈0.64, inability to audit complex automated systems). SHIELD instantiates these structural constraints in a specific contractual form with ε=0.58.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ibm_shield_contract_2026, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
