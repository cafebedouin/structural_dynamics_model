% ============================================================================
% CONSTRAINT STORY: sludge_bureaucratic_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sludge_bureaucratic_friction, []).

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
 *   constraint_id: sludge_bureaucratic_friction
 *   human_readable: Sludge (Intentional Administrative Friction)
 *   domain: political/economic
 *
 * SUMMARY:
 *   Sludge—intentional or consequential administrative friction—operates as a
 *   structural constraint on access to benefits, rights, and services. Unlike
 *   direct exclusion or price barriers, sludge works through procedural
 *   burden: excessive paperwork, opaque requirements, long wait times,
 *   fragmented systems, and deliberate complexity. The constraint exhibits
 *   multiple perspectives because it serves genuine coordination functions
 *   (eligibility verification, fraud prevention, resource allocation) while
 *   simultaneously extracting through discouragement. Benefit claimants
 *   experience it as pure suppression; fiscal authorities experience it as
 *   coordination; eliminating it requires technological and organizational
 *   redesign. The extractiveness has increased over the 20-year interval as
 *   systems have accumulated legacy complexity without corresponding
 *   simplification, and as intentional policy tightening has added friction
 *   layers atop legitimate procedural requirements.
 *
 * KEY AGENTS:
 *   - Benefit Claimants: Primary victims (powerless/trapped) — require benefits; cannot exit; bear full discouragement cost
 *   - Service Bureaucrats: Secondary actors (moderate/constrained) — execute sludge mechanisms; experience mixed coordination and extraction pressure
 *   - Fiscal Authority: Primary beneficiary (institutional/arbitrage) — controls resource allocation; experiences sludge as legitimate coordination tool
 *   - Sludge Elimination Coalition: Organized advocates (organized/constrained) — building technological and procedural alternatives; represent sunset pathway
 *   - Legacy Administrative System: Institutional actor (institutional/arbitrage) — paper-based, in-person infrastructure; maintains through path dependency
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing friction as inherent to all public administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sludge_bureaucratic_friction, 0.58).
domain_priors:suppression_score(sludge_bureaucratic_friction, 0.72).
domain_priors:theater_ratio(sludge_bureaucratic_friction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, 0.58).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sludge_bureaucratic_friction, tangled_rope).
narrative_ontology:human_readable(sludge_bureaucratic_friction, "Sludge (Intentional Administrative Friction)").
narrative_ontology:topic_domain(sludge_bureaucratic_friction, "political/economic").

domain_priors:requires_active_enforcement(sludge_bureaucratic_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, state_fiscal_apparatus).
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, administrative_gatekeepers).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, benefit_claimants).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, rights_seekers).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, service_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BENEFIT CLAIMANT (SNARE) — Low-income individuals navigating welfare systems face insurmountable procedural burdens: form complexity, document requirements, appointment scheduling, language barriers, transportation. Exit is not available—they need the benefits to survive. The sludge functions as pure extraction: gatekeepers accumulate discouragement rent through administrative burden. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SERVICE BUREAUCRAT (TANGLED ROPE) — Lower-level administrators experience sludge as both coordination (legitimate eligibility verification requires documentation) and extraction (upper management uses processing delays as a lever to reduce benefit rolls without legislative action). They are trapped between genuine coordination requirements and institutional pressure to discourage access. Moderate power with constrained exit—they cannot abandon the role without career loss.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FISCAL AUTHORITY (ROPE) — Central budget authority experiences sludge as pure coordination: documentation requirements genuinely enable verification of eligibility, prevent fraud, and distribute limited resources fairly. From this institutional perspective, the friction is the legitimate cost of information-gathering. The authority has arbitrage options: it can adjust system design, technology, staffing. Net beneficiary—sludge enables their control function.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SLUDGE ELIMINATION COALITION (SCAFFOLD) — Civil rights organizations, benefits advocacy groups, and some state agencies are building alternative pathways: automated eligibility verification, simplified forms, mobile applications, presumptive eligibility. These constitute a sunset mechanism—as digital systems and process redesign mature, the friction mechanism loses its suppressive force. Organized agents with constrained exit but visible decrement over 10-15 year horizon.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY PAPER-BASED SYSTEM (PITON) — The physical infrastructure of bureaucracy (in-person offices, paper forms, scheduling systems) persists through institutional inertia despite technology that could eliminate most friction. The theater_ratio is high (0.65) because much ritual persists: form-filling, waiting, documentation verification. The system maintains itself through path dependency and staff training, not because it functions well. A degraded Snare that used to extract but now mostly performs.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / VERIFICATION PROBLEM (MOUNTAIN) — From a civilizational/universal perspective, all resource-allocation systems face an inherent information problem: distributing limited benefits requires verifying need and eligibility. Some friction is structurally unavoidable—you cannot allocate public goods without knowing who qualifies. However, the engine will detect this as a false summit: modern technology and administrative design have reduced verification friction to < 0.15 in some jurisdictions (Estonia's digital government, simplified tax-return systems). The naturalization of bureaucratic friction as immutable misses the contingency.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sludge_bureaucratic_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sludge_bureaucratic_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sludge_bureaucratic_friction, TR),
    TR >= 0.70.

:- end_tests(sludge_bureaucratic_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Sludge extraction flows from claimants to the fiscal apparatus through discouragement. However, the extraction is not total (0.70+) because legitimate coordination functions exist: eligibility verification genuinely requires information; fraud prevention requires barriers; resource constraints require rationing. The measured 0.58 reflects that roughly half the observed friction plausibly serves coordination, while the remainder is extractive overage (intentional discouragement or unmotivated legacy complexity). Suppression (0.72): High. Multiple barriers compound: documentation requirements, appointment scheduling, language/literacy barriers, transportation costs, time-off-work costs, form complexity, knowledge asymmetry. Claimants have no formal appeal for 'friction is too high'—the system defines its own legitimacy. Theater ratio (0.65): Moderate-high. Much ritual persists: waiting rooms, filing rituals, form submission, documentation proof. However, a residual coordination core remains genuine—some waiting and documentation reflects actual verification work, not pure theater. Digital systems in advanced jurisdictions reduce theater to 0.20-0.30 while maintaining verification function, demonstrating that current theater includes performative components.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates the fiscal authority's (Rope) experience from the claimant's (Snare). The authority sees procedural burden as legitimate coordination cost—eligibility verification genuinely requires information-gathering. The claimant sees pure extraction—they must comply or lose needed benefits. Both are structurally correct: sludge serves both functions simultaneously. The organized coalition perspective (Scaffold) introduces the critical insight that this is not inevitable: digital systems and simplified processes can reduce friction from 0.72 to 0.25 while maintaining verification integrity. This possibility reveals the current sludge as a Piton (degraded legacy system) or a Snare (intentional extraction), not as a Mountain (immutable coordination requirement). The analytical observer's mountain perspective is false—it naturalizes what is demonstrably contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from structural position. Benefit claimants with trapped exit and victim status experience maximum d ≈ 0.95, producing high f(d) ≈ 1.42 and perceived χ after scope adjustment. Fiscal authorities with institutional power and arbitrage exit experience low d ≈ 0.05, producing negative f(d) ≈ -0.12 and net-beneficiary χ. Service bureaucrats with moderate power and constrained exit occupy middle ground: d ≈ 0.60, f(d) ≈ 0.85, experiencing tangled_rope proportions where both coordination and extraction are real. The organized coalition has organized power with constrained exit but visible pathway out—their d ≈ 0.40 reflects that they can influence system design even if they cannot opt out individually.
 *
 * MANDATROPHY ANALYSIS:
 *   Sludge resolves the mandatrophy by documenting that all perspectives are correct within their observational frame. The mandatrophy would accuse the framework of contradiction: 'Is sludge coordination (Rope) or extraction (Snare)?' The answer is: both, from different structural positions. The fiscal authority genuinely uses sludge for coordination—they need verification tools. The claimant genuinely experiences extraction—they bear the friction cost and can be deterred. The tangled_rope classification for service bureaucrats captures the hybrid: they enforce coordination requirements while being pressured to use friction as a discouragement mechanism. The scaffold classification reveals that the mandatrophy itself is based on false necessity: the apparent trade-off between 'coordination friction' and 'pure extraction' is artifact of legacy technology. Digital systems and redesigned processes (Estonia's approach) achieve both lower friction and higher verification integrity, demonstrating that the trade-off was contingent, not structural. The piton classification documents that much of the observed sludge is now theater—maintenance of forms, rituals, and office visits that persist without functional justification. This resolution does not eliminate the constraint; it recategorizes it as a legacy extraction mechanism (Piton) with a visible exit pathway (Scaffold), not as a coordination necessity (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'What constitutes ''intentional'' administrative friction versus legitimate procedural complexity?',
    'Comparative analysis: jurisdictions with low-friction systems (Estonia, Taiwan) vs high-friction systems (US welfare, UK disability assessment); measurement of discretionary complexity removal where politically feasible',
    'If intentionality is documentable (memo traffic, policy statements): snare classification strengthens. If friction emerges from budget constraints only: tangled_rope classification solidifies (legitimate coordination + unintended extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_threshold, empirical, 'Distinction between intentional and inadvertent administrative friction').

omega_variable(
    discouragement_extraction_rate,
    'What percentage of eligible claimants are deterred by sludge rather than self-selecting out of genuine ineligibility?',
    'Randomized administrative burden reduction (Oregon SNAP simplification pilot, UK pension form shortening); measurement of uptake elasticity with respect to friction reduction',
    'If > 30% of non-claimants are sludge-deterred: snare extraction is quantifiable. If < 10%: friction may be incidental rather than extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discouragement_extraction_rate, empirical, 'Empirical discouragement rate from administrative friction').

omega_variable(
    legitimate_verification_floor,
    'What is the minimum friction required for effective fraud prevention and eligibility verification?',
    'Comparative analysis of fraud rates across low- and high-friction systems; technical audit of simplified verification methods; pilot programs with friction floors at different levels',
    'If minimum floor is 0.15: most observed sludge (0.72 suppression) is extractive overage. If minimum floor is 0.45: sludge is mostly legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_verification_floor, empirical, 'Minimum friction required for legitimate eligibility verification').

omega_variable(
    digital_transition_viability,
    'Can automated/digital eligibility verification achieve feature parity with paper-based systems at substantially lower friction?',
    'Technical feasibility studies; international comparison of digital systems (Estonia, Singapore, Korea); cost-benefit analysis of automation investments',
    'If yes: scaffold classification is confirmed—digital pathway represents real sunset mechanism. If no: sludge is structurally necessary, and mitigation rather than elimination is realistic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_transition_viability, empirical, 'Whether digital systems can replace paper-based friction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sludge_bureaucratic_friction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sludge_tr_t0, sludge_bureaucratic_friction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sludge_tr_t10, sludge_bureaucratic_friction, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sludge_tr_t20, sludge_bureaucratic_friction, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(sludge_be_t0, sludge_bureaucratic_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sludge_be_t10, sludge_bureaucratic_friction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sludge_be_t20, sludge_bureaucratic_friction, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sludge_bureaucratic_friction, resource_allocation).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, welfare_eligibility_verification).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, means_testing_extraction).
narrative_ontology:affects_constraint(sludge_bureaucratic_friction, bureaucratic_capture).

% DUAL FORMULATION NOTE:
% Sludge is downstream of eligibility verification requirements and upstream of actual benefit delivery. The constraint's extractiveness (0.58) reflects a hybrid of legitimate verification friction and intentional or path-dependent discouragement. Upstream constraint 'welfare_eligibility_verification' has lower extractiveness (ε ≈ 0.20) reflecting pure coordination; downstream constraint 'means_testing_extraction' has higher extractiveness reflecting concentrated extraction pressure on the poorest claimants. Sludge mediates between these two, converting verification requirements into discouragement rents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sludge_bureaucratic_friction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
