% ============================================================================
% CONSTRAINT STORY: emergency_oversight_bureau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_oversight_bureau, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emergency_oversight_bureau
 *   human_readable: The Crisis Scaffold: Emergency Oversight Bureau
 *   domain: political/crisis_administration
 *
 * SUMMARY:
 *   Emergency oversight bureaus exemplify the scaffold constraint type:
 *   temporary administrative bodies created to coordinate crisis response
 *   with explicit sunset clauses designed to restore normal governance. The
 *   constraint operates across six distinct institutional positions with
 *   fundamentally different structural relationships. For coordinating
 *   agencies and the affected population, the bureau represents legitimate
 *   temporary coordination that solves acute collective action problems
 *   (distributing emergency resources, unified command structure, rapid
 *   decision-making) while suppression and theater are designed to decline as
 *   the crisis subsides. For marginalized populations, however, the same
 *   bureau consolidates extractive power: emergency procedures restrict
 *   movement, deny due process, and concentrate control over vital services
 *   without accountability mechanisms. The parent government structure
 *   benefits from crisis offloading while avoiding permanent authority
 *   restructuring. Bureau leadership captures coordination benefits alongside
 *   expanded emergency authority that may persist beyond functional
 *   necessity. The critical gate for scaffold classification is the sunset
 *   clause: without explicit legislative sunset with enforcement mechanisms,
 *   the structure degrades toward a snare (extractive) or tangled rope (mixed
 *   coordination-extraction) constraint. Theater ratio serves as the key
 *   drift metric: declining theater_ratio toward normal governance baseline
 *   indicates genuine recovery; persistent or rising theater_ratio indicates
 *   mission creep or extraction normalization.
 *
 * KEY AGENTS:
 *   - Affected Population: Primary beneficiary (moderate/constrained) — depends on emergency services; experiences constraint as temporary coordination with explicit endpoint
 *   - Coordinating Agencies: Organized beneficiary (organized/constrained) — benefit from centralized command structure; see bureau as legitimate temporary mechanism with sunset
 *   - Bureau Leadership: Institutional actor with dual role (powerful/mobile) — gains expanded authority and operational independence; benefits from coordination function but also from emergency powers extraction
 *   - Parent Government Structure: Institutional beneficiary (institutional/arbitrage) — offloads crisis management without permanent restructuring; maintains normal governance parallel track
 *   - Marginalized Subpopulations: Primary victim (powerless/trapped) — no exit from emergency systems; experience extraction disguised as humanitarian response; no guaranteed service restoration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as legitimate IF sunset is enforced, theater declines, and suppression truly designed to decrease
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_oversight_bureau, 0.28).
domain_priors:suppression_score(emergency_oversight_bureau, 0.45).
domain_priors:theater_ratio(emergency_oversight_bureau, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_oversight_bureau, extractiveness, 0.28).
narrative_ontology:constraint_metric(emergency_oversight_bureau, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(emergency_oversight_bureau, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_oversight_bureau, scaffold).
narrative_ontology:human_readable(emergency_oversight_bureau, "The Crisis Scaffold: Emergency Oversight Bureau").
narrative_ontology:topic_domain(emergency_oversight_bureau, "political/crisis_administration").

domain_priors:requires_active_enforcement(emergency_oversight_bureau).
narrative_ontology:has_sunset_clause(emergency_oversight_bureau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, affected_population).
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, coordinating_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED POPULATION (SCAFFOLD) — Constrained by dependency on emergency services during crisis phase. Experiences bureau as temporary coordination mechanism with explicit sunset. Theater ratio declines as crisis subsides and normal governance returns. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.21. Suppression high during acute phase but declining toward zero as recovery progresses.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: COORDINATING AGENCIES (SCAFFOLD) — Organized actors (FEMA, state emergency management, local authorities) see bureau as legitimate temporary coordination mechanism. Suppression arises from resource concentration and command hierarchy during crisis but designed to decrease as crisis resolves. Theater ratio starts high (emergency protocols are performative) but declines. d≈0.45, f(d)≈0.55, σ=1.0 → χ≈0.15. Sunset clause ensures agencies regain normal jurisdictional autonomy.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUREAU LEADERSHIP (TANGLED ROPE) — Powerful institutional actors (director, deputy coordinators) experience coordination benefits (unified command, resource pooling) but also extraction benefits (expanded authority, emergency powers, operational independence from normal oversight). High theater ratio (0.60) reflects that emergency protocols mix genuine coordination with performative emergency procedure. d≈0.20, f(d)≈0.10, σ=1.0 → χ≈0.03. Net beneficiary but extraction component visible.
constraint_indexing:constraint_classification(emergency_oversight_bureau, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PARENT GOVERNMENT (ROPE) — Benefits from crisis offloading (delegated authority reduces political friction). Experiences bureau as pure coordination mechanism that centralizes decision-making to accelerate response. Sunset clause protects normal governance structure from permanent authority migration. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.03. Net beneficiary through arbitrage (creates crisis response without restructuring permanent institutions).
constraint_indexing:constraint_classification(emergency_oversight_bureau, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MARGINALIZED SUBPOPULATIONS (SNARE) — Trapped populations (homeless, undocumented, institutionalized) have no exit from emergency systems. Bureau consolidates control over vital resources (shelter, food, medical access) without accountability to this population. Theater ratio high (humanitarian framing of emergency procedures). No sunset clause guarantees return of services to normal community structures. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.39. Hidden extraction: emergency authority used to restrict movement, deny due process, or enforce compliance.
constraint_indexing:constraint_classification(emergency_oversight_bureau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — Sees emergency bureaus as legitimate temporary administrative structures IF three conditions hold: (1) explicit sunset clause in founding legislation, (2) theater_ratio declining over interval (emergency performance yields to functional governance), (3) suppression explicitly designed to decrease toward normal baseline. This constraint satisfies all three. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.32. Classification depends entirely on sunset gate compliance.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_oversight_bureau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(emergency_oversight_bureau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. Emergency bureaus have legitimate coordination functions (unified resource distribution, rapid decision-making, crisis communication) that justify some temporary authority concentration. The base extractiveness reflects that coordination benefits outweigh extraction in most scenarios, but latent extraction potential exists (emergency powers can be used coercively). Theater ratio (0.60): Moderate-high. Emergency protocols are substantially performative during acute crisis (sirens, briefings, emergency declarations create confidence) but decline as crisis subsides and routine governance replaces emergency procedure. Initial theater_ratio (0.75) declines toward 0.50 as recovery proceeds. Suppression (0.45): Moderate. Emergency authority necessarily restricts normal freedoms (movement restrictions, quarantine, curfews, commandeered resources) but suppression is designed to be temporary and crisis-scoped. The scaffold gate requires suppression to decline measurably over the recovery interval. Sunset clause (true): Mandatory gate requirement. Without explicit legislative sunset, the constraint degrades to snare (permanent extraction under emergency authority). Theater ratio declining over interval (true): Indicates genuine recovery rather than authority normalization. If theater persists above 0.65 past crisis resolution, mission creep is occurring.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival gap across power levels. Coordinating agencies and the general affected population see legitimate temporary coordination (Scaffold) with explicit sunset. Bureau leadership sees coordination benefits mixed with expanded authority (Tangled Rope). Parent government sees pure arbitrage benefit without permanent restructuring (Rope). Marginalized populations see extraction without access to coordination benefits (Snare) — emergency authority concentrates control over vital services without accountability. The analytical observer sees a true scaffold IF and ONLY IF sunset enforcement is credible, theater declines, and suppression truly decreases toward baseline. This gap is not perspectival disagreement but structural reality: the same bureau structure delivers different constraint types to different populations depending on their exit options and structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Affected population: Benefits from coordination + constrained exit → d≈0.55, f(d)≈0.75. Experiences constraint as legitimate temporary support with sunset. Coordinating agencies: Beneficiary + constrained → d≈0.45, f(d)≈0.55. Organized actors see coordination function clearly and accept temporary authority concentration. Bureau leadership: Beneficiary + mobile → d≈0.20, f(d)≈0.10. High power and mobile exit create low d; net beneficiary through arbitrage of normal authority. Marginalized populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction potential; no exit from emergency systems; dependent on bureau-controlled resources. Analytical observer: neutral → d≈0.72, f(d)≈1.15. Classification depends on constraint properties (sunset enforcement, theater decline, suppression reversibility), not observer position. Overrides: none necessary. Structural derivation captures directionality accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the sunset clause gate: the constraint CANNOT be classified as scaffold without explicit statutory sunset with enforcement mechanisms. This prevents the false positive of misclassifying an indefinite extractive emergency authority as temporary coordination. The theater_ratio metric serves as the drift detection gate: if theater_ratio persists above 0.65 beyond the crisis recovery window, mission creep is indicated and the constraint transitions toward snare or tangled rope. The suppression metric must be designed to decline toward normal governance baseline; if suppression remains elevated during recovery phase, extraction normalization has occurred and the scaffold classification fails. The marginalized subpopulation snare perspective is NOT a mandatrophy failure — it reflects genuine structural reality that the same bureau delivers different constraint types to different populations. This is perspectival legitimacy, not constraint ambiguity. The true mandatrophy resolution is: (1) sunset clause is statutorily binding, (2) theater_ratio measurably declines, (3) suppression designed to decrease, and (4) coordination benefits distribute to all affected populations. If any gate fails, the constraint is not a true scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_enforcement_mechanism,
    'What institutional mechanisms actually enforce sunset clause execution when political actors benefit from extended emergency authority?',
    'Historical analysis of emergency bureau dissolutions; comparison of actual sunset dates with statutory dates; identification of extension mechanisms and their frequency',
    'If enforcement weak: sunset is performative and constraint degrades to Snare or Tangled Rope. If enforcement strong: scaffold classification holds and theater_ratio decline is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_enforcement_mechanism, empirical, 'Whether statutory sunset clauses are actually enforced against political pressure to extend').

omega_variable(
    suppression_baseline_recovery,
    'How much suppression (of normal rights, due process, movement) is inherent to crisis response versus what fraction can decline during recovery phase?',
    'Measurement of emergency powers used during acute vs recovery phases; timeline analysis of rights restoration; comparative case studies of bureaus with similar crisis types',
    'If > 80% of suppression reversible: scaffold classification holds. If < 50% reversible: suppression becomes structural and constraint shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_baseline_recovery, empirical, 'Fraction of emergency suppression that can be reversed as recovery proceeds').

omega_variable(
    coordination_benefit_distribution,
    'Do the coordination benefits of centralized emergency authority distribute equitably to all affected populations, or do marginalized groups experience pure extraction?',
    'Analysis of resource allocation by demographic group; measurement of service quality disparities; tracking of coercive actions (detention, forced relocation, denial of due process) by population subgroup',
    'If equitable: beneficiaries list includes all populations and constraint is true Scaffold for most perspectives. If inequitable: extraction concentrates on powerless and constraint is Snare from those perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_distribution, empirical, 'Whether emergency coordination benefits distribute equitably across populations').

omega_variable(
    scope_boundary_mission_creep,
    'Do emergency bureau authorities remain scoped to the identified crisis or does mission creep expand scope beyond original justification?',
    'Comparison of founding charter scope vs actual operations; tracking of authority expansion through executive directive vs legislative amendment; analysis of resource allocation to crisis-unrelated functions',
    'If scope strictly maintained: scaffold classification stable. If mission creep occurs: extractiveness increases and theater_ratio remains high (emergency framing persists despite expanded mission).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_boundary_mission_creep, empirical, 'Whether emergency bureau authorities expand beyond original crisis scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_oversight_bureau, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eob_tr_t0, emergency_oversight_bureau, theater_ratio, 0, 0.75).
narrative_ontology:measurement(eob_tr_t3, emergency_oversight_bureau, theater_ratio, 3, 0.65).
narrative_ontology:measurement(eob_tr_t6, emergency_oversight_bureau, theater_ratio, 6, 0.6).

% Extraction over time
narrative_ontology:measurement(eob_be_t0, emergency_oversight_bureau, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eob_be_t3, emergency_oversight_bureau, base_extractiveness, 3, 0.23).
narrative_ontology:measurement(eob_be_t6, emergency_oversight_bureau, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_oversight_bureau, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_oversight_bureau, emergency_authority_expansion).
narrative_ontology:affects_constraint(emergency_oversight_bureau, temporary_rights_suspension).

% DUAL FORMULATION NOTE:
% Emergency oversight bureaus may decompose into distinct constraints: (1) the coordination function (resource distribution, unified command) maps to pure rope or scaffold; (2) the extraction potential (emergency authority, suppression mechanisms, coercive control) maps to snare or tangled rope. This story unifies both dimensions. If decomposing, the coordination story would have ε≈0.15 (rope/scaffold), while the extraction story would have ε≈0.45+ (tangled rope/snare). The present story at ε=0.28 reflects mixed coordination-extraction equilibrium.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
