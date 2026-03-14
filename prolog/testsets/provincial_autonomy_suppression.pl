% ============================================================================
% CONSTRAINT STORY: provincial_autonomy_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_autonomy_suppression, []).

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
 *   constraint_id: provincial_autonomy_suppression
 *   human_readable: Provincial Autonomy Suppression in Centralized Federal Systems
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Provincial autonomy suppression in centralized federal systems presents a
 *   structural tension between the theoretical delegation of power to
 *   regional governments and the practical extraction of authority, revenue,
 *   and policy control by central state apparatus. The constraint operates
 *   through multiple interlocking mechanisms: fiscal dependency (centralized
 *   revenue collection and transfer), regulatory override (emergency powers,
 *   concurrent jurisdictions, federal paramountcy clauses), and identity
 *   capture (nation-building projects that subsume regional identity into
 *   national frame). The extractiveness has increased over the measurement
 *   interval as central governments have expanded emergency authorities and
 *   conditional transfer programs, tightening control under the appearance of
 *   federal coordination. Theater ratio (0.65) reflects persistent formal
 *   structures of provincial representation and
 *   consultation—intergovernmental conferences, revenue-sharing formulas,
 *   constitutional provisions—that maintain appearance of autonomy while
 *   decisions are increasingly predetermined by central authority. The
 *   constraint exhibits all six DR types from different perspectives: pure
 *   extraction to trapped populations (snare), mixed coordination and
 *   extraction to provinces (tangled_rope), genuine coordination to central
 *   government (rope), temporary institutional problem to organized
 *   devolution movements (scaffold), degraded ritual structures to the
 *   federal apparatus itself (piton), and apparent natural law to
 *   civilizational analysts who risk naturalizing contingent institutional
 *   arrangements.
 *
 * KEY AGENTS:
 *   - Central Government: Primary beneficiary (institutional/arbitrage) — captures revenue, sets policy direction, maintains national identity frame; experiences constraint as necessary coordination
 *   - Provincial Governments: Primary victims (powerless/trapped) — formal jurisdiction without material capacity; constrained by fiscal dependency and regulatory override; bear extraction costs
 *   - Regional Populations: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with national citizenship; suppression manifests as policy misalignment and identity erasure
 *   - Provincial Elite: Tertiary actors (moderate/constrained) — partial cooption into central rent-seeking; some benefit from asymmetric control within provinces; constrained exit within national career system
 *   - Devolution Movements: Organized resistance (organized/constrained) — seek constitutional reform and genuine federalism; perceive sunset pathway through institutional change
 *   - Federal Apparatus: Institutional performer (institutional/arbitrage) — maintains ritual structures; sees own process as degraded but continues through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent central control as inherent to scaled governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_autonomy_suppression, 0.58).
domain_priors:suppression_score(provincial_autonomy_suppression, 0.72).
domain_priors:theater_ratio(provincial_autonomy_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_autonomy_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(provincial_autonomy_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(provincial_autonomy_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_autonomy_suppression, tangled_rope).
narrative_ontology:human_readable(provincial_autonomy_suppression, "Provincial Autonomy Suppression in Centralized Federal Systems").
narrative_ontology:topic_domain(provincial_autonomy_suppression, "political/institutional").

domain_priors:requires_active_enforcement(provincial_autonomy_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_autonomy_suppression, central_government).
narrative_ontology:constraint_beneficiary(provincial_autonomy_suppression, national_bureaucracy).
narrative_ontology:constraint_victim(provincial_autonomy_suppression, provincial_governments).
narrative_ontology:constraint_victim(provincial_autonomy_suppression, regional_populations).
narrative_ontology:constraint_victim(provincial_autonomy_suppression, local_policy_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL GOVERNMENT (SNARE) — Formal jurisdiction exists on paper but critical revenue sources, regulatory authority, and policy implementation capacity are centrally controlled. Exit via secession faces insurmountable military, economic, and legal barriers. Bears full extraction cost through revenue extraction and mandate imposition with minimal coordination benefit.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL POPULATION (SNARE) — Structurally mobile (can migrate, organize) but identity-locked within national citizenship frame and cultural integration with dominant ethnic/linguistic group. Cannot exercise mobility without abandoning identity. Bears disproportionate extraction through policy misalignment and resource scarcity while enduring suppression of local identity expression and decision-making.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL ELITE (TANGLED ROPE) — Partial coordination function: provinces administer distributed services (education, healthcare, local infrastructure). Significant extraction through revenue capture and policy override. Genuine asymmetry — some provincial elites coopt central rents while majorities bear costs. Constrained exit: career mobility exists within national system but typically requires acceptance of central priorities.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL GOVERNMENT (ROPE) — Experiences the constraint as pure coordination: federal unity, standardized service delivery, unified fiscal policy, and national defense require centralized control and provincial compliance. Net beneficiary with exit option (can dissolve provinces entirely, historically). Extraction runs toward this agent via tax centralization and policy compliance.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVOLUTION MOVEMENT (SCAFFOLD) — Organized agents (provincial coalitions, independence movements, federalist reformers) see autonomy suppression as a temporary institutional failure with a sunset: constitutional reforms, regional representation, and fiscal federalism redesigns represent pathways to genuine autonomy. Low effective extraction because organized agents have agency and see exit/transformation mechanisms, even if constrained by central state power.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL-PROVINCIAL APPARATUS (PITON) — Formal structures (intergovernmental conferences, revenue-sharing formulas, consultation protocols) persist through institutional inertia despite performing minimal actual power distribution. Meetings occur ritually; central authority overrides decisions consistently; theater ratio high because the apparatus maintains appearance of negotiation while outcomes are predetermined. Degraded because its ostensible function (genuine provincial input to national policy) has atrophied.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of central coordination is inherent to multi-region polities: defense, infrastructure networks, and fiscal stability require unified authority. Autonomy suppression appears as an immutable feature of scaled governance. However, empirical variation across federal systems (genuine power distribution in Switzerland, Australia, Canada vs centralized suppression in Russia, China, India) reveals the natural law framing as false naturalization. Engine will flag as false summit.
constraint_indexing:constraint_classification(provincial_autonomy_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_autonomy_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(provincial_autonomy_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(provincial_autonomy_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_autonomy_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(provincial_autonomy_suppression, TR),
    TR >= 0.70.

:- end_tests(provincial_autonomy_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The central government captures substantial revenue through progressive centralization of tax bases and conditional transfer schemes; increasingly overrides provincial policy through emergency clauses and concurrent jurisdiction expansion. Not maximal extraction because provinces retain some genuine administrative functions (education, healthcare delivery, local services) that provide coordination benefit. Suppression (0.72): High. Multiple barriers prevent provincial exit or meaningful autonomy: legal (constitutional paramountcy, emergency powers), economic (fiscal dependency on central transfers), military (unequal force distribution favors central state), and identity-based (nation-building frames that make autonomy seem like betrayal of national community). Theater ratio (0.65): Moderate-high and increasing. Federal-provincial negotiation apparatus maintains structures of consultation and shared governance while outcomes increasingly reflect central preferences. Revenue-sharing formulas update nominally but do not change fundamental dependency; provincial representation in national bodies is token; constitutional amendment for genuine federalism faces insurmountable central opposition. The theater has increased as formal autonomy has decreased—the apparatus has become more elaborate (more conferences, more committees) even as real power has centralized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The central government sees pure coordination (Rope) — federal unity, unified policy, national defense — as the genuine function. Provinces see extraction with minimal coordination benefit (Snare) — revenue capture, policy override, inability to serve regional populations. Organized movements see a temporary institutional problem with reform pathways (Scaffold) — devolution, constitutional federalism, regional representation are structural possibilities. The federal apparatus sees its own degradation (Piton) — ritual consultation replacing real negotiation; theater-to-function ratio increasing over time. The civilizational analyst risks seeing natural law (Mountain) — multi-region polities require central coordination — but empirical variation across federal systems (genuine Swiss cantonal power, genuine Australian state authority, genuine Canadian provincial capacity vs minimal Russian, Chinese, Indian provincial autonomy) reveals the natural law as false. The perspectival gap between central government's rope and provincial government's snare is the primary diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position, beneficiary/victim status, and exit options. Central government: d ≈ 0.15 (beneficiary + arbitrage exit → low d → negative χ; extraction flows toward this agent). Provincial governments: d ≈ 0.92 (victim + trapped exit → high d → χ ≈ 0.87 estimated; near-maximal extraction). Regional populations: d ≈ 0.88 (victim + identity_locked exit → high d because identity-locking prevents exercise of structural mobility; χ ≈ 0.84 estimated). Provincial elite: d ≈ 0.65 (mixed victim/beneficiary status, constrained exit → moderate d → χ ≈ 0.48 estimated). Devolution movements: d ≈ 0.48 (victim-aligned but organized exit → lower d than trapped victims; χ ≈ 0.35-0.40 estimated due to agency and perceived sunset). Federal apparatus: d ≈ 0.20 (beneficiary via continued institutional role + arbitrage → low d; piton classification derives from theater_ratio gate, not high χ).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through perspectival indexing. The question 'Is this coordination or extraction?' has different answers depending on structural position. Central government genuinely experiences coordination requirements (cannot fund national defense without revenue extraction; cannot maintain fiscal stability without policy override). Provinces genuinely experience extraction without reciprocal benefit (formal autonomy that is overridden; revenue bases that are captured; policy mandates that they cannot refuse). The constraint is BOTH coordination and extraction simultaneously — the mandatrophy is the structural fact that one agent's coordination requirement is another agent's extraction, and the asymmetry between them prevents the constraint from being classified as pure coordination (Rope). Hence Tangled Rope is correct: genuine coordination function exists (federal unity, scaled governance) alongside asymmetric extraction (revenue capture, policy override) and active enforcement (emergency powers, constitutional override). The false mountain perspective (natural law) is flagged by the engine because empirical variation proves that autonomy suppression is contingent institutional design, not physical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_dependency_threshold,
    'At what ratio of provincial revenue-raising capacity to central transfers does formal autonomy become structural trap rather than contingent constraint?',
    'Comparative fiscal federalism analysis; mapping of revenue bases and transfer mechanisms across systems; threshold identification where provincial budgets become structurally dependent',
    'If threshold < 40% local revenue: most provinces in systemic entrapment (snare reclassification). If threshold > 60%: autonomy suppression is enforceable override rather than structural inevitability (remains tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_dependency_threshold, empirical, 'Fiscal dependency threshold for structural vs contingent autonomy suppression').

omega_variable(
    identity_lock_mechanism,
    'Is regional population identity-locking a structural feature of the autonomy suppression or a contingent result of specific nation-building policies?',
    'Historical analysis of regional identity strength prior to centralization; comparison of identity persistence across populations with different autonomy histories; data on migration patterns and identity fluidity when exit barriers are removed',
    'If structural: identity_locked exit classification is primary mechanism; suppression persists even if formal barriers are removed. If contingent: identity-locking is a secondary enforcement layer; removing fiscal/legal barriers would substantially weaken constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether regional identity-locking is structural or contingent').

omega_variable(
    coordination_necessity_verification,
    'Which specific national-scale coordination functions genuinely require suppression of provincial autonomy, and which could function with genuine federal delegation?',
    'Cross-system comparison of coordination outcomes under high vs low autonomy suppression; identification of failed coordination attempts in federal systems with genuine provincial power; analysis of whether coordination failures correlate with provincial power or other variables',
    'If broad necessity: mountain perspective gains credibility — some autonomy suppression is inherent to scaled governance. If narrow necessity: only specific functions (defense, monetary policy) require suppression; most provincial policy could be genuinely autonomous, reclassifying constraint as snare rather than mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_verification, empirical, 'Which coordination functions necessitate autonomy suppression').

omega_variable(
    devolution_sunset_feasibility,
    'Are the constitutional and institutional pathways to genuine provincial autonomy (devolution, federalism reform) actually available, or are they theater masking permanent central control?',
    'Historical analysis of devolution attempts; mapping of constitutional amendment pathways and their actual success rates; documentation of central state blocking mechanisms',
    'If pathways are real: scaffold classification is appropriate; sunset is structural possibility. If theater: devolution movements are safety valves without exit capacity; constraint remains snare regardless of reform discourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(devolution_sunset_feasibility, empirical, 'Whether devolution pathways provide genuine autonomy sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_autonomy_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_auto_tr_t0, provincial_autonomy_suppression, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prov_auto_tr_t10, provincial_autonomy_suppression, theater_ratio, 10, 0.58).
narrative_ontology:measurement(prov_auto_tr_t20, provincial_autonomy_suppression, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(prov_auto_be_t0, provincial_autonomy_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prov_auto_be_t10, provincial_autonomy_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(prov_auto_be_t20, provincial_autonomy_suppression, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_autonomy_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_autonomy_suppression, fiscal_federalism_dependency).
narrative_ontology:affects_constraint(provincial_autonomy_suppression, national_identity_suppression).
narrative_ontology:affects_constraint(provincial_autonomy_suppression, emergency_power_scope_creep).

% DUAL FORMULATION NOTE:
% Provincial autonomy suppression decomposes into distinct structural constraints: fiscal dependency (ε≈0.65), identity-locking (ε≈0.72), and regulatory override (ε≈0.48). This story aggregates all three into a single tangled_rope. Separate stories could analyze each mechanism independently. Fiscal dependency alone would classify as snare; identity-locking in isolation is a snare with identity_locked exit; regulatory override is tangled_rope. The unified story captures the entanglement of all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_autonomy_suppression, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
