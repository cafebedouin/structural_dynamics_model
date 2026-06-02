% ============================================================================
% CONSTRAINT STORY: congressional_appropriations_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_congressional_appropriations_power, []).

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
 *   constraint_id: congressional_appropriations_power
 *   human_readable: Congressional Appropriations Power
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Congressional appropriations power is the constitutional authority vested
 *   in Congress to authorize and control federal spending. This constraint
 *   generates a fundamental structural tension: appropriations authority is
 *   the primary check on executive power, but the mechanism itself
 *   concentrates power in congressional leadership and appropriations
 *   committee members while extracting flexibility from executive agencies
 *   and subordinating policy priorities that lack political constituencies.
 *   The constraint exhibits all six classification types depending on the
 *   observer's structural position. From the perspective of trapped agency
 *   administrators, it is a snare. From the perspective of appropriations
 *   leadership, it is pure coordination. From the perspective of underfunded
 *   policy priorities, it is extraction. The constraint has intensified over
 *   the 50-year interval: extractiveness has increased as appropriations
 *   processes have become more concentrated in leadership and committee power
 *   structures, and theater ratio has increased as the formal legislative
 *   process has become increasingly performative (continuing resolutions,
 *   omnibus bills, shutdown theater) relative to actual budget allocation
 *   decisions.
 *
 * KEY AGENTS:
 *   - Congressional Appropriations Committee Leadership: Primary beneficiary (institutional/arbitrage) — controls agenda, earmark allocation, and strategic leverage over agencies
 *   - Executive Agency Administrators: Primary victim (powerless/trapped) — must operate within annual appropriations constraints; no flexibility to respond to emergent priorities
 *   - Career Federal Employees: Secondary victim/partial beneficiary (moderate/constrained) — benefit from statutory protections but constrained by hiring freezes and pay compression
 *   - Underfunded Policy Priorities: Secondary victim (powerless/trapped) — deprioritized programs with no constituency to advocate for appropriations
 *   - Contractor and Vendor Network: Partial beneficiary/secondary victim (organized/constrained) — benefits from guaranteed procurement; extracted from via earmarking and lobbying dependency
 *   - Party Leadership and Floor Management: Secondary beneficiary (institutional/arbitrage) — controls scheduling and floor strategy; derives power from budget negotiation leverage
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent power structure as constitutional immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(congressional_appropriations_power, 0.58).
domain_priors:suppression_score(congressional_appropriations_power, 0.72).
domain_priors:theater_ratio(congressional_appropriations_power, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(congressional_appropriations_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(congressional_appropriations_power, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(congressional_appropriations_power, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(congressional_appropriations_power, tangled_rope).
narrative_ontology:human_readable(congressional_appropriations_power, "Congressional Appropriations Power").
narrative_ontology:topic_domain(congressional_appropriations_power, "political_economy/governance").

domain_priors:requires_active_enforcement(congressional_appropriations_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(congressional_appropriations_power, congressional_leadership).
narrative_ontology:constraint_beneficiary(congressional_appropriations_power, appropriations_committee_members).
narrative_ontology:constraint_beneficiary(congressional_appropriations_power, politically_connected_contractors).
narrative_ontology:constraint_victim(congressional_appropriations_power, executive_agencies).
narrative_ontology:constraint_victim(congressional_appropriations_power, competing_policy_priorities).
narrative_ontology:constraint_victim(congressional_appropriations_power, public_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXECUTIVE AGENCY ADMINISTRATOR (SNARE) — Trapped within annual appropriations cycles with no exit. Must request funds through congressional authorization; cannot redirect resources to emergent priorities without reauthorization. Faces organizational paralysis, hiring freezes, and unfunded mandates. Suppression is structural: legal requirement to spend appropriated funds as designated; no discretion to reallocate. Full experience of extraction — the constraint strips agency capacity while concentrating power upstream in Congress.
constraint_indexing:constraint_classification(congressional_appropriations_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER FEDERAL EMPLOYEE (TANGLED ROPE) — Constrained by civil service rules and pension/benefit lock-in. Also benefits from statutory salary protections and overtime rules that Congress has enacted. Experiences genuine coordination (appropriations enable operations) alongside extraction (appropriations cycles create job uncertainty, hiring freezes, pay compression relative to private sector). High suppression of alternative employment paths; moderate exit cost.
constraint_indexing:constraint_classification(congressional_appropriations_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: APPROPRIATIONS COMMITTEE LEADERSHIP (ROPE) — Experiences the constraint as pure coordination with significant benefits. Annual appropriations cycles provide stable governance mechanism; committee leadership captures agenda-setting power, earmark control, and strategic influence over agency priorities. Multiple exit options: can shift to other committees, leadership roles, private sector. Net beneficiary — extracts value but mechanism is genuinely coordinative (ensures budgetary process continues).
constraint_indexing:constraint_classification(congressional_appropriations_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNDERFUNDED POLICY PRIORITIES (SNARE) — Abstract collective of delayed or abandoned policy goals (infrastructure maintenance, climate adaptation, preventive public health) that have no constituency to advocate for appropriations. Trapped in zero-sum budget negotiations; perennially crowded out by crisis spending and politically favored programs. No exit mechanism; suppressed by structural inability of deprioritized programs to organize.
constraint_indexing:constraint_classification(congressional_appropriations_power, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTRACTOR AND VENDOR NETWORK (TANGLED ROPE) — Benefits from guaranteed federal procurement (coordination function: enables stable contracts and planning). Also extracted from via earmarks, cost-plus structures, and preferred contractor arrangements. Well-organized through industry associations and lobbying; has constrained exit (can attempt to diversify to private sector clients, but federal contracts provide stable revenue). Mixed experience: genuine coordination of procurement + asymmetric extraction of monopoly rents through earmarking and lobbying influence.
constraint_indexing:constraint_classification(congressional_appropriations_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AUTHORIZATION RITUAL (PITON) — From civilizational view, congressional appropriations power is a degraded constitutional safeguard. The power was designed to check executive overreach; now functions mostly performatively. Theater elements: reconciliation procedures, budget resolutions, continuing resolutions, and shutdown threats create performative crisis cycles while actual power concentration (in committee leadership, party leadership, lobbyists) persists. The ritual maintenance of 'Congress controls the purse' masks that effective control has shifted to automated continuing resolutions and emergency supplementals. Theater ratio reflects that the formal authorization process is increasingly theatrical relative to actual allocation decisions.
constraint_indexing:constraint_classification(congressional_appropriations_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CONSTITUTIONAL STRUCTURE VIEW (MOUNTAIN) — From constitutional/legal perspective, congressional control of appropriations is an immutable structural feature: Article I, Section 9 of Constitution requires all appropriations to pass Congress. No Money shall be drawn from the Treasury, but in Consequence of Appropriations made by Law. This is foundational law. However, structural data contradicts mountain classification: appropriations power is heavily delegated, circumvented through continuing resolutions, and concentrated in committee leadership and party whips. The analytical observer risks naturalizing a contingent power structure as constitutional immutability.
constraint_indexing:constraint_classification(congressional_appropriations_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(congressional_appropriations_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(congressional_appropriations_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(congressional_appropriations_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(congressional_appropriations_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(congressional_appropriations_power, TR),
    TR >= 0.70.

:- end_tests(congressional_appropriations_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Congressional appropriations power concentrates control in committee and party leadership while constraining agency flexibility and subordinating non-favored priorities. The extraction derives from the annual appropriations cycle (creates crisis pressure, agenda-setting power), earmark system (political allocation of resources), and suppression of agency reallocation (structural rigidity). The value reflects that genuine coordination function exists (appropriations process does authorize and plan federal spending) but is substantially occluded by extraction mechanisms. Suppression (0.72): High. Structural legal prohibition on executive agencies to reprogram funds without congressional consent. Political suppression through committee gatekeeping and party leverage over agency budgets. Appropriations committees can use threat of defunding to enforce compliance on unrelated policy issues. Suppression has increased over the interval as committee power has concentrated and as emergency supplementals have normalized the use of funding as coercive leverage. Theater ratio (0.48): Moderate-low. The formal appropriations process retains functional elements (budget authorization, program oversight, resource allocation). But performance gap has increased: continuing resolutions bypass formal process; omnibus bills obscure individual program allocation; shutdown theater creates crisis atmosphere without substantive budget debate. Theater is increasing because the performative crisis (shutdown threat) has become the dominant mechanism driving actual compromises, while formal legislative processes have become increasingly ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence across the presheaf. The same constraint (annual appropriations requirement) produces classifications ranging from Rope (beneficiaries) to Snare (victims) to Piton (institutional view) to Mountain (false summit from constitutional perspective). The gap reveals that the constraint's type depends entirely on structural position: power, exit options, directionality. The committee leadership genuinely sees coordination (Rope); the administrator genuinely sees extraction (Snare). Both are correct from their respective contexts. The analytical observer risks the false summit (Mountain) by naturalizing the constitutional text without examining the contingent power structure it enables. This gap is diagnostic: when a single constraint produces contradictory classifications from different perspectives, the DR framework is working correctly — it is capturing the perspectival reality that observers at different structural positions perceive genuinely different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives. Appropriations committee leadership derives d ≈ 0.05 (full beneficiary with arbitrage exit): they control agenda, capture earmark resources, set priorities. Agency administrators derive d ≈ 0.92 (near-total target): trapped by legal requirement to operate under appropriations constraints; no alternative funding sources; constrained by political pressure not to reallocate. Career employees derive d ≈ 0.55 (symmetric, slight target): benefit from statutory protections but constrained by hiring and pay caps; beneficiary status of job security partially offset by victim status of pay compression. Contractors derive d ≈ 0.35 (mixed but beneficiary-leaning): benefit from guaranteed procurement; extracted from via earmarking and lobbying costs. Underfunded priorities derive d ≈ 0.98 (maximal target): have no beneficiary status, chronically suppressed, no self-advocacy mechanism. The party leadership derives d ≈ 0.08 (beneficiary): derives leverage and control through budget negotiation. The sigmoid f(d) transforms these directionality values into effective extractiveness experience: high-d agents experience χ ≈ 1.15 × ε (amplified); low-d agents experience χ ≈ -0.12 × ε (negative, beneficiary experience).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: Congressional appropriations power resolves the mandatrophy not by identifying a single 'correct' type but by recognizing that the type depends on structural position and that the actual constraint is a hybrid (Tangled Rope). The core tension: genuine coordination function (authorization process) coexists with asymmetric extraction (power concentration, suppression of flexibility, earmarking). Both functions are real and structural. The beneficiaries (committee leadership, party leadership) genuinely benefit from the coordination mechanism — it provides stable agenda-setting power and leverage. The victims (agencies, underfunded priorities) genuinely experience extraction — they are trapped in cycles of constrained flexibility and zero-sum competition. The misclassification risk: naturalizing either the 'constitutional authority' (false Mountain) or the 'pure extraction' (false Snare) without recognizing the hybrid. The analytical resolution: the constraint IS tangled rope at the structural level (genuine coordination + asymmetric extraction) and appears as different types from different positions (Rope to beneficiaries, Snare to victims, Piton to institutional observers, Mountain to constitutional absolutists, Scaffold if reform movements succeed in creating sunset provisions). The presheaf perspective resolves mandatrophy: the constraint's true type is the covering presheaf, not any single fiber.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_versus_abrogation,
    'At what point does delegation of appropriations power (to continuing resolutions, omnibus spending bills, committee leadership) constitute effective abrogation of congressional authority?',
    'Structural analysis of decision-making locus: tracing actual allocation decisions from formal legislative process backward to identify where real power concentration occurs. Comparison of theoretical authority (full Congress) vs. de facto authority (party leadership + committee chairs).',
    'If delegation < 40%: congressional control remains substantive despite procedures. If delegation > 60%: appropriations power has effectively migrated to executive-level budget negotiations and committee gatekeeping. Classification pivots from Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_versus_abrogation, empirical, 'Extent of delegation versus effective abrogation of congressional appropriations authority').

omega_variable(
    earmark_extraction_mechanism,
    'Do earmarks function as genuine distributive fairness mechanism or as pure extraction of federal resources by politically connected interests?',
    'Comparative analysis of earmarked vs. non-earmarked project outcomes: cost overruns, completion rates, benefit-cost ratios. Tracing of earmark recipients to campaign contributions and lobbying expenditures.',
    'If earmarks have comparable outcomes to merit-based allocation: genuine coordination mechanism (Rope classification strengthens). If earmarks show systematic cost premium and lower completion rates: extraction mechanism (Snare classification for contractor network strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(earmark_extraction_mechanism, empirical, 'Whether earmarks function as distributive mechanism or pure extraction').

omega_variable(
    continuing_resolution_normalization,
    'Has the shift from regular appropriations process to continuing resolutions and omnibus bills normalized a de facto delegation of budget authority away from full Congress?',
    'Historical trend analysis of appropriations process: frequency of continuing resolutions vs. regular appropriations bills. Measurement of decision concentration in leadership and committee chairs under different legislative structures.',
    'If CR normalization correlates with increased power concentration in leadership/committees: piton classification confirmed — the formal process is increasingly theatrical. If CRs are anomalies with stable legislative power distribution: Mountain/Rope view more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuing_resolution_normalization, empirical, 'Normalization of continuing resolutions and power delegation effects').

omega_variable(
    suppression_of_interagency_reallocation,
    'How much of the suppression experienced by executive agencies derives from legal prohibition (appropriations law) versus political expectation (committee and party pressure)?',
    'Analysis of agencies that have attempted to reprogram, transfer, or reallocate funds: which faced legal barriers vs. political backlash? Tracking of agency attempts to innovate within appropriations constraints.',
    'If suppression is primarily legal: structural and immutable (Mountain characteristics strengthen). If suppression is primarily political (enforcement through funding threats, committee hostility): contingent and renegotiable (Tangled Rope characteristics strengthen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_interagency_reallocation, empirical, 'Whether suppression of agency flexibility is legal or political').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(congressional_appropriations_power, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(congr_approp_tr_t0, congressional_appropriations_power, theater_ratio, 0, 0.28).
narrative_ontology:measurement(congr_approp_tr_t25, congressional_appropriations_power, theater_ratio, 25, 0.38).
narrative_ontology:measurement(congr_approp_tr_t50, congressional_appropriations_power, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(congr_approp_be_t0, congressional_appropriations_power, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(congr_approp_be_t25, congressional_appropriations_power, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(congr_approp_be_t50, congressional_appropriations_power, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(congressional_appropriations_power, enforcement_mechanism).
narrative_ontology:affects_constraint(congressional_appropriations_power, executive_discretion_limitations).
narrative_ontology:affects_constraint(congressional_appropriations_power, agency_policy_implementation_capacity).
narrative_ontology:affects_constraint(congressional_appropriations_power, federal_workforce_flexibility).

% DUAL FORMULATION NOTE:
% Congressional appropriations power is structurally related to executive discretion limitations and federal workforce rigidity. Appropriations authority is the upstream constraint that creates downstream effects on agency capacity and worker flexibility. These constraints form a governance family where appropriations power is the binding mechanism that enforces rigidity in agency operations and workforce structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(congressional_appropriations_power, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
