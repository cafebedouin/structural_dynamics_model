% ============================================================================
% CONSTRAINT STORY: global_protocol_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_protocol_entrenchment, []).

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
 *   constraint_id: global_protocol_entrenchment
 *   human_readable: The Universal Standard Lock
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Global protocol standardization creates a structural constraint where
 *   incumbent vendors and standard-setting organizations benefit from
 *   entrenchment while competing protocol developers, regional innovators,
 *   and jurisdictions dependent on legacy infrastructure bear extraction
 *   costs. The constraint exhibits tension between its genuine coordination
 *   function (solving interoperability problems across distributed systems)
 *   and its extractive mechanism (locking developers and jurisdictions into
 *   vendor ecosystems, preventing alternative evolution paths). At the global
 *   scope, network effects amplify both the coordination value and the
 *   lock-in severity. The theater ratio (0.58) reflects that much
 *   standardization activity is performative compliance rather than technical
 *   progress — certification processes, governance meetings, and
 *   backward-compatibility maintenance consume resources disproportionate to
 *   innovation enabled. The constraint is classified as Tangled Rope at its
 *   analytical core: it performs real coordination while simultaneously
 *   enforcing asymmetric extraction through incumbent control of governance
 *   and implementation specifications.
 *
 * KEY AGENTS:
 *   - Incumbent Standard Holders: Primary beneficiary (institutional/arbitrage) — tech giants and standards bodies capture governance seats and shape protocol evolution; enjoy network effects moat
 *   - Competing Protocol Developers: Primary victim (powerless/trapped) — face prohibitive switching costs and cannot establish competing protocols due to network effects lock-in
 *   - Regional Technology Communities: Secondary victim (moderate/constrained) — mid-tier actors benefit from standardization stability but cannot meaningfully influence protocol direction
 *   - Open Source Coalition: Organized actor (organized/mobile) — developers, foundations, and decentralized communities building alternative implementations and exit pathways
 *   - Standard-Setting Organizations: Institutional actor (institutional/arbitrage) — ISO, IEEE, IETF, W3C coordinate interoperability and govern evolution; capture rents through standardization necessity
 *   - Jurisdictions Dependent on Legacy Systems: Secondary victim (moderate/constrained) — governments, critical infrastructure operators, developing nations bear costs of upgrade cycles and vendor lock-in
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing network effects as immutable law rather than contingent economic structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_protocol_entrenchment, 0.52).
domain_priors:suppression_score(global_protocol_entrenchment, 0.68).
domain_priors:theater_ratio(global_protocol_entrenchment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_protocol_entrenchment, extractiveness, 0.52).
narrative_ontology:constraint_metric(global_protocol_entrenchment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_protocol_entrenchment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_protocol_entrenchment, tangled_rope).
narrative_ontology:human_readable(global_protocol_entrenchment, "The Universal Standard Lock").
narrative_ontology:topic_domain(global_protocol_entrenchment, "technological/economic").

domain_priors:requires_active_enforcement(global_protocol_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_protocol_entrenchment, incumbent_standard_holders).
narrative_ontology:constraint_victim(global_protocol_entrenchment, competing_protocol_developers).
narrative_ontology:constraint_victim(global_protocol_entrenchment, jurisdictions_dependent_on_legacy_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN DEVELOPER (SNARE) — Developer ecosystems, companies, and jurisdictions that depend on legacy protocol infrastructure face prohibitive costs to migrate. Network effects lock them in. Cannot exit without catastrophic sunk cost loss. Maximum experienced extraction through standardization inertia.
constraint_indexing:constraint_classification(global_protocol_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL INNOVATOR (TANGLED ROPE) — Mid-tier technology actors and regional standard bodies can propose improvements or alternative protocols, but face coordination barriers and market fragmentation costs. Benefit from the standard's stability while bearing extraction through standardization governance lock-in. Constrained but not trapped.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STANDARD-SETTING ORGANIZATION (ROPE) — ISO, IEEE, IETF, W3C coordinate global interoperability. Experience the constraint as pure coordination: establishing the protocol solves a collective action problem. Enjoy institutional arbitrage through setting governance rules. Net beneficiary.
constraint_indexing:constraint_classification(global_protocol_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (SCAFFOLD) — Organized developers, Linux foundations, and open-standards advocates are building alternative implementation pathways and sunset mechanisms. Lower effective extraction because coalition actors have exit options through forking, alternative implementations, and decentralized protocol development. Theater of 'standardization necessity' is being displaced by distributed verification.
constraint_indexing:constraint_classification(global_protocol_entrenchment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY STANDARDS BODY (PITON) — Older standards bodies (pre-internet protocols, deprecated telecom standards) maintain enforcement power through regulatory capture and institutional inertia despite reduced functional value. Certification and compliance theater persists even as the technical standard becomes obsolete. Maintained through bureaucratic lock-in rather than genuine coordination need.
constraint_indexing:constraint_classification(global_protocol_entrenchment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT TECH GIANT (TANGLED ROPE) — Major vendors (Microsoft, Apple, Google, Amazon) benefit from standardization governance seats and can shape protocol evolution but face constraint from maintaining backward compatibility and cannot fully escape standardization bodies without losing legitimacy. High extraction capacity constrained by reputational risk and regulatory scrutiny.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NETWORK EFFECTS VIEW (MOUNTAIN) — From a civilizational perspective, protocol standardization may appear as an inevitable natural law: network effects and coordination costs create inherent barriers to switching that no institutional actor can fully overcome. However, the base metrics (extractiveness 0.52, suppression 0.68) contradict true mountain classification — the engine detects this as false naturalization of contingent economic incentive structures.
constraint_indexing:constraint_classification(global_protocol_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_protocol_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_protocol_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_protocol_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_protocol_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_protocol_entrenchment, TR),
    TR >= 0.70.

:- end_tests(global_protocol_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Initial extractiveness (0.28) reflects genuine coordination benefit — early standardization genuinely solves interoperability problems with minimal rent-seeking. Over 30 years, extractiveness rises to 0.52 as: (1) incumbent vendors consolidate control of standard-setting governance, (2) backward compatibility requirements lock out competing protocols, (3) certification and compliance theater grows disproportionate to technical progress. The trajectory reflects rent-seeking layered onto coordination. Suppression (0.68): High. Barriers to exit include: (a) network effects (switching costs rise with ecosystem size), (b) switching costs (massive retraining, infrastructure replacement, data migration), (c) regulatory reliance on the standard (governments mandate compliance), (d) path dependency (new developers must adopt incumbent protocols to achieve interoperability). Suppression reflects structural immobility. Theater ratio (0.58): Moderate-high and rising. Initial theater (0.35) represents genuine technical work — protocol design, testing, implementation. Over time, theater rises to 0.58 as: (1) certification processes proliferate with reduced technical substance, (2) governance meetings grow performative (decisions already made by incumbent committees), (3) backward compatibility maintenance becomes theater (required for lock-in, not for technical progress). Claimed type (Tangled Rope) derives from beneficiaries (incumbent holders), victims (competing developers, locked-in jurisdictions), and enforcement (governance capture + network effects + regulatory mandate).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement across scale. The locked-in developer sees pure extraction (Snare) — the protocol is an immovable constraint they cannot escape. The open source coalition sees a temporary problem with exit pathways (Scaffold) — alternative implementations and distributed governance are building sunset mechanisms. The standard-setting organization sees pure coordination (Rope) — their role is solving interoperability. The incumbent vendor sees mixed extraction and coordination (Tangled Rope) — they benefit from standardization governance but face constraint from regulatory scrutiny and reputational risk. The analytical observer risks seeing natural law (Mountain) — network effects as immutable — but the base metrics reveal false naturalization: the suppression (0.68) and rising theater (0.58) are institutional choices, not physics. The global scope amplifies both the coordination benefits (planetary interoperability) and the extraction risks (lock-in scales with network size). This perspectival gap is the core diagnostic value of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position within the global standardization regime. Incumbent vendors with arbitrage options (can shape protocol, can exit by forking while maintaining market dominance) experience low or negative effective extraction — they are beneficiaries. Locked-in developers with no exit options (cannot abandon the protocol, cannot establish competing standard, cannot coordinate mass migration) experience maximum extraction — d approaches 1.0. Regional innovators with constrained exit (can propose improvements, cannot fully fork, face coordination barriers) experience moderate extraction. The open source coalition with mobile exit options (can fork, can build alternative implementations, can coordinate distributed verification) experiences reduced extraction despite global scope. Standard-setting organizations with institutional arbitrage (set rules, govern evolution, capture compliance rents) experience negative extraction — they are institutional beneficiaries. The incumbent tech giant perspective (institutional/constrained) reflects capture: benefits from governance power but cannot fully escape standardization bodies (reputational, regulatory pressure) — a mixed Tangled Rope that shows regulatory risk constrains what would otherwise be pure arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing how 'standardization' conflates genuine coordination with rent-seeking extraction. The false summit risk is acute: network effects and switching costs can be naturalized as immutable laws of technology ('there must be one standard for the internet to work'). The structural data contradicts this. Extractiveness (0.52) is substantial but not inevitable — it reflects incumbent control of governance, not technical necessity. Suppression (0.68) reflects regulatory lock-in and sunk costs, not immutable switching costs — alternative protocols exist and have lower switching barriers, but governance capture prevents their emergence. Theater (0.58) reflects performative certification and compliance, not essential interoperability work — much could be automated or eliminated. The Tangled Rope classification prevents the false naturalization by making visible the distinction between coordination (genuine interoperability benefit) and extraction (incumbent rent-seeking through governance capture). The mandatrophy is resolved by showing that standardization CAN be coordinative (open governance, minimal theater, low suppression) but the current global regime is Tangled Rope because incumbents have layered extraction mechanisms onto the coordination base. Policy interventions (distributed governance, protocol forking rights, open implementation standards, interoperability mandates) could shift toward pure Rope; absence of such intervention confirms the Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interoperability_vs_lock_in_threshold,
    'At what network size does a protocol''s coordination function transition from genuine interoperability benefit to extractive lock-in?',
    'Comparative analysis of protocol switching costs across network scales; measurement of developer satisfaction and innovation velocity within standardized ecosystems',
    'If threshold is near-total: most standards are primarily extractive (high χ). If threshold is late: many standards are primarily coordinative (low χ). Determines whether ''universal standard'' frames as Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_vs_lock_in_threshold, empirical, 'Threshold where protocol benefits transition to extraction').

omega_variable(
    fork_feasibility_and_governance_capture,
    'Do technical capabilities for protocol forking and alternative implementation actually reduce extraction, or does governance capture by incumbents prevent effective exits?',
    'Case studies of successful protocol forks (IPv6, HTTP/3, Bluetooth alternatives); analysis of governance voting power concentration in standards bodies; tracking of adoption rates for genuinely open alternatives',
    'If forks are feasible and governance is open: scaffold perspective confirmed, extraction is moderate (tangled_rope). If governance is captured: open alternatives remain marginal, extraction is severe (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fork_feasibility_and_governance_capture, empirical, 'Whether forks provide real exit options despite governance capture').

omega_variable(
    regulatory_mandate_vs_market_efficiency,
    'Is mandatory protocol standardization (e.g., EU interoperability mandates, 3GPP cellular standards) driven by genuine market failure correction or by incumbent rent-seeking disguised as regulation?',
    'Historical comparison of pre/post-mandate innovation rates; measurement of consumer welfare change; analysis of regulatory comment periods and lobbying influence by incumbents',
    'If mandate corrects market failure: standardization is cooperative good (Rope). If mandate preserves rents: standardization is extraction mechanism (Snare/Tangled_Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_vs_market_efficiency, conceptual, 'Whether regulation addresses market failure or preserves rents').

omega_variable(
    open_standard_credibility,
    'Do standards labeled ''open'' (W3C, IETF, ISO/IEC JTC1) actually prevent capture by incumbent vendors, or does the label mask the same power asymmetry?',
    'Voting pattern analysis in standards committees; tracking of feature proposals by company size/resources; measurement of implementation compliance and fragmentation in ''open'' standards',
    'If open standards genuinely distribute power: theater_ratio drops, suppression falls, classification shifts toward Rope. If label masks capture: theater_ratio remains high, suppression remains high, Tangled_Rope/Snare classifications confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_standard_credibility, empirical, 'Whether ''open standards'' actually decentralize power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_protocol_entrenchment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpe_tr_t0, global_protocol_entrenchment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gpe_tr_t15, global_protocol_entrenchment, theater_ratio, 15, 0.48).
narrative_ontology:measurement(gpe_tr_t30, global_protocol_entrenchment, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(gpe_be_t0, global_protocol_entrenchment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpe_be_t15, global_protocol_entrenchment, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(gpe_be_t30, global_protocol_entrenchment, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_protocol_entrenchment, information_standard).
narrative_ontology:affects_constraint(global_protocol_entrenchment, internet_protocol_lock_in).
narrative_ontology:affects_constraint(global_protocol_entrenchment, vendor_certification_capture).
narrative_ontology:affects_constraint(global_protocol_entrenchment, critical_infrastructure_dependency).

% DUAL FORMULATION NOTE:
% Global protocol entrenchment decomposes into multiple constraint families. This story tracks the institutional standardization mechanism (governance capture, network effects, lock-in). Downstream constraints address specific protocol families (TCP/IP, HTTP, cellular standards) and their extractive mechanisms. Network edges link to vendor capture stories and critical infrastructure dependency constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_protocol_entrenchment, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
