% ============================================================================
% CONSTRAINT STORY: path_dependent_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_path_dependent_lock_in, []).

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
 *   constraint_id: path_dependent_lock_in
 *   human_readable: Path Dependent Lock-In: Coordination with Embedded Extraction
 *   domain: institutional_economics/technology_diffusion
 *
 * SUMMARY:
 *   Path-dependent lock-in is a structural constraint that arises when
 *   adoption of a particular technology or institutional arrangement creates
 *   irreversible or high-cost switching dynamics, enabling incumbents to
 *   extract rents from adopters who cannot exit without bearing prohibitive
 *   costs. The constraint combines genuine coordination functions — the
 *   installed base enables vendors to make long-term investments and provide
 *   stable services — with asymmetric extraction, wherein the incumbent's
 *   market power derives not from superior performance but from the cost
 *   structure of exit. This constraint exhibits the full six-type
 *   perspectival range: adopters experience it as a snare (trapped), superior
 *   alternatives experience it as tangled rope (mixed
 *   coordination-extraction), incumbents experience it as rope (pure
 *   coordination benefit), organized transition efforts see it as scaffold
 *   (temporary with sunset), legacy systems perpetuate it as piton
 *   (performative), and the analytical observer risks seeing it as mountain
 *   (immutable feature of increasing-returns systems). The rising
 *   extractiveness over the measurement interval (0.35→0.58) reflects
 *   accumulating rents as lock-in deepens; rising theater ratio reflects the
 *   increasing performative burden of maintaining legacy compatibility while
 *   technically superior alternatives mature.
 *
 * KEY AGENTS:
 *   - Incumbent Technology Vendor: Primary beneficiary (institutional/arbitrage) — extracts rents through lock-in mechanism; experiences constraint as coordination enabling long-term planning
 *   - Adopters Locked to Inferior Path: Primary victim (powerless/trapped) — bear switching costs; cannot exit without prohibitive loss; experience maximum extraction
 *   - Superior Alternative Developers: Secondary victim (moderate/constrained) — technically superior but face coordination problem: installed base has no incentive to migrate; excluded from market despite merit
 *   - Transition Coalition: Organized agents (organized/constrained) — standards bodies, open-source communities, regulatory agencies building interoperability layers and migration pathways
 *   - Legacy Compliance Infrastructure: Institutional actor (institutional/arbitrage) — regulatory frameworks and enterprise standards perpetually reference incumbent technology; maintains through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(path_dependent_lock_in, 0.58).
domain_priors:suppression_score(path_dependent_lock_in, 0.65).
domain_priors:theater_ratio(path_dependent_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(path_dependent_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(path_dependent_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(path_dependent_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(path_dependent_lock_in, tangled_rope).
narrative_ontology:human_readable(path_dependent_lock_in, "Path Dependent Lock-In: Coordination with Embedded Extraction").
narrative_ontology:topic_domain(path_dependent_lock_in, "institutional_economics/technology_diffusion").

domain_priors:requires_active_enforcement(path_dependent_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(path_dependent_lock_in, incumbent_technology_vendor).
narrative_ontology:constraint_beneficiary(path_dependent_lock_in, installed_base_stakeholders).
narrative_ontology:constraint_victim(path_dependent_lock_in, superior_alternative_developers).
narrative_ontology:constraint_victim(path_dependent_lock_in, adopters_constrained_to_inferior_path).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED ADOPTER (SNARE) — Cannot exit the established technology path without bearing prohibitive switching costs. Lock-in is enforced through complementary asset requirements, data format incompatibility, skill depreciation, and network effects. The adopter experiences maximum extraction: the incumbent extracts rents precisely because exit is structurally impossible at biographical timescale. No alternatives exist at acceptable cost.
constraint_indexing:constraint_classification(path_dependent_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUPERIOR ALTERNATIVE DEVELOPER (TANGLED ROPE) — Technically superior solution exists but faces coordination problem: installed base has no incentive to migrate. High-cost barrier to entry due to network effects and switching costs. However, the constraint also functions as coordination: the installed base's lock-in enables the incumbent to make long-term investments that create genuine value. The developer bears extraction (excluded from market despite technical superiority) but the overall system coordinates stable technology transitions. Moderate power and constrained exit reflect high but not insurmountable barriers.
constraint_indexing:constraint_classification(path_dependent_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT VENDOR (ROPE) — Experiences lock-in as pure coordination benefit. The installed base dependency enables long-term planning, justifies infrastructure investment, and creates stable customer relationships. Lock-in functions as coordination for this agent: committed installed base allows vendor to invest in improvements rather than constantly competing on acquisition. The vendor has arbitrage options (licensing, platform extensions) and benefits from the constraint's coordination function. Effective extraction runs toward the vendor, not away.
constraint_indexing:constraint_classification(path_dependent_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSITION COALITION (SCAFFOLD) — Standards bodies, open-source communities, regulatory agencies (organized agents) are building interoperability layers and migration pathways that reduce switching costs. Backward-compatibility standards, API translation layers, and data format converters create a temporary scaffold that enables adopters to exit the path without total loss of legacy investments. The coalition sees lock-in as solvable through coordinated standards work with a sunset: as transition infrastructure matures, switching costs decline and lock-in loses force. Theater ratio low for this perspective because the work is substantive (technical standards) not performative.
constraint_indexing:constraint_classification(path_dependent_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY COMPLIANCE THEATER (PITON) — Regulatory compliance frameworks and enterprise architecture standards perpetually reference the incumbent technology (Windows in enterprise, COBOL in banking, x86 in computing) not because they are optimal but because organizational inertia maintains the reference. New systems are certified for 'backward compatibility' with legacy tech; regulations mandate interoperability with systems using outdated standards. The compliance infrastructure is substantially theatrical — regulatory bodies maintain reference to legacy tech because alternatives haven't fully replaced it, not because the legacy is functionally optimal. Theater ratio high (0.70+) reflecting performative compliance.
constraint_indexing:constraint_classification(path_dependent_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, path dependence is an immutable feature of any system with increasing returns, network externalities, or complementary asset requirements. Once multiple adopters commit to a technology, the cost structure itself becomes mathematically favorable for incumbents — path dependence is not a policy choice but a structural consequence of how adoption curves work. This perspective risks naturalizing what may be a contingent institutional arrangement (e.g., the specific regulatory and intellectual property structures that enable lock-in) as a law of economics.
constraint_indexing:constraint_classification(path_dependent_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(path_dependent_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(path_dependent_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(path_dependent_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(path_dependent_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(path_dependent_lock_in, TR),
    TR >= 0.70.

:- end_tests(path_dependent_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Lock-in generates rents for incumbents, but extraction is constrained by the need to maintain quality sufficient to prevent wholesale migration and by regulatory pressure. Rising from 0.35 to 0.58 over the interval reflects accumulating rents as installed base grows and switching cost ratios sharpen — early in lifecycle, paths are not yet locked; later, they are. Suppression (0.65): High. Switching costs are substantial and multidimensional: technical (data migration, API relearning), economic (licensing costs, infrastructure duplication), organizational (process redesign, workforce retraining), and psychological (preference lock, familiarity bias). Suppression rises with installed base size because early adopters had more alternatives. Theater ratio (0.58): Moderate-high. Lock-in itself is not performative, but the compliance infrastructure built around it increasingly is — regulations mandate backward compatibility with legacy tech not because it's optimal but because alternatives haven't fully displaced it. Claimed type (Tangled Rope): Satisfies gates — has beneficiaries (incumbent, installed base stakeholders) and victims (superior alternatives, locked adopters), requires active enforcement (through IP, licensing, interoperability sabotage), exhibits mixed coordination (enabling long-term planning) and extraction (rents from switching costs).
 *
 * PERSPECTIVAL GAP:
 *   The incumbent experiences lock-in as pure coordination (Rope) — the installed base enables stable planning and justifies infrastructure investment. The adopter experiences it as pure extraction (Snare) — they cannot escape and bear full cost. The superior alternative experiences it as mixed (Tangled Rope) — the system does coordinate technology transitions at some rate, but the existing adopter base's lock-in excludes better solutions. The organized coalition sees a solvable problem with a sunset (Scaffold) — backward compatibility standards and migration layers reduce switching costs over time. The legacy compliance system sees itself as performing a function (Piton) — maintaining regulatory reference to incumbent technology despite technical obsolescence. The analytical observer risks seeing immutable economics (Mountain) — but the structural data reveals this as a false summit: the specific intellectual property, regulatory, and organizational structures that enable lock-in are contingent, not laws of nature. The perspectival gap is large because the constraint's extraction mechanism is not force or fraud but structural cost asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent vendor's directionality (d) is low despite market power because they are structurally a net beneficiary: locked-in adopters enable long-term planning, complementary service offerings, and stable revenue. This produces low d (beneficiary status) and low f(d), making their experienced extractiveness negative (they benefit from the constraint). Adopters have high d (victim status) and trapped exit options, producing high f(d) and high experienced extractiveness. Superior alternatives have moderate d (victim but with constrained exit to market through coalition support) and moderate f(d). The organized coalition has low d relative to their power level because they are solving a coordination problem — their position is beneficiary-aligned (reducing switching costs helps all parties eventually, including current adopters). The compliance infrastructure has low d (beneficiary status, arbitrage options) because maintaining legacy reference is organizationally convenient. The analytical observer's d is canonical (0.73 for analytical power), but their classification as mountain is subject to false-summit detection: the structural data contradicts the mountain gates.
 *
 * MANDATROPHY ANALYSIS:
 *   Path-dependent lock-in resolves mandatrophy by showing that the constraint's classification depends critically on the time horizon and exit options of the observing agent. At immediate/trapped, it is pure extraction (Snare). At generational/constrained, it is mixed (Tangled Rope). At civilizational analytical, it risks false naturalization (Mountain). The constraint prevents mislabeling coordination as extraction by requiring explicit declaration of the coordination function: the incumbent's ability to plan long-term and invest in quality is genuine coordination benefit. It prevents mislabeling extraction as pure coordination by requiring explicit declaration of victims and barriers to exit: locked adopters bear real costs precisely because alternatives exist but are inaccessible. The mandate resolves by declaring BOTH: beneficiaries (who experience coordination), victims (who experience extraction), and the active enforcement mechanism (how the vendor maintains the lock-in). The tangled_rope classification is forced; the analysis must justify why both coordination and extraction are structurally real, not why one dominates the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurability,
    'What portion of measured switching costs are technological vs. institutional/organizational vs. psychological?',
    'Decompose switching cost studies by category: hardware compatibility, software licensing, data migration, employee retraining, organizational process change. Compare actual vs perceived costs post-migration.',
    'If primarily technological: path dependence may be solvable through interoperability standards (scaffold viable). If primarily institutional/psychological: path dependence is stickier than technology can fix, requiring regulatory intervention or organizational change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_measurability, empirical, 'Composition of switching costs by category').

omega_variable(
    network_effect_threshold,
    'What installed-base size threshold makes lock-in mathematically irreversible vs reversible?',
    'Historical analysis of technology transitions: identify cases where superior technologies displaced entrenched incumbents despite network effects, measure installed-base size ratios at transition point.',
    'If threshold is crossed early in lifecycle: lock-in is structurally temporary (scaffold sunset ~10-15 years). If threshold enables indefinite lock-in: constraint approaches mountain classification (irreversible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_threshold, empirical, 'Threshold at which network effects become mathematically irreversible').

omega_variable(
    incumbent_active_extraction,
    'Does the incumbent actively enforce lock-in (through pricing, licensing restrictions, interoperability sabotage) or is lock-in a passive structural outcome of network effects?',
    'Document incumbent behavior: pricing strategy during transition periods, interoperability policy changes, intellectual property enforcement. Compare to passive network-effect predictions.',
    'If active enforcement: tangled_rope classification is correct (requires active enforcement gate). If purely passive: may reclassify closer to rope (coordination without enforcement), reducing extraction component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_active_extraction, empirical, 'Whether incumbent actively enforces lock-in or lock-in is passive structural outcome').

omega_variable(
    coalition_success_rates,
    'Do organized transition efforts (interoperability standards, open-source alternatives, regulatory mandates) actually reduce switching costs at measurable rates?',
    'Track migration rates before/after standards completion; measure adoption of interoperability layers; compare organizations that adopt transition infrastructure vs those that don''t.',
    'If successful: scaffold perspective validated — lock-in has real sunset timescale (generational). If unsuccessful: scaffold is aspirational, and lock-in behaves more like snare for most adopters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_success_rates, empirical, 'Efficacy of organized coalition efforts to reduce switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(path_dependent_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdli_tr_t0, path_dependent_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pdli_tr_t5, path_dependent_lock_in, theater_ratio, 5, 0.52).
narrative_ontology:measurement(pdli_tr_t10, path_dependent_lock_in, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pdli_be_t0, path_dependent_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pdli_be_t5, path_dependent_lock_in, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pdli_be_t10, path_dependent_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(path_dependent_lock_in, resource_allocation).
narrative_ontology:affects_constraint(path_dependent_lock_in, network_effects_positive_feedback).
narrative_ontology:affects_constraint(path_dependent_lock_in, vendor_lock_in_economic_rents).
narrative_ontology:affects_constraint(path_dependent_lock_in, interoperability_standards_fragmentation).

% DUAL FORMULATION NOTE:
% Path-dependent lock-in is upstream of specific vendor lock-in mechanisms and downstream of general network effects. The constraint captures the structural bridge between positive feedback dynamics (network effects) and asymmetric extraction (vendor rents). Separate stories address the coordination benefits (network effects) and the extraction mechanism (switching costs + IP enforcement), linked by this story's affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(path_dependent_lock_in, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
