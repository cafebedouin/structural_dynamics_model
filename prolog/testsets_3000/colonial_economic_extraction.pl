% ============================================================================
% CONSTRAINT STORY: colonial_economic_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colonial_economic_extraction, []).

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
 *   constraint_id: colonial_economic_extraction
 *   human_readable: Colonial Economic Extraction Mechanism
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   Colonial economic extraction represents a structural constraint that
 *   persisted across centuries through military enforcement, institutional
 *   monopolization, legal prohibition, and psychological internalization. The
 *   constraint exhibits the full DR typology from different structural
 *   positions: the colonized population experiences pure extraction (snare);
 *   local elites experience mixed coordination and extraction (tangled rope);
 *   the metropolitan core experiences pure coordination (rope); indigenous
 *   institutions experience systematic subordination (snare); the
 *   justification apparatus persists performatively as enforcement declines
 *   (piton); and the analytical observer risks naturalizing contingent power
 *   asymmetries as immutable law (false summit mountain). The constraint's
 *   extractiveness (0.78) reflects sustained asymmetric resource transfer
 *   with high suppression (0.82) — colonized populations face military
 *   coercion, legal restriction of trade, confiscation of land, and coerced
 *   labor. The theater ratio (0.65) indicates that significant justificatory
 *   effort (civilizing mission, racial hierarchy, religious conversion)
 *   accompanied the extraction, and this justification persisted and evolved
 *   even as the economic system weakened. The constraint shows partial
 *   degradation over the measurement interval: extractiveness declines from
 *   0.82 to 0.71 as resistance movements, trade competition from other
 *   empires, and industrial shifts in the metropole reduce extraction
 *   capacity. Simultaneously, theater ratio increases from 0.42 to 0.65 as
 *   the constraint becomes increasingly dependent on ideological
 *   justification rather than pure military enforcement — the functional role
 *   atrophies while the narrative persists.
 *
 * KEY AGENTS:
 *   - Colonized Populations: Primary victim (powerless/trapped) — face coerced labor, land confiscation, trade prohibition, military enforcement with no legal or structural exit
 *   - Metropolitan Core: Primary beneficiary (institutional/arbitrage) — extracts raw materials, raw labor, and market access with minimal administrative cost through delegation to local elites
 *   - Local Colonial Elites: Intermediate layer (moderate/constrained) — receive administrative power and commercial privilege in exchange for enforcing suppression on lower tiers; their exit is costly but theoretically possible
 *   - Colonial Trading Monopolies: Secondary beneficiary (institutional/arbitrage) — profit from legally protected trade routes and access to colonized resources without competition
 *   - Indigenous Production Institutions: Secondary victim (powerless/trapped) — traditional craft and trade networks are dismantled or subordinated; cannot exit because alternatives are legally prohibited
 *   - Colonial Justification Apparatus: Institutional actor (institutional/arbitrage) — maintains ideological and legal framework (civilizing mission, racial hierarchy) that legitimizes extraction; persists through inertia as economic function declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating power asymmetry as an immutable law rather than recognizing it as contingent on military capacity and institutional scaling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colonial_economic_extraction, 0.78).
domain_priors:suppression_score(colonial_economic_extraction, 0.82).
domain_priors:theater_ratio(colonial_economic_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colonial_economic_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(colonial_economic_extraction, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(colonial_economic_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colonial_economic_extraction, snare).
narrative_ontology:human_readable(colonial_economic_extraction, "Colonial Economic Extraction Mechanism").
narrative_ontology:topic_domain(colonial_economic_extraction, "economic/political/historical").

domain_priors:requires_active_enforcement(colonial_economic_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colonial_economic_extraction, metropolitan_core).
narrative_ontology:constraint_beneficiary(colonial_economic_extraction, colonial_trading_monopolies).
narrative_ontology:constraint_victim(colonial_economic_extraction, colonized_populations).
narrative_ontology:constraint_victim(colonial_economic_extraction, local_production_capacity).
narrative_ontology:constraint_victim(colonial_economic_extraction, indigenous_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIZED POPULATION (SNARE) — Trapped within colonial economic structure with no viable exit. Faces military enforcement, legal prohibition of alternative trade routes, confiscation of land and productive assets, and coerced labor extraction. Maximum suppression and maximum extraction from the agent's experience. The constraint is total — cannot emigrate, cannot trade independently, cannot accumulate capital.
constraint_indexing:constraint_classification(colonial_economic_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL COLONIAL ELITES (TANGLED ROPE) — Constrained but not trapped. Receive administrative positions and commercial privileges in exchange for enforcing the extraction system. Experience the constraint as mixed: genuine coordination of trade infrastructure and administrative efficiency coexists with asymmetric extraction — their privileges depend on suppressing lower-tier colonized groups. Exit is costly (loss of status, retaliation) but theoretically possible. The constraint provides real benefits alongside real extraction.
constraint_indexing:constraint_classification(colonial_economic_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METROPOLITAN CORE (ROPE) — Experiences the constraint as pure coordination with strong efficiency benefits. The colonial economic system solves the problem of acquiring raw materials, accessing new markets, and expanding geopolitical influence at minimal administrative cost (delegated to local elites). The extraction runs toward this agent exclusively; they experience coordination benefits without paying suppression costs. The constraint appears to the metropole as a well-functioning trade network.
constraint_indexing:constraint_classification(colonial_economic_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIGENOUS PRODUCTION INSTITUTIONS (SNARE) — Traditional craft guilds, agricultural cooperatives, and trade networks are systematically dismantled or subordinated. Cannot exit because the alternative institutions (colonial monopolies) are legally protected and militarily enforced. The suppression is structural: craft knowledge is devalued, land is confiscated, trade is redirected through colonial channels. This agent (collective institutional capacity) bears maximum extraction with no compensatory benefit.
constraint_indexing:constraint_classification(colonial_economic_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLONIAL JUSTIFICATION APPARATUS (PITON) — The ideological and legal frameworks (civilizing mission, racial hierarchy, scientific racism, Christian duty) that rationalize the extraction are substantially performative. As actual colonial extraction mechanisms weaken, the justification apparatus persists through institutional inertia long after its functional role has atrophied — documented in legal codes, academic curricula, and cultural narratives that outlive the economic system they once legitimized. Theater ratio is high because the moral and legal arguments are maintained even as enforcement and extraction decline.
constraint_indexing:constraint_classification(colonial_economic_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / POWER ASYMMETRY VIEW (MOUNTAIN) — From the civilizational perspective, colonial extraction reflects a fundamental asymmetry in military-organizational capacity: any society with superior naval technology and organizational scaling can extract from any society without these capabilities. This appears as an immutable law of geopolitical physics. However, the structural data contradicts the mountain classification — the constraint depends on continuous enforcement, is not irreducible, and has been partially reversed through independence movements. The engine will flag this as a false summit, revealing naturalization of contingent power asymmetries.
constraint_indexing:constraint_classification(colonial_economic_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colonial_economic_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colonial_economic_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colonial_economic_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colonial_economic_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colonial_economic_extraction, TR),
    TR >= 0.70.

:- end_tests(colonial_economic_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts substantial wealth, labor, and resources from colonized populations to the metropolitan core. The measure reflects the asymmetry: the colonized population loses production capacity, land, and future economic potential; the metropole gains raw materials, labor surplus, and market access. The extraction is not total (0.85+) because some coordination functions are genuine — colonial infrastructure, trade systems, and administrative efficiency do provide goods and services, albeit at highly asymmetric terms of exchange. Suppression (0.82): Very high. Colonized populations face multiple overlapping suppression mechanisms: military garrison and threat of violence; legal prohibition of independent trade; confiscation of land and productive assets; coerced labor (slavery, indentured servitude, corvée); educational exclusion from positions of authority; cultural denigration and psychological demoralization. These are structural (external, material barriers) rather than primarily internalized — the suppression persists even when resistance emerges. Theater ratio (0.65): Moderate-high. The constraint requires continuous ideological justification: civilizing mission narratives, racial hierarchy pseudo-science, religious conversion imperatives, and claims of beneficial development. This theatrical component becomes more prominent over time as military enforcement alone proves inadequate and as metropolitan publics require moral narratives to justify extraction. The theater is neither minimal (≤0.25, pure function) nor maximal (≥0.85, pure performance) — it accompanies genuine extraction mechanisms while becoming progressively more important relative to pure force.
 *
 * PERSPECTIVAL GAP:
 *   Fundamental gap between how the metropole and colonized population perceive the constraint. The metropole experiences rope — a coordination mechanism that solves resource acquisition and market access problems. The colonized population experiences snare — pure extraction with total suppression and no compensatory coordination benefits. This gap is not a measurement error; it reflects genuine structural asymmetry: what the metropole calls 'trade coordination' is extraction from the colonized perspective because the terms of exchange are asymmetric and coerced. Local elites occupy an intermediate position that captures both perspectives — they experience genuine coordination benefits relative to the baseline (pre-colonial economic organization) but massive extraction relative to the metropole's position. The analytical observer risks naturalizing this gap as an immutable law (mountain — 'power always flows to the more capable') rather than recognizing it as contingent on enforcement capacity and institutional scaling. Historical resistance movements demonstrate the gap is not permanent: when enforcement capacity declines, the constraint downgrades from snare to tangled_rope (with greater agency) and eventually to piton (vestigial).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's power level, exit options, and relationship to the extraction flow. The colonized population (powerless/trapped) derives d ≈ 0.95 (near-total victim): no structural mobility, full extraction target. The metropole (institutional/arbitrage) derives d ≈ 0.05 (near-total beneficiary): high mobility, extraction source. Local elites (moderate/constrained) derive d ≈ 0.55 (mixed): constrained mobility, benefits and costs balanced. Indigenous institutions (powerless/trapped) derive d ≈ 0.98 (near-total victim): institutional capacity is systematically subordinated. The directionality overrides are unnecessary here because the power atoms and exit options accurately reflect structural position. The constraint's effective extractiveness χ is scaled by f(d) × σ(S) for each perspective: the colonized population at local scope with d=0.95 experiences maximum χ; the metropole at global scope with d=0.05 experiences χ amplified by scope but diminished by near-total beneficiary status (f(d) ≈ -0.12). This produces the perspectival pattern: the extraction feels overwhelming from the target's view (high d → high f(d) → high χ) and coordinating from the beneficiary's view (low d → low f(d) → low χ).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival multiplicity: The constraint cannot be classified as a single type because it is genuinely different constraints from different structural positions. From the colonized perspective, it is a snare (ε=0.78, χ ≥ 0.66, pure extraction). From the metropolitan perspective, it is rope (low χ, pure coordination benefit). From the local elite perspective, it is tangled rope (mixed benefits and costs). This is not classification failure; it is the framework working correctly — the constraint manifests differently across the observation site. The falsity of the mountain classification (from the analytical observer perspective) reveals that the 'immutable law' framing is a naturalization of contingent power asymmetries. Colonial extraction persists only through continuous enforcement; its eventual reversal (via independence movements) proves it is not an irreducible limit. The mandatrophy is resolved by recognizing that multiple types are simultaneously valid from their respective structural positions, and that the perspectival gap is itself the diagnostic signal — constraints that appear as rope to beneficiaries and snare to victims are the definition of asymmetric extraction requiring active justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_dependency,
    'Is colonial economic extraction fundamentally dependent on continuous military enforcement, or does it persist through institutional inertia and internalized subordination?',
    'Comparative analysis of extraction mechanisms before and after military withdrawal; measurement of extraction rates when enforcement capacity declines (armed resistance, independence movements, reduced garrison capacity)',
    'If military-dependent: the constraint is a snare contingent on force. If internalized: post-colonial extraction persists through identity-locked and institutional mechanisms, requiring analysis of cognitive and institutional capture. If both: the constraint transforms from snare to tangled_rope during decolonization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_dependency, empirical, 'Whether extraction depends on military enforcement or internalized mechanisms').

omega_variable(
    local_elite_capture,
    'Do local colonial elites constitute a genuine intermediate layer with mixed incentives (tangled_rope), or are they fully captured subordinates whose interests are identical to the metropole (rope from their perspective)?',
    'Analysis of elite behavior during independence crises; measurement of whether elites defect, negotiate independently, or remain loyal; examination of post-colonial elite trajectories and institutional inheritance',
    'If genuine intermediaries: tangled_rope classification is correct, and decolonization requires negotiation with local elites. If fully captured: elites are not a structural layer, the system is snare + rope (no tangled_rope), and decolonization requires bypassing elite channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_elite_capture, empirical, 'Whether local elites have independent agency or are fully captured').

omega_variable(
    institutional_reversal_cost,
    'What is the cost of reversing colonial institutional arrangements (land restitution, trade route reopening, capacity reconstruction) relative to maintaining extraction mechanisms?',
    'Empirical measurement of institutional reconstruction costs in post-colonial states; comparative analysis of extraction cessation costs vs reversal costs; measurement of whether reversal was politically feasible given the cost asymmetry',
    'If reversal cost >> extraction benefit: the metropole will resist decolonization even if military enforcement becomes costly — the constraint persists as neocolonial economic dependence. If reversal cost < extraction benefit: independence becomes structurally possible, and the constraint downgrades to tangled_rope or scaffold during transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reversal_cost, empirical, 'Cost asymmetry between maintaining and reversing extraction').

omega_variable(
    psychological_internalization,
    'To what extent has the colonized population internalized the constraint as natural or deserved (identity_locked), vs. perceiving it as external oppression (trapped)?',
    'Analysis of resistance movements, slave revolts, and independence rhetoric; examination of how quickly psychological internalization reverses after enforcement withdrawal; measurement of internalization persistence in post-colonial societies',
    'If fully internalized: the constraint persists as identity-locked even after formal independence, requiring decolonization of the mind. If primarily structural: removal of enforcement mechanisms produces rapid mental reorientation. Mixed: indicates the constraint is partially self-reproducing through psychology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_internalization, empirical, 'Degree of psychological internalization vs structural entrapment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colonial_economic_extraction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colext_theater_initial, colonial_economic_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(colext_theater_mid, colonial_economic_extraction, theater_ratio, 50, 0.58).
narrative_ontology:measurement(colext_theater_late, colonial_economic_extraction, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(colext_initial_peak, colonial_economic_extraction, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(colext_mid_sustained, colonial_economic_extraction, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(colext_late_decline, colonial_economic_extraction, base_extractiveness, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colonial_economic_extraction, resource_allocation).
narrative_ontology:affects_constraint(colonial_economic_extraction, neocolonial_economic_dependence).
narrative_ontology:affects_constraint(colonial_economic_extraction, institutional_postcolonial_inheritance).
narrative_ontology:affects_constraint(colonial_economic_extraction, trade_asymmetry_persistence).

% DUAL FORMULATION NOTE:
% Colonial economic extraction exists as a structural constraint distinct from its justification apparatus (colonial_justification_apparatus is a separate story with higher theater_ratio). The extraction constraint also upstream-affects neocolonial dependence — as formal colonialism ended, the economic extraction mechanisms persisted through institutional lock-in and debt dependence, creating a transformed but continuous constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
