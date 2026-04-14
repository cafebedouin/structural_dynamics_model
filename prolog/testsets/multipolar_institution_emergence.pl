% ============================================================================
% CONSTRAINT STORY: multipolar_institution_emergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multipolar_institution_emergence, []).

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
 *   constraint_id: multipolar_institution_emergence
 *   human_readable: Multipolar Institution Emergence and Coordination Failure
 *   domain: political_economy/institutional_design
 *
 * SUMMARY:
 *   The emergence of multipolar institutional order creates a structural
 *   tension between the coordination requirements of a multipolar system and
 *   the institutional inheritance of a unipolar/bipolar framework. As rising
 *   powers accumulate capacity to act independently, they face simultaneous
 *   incentives to (a) participate in existing institutions to influence their
 *   rules and (b) create alternative institutions to establish spaces where
 *   their voice is not subordinated to existing power distributions. This
 *   generates institutional proliferation, fragmentation of rules and
 *   standards, and a fundamental ambiguity about whether multipolarity
 *   represents progress toward a more responsive global coordination system
 *   or decline toward a least-common-denominator equilibrium. The constraint
 *   exhibits diagnostic complexity: the same structural shift appears as pure
 *   coordination (rope) from the hegemon's position, as extraction (snare)
 *   from marginalized states, as mixed coordination-plus-extraction (tangled
 *   rope) from rising powers, as degraded ritual (piton) from the perspective
 *   of the Bretton Woods architecture, as temporary coordination problem with
 *   sunset (scaffold) from emerging multipolar mechanisms, and as immutable
 *   law (mountain) from structural realism. Theater ratio has increased from
 *   0.48 to 0.58 over the interval, indicating that institutional activity is
 *   becoming increasingly performative relative to functional as
 *   proliferation makes genuine consensus-based decision-making harder and
 *   forum-shopping for favorable outcomes increases.
 *
 * KEY AGENTS:
 *   - Incumbent Hegemon (U.S./Western bloc): Institutional/arbitrage — maintains arbitrage options through extrainstitutional power; sees multipolarity as coordination problem to manage
 *   - Rising Powers (China, India, regional hegemons): Organized/constrained — gain exit options through institution creation; experience mixed coordination and extraction
 *   - Global South States (Marginalized periphery): Powerless/trapped — bear costs of fragmentation and competition without capacity to participate in institution-building; no exit options
 *   - Bretton Woods Institutions (UN, WTO, IMF, World Bank): Institutional/arbitrage — maintain themselves through sunk legitimacy; increasingly dependent on great-power enforcement; experiencing functional decline
 *   - Emerging Multipolar Institutions (BRICS, SCO, regional development banks): Organized/constrained — experiment with consensus-based decision-making; voluntary participation lowers suppression; sunset logic implicit
 *   - Global Coordination Capacity (Abstract collective good): Powerless/trapped — fragmentation increases transaction costs and reduces capacity to solve shared problems; bears costs without voice in institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multipolar_institution_emergence, 0.52).
domain_priors:suppression_score(multipolar_institution_emergence, 0.48).
domain_priors:theater_ratio(multipolar_institution_emergence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multipolar_institution_emergence, extractiveness, 0.52).
narrative_ontology:constraint_metric(multipolar_institution_emergence, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(multipolar_institution_emergence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multipolar_institution_emergence, tangled_rope).
narrative_ontology:human_readable(multipolar_institution_emergence, "Multipolar Institution Emergence and Coordination Failure").
narrative_ontology:topic_domain(multipolar_institution_emergence, "political_economy/institutional_design").

domain_priors:requires_active_enforcement(multipolar_institution_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(multipolar_institution_emergence, incumbent_hegemon).
narrative_ontology:constraint_beneficiary(multipolar_institution_emergence, rising_powers_selective).
narrative_ontology:constraint_victim(multipolar_institution_emergence, global_coordination_capacity).
narrative_ontology:constraint_victim(multipolar_institution_emergence, marginal_states).
narrative_ontology:constraint_victim(multipolar_institution_emergence, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH EXCLUDED STATES (SNARE) — States without great-power status experience maximum extraction and suppression. Cannot exit the multipolar order; cannot form alternative institutions with sufficient legitimacy; face competition from rising powers for alignment but no structural improvement in bargaining position. Trapped by economic dependency, geopolitical vulnerability, and lack of sufficient scale or resources. No coordination benefit flows to this group — they bear costs of institutional competition and fragmentation without participating in institution-building.
constraint_indexing:constraint_classification(multipolar_institution_emergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISING POWERS (TANGLED ROPE) — China, India, other regional hegemons experience both coordination and extraction. Genuine benefit from competing institution-building (Shanghai Cooperation Organization, BRICS, etc.) that elevates their voice and creates alternative mechanisms. But also bear costs: duplicative institutions create inefficiency; fragmented rules increase transaction costs; arms-race dynamics in institution creation consume resources. Constrained by existing institutional dominance but gaining exit options through institution creation. Mixed experience of the constraint.
constraint_indexing:constraint_classification(multipolar_institution_emergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT HEGEMON (ROPE) — The U.S. or dominant Western bloc experiences multipolarity as a coordination problem, not extraction. The constraint they manage is: how to preserve institutional leverage in a world where setting rules no longer guarantees compliance. Multipolarity creates inefficiency (competing forums, conflicting standards) that the hegemon must navigate, but the hegemon retains arbitrage options — can exit via unilateral action, can play great powers against each other, can selectively enforce existing rules. For the hegemon, the constraint is primarily coordination (maintaining enough institutional coherence to preserve influence) with minimal extraction.
constraint_indexing:constraint_classification(multipolar_institution_emergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL INSTITUTIONAL REGIME (TANGLED ROPE) — The set of institutions (UN, WTO, IMF, World Bank, etc.) experiences multipolarity as both coordination opportunity and extraction mechanism. Genuine coordination function: multiple great powers must coordinate on shared problems (climate, pandemics, financial stability). But also asymmetric extraction: rising powers extract legitimacy and voice from engagement; incumbents extract compliance through institutional control; both extract resources from the regime's operation. Theater ratio (0.58) reflects that much institutional activity is performative — negotiation theater that signals commitment without producing decisions. Active enforcement required to maintain the regime despite fragmentation.
constraint_indexing:constraint_classification(multipolar_institution_emergence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS ARCHITECTURE (PITON) — The post-1945 institutional framework (institutions designed for bipolar or unipolar world) persists through inertia despite reduced functional capacity. The institutions continue to operate, but much of their activity is ritual performance: voting procedures designed for 1945 power distributions, dispute resolution mechanisms undermined by great-power veto, decision rules that cannot accommodate multipolar consensus. The architecture maintains itself through sunk legitimacy and because no hegemon has successfully replaced it, but it functions at increasingly reduced capacity. Theater ratio reflects that procedural compliance persists even as substantive decision-making happens elsewhere (great-power bilaterals, regional forums, informal networks). Active enforcement declines as rising powers invest in alternative institutions.
constraint_indexing:constraint_classification(multipolar_institution_emergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING MULTIPOLAR COORDINATION MECHANISMS (SCAFFOLD) — New institutions (BRICS, SCO, regional development banks, plurilateral agreements) represent organized attempts to solve multipolarity as a temporary coordination problem with embedded sunset logic. These mechanisms see the fragmentation as soluble through deliberate institution design: summit diplomacy, consensus-building rules, rotating leadership. Theater ratio is moderate because emerging institutions are still experimenting with functional forms, not yet ossified into ritual. Suppression is lower because participation is voluntary. Temporal scope is generational because the hypothesis is that these mechanisms will either mature into stable multipolar coordination systems OR be reabsorbed into reformed hegemonic institutions OR fragment into regional silos. The sunset clause is implicit: if coordination succeeds, fragmentation decreases; if it fails, some institutions collapse or consolidate.
constraint_indexing:constraint_classification(multipolar_institution_emergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From a civilizational/universal perspective, multipolarity is an immutable feature of international relations: whenever power becomes distributed across multiple actors with independent capacity, each actor faces irreducible coordination problems. No power distribution can create perfect transparency or enforcement capacity; some institutional fragmentation is inherent to the structure. This perspective naturalizes multipolarity as a law of geopolitics — unchangeable given the distribution of material capabilities. However, the structural data reveals this as a false summit: the degree of fragmentation, the form of institutions, the mechanisms of coordination are all contingent design choices and historical accidents, not immutable laws.
constraint_indexing:constraint_classification(multipolar_institution_emergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multipolar_institution_emergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(multipolar_institution_emergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multipolar_institution_emergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(multipolar_institution_emergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(multipolar_institution_emergence, TR),
    TR >= 0.70.

:- end_tests(multipolar_institution_emergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Rising powers extract legitimacy and institutional voice through institution creation and great-power diplomacy; incumbents extract compliance through control of existing institutions; both extract resources from institutional operation itself (bureaucracy, research, monitoring). The extraction is significant but not total — some genuine coordination benefit flows to all parties (all prefer functioning institutions to pure conflict). Theater ratio (0.58): Moderate. Institutional activity includes significant functional components (actual problem-solving, resource allocation, negotiation) but increasingly includes performative elements (summit theater, procedural compliance without substance, competition for legitimacy). Theater has increased over the interval as institutional proliferation makes genuine consensus harder. Suppression (0.48): Moderate. Barriers to exit from the multipolar order are high (no power can unilaterally opt out of geopolitics), but mechanisms of suppression are primarily structural rather than coercive — the constraint is embedded in material distribution of capabilities rather than explicit enforcement. Rising powers have lower suppression (they can create alternatives) than Global South states (trapped by dependency and lack of scale).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement. The incumbent hegemon sees coordination (Rope) — managing the technical problem of maintaining institutional coherence. Rising powers see mixed coordination and extraction (Tangled Rope) — they benefit from alternatives but pay costs of duplication and arms-race dynamics. The Global South sees pure extraction (Snare) — they bear costs of institutional competition and fragmentation without participating in solution. The Bretton Woods architecture sees its own degradation (Piton) — institutions persist through inertia but lose functional capacity. Emerging multipolar mechanisms see a temporary coordination failure being solved (Scaffold) — new institutions will mature into multipolar equilibrium. The civilizational analyst risks seeing immutable structural realism (Mountain) — multipolarity is inherent to distributed power — but the structural data reveals this as naturalization of contingent design choices. The constraint's core ambiguity is whether multipolarity is inherent or optional: if inherent, institutions must adapt (scaffold perspective valid). If optional (hegemon has choice), then institutional fragmentation is preference-driven extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the multipolar order. Incumbent hegemon: beneficiary status + arbitrage options → low d (approximately 0.15-0.25). They experience the constraint as external coordination problem they manage, not extraction flowing from them. Rising powers: victim status (constrained by existing institutions) + beneficiary status (benefit from alternatives) + constrained exit → moderate d (approximately 0.50-0.60). Mixed structural position produces mixed extraction experience. Global South: victim status (bear costs of fragmentation) + no beneficiary status + trapped exit → high d (approximately 0.85-0.95). They experience maximum extraction relative to any coordination benefit. Bretton Woods institutions: beneficiary status (sunk legitimacy, continued control of resources) + arbitrage options (great powers continue to use them) → low d (approximately 0.20-0.30). Emerging multipolar institutions: victim status (constrained by hegemonic competition for institutional investment) + beneficiary status (represent rising power voice) + constrained exit → moderate d (approximately 0.55-0.65). The analytical observer's structural position is neutral (analytical/analytical) but the mountain classification is vulnerable to false summit detection because it naturalizes contingent choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that multipolarity is neither a pure natural law (mountain) nor a pure extraction mechanism (snare), but a constraint that operates differently depending on which institutional actor one observes from. The coordinate set of all six perspectives reveals that (a) genuine coordination problems exist (multiple great powers must solve shared problems), (b) genuine extraction occurs (institutional competition channels benefits to powerful actors), (c) the distinction between coordination and extraction is observer-relative, and (d) the problem is structurally solvable (alternative institutional designs exist) but design choices are constrained by power distribution. The false summit (mountain from analytical position) occurs when observers naturalize existing institutional fragmentation as inevitable rather than seeing the fragmentation as a specific outcome of choices made by powerful actors. This naturalizing move hides the extraction (global south bears costs) and makes institutional reform appear impossible rather than hard-but-doable. The mandatrophy resolves by showing that institutional redesign is possible but requires negotiating extraction concerns: rising powers want voice redistribution, incumbents want to preserve leverage, and marginalized states want exit from the extraction structure entirely. No single type captures the full picture; the perspectival gap IS the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hegemonic_decline_mechanism,
    'Is institutional fragmentation driven by objective decline of hegemonic capacity, or by rising powers choosing to create alternatives despite hegemonic capacity remaining sufficient to enforce existing institutions?',
    'Comparative analysis of hegemonic enforcement capacity (military spending, economic leverage, institutional control) vs. willingness/costs of rising powers to challenge; examination of whether alternative institutions emerge because they solve problems the incumbent cannot solve, or because rising powers preferentially invest in alternatives',
    'If objective decline: multipolarity is structural decline of hegemon, extraction flows from weakened incumbents. If preference-driven: rising powers actively choose fragmentation, extraction flows from institutional competition. Classification and directionality change accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_decline_mechanism, empirical, 'Whether fragmentation is driven by hegemonic decline or rising power preference').

omega_variable(
    coordination_function_empirical,
    'Do multipolar institutions actually solve shared coordination problems (climate, pandemic, financial stability) or do they primarily serve power distribution signaling with minimal functional coordination?',
    'Measurement of problem-solving capacity: climate emissions reductions tied to IPCC coordination, pandemic response coordination measured by vaccination equity and variant tracking effectiveness, financial stability measured by crisis prevention and contagion containment',
    'If functionally effective: tangled rope classification is accurate; genuine coordination function justifies some extraction overhead. If primarily theatrical: snare classification from global south perspective is stronger; extraction serves power distribution not cooperation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_empirical, empirical, 'Whether multipolar institutions solve functional coordination problems').

omega_variable(
    institutional_competition_race_dynamics,
    'Is institutional proliferation a positive-sum competition that increases choice and reduces hegemon monopoly, or a negative-sum arms race that consumes resources and fragments problem-solving capacity?',
    'Network analysis of institutional overlap and contradiction; measurement of transaction costs for states engaging with multiple institutional frameworks; tracking of institutional lifespan and consolidation patterns',
    'If positive-sum: rising powers'' rope perspective valid; competition increases institutional diversity and responsiveness. If negative-sum: global coordination capacity declines; the constraint becomes extractive for all parties. Directionality and type classifications shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_competition_race_dynamics, empirical, 'Whether institutional competition is positive or negative-sum').

omega_variable(
    theater_ratio_measurement_ambiguity,
    'What proportion of multipolar institutional activity is performative (signaling, legitimacy maintenance) vs. functional (actual decision-making, resource allocation, problem-solving)?',
    'Content analysis of institution outputs: comparison of announced commitments vs. implemented policies; tracking of decision implementation rates and compliance; measurement of institutional budget allocation to implementation vs. procedural overhead',
    'If theater_ratio > 0.70: piton classification becomes dominant; institutions are maintained by inertia. If theater_ratio < 0.40: tangled rope and rope classifications strengthen; genuine functional coordination is present. Current measurement (0.58) places constraint in ambiguous zone between structural and theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_measurement_ambiguity, empirical, 'Proportion of multipolar institutional activity that is performative vs functional').

omega_variable(
    identity_lock_in_hegemon,
    'Does the incumbent hegemon''s institutional behavior reflect structural capacity constraints or identity fusion with the post-1945 architectural framework? Can the hegemon imagine institutional redesign compatible with multipolarity?',
    'Discourse analysis of hegemon policy elites: frequency of multipolarity acknowledgment vs. unipolar nostalgia; institutional reform proposals; willingness to accept voting reforms or power redistribution; examination of whether constraints on reform are material or ideological',
    'If identity_locked: hegemon classified as constrained by internalized post-1945 identity; this explains resistance to institutional reform and preference for extrainstitutional action. If materially constrained: hegemon has objective reasons to preserve existing architecture regardless of identity. This affects whether multipolarity is seen as structural realism (mountain) or contingent institutional design (rope/scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_hegemon, conceptual, 'Whether hegemon is identity-locked to post-1945 architecture or materially constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multipolar_institution_emergence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mpie_tr_t0, multipolar_institution_emergence, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mpie_tr_t10, multipolar_institution_emergence, theater_ratio, 10, 0.54).
narrative_ontology:measurement(mpie_tr_t20, multipolar_institution_emergence, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(mpie_be_t0, multipolar_institution_emergence, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mpie_be_t10, multipolar_institution_emergence, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mpie_be_t20, multipolar_institution_emergence, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multipolar_institution_emergence, enforcement_mechanism).
narrative_ontology:affects_constraint(multipolar_institution_emergence, hegemonic_decline).
narrative_ontology:affects_constraint(multipolar_institution_emergence, global_coordination_failure).
narrative_ontology:affects_constraint(multipolar_institution_emergence, emerging_power_asymmetry).

% DUAL FORMULATION NOTE:
% Multipolar institution emergence can be decomposed into three structurally distinct constraints: (1) hegemonic decline (ε ≈ 0.35, Mountain from realist view, Rope from pragmatist view) — the objective decline of unipolar enforcement capacity; (2) rising power institution-building (ε ≈ 0.48, Tangled Rope) — the extraction mechanism embedded in competitive institution creation; (3) global south exclusion (ε ≈ 0.68, Snare) — pure extraction from fragmentation without participation. This story treats multipolarity as an integrated phenomenon, but the three substrates can be modeled separately with network linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(multipolar_institution_emergence, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
