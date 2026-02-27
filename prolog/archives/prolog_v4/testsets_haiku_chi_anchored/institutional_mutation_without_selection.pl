% ============================================================================
% CONSTRAINT STORY: institutional_mutation_without_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mutation_without_selection, []).

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
 *   constraint_id: institutional_mutation_without_selection
 *   human_readable: The Zombie Bureaucracy Drift
 *   domain: organizational/political
 *
 * SUMMARY:
 *   The Zombie Bureaucracy Drift occurs when an institution's internal goals,
 *   resource allocation, and decision-making processes progressively decouple
 *   from its original mandate, but the institution is insulated from
 *   competitive selection or accountability mechanisms that would normally
 *   force correction. This creates a structural constraint where the
 *   institution persists long after it ceases to serve its stated purpose.
 *   The constraint is 'zombie' because the institution appears functional
 *   from inside (employees coordinate effectively, budgets are allocated,
 *   work continues) and nominally functional from outside (reporting metrics
 *   show compliance), but functionally dysfunctional relative to the original
 *   public mandate. The drift is driven by: (1) absence of exit mechanisms
 *   for the original constituency, (2) diffuse and hard-to-observe internal
 *   mutation, (3) performative accountability (oversight becomes theater),
 *   (4) institutional protection from competitive alternatives, and (5)
 *   career incentives for internal coherence rather than external alignment.
 *   The constraint exhibits an extreme perspectival gap: internal actors
 *   (administrators, mid-level managers) experience the institution as a
 *   well-coordinated system solving real organizational problems, while
 *   external actors (taxpayers, original beneficiaries, oversight bodies)
 *   experience it as extraction shielded by institutional opacity and
 *   political lock-in.
 *
 * KEY AGENTS:
 *   - Original Constituency: Primary victim (powerless/trapped) — citizens or beneficiaries of the institution's original mandate; cannot exit or reallocate resources
 *   - Taxpayers: Primary victim (moderate/trapped) — mandatory funders with no exit option; extraction is diffuse and hard to observe
 *   - Bureaucratic Apparatus: Beneficiary (institutional/arbitrage) — internal actors experience institutional mutation as benign optimization and coordination; they have arbitrage exit (can move to other institutions or private sector)
 *   - Oversight Body: Secondary actor (institutional/constrained) — nominally accountable but reviews have become performative; theater_ratio measures the gap between reporting and reality
 *   - Rival Institutional Actors: Victim (powerful/trapped) — competing institutions or alternative providers cannot displace the zombie due to entrenchment and legal monopolies
 *   - Reform-Minded Coalition: Organized actor (organized/mobile) — agents advocating for institutional restructuring, transparency, and accountability; have partial exit (can lobby, build alternatives) but face structural barriers
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable bureaucratic decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mutation_without_selection, 0.58).
domain_priors:suppression_score(institutional_mutation_without_selection, 0.68).
domain_priors:theater_ratio(institutional_mutation_without_selection, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mutation_without_selection, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mutation_without_selection, snare).
narrative_ontology:human_readable(institutional_mutation_without_selection, "The Zombie Bureaucracy Drift").
narrative_ontology:topic_domain(institutional_mutation_without_selection, "organizational/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(institutional_mutation_without_selection, public_beneficiaries_of_original_mandate).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, taxpayers).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, oversight_bodies).
narrative_ontology:constraint_victim(institutional_mutation_without_selection, rival_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL CONSTITUENCY (SNARE) — Citizens, users, or beneficiaries of the institution's original mandate have no exit. They cannot defund, replace, or remove the institution through normal democratic mechanisms. The institution's mutation away from its original purpose traps them in a structure nominally serving them but actually serving institutional self-preservation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAXPAYER BASE (SNARE) — Mandatory funders with no exit option. The institution extracts resources while internal mutation decouples resource allocation from stated mission. Taxpayers cannot reallocate funds without political consensus that is structurally difficult to achieve because the institution's mutation is diffuse, unobservable, and framed as operational adjustment rather than drift. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BUREAUCRATIC APPARATUS (ROPE) — Internal actors (administrators, mid-level managers, departmental heads) experience institutional mutation as pure coordination: aligning resources, goals, and reporting structures within the organization. The mutation enables internal efficiency and reduces cognitive dissonance between internal goals and actual resource flows. From inside, this is benign organizational optimization. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OVERSIGHT BODY (PITON) — Nominally accountable to the original constituency (legislators, audit boards, inspector general), the oversight body's review rituals have become performative. Formal reporting demonstrates compliance with stated mission while actual institutional drift proceeds unobserved. The review process becomes theater: metrics are gamed, reports are decoupled from operations, and audits certify alignment with a mission that no longer governs resource allocation. theater_ratio=0.81 satisfies piton gate. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RIVAL INSTITUTIONAL ACTORS (SNARE) — Other institutions (competing agencies, private sector alternatives, NGO sectors) that might offer superior service delivery are structurally prevented from displacing the zombie bureaucracy. Institutional entrenchment, legal monopolies, political lock-in, and path-dependent funding prevent market or competitive selection. Rivals cannot exit from the constraint that shields zombies from selection pressure. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM-MINDED COALITION (TANGLED ROPE) — Organized agents (audit reformers, transparency advocates, institutional restructuring movements) see both coordination and extraction. The constraint has a coordination function: it does enable the bureaucracy to function coherently internally. But it also extracts: the institution's mutation away from public mandate enables self-serving resource capture disguised as operational adjustment. The coalition has partial exit (can lobby, litigate, build alternatives) but faces institutional inertia. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational view, institutional drift and mutation without selection could appear as an immutable feature of bureaucratic systems: 'All organizations drift from their original purpose over time; this is inevitable.' However, the structural data (ε=0.58, suppression=0.68, theater=0.81) contradicts mountain thresholds. The engine will classify this as a false summit — what appears to be a natural law is actually a contingent institutional arrangement (absence of selection mechanisms, weak accountability, information asymmetry). The naturalizing framing itself is part of the extraction mechanism.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_without_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mutation_without_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mutation_without_selection, TR),
    TR >= 0.70.

:- end_tests(institutional_mutation_without_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The institution extracts resources (funding, political priority, regulatory authority) while progressively misallocating them away from the original mandate. The extraction is not maximal (0.66+) because some institutional functions do continue to serve legacy purposes, and the mutation is incremental rather than total capture. But the trajectory is clearly extractive — over the 30-year interval, base extractiveness rises from 0.15 (low initial drift) to 0.58 (moderate extraction of tax resources for internal purposes). Suppression (0.68): High. The mechanism that shields the institution from selection pressure is strong: legal monopolies prevent competition, political lock-in prevents defunding, and the mutation is diffuse enough to escape clear accountability. Taxpayers and the original constituency have no credible exit threat. Theater ratio (0.81): Very high. The constraint's persistence despite widespread awareness of drift is maintained through performative accountability: annual reports show compliance with stated mission, audits certify proper procedures, and metrics are gamed to demonstrate alignment. The theater has increased over time as the gap between stated mission and actual operations has widened — more theater is needed to maintain the illusion of coherence. This high theater_ratio combined with moderately high extractiveness creates a Piton-like institutional structure: the zombie persists not because it's still functional but because it's theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates an extreme gap between internal and external perspectives. From inside the bureaucracy, institutional mutation is experienced as rational organizational adjustment: departments optimize their processes, reporting structures align with actual workflow, and resource allocation moves toward sustainable patterns. This is the Rope perspective — pure coordination solving real organizational problems. From outside, the same changes are experienced as drift: the institution's original purpose is progressively abandoned, accountability to the external mandate weakens, and resources flow toward institutional self-preservation rather than public benefit. This is the Snare perspective — extraction shielded from selection. The oversight body's Piton perspective shows that accountability mechanisms exist formally but have become theater: reviews happen, reports are filed, metrics are reported, yet none of this prevents the drift because the theater is decoupled from operational reality. The reform coalition's Tangled Rope perspective recognizes both aspects: the institution does solve internal coordination problems (rope function) but uses those coordination capabilities to insulate itself from external accountability (extraction function). The analytical observer risks naturalizing this as inevitable bureaucratic decline (false Mountain) when it is actually a contingent institutional arrangement that could be disrupted by genuine selection mechanisms (restructuring, defunding, replacement).
 *
 * DIRECTIONALITY LOGIC:
 *   Original constituency: Victim + trapped → d≈0.95, f(d)≈1.42, χ≈0.82. Maximum extraction because this group cannot exit and bears the full cost of mission drift. Taxpayers: Victim + trapped → d≈0.92, f(d)≈1.38, χ≈0.80. Mandatory funders with no exit and no control over resource allocation. Bureaucratic apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11, χ≈-0.06. Internal actors benefit from institutional coherence and have arbitrage exit; they experience the constraint as coordination, not extraction. Oversight body: Constrained institutional → d≈0.42, f(d)≈0.42, χ≈0.24. Nominally powerful but actually constrained by institutional inertia and the difficulty of observable institutional drift; Piton classification comes from theater gate rather than from derivation chain. Rival institutions: Victim + trapped (powerful) → d≈0.70, f(d)≈1.08, χ≈0.63. Despite institutional power, rivals cannot exit from the constraint that shields the zombie from competitive selection. Reform coalition: Organized + mobile → d≈0.55, f(d)≈0.75, χ≈0.44. Organized agents have partial exit (can lobby, litigate, build alternatives) and see mixed coordination/extraction; Tangled Rope classification reflects the coalition's genuine ability to see both aspects of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The zombie bureaucracy drift resolves mandatrophy by showing that a constraint can be simultaneously a Snare (from the perspective of victims), a Rope (from the perspective of internal coordinators), and a Piton (from the perspective of degraded oversight). The constraint is NOT all six types — Mountain is explicitly a false summit (the drift is contingent, not inevitable), and Scaffold does not apply (there is no sunset clause, only indefinite persistence). But the constraint is legitimately Snare, Rope, Tangled Rope, and Piton across different structural positions. The mandatrophy is resolved by recognizing that: (1) the internal Rope experience is real but structurally distinct from the external Snare experience; (2) the Piton classification is not a degradation of Snare but a separate structural phenomenon (theater replacing function); (3) the false Mountain is explicitly rejected as a false summit that naturalizes contingent arrangements. The high extractiveness (0.58) combined with high theater (0.81) creates a Snare-Piton hybrid: the institution extracts through suppression of external accountability AND through theatrical performance that maintains internal legitimacy. This is not a contradiction — it is a complete structural picture of how institutional mutation persists without selection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_mechanism_failure_mode,
    'Is institutional mutation without selection driven by absence of selection mechanisms (Snare causality) or by selection mechanisms that have failed/been captured (Piton causality)?',
    'Historical analysis: do reform attempts fail due to structural impossibility or due to deliberate sabotage/institutional inertia? Can oversight be re-empowered (Piton) or are structural barriers inherent (Snare)?',
    'If absence: institution is a true Snare — victims cannot exit and extraction is structural. If capture: institution is a Piton — degraded oversight can be revived, making this Scaffold-like. Classification shifts from pure Snare toward hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selection_mechanism_failure_mode, empirical, 'Whether selection failure is structural or due to captured oversight').

omega_variable(
    mutation_observability_threshold,
    'At what threshold does internal institutional mutation become visible to external observers, triggering accountability mechanisms?',
    'Comparative analysis: how much drift can occur before: (a) budget audits detect it, (b) performance metrics diverge from stated goals sufficiently to trigger political attention, (c) whistleblowers or reform campaigns achieve visibility?',
    'If threshold is high (mutation can proceed far before detection): Snare classification is robust. If threshold is low: institution has weak suppression, more Rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutation_observability_threshold, empirical, 'Mutation detection threshold before accountability triggers').

omega_variable(
    beneficiary_concealment_mechanism,
    'Who, if anyone, is the actual beneficiary of the institutional mutation? (Is it senior leadership? A captured client? Distributed inertia?)',
    'Resource flow analysis: trace funding, personnel allocation, and decision-making over the mutation interval. Identify who has gained relative to original mandate.',
    'If beneficiary is identifiable (senior leadership, captured client class): this is Snare with clear extraction. If beneficiary is diffuse (organizational inertia, risk-averse career incentives): this is Piton (no clear extraction agent, just theatrical persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concealment_mechanism, empirical, 'Identity and intentionality of mutation beneficiaries').

omega_variable(
    escape_route_viability,
    'Can the original constituency actually exit if they achieve political consensus (e.g., defund, restructure, replace the institution)?',
    'Legal and political structural analysis: are there formal mechanisms to dissolve, reform, or replace? Or are there legal barriers, sunk costs, or path-dependent lock-in that make exit theoretically possible but practically infeasible?',
    'If genuine exit is possible: this is a Snare with weak selection mechanisms (classification confirmed). If exit is legally impossible or prohibitively costly: this is closer to Mountain — the constraint is structural, not just institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_route_viability, conceptual, 'Whether exit from the constraint is structurally possible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_without_selection, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zombi_tr_t0, institutional_mutation_without_selection, theater_ratio, 0, 0.25).
narrative_ontology:measurement(zombi_tr_t15, institutional_mutation_without_selection, theater_ratio, 15, 0.55).
narrative_ontology:measurement(zombi_tr_t30, institutional_mutation_without_selection, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(zombi_be_t0, institutional_mutation_without_selection, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(zombi_be_t15, institutional_mutation_without_selection, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(zombi_be_t30, institutional_mutation_without_selection, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mutation_without_selection, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, regulatory_capture).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, public_sector_performance_metrics_gaming).
narrative_ontology:affects_constraint(institutional_mutation_without_selection, institutional_path_dependency).

% DUAL FORMULATION NOTE:
% The zombie bureaucracy drift is a specific manifestation of broader constraints: regulatory capture (external beneficiary distorts institution toward their interests), performance metrics gaming (theater masquerades as accountability), and path dependency (historical lock-in prevents exit from drifted institution). Each of these has distinct ε and structural relationships, but all three are downstream of the absence of genuine selection mechanisms that would force institutional correction. The zombie bureaucracy constraint is the meta-level observation that institutional mutation persists when selection is absent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_mutation_without_selection, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
