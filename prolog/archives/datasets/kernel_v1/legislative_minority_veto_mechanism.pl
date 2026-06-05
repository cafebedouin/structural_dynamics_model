% ============================================================================
% CONSTRAINT STORY: legislative_minority_veto_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legislative_minority_veto_mechanism, []).

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
 *   constraint_id: legislative_minority_veto_mechanism
 *   human_readable: Legislative Minority Veto Mechanism
 *   domain: political/governance
 *
 * SUMMARY:
 *   Legislative minority veto mechanisms — supermajority requirements,
 *   filibuster rules, absolute veto powers held by numerically small
 *   coalitions — create a structural constraint where the ability to block
 *   policy becomes a form of extraction. Originally designed as a protection
 *   against tyranny-of-the-majority and a coordination device ensuring broad
 *   consensus on transformative policies, veto mechanisms evolve into
 *   extractive instruments when the minority holding veto power begins to use
 *   veto authority to extract concessions, funding, or favorable policy
 *   positions worth far more than their proportional representation would
 *   justify. This constraint exhibits a sharp perspectival gap between the
 *   majority (who experience snare classification) and the minority veto
 *   holder (who experience tangled rope — coordination benefit plus
 *   asymmetric extraction). The temporal trajectory shows rising
 *   extractiveness and suppression requirement over the measurement interval,
 *   indicating that as minority coalitions stabilize and learn to weaponize
 *   veto authority, the extraction mechanism becomes more efficient and the
 *   suppression floor (the costs imposed on the majority to maintain their
 *   subordination to veto rules) increases. The theater ratio remains
 *   moderate because the veto mechanism retains some genuine coordination
 *   function — it does prevent swift majoritarian overreach — but an
 *   increasing portion of legislative activity becomes performative
 *   obstruction rather than functional coordination.
 *
 * KEY AGENTS:
 *   - Majority Constituency: Primary victim (powerless/trapped) — numerically large but structurally subordinated; electoral preference cannot translate to policy without minority consent
 *   - Minority Veto Coalition: Primary beneficiary (powerful/constrained) — numerically small but structurally powerful; extracts concessions and agenda control far exceeding proportional representation
 *   - Constitutional Framework: Institutional arbiter (institutional/arbitrage) — maintains neutral coordination rule; perspective sees pure rope function, not extraction
 *   - Historically Disadvantaged Coalition: Secondary victim and partial beneficiary (organized/constrained) — may hold veto power for group protection, creating mixed extraction and coordination benefit
 *   - Constitutional Reform Movement: Organized agent (organized/constrained) — sees sunset clause opportunity; treats current mechanism as temporary institutional stage awaiting revision
 *   - Legislative Theater Institution: Institutional actor (institutional/arbitrage) — maintains performative obstruction rituals; sees own veto function as degraded through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as immutable structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legislative_minority_veto_mechanism, 0.58).
domain_priors:suppression_score(legislative_minority_veto_mechanism, 0.62).
domain_priors:theater_ratio(legislative_minority_veto_mechanism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legislative_minority_veto_mechanism, tangled_rope).
narrative_ontology:human_readable(legislative_minority_veto_mechanism, "Legislative Minority Veto Mechanism").
narrative_ontology:topic_domain(legislative_minority_veto_mechanism, "political/governance").

domain_priors:requires_active_enforcement(legislative_minority_veto_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legislative_minority_veto_mechanism, minority_coalition_holders).
narrative_ontology:constraint_beneficiary(legislative_minority_veto_mechanism, ideological_gatekeepers).
narrative_ontology:constraint_victim(legislative_minority_veto_mechanism, majority_constituent_preferences).
narrative_ontology:constraint_victim(legislative_minority_veto_mechanism, policy_implementation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAJORITY CONSTITUENCY (SNARE) — Numerically large but structurally powerless; their electoral preference cannot translate into policy without minority consent. Trapped by constitutional rules that subordinate majority will to supermajority thresholds. Bears full extraction cost: delayed or blocked policies, agenda control by the minority, forced compromises that dilute their preferences.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY VETO COALITION (TANGLED ROPE) — Numerically small but structurally powerful; veto power provides genuine coordination benefit (any major policy requires cross-coalition agreement, preventing swift majoritarian overreach) alongside asymmetric extraction (the minority extracts concessions, fund allocation, and agenda control worth far more than their proportional representation). Constrained by eventual majority backlash or electoral realignment, but holds temporary structural advantage.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL FRAMEWORK (ROPE) — Pure coordination mechanism from the framers' perspective. Supermajority thresholds and veto rules exist to solve the collective action problem of transformative policy: requiring consensus ensures legitimacy and stability. No inherent extraction — the framework itself is neutral, enabling coordination across ideological divides.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HISTORICALLY DISADVANTAGED COALITION (TANGLED ROPE) — For coalitions that were historically marginalized (e.g., regional minorities, ethnic groups), veto power provides both coordination benefit (protection from majoritarian exclusion) and mixed extraction (they extract protection from hostile majorities but may also extract targeted benefits). The extraction is more muted than for simple ideological minorities because the stakes include group survival and equal citizenship.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized agents pushing to modify supermajority thresholds, reduce filibuster scope, or implement alternative structures (e.g., simple majority with sunset clauses, proportional veto to representation size) see the veto mechanism as a temporary problem with a sunset clause embedded in democratic evolution. Reform movements treat the current mechanism as a transitional institutional stage awaiting revision, not as permanent law.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE THEATER RITUAL (PITON) — From civilizational/institutional perspective, the veto mechanism has degraded into performative obstruction. The original coordination function (ensuring broad consensus on transformative policies) persists in narrative but operates at low functional capacity. Supermajority requirements nominally ensure legitimacy, but they now primarily delay inevitable passage or prevent revision of prior majoritarian commitments. The mechanism persists through institutional inertia — changing veto rules is itself subject to supermajority veto, creating a recursive enforcement loop.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, veto mechanisms may appear as an immutable structural requirement of democratic governance: any system coordinating multiple groups must give each group exit capacity (veto) or risk instability. This perspective risks naturalizing the contingent institutional arrangement as a law of political physics. However, empirical counterexamples (democracies functioning with simple-majority rule, supermajority thresholds varying by policy domain rather than universal application) reveal this as a false summit.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legislative_minority_veto_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legislative_minority_veto_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legislative_minority_veto_mechanism, TR),
    TR >= 0.70.

:- end_tests(legislative_minority_veto_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The minority veto holder captures substantial extraction value during periods when the minority coalition is stable and unified. The extraction manifests as forced policy concessions, disproportionate resource allocation, and veto over transformative policy. However, extraction is not maximal (ε ≥ 0.66 for snares) because the veto mechanism retains genuine coordination function — it does prevent swift majoritarian overreach without process or consultation. The temporal trajectory (0.35 → 0.58 over 30 years) reflects learning effects: as minority coalitions stabilize and develop sophisticated veto strategies (obstruction, brinkmanship, packaged demands), the extraction mechanism becomes more efficient. Suppression (0.62): High. The majority faces substantial suppression: constitutional requirement to obtain supermajority support, electoral system features (geographic dispersion, two-party system) that amplify minority power, and the recursive difficulty of amending veto rules themselves (requiring supermajority vote). Suppression has risen (0.45 → 0.62) as minority coalitions have stabilized and become more cohesive, reducing opportunities for coalition defection. Theater ratio (0.48): Moderate, showing some functional degradation. The veto mechanism was designed as a coordination device with real function — preventing hasty majority action without consensus. But over 30 years, an increasing portion of veto usage becomes performative obstruction (threatening filibuster without genuine policy concern, ritualized brinkmanship, extractive obstruction unrelated to coordination failure) rather than genuine coordination signals. The mechanism persists because changing it requires the very supermajority vote the mechanism protects.
 *
 * PERSPECTIVAL GAP:
 *   The majority sees snare; the minority sees tangled rope. This gap is not disagreement about the mechanism's function but structural divergence: same rule, opposite experiences. The majority's electoral preference is subordinated; the minority's negotiating power is amplified. The constitutional framework's neutral perspective enables this gap to persist: the rule is formally impartial, but distributive effects diverge sharply. The reform movement's scaffold perspective offers a third view: this is a temporary stage, solvable through constitutional amendment (difficult but not impossible). The legislative institution's piton view suggests that the mechanism persists through inertia despite reduced function — veto rules are maintained because changing them is itself subject to veto. The analytical observer's mountain view risks the greatest error: naturalizing what is actually a contingent institutional choice as an immutable law of democratic governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations plus exit options. Majority constituency: victims + trapped → d = 0.95 → high f(d). Minority veto coalition: beneficiaries + constrained → d = 0.45 → moderate f(d). Constitutional framework: neutral arbiter + arbitrage → d = 0.50 → baseline f(d). Historically disadvantaged coalition: partial beneficiary/partial victim + constrained → d = 0.65 → moderate-high f(d). Reform movement: organized agents + constrained → d = 0.40 → baseline f(d). Legislative institution: beneficiary (maintains power structure) + arbitrage → d = 0.35 → low-moderate f(d). Analytical observer: analytical position + analytical exit → d = 0.72 → canonical analytical f(d). No directionality overrides needed: derivation chain captures structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC OF PERSPECTIVAL PLURALITY: This constraint resolves the mandatrophy by demonstrating that all six types are legitimate readings from different structural positions. The question 'what type is the veto mechanism?' has no single answer — it depends on whether you are the majority (snare), the minority (tangled rope), the constitutional arbiter (rope), the reform advocate (scaffold), the institutional performer (piton), or the civilizational analyst (at risk of mountain). The mandatrophy is resolved by recognizing that the constraint is a **presheaf over observation positions**: each perspective represents a genuine structural reality, not a measurement error or disagreement about facts. The majority truly is trapped; the minority truly does enjoy mixed coordination-extraction; the framework truly is neutral; the reform truly sees a sunset opportunity. The analytical observer's mountain classification is the only one at risk — the risk is not that the perspective is wrong but that it naturalizes contingent institutional design as immutable law. The false summit detector should flag this: the mountain perspective will have beneficiaries (the minority veto holder) and victims (the majority), structural data that contradicts genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_coordination_vs_extraction_threshold,
    'At what supermajority threshold does the veto mechanism transition from genuine coordination (preventing tyranny) to extractive obstruction (enabling minority capture)?',
    'Empirical analysis of policy gridlock rates by supermajority threshold; correlation between veto power and extracted concessions vs. baseline representation share; cross-country comparison of legislative productivity under different threshold regimes',
    'If threshold is strict (e.g., 66% supermajority shows high extraction): lower thresholds (55%) might preserve coordination while reducing extraction. If threshold is permissive (simple majority + constitutional veto shows high extraction): the problem is not the numeric threshold but the scope of veto authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_coordination_vs_extraction_threshold, empirical, 'Numeric threshold separating coordination from extraction in veto mechanisms').

omega_variable(
    minority_coalition_stability,
    'Is the minority veto coalition stable across electoral cycles, or is it a temporary artifact of current electoral alignment?',
    'Historical analysis of coalition composition over 3+ electoral cycles; prediction models for coalition persistence under demographic and preference drift; measurement of defection rates within the minority coalition',
    'If coalition is stable: the minority has genuine structural power and the extraction can persist long-term. If coalition is temporary: the veto mechanism transfers extraction power to whichever coalition becomes the numerical minority after realignment, perpetuating the dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_coalition_stability, empirical, 'Whether minority veto coalitions persist across electoral cycles').

omega_variable(
    alternative_coordination_sufficiency,
    'Can alternative coordination mechanisms (cross-party committees, supermajority requirement only for specific policy domains, sunset clauses requiring re-approval) achieve the coordination benefit of veto mechanisms without enabling extraction?',
    'Comparative analysis of governance outcomes in jurisdictions with alternative mechanisms; case studies of policy domains with narrower veto scope; pilot programs implementing alternative thresholds',
    'If alternatives achieve coordination: veto mechanism is not a structural necessity but a design choice, and the extraction is eliminable. If alternatives fail: veto mechanism is necessary for stability, but the extraction cost may be the price of avoiding tyranny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether alternative coordination mechanisms can replace veto mechanisms').

omega_variable(
    constitutional_amendment_paradox,
    'Does requiring a supermajority vote to amend the veto mechanism itself create a recursive lock — the veto power to prevent veto reform?',
    'Historical analysis of constitutional amendment attempts targeting veto rules; examination of whether successful amendments required extraordinary political circumstances (war, economic crisis) that reduced effective veto power temporarily',
    'If recursive lock is operative: the veto mechanism becomes self-protecting and effectively immutable, increasing its classification severity toward pure snare for the majority. If amendments are possible through normal political processes: the mechanism retains some democratic responsiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_paradox, empirical, 'Whether supermajority amendment rules create recursive self-protection of veto mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legislative_minority_veto_mechanism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmv_tr_t0, legislative_minority_veto_mechanism, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lmv_tr_t15, legislative_minority_veto_mechanism, theater_ratio, 15, 0.4).
narrative_ontology:measurement(lmv_tr_t30, legislative_minority_veto_mechanism, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(lmv_be_t0, legislative_minority_veto_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lmv_be_t15, legislative_minority_veto_mechanism, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(lmv_be_t30, legislative_minority_veto_mechanism, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lmv_su_t0, legislative_minority_veto_mechanism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lmv_su_t15, legislative_minority_veto_mechanism, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(lmv_su_t30, legislative_minority_veto_mechanism, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legislative_minority_veto_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(legislative_minority_veto_mechanism, electoral_system_amplification).
narrative_ontology:affects_constraint(legislative_minority_veto_mechanism, two_party_system_duopoly).
narrative_ontology:affects_constraint(legislative_minority_veto_mechanism, constitutional_amendment_immutability).

% DUAL FORMULATION NOTE:
% Legislative minority veto operates at the intersection of formal constitutional rules and practical political dynamics. The base extractiveness (0.58) captures the mechanism's asymmetric power distribution. Upstream constraints (two-party system, electoral geography) amplify veto holder power by reducing coalition diversity and increasing minority cohesion. Downstream constraints (constitutional amendment rules requiring supermajority) create recursive self-protection of the veto mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
