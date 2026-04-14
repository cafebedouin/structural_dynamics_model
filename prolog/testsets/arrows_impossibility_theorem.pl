% ============================================================================
% CONSTRAINT STORY: arrows_impossibility_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arrows_impossibility_theorem, []).

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
 *   constraint_id: arrows_impossibility_theorem
 *   human_readable: Arrow's Impossibility Theorem (as a political justification)
 *   domain: political/economic
 *
 * SUMMARY:
 *   Arrow's Impossibility Theorem, published in 1951, proves that no voting
 *   system can simultaneously satisfy five axioms: unrestricted domain,
 *   non-dictatorship, Pareto efficiency, independence of irrelevant
 *   alternatives, and collective rationality. This mathematical result has
 *   become a primary political justification for maintaining existing voting
 *   systems and dismissing electoral reform proposals. The constraint
 *   operates as a Tangled Rope: it serves a genuine coordination function
 *   (allowing all parties to agree that 'no perfect system exists') while
 *   simultaneously extracting political value by legitimizing inaction and
 *   suppressing alternatives. The theater ratio (0.68) reflects the
 *   increasing performative invocation of Arrow in policy discourse detached
 *   from the mathematical nuances — the theorem is cited as immutable law
 *   while academic work on voting systems has moved far beyond it (liquid
 *   democracy, participatory budgeting, preference-intensity models). The
 *   extractiveness (0.52) reflects moderate asymmetric costs: beneficiaries
 *   (incumbent coalitions) capture the suppression of reform while bearing no
 *   cost of system flaws; victims (disenfranchised preference groups,
 *   collective choice accuracy) pay through reduced representation and policy
 *   suboptimality. The suppression (0.65) is substantial but not total —
 *   voting reform movements have achieved real wins at municipal scale,
 *   demonstrating that the arrow theorem does not mandate the status quo,
 *   only that any system must relax at least one axiom.
 *
 * KEY AGENTS:
 *   - Incumbent Political Coalition: Primary beneficiary (institutional/arbitrage) — leverages Arrow theorem to dismiss reform, consolidates power under current system, experiences constraint as pure coordination tool
 *   - Disenfranchised Preference Groups: Primary victim (powerless/trapped) — excluded or near-zero-weighted by current voting system, cannot exit, bear suppression of reform alternatives
 *   - Electoral Reform Movement: Organized secondary actor (organized/constrained) — seeks proportional representation, ranked-choice voting, and participatory budgeting; sees Arrow as temporary constraint with sunset path
 *   - Electoral Administration Institution: Secondary institutional actor (institutional/arbitrage) — coordinates voting but locked into defending current system, uses Arrow to justify path dependence
 *   - Academic Mathematical Community: Authority source (institutional/arbitrage) — produces theorems and extensions to Arrow framework but is disengaged from political application; community has moved beyond pure Arrow work but original result remains frozen in policy discourse
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a mathematical axiom choice as an immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arrows_impossibility_theorem, 0.52).
domain_priors:suppression_score(arrows_impossibility_theorem, 0.65).
domain_priors:theater_ratio(arrows_impossibility_theorem, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arrows_impossibility_theorem, extractiveness, 0.52).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arrows_impossibility_theorem, tangled_rope).
narrative_ontology:human_readable(arrows_impossibility_theorem, "Arrow's Impossibility Theorem (as a political justification)").
narrative_ontology:topic_domain(arrows_impossibility_theorem, "political/economic").

domain_priors:requires_active_enforcement(arrows_impossibility_theorem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, incumbent_political_coalition).
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, voting_system_administrators).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, disenfranchised_preference_groups).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, collective_choice_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED MINORITY (SNARE) — Powerless voters whose preferences are structurally excluded or weighted near-zero by the chosen voting system. The Arrow theorem is invoked to justify why no better system exists, trapping them in the current mechanism. No exit option: they cannot adopt an alternative voting system unilaterally. Maximum extraction — their voice is suppressed by a 'necessity' that is actually institutional choice.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-SEEKING COALITION (TANGLED ROPE) — Moderate power agents seeking ranked-choice voting or proportional representation. They benefit from the Arrow theorem as a conversation tool (coordination function: 'we all know no perfect system exists, so let's discuss tradeoffs') but are also trapped by it (extraction function: the invocation of Arrow in policy discourse legitimizes inaction and suppresses voting system reform). Asymmetric extraction — they bear the cost of gridlock while benefiting from the vocabulary of the discussion.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT POLITICAL COALITION (ROPE) — Institutional actors (majority party, sitting legislators) benefit from the current voting system by design. Arrow's theorem is their primary coordination tool: it allows them to claim that no alternative system is 'better' and to dismiss reform proposals as theoretically naive. They experience the constraint as pure coordination: leveraging Arrow legitimizes the status quo through mathematical authority. Net beneficiary position.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC MATHEMATICAL COMMUNITY (PITON) — Possesses the authoritative voice on Arrow's theorem but is largely disengaged from its political application. The theorem is cited extensively by non-specialists; the mathematics community has moved beyond Arrow to richer preference frameworks (preference intensities, liquid democracy, quadratic voting), but the original theorem remains deployed in policy discourse as an immutable law. The academic authority is maintained performatively through constant citation despite technical obsolescence. Theater ratio driven by the gap between sophisticated current work (liquid democracy, participatory budgeting) and the frozen invocation of 1950s result.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VOTING REFORM MOVEMENT (SCAFFOLD) — Organized agents (local electoral commissions, voting reform nonprofits, participatory democracy practitioners) are creating alternative voting mechanisms at scale: ranked-choice voting in municipalities, proportional representation experiments, liquid democracy pilots. These experiments demonstrate that Arrow's theorem does not mandate the current system — it only mandates that no voting system satisfies ALL of Arrow's five axioms simultaneously. The reform movement sees a sunset path: as alternatives prove functional in practice, the invocation of Arrow loses its suppressive force. Sunset timeline: 15-25 years as municipal experiments accumulate evidence.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MATHEMATICAL IMPOSSIBILITY (MOUNTAIN) — From a civilizational/universal perspective, Arrow's theorem is presented as a natural law: no voting system can simultaneously satisfy the five axioms (unrestricted domain, non-dictatorship, Pareto efficiency, independence of irrelevant alternatives, collective rationality). This perspective treats the mathematical result as immutable — any actual voting system must violate at least one axiom. However, the structural data contradicts the mountain classification. The axioms are chosen premises, not natural laws. The theorem is a logical result conditioned on accepting the five axioms. This is a false summit — mathematical truth is being naturalized to justify political inaction.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ELECTORAL ADMINISTRATION INSTITUTION (TANGLED ROPE) — Administrators of voting systems (election commissions, election officials) coordinate voters and tabulate results (Rope function) but also extract lock-in costs through system switching resistance. They benefit from Arrow invocation because it justifies path-dependent inaction ('changing systems is theoretically impossible'). They bear costs from complexity mismanagement ('we have to operate a system we know is flawed'). Asymmetric: they have arbitrage exit (they can propose reform) but use Arrow as justification for constraining it.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arrows_impossibility_theorem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arrows_impossibility_theorem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arrows_impossibility_theorem, TR),
    TR >= 0.70.

:- end_tests(arrows_impossibility_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent coalition captures real political value by invoking Arrow to suppress voting system reform. The extraction is not complete (ε not ≥ 0.66) because alternative voting systems do function at municipal scale, and the intellectual authority of Arrow is eroding as alternatives accumulate evidence. The constraint's extractive power is declining — it is working harder (theater ratio rising) to accomplish less (extractiveness modest). Suppression (0.65): Moderate-high. The invocation of Arrow creates substantial barriers to voting reform: any proposal faces the reflexive objection 'Arrow proved no system is better.' Negative framing of alternatives ('no perfect system') discourages experimentation. But suppression is not total — municipalities are adopting ranked-choice voting and proportional representation despite Arrow invocation. Theater ratio (0.68): Increasing. Over the 50-year interval, the political use of Arrow has become increasingly detached from mathematical content. The theorem is cited in policy discourse but without engagement with relaxed-axiom systems, participatory budgeting, liquid democracy, or the contingency of the axiom choices themselves. The performative invocation of Arrow increases while its actual intellectual justification weakens — classic Goodhart drift.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent coalition sees pure Rope (coordination utility: 'we all accept that no system is perfect'). The disenfranchised minority sees pure Snare (suppression: 'the theorem says reform is impossible'). The reform movement sees Scaffold with a real sunset (experiments in Maine, New Zealand, and Ireland show alternatives work; as they scale, Arrow loses suppressive force). The academic community sees itself as irrelevant (Piton — the original result is performatively cited but the field has moved to richer axiomatizations). The electoral administration institution sees Tangled Rope (coordination: running elections, extraction: locked into defending a flawed system). The civilizational analytical observer risks seeing Mountain (inevitability: 'no voting system can satisfy all axioms') but the structural data reveals this as a false summit — the axioms are chosen premises, not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (incumbent coalition, electoral administrators) experience low directionality (d ≈ 0.15–0.25) because they have arbitrage exit options and the constraint extraction flows toward them. The victims (disenfranchised groups) experience high d (d ≈ 0.85–0.95) because they are trapped and extraction flows away from them. The reform movement (organized/constrained) experiences intermediate d (d ≈ 0.55–0.65) because they have some agency but constrained exit. The academic community's d is negligible (d ≈ 0.05–0.15, institutional/arbitrage) — they produce the theorem but bear no political cost or benefit. The analytical observer (d ≈ 0.72) sees the full structure but risks internalizing the false summit framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Arrow's theorem IS a coordination mechanism (all parties can agree on its premise) AND an extraction mechanism (the incumbent coalition uses the impossibility conclusion to suppress reform). This is the canonical Tangled Rope structure: genuine coordination function co-resident with asymmetric extraction. The false summit (mountain classification from the analytical observer) is detected by the schema conditional: if claiming mountain, accessibility_collapse must ≥ 0.85, resistance ≤ 0.15, and emerges_naturally must be true. Arrow's theorem is mathematically sound but not a natural law — its five axioms are chosen premises, and the impossibility is conditional on accepting them. The accessibility_collapse of voting systems as a constraint is not ≥ 0.85 — alternatives exist and function. The resistance to alternatives is institutional inertia (0.65), not a fundamental impossibility (0.15). The false summit detection system flags this perspective as naturalization of a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_selection_contingency,
    'Are Arrow''s five axioms the unique set of mathematically necessary conditions for voting systems, or are they one particular choice among multiple defensible axiom sets?',
    'Literature review of alternative axiomatizations (liquid democracy axioms, participatory budgeting axioms, preference-intensity axioms); comparison of logical necessity vs. conventional choice for each axiom',
    'If axioms are contingent choices: Arrow''s theorem is a limited result applicable only to systems accepting those specific axioms. The invocation of impossibility becomes a deliberate constraint selection, not a discovery of natural law. If axioms are necessary: the theorem is broader and the natural law framing is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_selection_contingency, conceptual, 'Whether Arrow''s axioms are uniquely necessary or contingent choices').

omega_variable(
    practical_system_performance,
    'Do ranked-choice voting, approval voting, and proportional representation systems actually produce empirically better outcomes on measures like representativeness, voter satisfaction, and consensus-building compared to first-past-the-post?',
    'Comparative analysis of electoral outcomes in jurisdictions with different voting systems (Maine, Ireland, New Zealand, Belgium); voter satisfaction surveys; policy diversity metrics; coalition stability',
    'If alternatives perform better: Arrow invocation becomes pure rationalization (''we know a better system exists, but the theorem says none does''). If alternatives show similar or worse performance: Arrow invocation gains epistemic weight as a genuine theoretical constraint on improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_system_performance, empirical, 'Empirical performance of alternative voting systems compared to FPTP').

omega_variable(
    incumbent_advantage_magnitude,
    'How much of the political coalitioninvocation of Arrow is motivated by genuine belief in mathematical impossibility vs. motivated reasoning to protect incumbent advantage in the current system?',
    'Discourse analysis: comparison of Arrow citations when current system favors the speaker vs. when it disadvantages them; behavior change after electoral power shifts; comparative rhetoric across parties and policy debates',
    'If genuine: the constraint is a true intellectual binding (though false summit remains possible). If motivated: the constraint is primarily extractive rationalization, and suppression is intentional rather than believed-necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_advantage_magnitude, empirical, 'Extent of motivated reasoning in Arrow invocation by incumbent coalitions').

omega_variable(
    axiom_relaxation_feasibility,
    'Which of Arrow''s five axioms can be relaxed without creating catastrophic outcomes, and which relaxations produce better real-world voting performance?',
    'Literature review of relaxed-axiom voting systems (removal of IIA, collective rationality, etc.); empirical testing of these systems in municipal voting experiments; comparison of outcome quality metrics',
    'If most axioms can be relaxed productively: Arrow''s result is a boundary condition, not a fundamental limit. The framing of inevitability collapses. If relaxations create worse outcomes: Arrow''s theorem gains practical validity despite being a false summit mathematically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_relaxation_feasibility, empirical, 'Feasibility and outcomes of relaxing Arrow''s axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arrows_impossibility_theorem, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arrow_tr_t0, arrows_impossibility_theorem, theater_ratio, 0, 0.4).
narrative_ontology:measurement(arrow_tr_t25, arrows_impossibility_theorem, theater_ratio, 25, 0.54).
narrative_ontology:measurement(arrow_tr_t50, arrows_impossibility_theorem, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(arrow_be_t0, arrows_impossibility_theorem, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arrow_be_t25, arrows_impossibility_theorem, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(arrow_be_t50, arrows_impossibility_theorem, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arrows_impossibility_theorem, enforcement_mechanism).
narrative_ontology:affects_constraint(arrows_impossibility_theorem, first_past_the_post_lock_in).
narrative_ontology:affects_constraint(arrows_impossibility_theorem, voting_system_path_dependence).

% DUAL FORMULATION NOTE:
% Arrow's Impossibility Theorem can be analyzed as two distinct constraints: (1) the mathematical theorem itself (mountain or rope, depending on whether axioms are natural laws or chosen premises), and (2) its political invocation as justification for maintaining specific voting systems (tangled rope extraction with theater). This story models the second constraint — the use of the theorem as political justification. The first constraint (the mathematics) would be a separate story decomposed by the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arrows_impossibility_theorem, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
