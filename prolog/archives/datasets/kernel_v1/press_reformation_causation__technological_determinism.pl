% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: The Printing Press as Technological Determinant of the Reformation
 *   domain: history_of_technology/religious_history
 *
 * SUMMARY:
 *   The technological determinism reading of the press-Reformation kernel
 *   frames the printing press as an exogenous material force that made
 *   certain outcomes inevitable while foreclosing others. In this reading,
 *   the technology itself is the primary causal agent: by making it
 *   structurally impossible to suppress manuscript-scale reproduction through
 *   centralized control, the press created a new information landscape.
 *   Vernacular scripture became inevitable not because reformers
 *   strategically planned to print it, but because once the technology
 *   existed, reproducing vernacular texts at scale became cheaper and faster
 *   than suppressing them. Church resistance (the Index Librorum
 *   Prohibitorum, book burning, licensing controls) appears futile not
 *   because reformers were cleverer, but because they could exploit an
 *   exogenous technological shift. The printing press is the constraint —
 *   agents are positioned downstream of it, benefiting or suffering from its
 *   causal force. This reading classifies the technology itself as a
 *   Mountain: an immutable shift in what is possible, independent of human
 *   intention or institutional will.
 *
 * KEY AGENTS:
 *   - The Printing Press Technology: Primary causal agent (analytical/analytical) — exogenous constraint determining outcomes; no exit options (technology determines boundaries)
 *   - Reformers (Luther, Tyndale, etc.): Downstream beneficiaries (powerful/mobile) — exploit the press's causal force but do not create it; benefit from technological inevitability
 *   - The Roman Catholic Church: Victim of technological displacement (institutional/trapped) — faces immutable constraint on manuscript suppression; censorship becomes futile
 *   - Mass Reading Public: Downstream beneficiary (powerless/mobile) — gains access to vernacular scripture via technological inevitability; could not have been denied if press exists
 *   - Printers and Publishing Networks: Instrumental agents (powerful/mobile) — operate within the technological constraint; profit from market opportunities the press creates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.08).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.08).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "The Printing Press as Technological Determinant of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '3e2e48f4-c361-4271-a198-b71bb9f373a0').
narrative_ontology:cs_kernel_codification('3e2e48f4-c361-4271-a198-b71bb9f373a0', fixed_text).
narrative_ontology:cs_authority_grounding('3e2e48f4-c361-4271-a198-b71bb9f373a0', distributed).
narrative_ontology:cs_reading_relation('3e2e48f4-c361-4271-a198-b71bb9f373a0', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('3e2e48f4-c361-4271-a198-b71bb9f373a0', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('3e2e48f4-c361-4271-a198-b71bb9f373a0', foundational, technology_determines_outcomes_exogenously).
narrative_ontology:cs_axiom_status(technology_determines_outcomes_exogenously, holdable).
narrative_ontology:cs_axiom_grounding('3e2e48f4-c361-4271-a198-b71bb9f373a0', technology_determines_outcomes_exogenously, empirically_contingent).
narrative_ontology:cs_axiom('3e2e48f4-c361-4271-a198-b71bb9f373a0', foundational, censorship_becomes_structurally_impossible_above_cost_threshold).
narrative_ontology:cs_axiom_status(censorship_becomes_structurally_impossible_above_cost_threshold, holdable).
narrative_ontology:cs_axiom_grounding('3e2e48f4-c361-4271-a198-b71bb9f373a0', censorship_becomes_structurally_impossible_above_cost_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('3e2e48f4-c361-4271-a198-b71bb9f373a0', pre_printing_manuscript_economy).
narrative_ontology:cs_drift_state('3e2e48f4-c361-4271-a198-b71bb9f373a0', post_printing_standardization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3e2e48f4-c361-4271-a198-b71bb9f373a0', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TECHNOLOGICAL DETERMINISM (MOUNTAIN) — The printing press made handwritten manuscript censorship structurally impossible; the technology created an irreversible capacity for mass reproduction at low cost. Once that capacity existed, vernacular scripture reproduction became inevitable regardless of Church efforts. Technology determines the boundary conditions of the possible. No agent can exit this constraint — the press's causal force is exogenous to human intention.
constraint_indexing:constraint_classification(press_reformation_causation__technological_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 2: CHURCH'S STRUCTURAL IMMOBILITY (MOUNTAIN) — From the Church's institutional perspective, the printing press constraint is absolute: once the technology exists, the Church cannot prevent proliferation. Burning books becomes a futile gesture against exponential reproduction. The institutional power of the Church meets an impassable technological boundary. Exit options vanish entirely — the technology operates on a causal layer above institutional authority.
constraint_indexing:constraint_classification(press_reformation_causation__technological_determinism, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNIVERSAL INFORMATION DIFFUSION LAW (MOUNTAIN) — At the universal/civilizational level, this is a statement about information thermodynamics: once a technology achieves a cost/speed threshold for reproduction, information censorship becomes exponentially more difficult. The constraint is not about printing per se but about a deep structural property of information systems. The press is merely the instance — the underlying law is universal and immutable.
constraint_indexing:constraint_classification(press_reformation_causation__technological_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(press_reformation_causation__technological_determinism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causation__technological_determinism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. In the technological determinism reading, there is no agent extracting from another agent — the constraint IS the technology's causal force. The press does not extract; it enables. Suppression (0.02): Minimal. Once the technology exists, there is no active enforcement mechanism suppressing alternatives — the constraint is purely structural (technological capacity, not coercive). Theater ratio (0.15): Very low. The constraint's operation is mechanical and functional; there is little performative content. The press does or does not exist; it works or does not work. This is consistent with the mountain classification. Accessibility collapse (0.92): Very high. Once printing technology exists, handwritten manuscript suppression collapses — the technology's superior cost-efficiency makes alternatives inaccessible. Resistance (0.08): Very low. There is no meaningful resistance to the printing press's causal operation — it is a fact of material infrastructure, not a social arrangement that can be resisted. The Church could resist the *content* distributed via printing, but not the technology's existence or causal force.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as Mountain, reflecting that technological determinism is a uniform-type claim in this reading. The constraint is immutable from every observational position: even the Church, which is the primary victim of technological displacement, must acknowledge that the press's causal force is absolute. The perspectival gap appears not between classification types (all are Mountain) but in the framing of immutability: the analytical observer sees it as a law of information systems; the Church sees it as structural immobility before an external force; the universal view sees it as a deep principle about technology and information diffusion. These are perspectival framings of the same classification — all agree the constraint is immutable, but from different epistemic positions.
 *
 * DIRECTIONALITY LOGIC:
 *   In the technological determinism reading, directionality logic is inverted: the constraint is not an agent extracting from another agent, but a technological fact creating new possibility-spaces. The d value (directionality) is not derived from beneficiary/victim declarations in the traditional sense because there is no extraction mechanism. Instead, d reflects the agent's structural relationship to the technological shift: those positioned to exploit the press's capacity (reformers, printers, mass readership) have d values near 0 (beneficiary position), while those attempting to suppress what the press enables (the Church) have d values near 1.0 (victim position). However, in a pure Mountain, the d values are formally irrelevant — the classification is immutable regardless of agent position. The beneficiary and victim structure is declared for clarity, but the mountain classification does not depend on directionality in this case.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. The technological determinism reading avoids mandatrophy by maintaining strict causal priority: technology causes, agents respond. There is no confusion between coordination (beneficial mutual dependence) and extraction (asymmetric appropriation) because neither category applies — the constraint is a material/technological fact, not a social arrangement. The Church cannot negotiate with physics; reformers cannot coordinate with the press. Mandatrophy would arise only if this reading were merged with agent-centric perspectives (strategic_deployment, mutual_shaping), which this reading logically excludes. By keeping technology as the primary causal agent and agents as downstream responders, the technological_determinism reading avoids the classification ambiguities that generate mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_contingency,
    'Is the causal mechanism of the printing press truly deterministic (technology determines outcomes) or merely strongly constraining (technology enables outcomes that agents choose to pursue)?',
    'Comparative history: (1) Examine cases where print technology existed but Reformation-like movements did not occur (Islamic world, China post-printing invention). (2) Model counterfactuals: if printing had been invented 50 years later, would Luther have succeeded without it? Would manuscript networks have sufficed? (3) Analyze the temporal gap between printing invention (ca. 1440) and mass vernacular scripture production (1520s+) — if deterministic, why the lag?',
    'If deterministic: classification holds as Mountain (technology causes outcome). If merely enabling: reclassify to mutual_shaping or strategic_deployment reading (technology+agency co-produce outcome). This is the core kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(determinism_vs_contingency, conceptual, 'Whether printing press causation is deterministic or merely enabling').

omega_variable(
    censorship_impossibility_threshold,
    'At what threshold of reproduction capacity does censorship become ''impossible''? Was this threshold actually reached in early modern Europe?',
    'Historical analysis of suppression efforts: (1) Count banned books that nonetheless circulated (Index Librorum Prohibitorum effectiveness data). (2) Map the distribution of illegal/clandestine printing operations — were they actually impossible to control or merely costly to control? (3) Compare printing suppression to other technologies (heretical manuscript networks pre-1440 — were they also ''uncensorable'' just harder?). (4) Model cost curves: what would enforcement infrastructure have needed to be to achieve near-total censorship of printed books?',
    'If censorship remained feasible with sufficient effort: ''impossible'' is a theoretical claim about economic scaling, not an absolute constraint. If Church faced genuinely exponential enforcement costs: mountain classification holds. The difference determines whether this is technological determinism or economic constraints on institutional capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_impossibility_threshold, empirical, 'Whether vernacular printing censorship was truly impossible or merely costly').

omega_variable(
    reformer_independence_from_printing,
    'Would the core theological commitments of the Reformation have emerged in a printing-free world? Would manuscript dissemination have sufficed?',
    'Intellectual history analysis: (1) Trace the origins of reformist theology in pre-printing contexts (14th-century Lollards, Waldensians) — theological commitments exist independent of printing. (2) Model manuscript networks: how far did hand-copied Protestant texts actually spread? What scale did they reach? (3) Counterfactual: if printing had been invented in 1520 instead of 1440, could the Reformation have occurred at all, or only decades later?',
    'If theological commitments are pre-printing and manuscript networks could have achieved significant reach: agency reading forecloses pure technological determinism. If printing was the only mechanism reaching critical mass: determinism holds. This resolves whether reformers are passive beneficiaries of exogenous technology or active agents exploiting new capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_independence_from_printing, empirical, 'Whether Reformation theology would have emerged without printing').

omega_variable(
    reading_identity_under_contest,
    'This constraint instantiates the technological_determinism reading of the press_reformation_causation kernel. The sibling readings (mutual_shaping, strategic_deployment) reject the determinism premise. Can the determinism reading coexist with evidence that printing was strategically deployed, or does strategic deployment logically foreclose determinism?',
    'Clarify the kernel contest logically: (A) Determinism claims: technology causes outcomes (printing causes Reformation regardless of agent intent). (B) Strategic deployment claims: agents use technology intentionally (reformers deliberately exploited printing). (C) Mutual shaping claims: technology and agency co-evolved (printing and Reformation shaped each other). These are not all compatible in a single framework. The engine''s foreclosure analysis will compute which readings logically exclude others.',
    'If strategic deployment is evidenced: determinism reading faces foreclosure pressure. If mutual shaping is evidenced: determinism and strategic deployment both face pressure. Determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_under_contest, conceptual, 'Logical compatibility of technological determinism with strategic deployment or mutual shaping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(printing_determinism_t0_extractiveness, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The press_reformation_causation kernel is decomposed into three structurally distinct constraint stories, one per reading. Each reading produces a different classification, different beneficiary/victim structure, and different causal narrative. The technological_determinism reading frames printing as exogenous technology (Mountain); the strategic_deployment reading frames reformers and printers as intentional agents exploiting neutral capacity (Tangled Rope/Snare); the mutual_shaping reading frames technology and agency as co-constitutive (Rope/Tangled Rope). These are not alternate measurements of one constraint — they are genuinely different constraints embodying different causal theories. All three stories are linked via network.affects_constraints to represent the contested kernel and the causal dependencies between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
