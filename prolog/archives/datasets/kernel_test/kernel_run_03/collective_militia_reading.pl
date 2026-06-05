% ============================================================================
% CONSTRAINT STORY: collective_militia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_militia_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collective_militia_reading
 *   human_readable: Second Amendment Collective Militia Reading: Prefatory Clause Limits Operative Clause Scope
 *   domain: constitutional_law/political_theory/federalism
 *
 * SUMMARY:
 *   The collective militia reading of the Second Amendment interprets the
 *   prefatory clause ('A well regulated Militia, being necessary to the
 *   security of a free State') as a binding limitation on the operative
 *   clause ('the right of the people to keep and bear Arms, shall not be
 *   infringed'). Under this reading, the right to bear arms exists only in
 *   connection with service in or capacity to serve in a well-regulated state
 *   militia. This reading produces a structurally asymmetric constraint:
 *   organized militia members and federal/state regulatory authorities
 *   benefit from a stable interpretive frame that enables both militia
 *   protection and comprehensive firearms regulation, while individual gun
 *   owners outside militia service experience the constraint as denying their
 *   claimed constitutional right. The constraint exhibits genuine
 *   coordination function (protecting state militia capacity and federalism
 *   balance) alongside asymmetric extraction (restricting individual
 *   bearers). This is one of three competing readings of the Second Amendment
 *   kernel, distinguished from the individual-right reading (which
 *   subordinates or ignores the prefatory clause) and the
 *   sophisticated-collective reading (which protects individual bearers
 *   insofar as they participate in militia-like collective action). The
 *   measurement trajectory shows increasing extractiveness over the 50-year
 *   interval, reflecting regulatory drift that narrows militia definition and
 *   expands the victim set.
 *
 * KEY AGENTS:
 *   - Federal Regulatory Authority: Primary beneficiary (institutional/arbitrage) — gains stable interpretive frame for comprehensive firearms regulation while preserving militia carve-outs
 *   - Organized State Militias: Secondary beneficiary (organized/constrained) — receive constitutional protection for collective armed capacity under state sovereignty
 *   - State Governments: Tertiary beneficiary (institutional/arbitrage) — retain prerogative to define militia composition and maintain armed forces independent of federal control
 *   - Individual Gun Owners Outside Militia: Primary victim (powerless/trapped) — face textual denial of claimed individual right unless militia-connected
 *   - Rural Communities with Hunting Traditions: Secondary victim (moderate/constrained) — experience regulatory friction and ambiguity about hunting tradition's militia-adjacency
 *   - Analytical Observer: Civilizational reading (analytical/analytical) — sees both coordination and extraction functions as structurally real
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_militia_reading, 0.58).
domain_priors:suppression_score(collective_militia_reading, 0.65).
domain_priors:theater_ratio(collective_militia_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_militia_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(collective_militia_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(collective_militia_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_militia_reading, tangled_rope).
narrative_ontology:human_readable(collective_militia_reading, "Second Amendment Collective Militia Reading: Prefatory Clause Limits Operative Clause Scope").
narrative_ontology:topic_domain(collective_militia_reading, "constitutional_law/political_theory/federalism").

domain_priors:requires_active_enforcement(collective_militia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(collective_militia_reading, 'ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945').
narrative_ontology:cs_created_at('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', '').
narrative_ontology:cs_kernel_codification('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', fixed_text).
narrative_ontology:cs_authority_grounding('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', lineage).
narrative_ontology:cs_interpretation_layer_present('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945').
narrative_ontology:cs_kernel_id(collective_militia_reading, second_amendment_text).
narrative_ontology:cs_reading_relation('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', sophisticated_collective_reading, influences).
narrative_ontology:cs_axiom('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', foundational, prefatory_clause_binding).
narrative_ontology:cs_axiom_status(prefatory_clause_binding, holdable).
narrative_ontology:cs_axiom_grounding('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', prefatory_clause_binding, conventional).
narrative_ontology:cs_axiom('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', foundational, state_militia_primacy).
narrative_ontology:cs_axiom_status(state_militia_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', state_militia_primacy, conventional).
narrative_ontology:cs_reference_frame('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', founding_era_militia_system).
narrative_ontology:cs_drift_state('ebe84eb9-6a0c-42a2-a2d9-9df5a2b73945', contemporary_regulatory_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_militia_reading, organized_state_militias).
narrative_ontology:constraint_beneficiary(collective_militia_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(collective_militia_reading, individual_gun_owners_outside_militia).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNER (SNARE) — Under this reading, the individual claiming a right to bear arms outside organized militia service has no constitutional exit: the prefatory clause forecloses the operative clause's application to non-militia bearers. The individual is trapped by textual interpretation with no alternative constitutional frame available within this reading. Experiences maximum extraction — rights claim is structurally denied.
constraint_indexing:constraint_classification(collective_militia_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED MILITIA MEMBERS (TANGLED ROPE) — Militia members experience genuine coordination benefit (the prefatory clause protects their collective capacity for armed civic participation) alongside extraction (their participation is contingent on state authority's regulatory control). Constrained exit: they can leave the militia structure but face organizational and social barriers. Mixed experience of the constraint — genuine protection within a regulatory framework.
constraint_indexing:constraint_classification(collective_militia_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL REGULATORY AUTHORITY (ROPE) — Under this reading, the federal government benefits from the prefatory clause's interpretation as enabling comprehensive firearms regulation while preserving militia-related carve-outs. The constraint is experienced as pure coordination: the text provides a stable interpretive frame for delegating some arms regulation to states (militia) while reserving federal police powers. Arbitrage exit: regulatory authority can adjust application through administrative process.
constraint_indexing:constraint_classification(collective_militia_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SOVEREIGNTY OVER MILITIA (ROPE) — State governments benefit from the prefatory clause's emphasis on 'well-regulated state militia' as protecting state prerogative to maintain armed forces independent of federal control. This reading coordinates state authority with the Second Amendment's textual commitment. Arbitrage exit: states can modify militia composition and regulation within constitutional bounds.
constraint_indexing:constraint_classification(collective_militia_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RURAL HUNTING COMMUNITIES (TANGLED ROPE) — These communities experience mixed effects: some hunting is arguably militia-adjacent (militia training traditionally included marksmanship), enabling continued tradition, but constraint on individual non-militia bearing creates regulatory friction. Constrained exit: limited ability to challenge the reading without abandoning constitutional frame; significant practical barriers to challenging state regulation.
constraint_indexing:constraint_classification(collective_militia_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, this reading exhibits genuine coordination function (protecting militia capacity) alongside asymmetric extraction (denying individual claims unless militia-connected). The constraint is analytically coherent but structurally asymmetric. The observer can recognize both functions as real structural features of the reading, not naturalizations.
constraint_indexing:constraint_classification(collective_militia_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_militia_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_militia_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_militia_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_militia_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collective_militia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from individual non-militia bearers by denying their claimed right textually, but does not eliminate all arms bearing (militia members and state authorities retain protected capacity). The extraction is structural, not total. The measurement trajectory (0.42 → 0.58) reflects regulatory drift: as courts and legislators narrow militia definition, more individuals fall into the victim category, increasing extractiveness. Suppression (0.65): High. Individuals outside militia have limited alternatives: they cannot exit the constitutional frame without abandoning citizenship, cannot challenge the reading's coherence without extensive legal work, and face regulatory enforcement preventing non-militia bearing. Barriers are legal (textual interpretation), institutional (regulatory systems), and political (majority consensus on regulated firearms). Theater ratio (0.55): Moderate. The reading has genuine structural content (prefatory clause does impose real textual limits) but also requires interpretive work to maintain boundary between militia and non-militia bearing, and regulatory categories shift over time. Some of the boundary-maintenance is performative (is a concealed-carry permit militia-related? is a hunting rifle?), but the reading is not primarily theater-based.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between beneficiaries (federal authority, state governments, organized militia) who see the constraint as stable coordination with clear regulatory boundaries, and victims (individual bearers outside militia) who see the constraint as denying their claimed constitutional right. Beneficiaries experience the constraint as enabling governance (Rope). The individual victim experiences it as pure negation (Snare). The analytical observer sees both as real structural features of the same reading (Tangled Rope). The gap does not reflect disagreement about facts but about which agent's structural position determines how the constraint is experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from structural position relative to this constraint. Federal regulatory authority benefits from the reading (low d, negative χ) because it enables comprehensive regulation with constitutional cover. Organized militia members benefit partly (low-to-moderate d) because they receive protection, though contingent on state authority. Individual non-militia bearers bear maximum cost (high d, high f(d), high χ) because the reading textually denies their claimed right. Rural communities experience mixed effects (moderate d) because hunting may or may not fall under militia protection depending on boundary interpretation. The analytical observer at civilizational scope (d ≈ 0.73) sees both coordination and extraction as real structural features of the reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_power,
    'Does the prefatory clause (''well regulated Militia, being necessary to the security of a free State'') function as a limitation clause that restricts the operative clause''s scope, or merely as an explanatory preamble that motivates the operative clause without limiting it?',
    'Comparative constitutional law analysis: examination of how other constitutions handle prefatory vs operative clauses; historical grammatical analysis of 18th-century usage; consistency with other Bill of Rights interpretations (does the framework apply prefatory limitations to other amendments?)',
    'If prefatory clause is binding limitation: this reading''s snare classification for individuals is correct, and the operative clause applies only to militia service. If prefatory clause is motivational only: the individual right reading becomes structurally correct, and this reading''s beneficiary/victim structure collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_binding_power, conceptual, 'Whether prefatory clause functions as operative limitation or motivational preamble').

omega_variable(
    militia_definition_contingency,
    'What counts as a ''well-regulated state militia'' for purposes of Second Amendment protection? Does the definition remain fixed at the founding, evolve with state practice, or become ambiguous across state implementations?',
    'Historical tracking of state militia composition and federal recognition (National Guard vs active militia); analysis of whether non-recognition of informal militia groups affects their constitutional status; examination of whether the constraint''s victim set expands or contracts as militia definitions change',
    'If definition is rigid (founding-era militia only): constraint becomes increasingly divorced from actual state practice, reducing coherence. If definition evolves: victim set (non-militia bearers) expands or contracts over time, changing extractiveness. If ambiguous: different states generate different beneficiary sets, fragmenting the constraint across federalism lines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_contingency, empirical, 'Definition of ''well-regulated state militia'' and its temporal/spatial variation').

omega_variable(
    reading_kernel_contest,
    'Is the fundamental disagreement about the Second Amendment a disagreement about which reading of the text is correct (a coherent interpretive choice among legitimate alternatives), or is it a disagreement about whether the text itself is ambiguous, outdated, or needs amendment?',
    'Distinction between intra-reading dispute (which reading best captures the text) and extra-reading dispute (whether the text itself is binding). Analysis of whether advocates of the individual-right reading claim the collective reading is false or merely claim the text should be changed.',
    'If intra-reading: the three readings coexist as live interpretive options, each structurally coherent. If extra-reading: some advocates may be arguing that the collective reading is correct but the text should be amended, which changes the meta-constitutional status of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether disagreement is interpretive or meta-constitutional').

omega_variable(
    extractiveness_boundary_slippage,
    'As interpretive doctrine evolves, does the boundary between ''militia-connected arms'' and ''non-militia arms'' remain stable, or does regulatory drift move more bearers into the victim category?',
    'Longitudinal analysis of court decisions and regulatory actions: tracking whether courts narrow militia definition (reducing protected bearers) or broaden it (reducing victims); examination of whether new firearm types fall into protected or regulated categories',
    'If boundary drifts toward narrowing: extractiveness increases (more individuals fall into victim category). If boundary drifts toward broadening: extractiveness decreases, and the constraint approaches a sophiticated-collective reading that protects more bearers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_boundary_slippage, empirical, 'Stability of militia-connected vs non-militia boundary over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_militia_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, collective_militia_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(coll_tr_t25, collective_militia_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(coll_tr_t50, collective_militia_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, collective_militia_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coll_be_t25, collective_militia_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(coll_be_t50, collective_militia_reading, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_militia_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_militia_reading, individual_right_reading).
narrative_ontology:affects_constraint(collective_militia_reading, sophisticated_collective_reading).
narrative_ontology:affects_constraint(collective_militia_reading, federal_firearm_regulation_authority).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel has three competing readings, each instantiating a distinct constraint with different ε, different beneficiary/victim sets, and different classifications. This constraint (collective_militia_reading) is downstream of the kernel text but represents one specific structural interpretation. The sibling readings (individual_right_reading, sophisticated_collective_reading) are NOT alternative observables of this constraint — they are separate constraints generated by incompatible interpretations of the shared kernel. Each reading should have its own JSON file with its own metrics and perspectives. The three readings are linked via network edges documenting which reading forecloses or influences which others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
