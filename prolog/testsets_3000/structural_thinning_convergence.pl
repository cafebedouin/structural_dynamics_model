% ============================================================================
% CONSTRAINT STORY: structural_thinning_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_thinning_convergence, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_thinning_convergence
 *   human_readable: Structural Thinning Convergence: Simultaneous Degradation of Kin Networks and Institutional Reliability
 *   domain: social_philosophy/trust_theory/relational_ethics
 *
 * SUMMARY:
 *   The structural thinning convergence describes the simultaneous
 *   degradation of two historically complementary support systems: dense kin
 *   networks (extended family, multi-generational households, geographically
 *   proximate relatives) and reliable institutions (pensions, healthcare,
 *   social services, civic organizations). Pre-modern societies relied
 *   primarily on kin networks; mid-20th century welfare states substituted
 *   institutional reliability for declining kin density. The convergence
 *   represents a novel exposure: both systems are now thinning
 *   simultaneously, creating vulnerability neither framework was designed to
 *   address. An aging population with small families, geographically
 *   dispersed children, and underfunded institutions faces care gaps,
 *   economic precarity, and social isolation that neither traditional nor
 *   modern social structures can absorb. The constraint is presented as a
 *   demographic mountain — an inevitable consequence of modernization,
 *   urbanization, and the demographic transition. However, the presence of
 *   identifiable beneficiaries (markets that profit from atomization,
 *   industries that replace kin functions, state apparatus that benefits from
 *   weakened collective capacity) suggests this may be a false summit: a
 *   contingent policy outcome naturalized as demographic inevitability.
 *
 * KEY AGENTS:
 *   - Isolated Elder: Primary experiencer (powerless/trapped) — faces the convergence as biographical reality with no exit
 *   - Sandwich Generation Parent: Secondary experiencer (moderate/constrained) — caught between dependencies with limited agency
 *   - Social Policy Planner: Institutional manager (institutional/constrained) — attempts to manage demographic inevitability within fiscal constraints
 *   - Demographic Analyst: Analytical observer (analytical/analytical) — models the convergence as emergent property of modernity
 *   - Intentional Community Movement: Organized alternative-builders (organized/mobile) — create local exceptions but cannot reverse global trend
 *   - Atomized Consumer Markets: Potential beneficiary (institutional/arbitrage) — profit from replacement of kin functions with market services
 *   - Professional Service Industries: Potential beneficiary (institutional/arbitrage) — elder care, childcare, therapy, financial planning industries expand as kin networks thin
 *   - State Surveillance Apparatus: Potential beneficiary (institutional/arbitrage) — weakened collective capacity reduces resistance to monitoring and control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_thinning_convergence, 0.08).
domain_priors:suppression_score(structural_thinning_convergence, 0.03).
domain_priors:theater_ratio(structural_thinning_convergence, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_thinning_convergence, extractiveness, 0.08).
narrative_ontology:constraint_metric(structural_thinning_convergence, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(structural_thinning_convergence, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_thinning_convergence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(structural_thinning_convergence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_thinning_convergence, mountain).
narrative_ontology:human_readable(structural_thinning_convergence, "Structural Thinning Convergence: Simultaneous Degradation of Kin Networks and Institutional Reliability").
narrative_ontology:topic_domain(structural_thinning_convergence, "social_philosophy/trust_theory/relational_ethics").

domain_priors:emerges_naturally(structural_thinning_convergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_thinning_convergence, atomized_consumer_markets).
narrative_ontology:constraint_beneficiary(structural_thinning_convergence, professional_service_industries).
narrative_ontology:constraint_beneficiary(structural_thinning_convergence, state_surveillance_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED ELDER (MOUNTAIN) — Experiences the convergence as an unchangeable demographic reality. Small family size (fertility decline), geographic dispersion (children moved for work), and institutional unreliability (underfunded elder care) appear as immutable facts of modern life. No perception of agency or alternatives at biographical timescale.
constraint_indexing:constraint_classification(structural_thinning_convergence, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SANDWICH GENERATION PARENT (MOUNTAIN) — Caught between aging parents and dependent children with no extended family support network. Experiences the thinning as a structural constraint beyond individual control: cannot reverse fertility trends, cannot undo geographic dispersion, cannot restore institutional capacity. Constrained exit reflects some agency (could relocate, could change careers) but the underlying demographic and institutional trends are perceived as fixed.
constraint_indexing:constraint_classification(structural_thinning_convergence, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOCIAL POLICY PLANNER (MOUNTAIN) — Institutional actor facing demographic inevitability. Fertility rates below replacement, aging population, declining institutional trust, and fiscal constraints appear as structural parameters to manage, not reverse. Constrained exit reflects policy options exist but cannot change the underlying trends. Generational timescale shows the convergence as a slow-moving crisis with high inertia.
constraint_indexing:constraint_classification(structural_thinning_convergence, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the convergence appears as a phase transition in social organization driven by irreversible structural forces: urbanization, female labor force participation, pension system design, healthcare cost curves, and trust erosion feedback loops. The analytical view sees these as emergent properties of modernity with extremely high resistance to reversal. This is the claimed natural law: advanced economies inevitably experience simultaneous kin-network thinning and institutional strain.
constraint_indexing:constraint_classification(structural_thinning_convergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: INTENTIONAL COMMUNITY MOVEMENT (MOUNTAIN) — Even organized agents attempting to build alternative structures (co-housing, mutual aid networks, chosen family models) perceive the broader convergence as a fixed background condition. They can create local exceptions but cannot reverse the global trend. Mobile exit reflects ability to relocate and organize, but the demographic and institutional trends remain immutable at the societal scale.
constraint_indexing:constraint_classification(structural_thinning_convergence, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_thinning_convergence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(structural_thinning_convergence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_thinning_convergence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(structural_thinning_convergence, ExtMetricName, E),
    domain_priors:suppression_score(structural_thinning_convergence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(structural_thinning_convergence),
    narrative_ontology:constraint_metric(structural_thinning_convergence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(structural_thinning_convergence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(structural_thinning_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The convergence appears to extract minimally because it is framed as a natural demographic process rather than an imposed constraint. The slight extraction reflects that some agents (professional service industries, atomized consumer markets) do benefit from the thinning, but this benefit is obscured by the naturalization framing. The low value is appropriate for a claimed mountain but sits at the threshold where false summit detection becomes relevant. Suppression (0.03): Very low. Individuals are not coerced into small families or geographic dispersion — these are presented as free choices within structural constraints (housing costs, labor mobility, career opportunities). The minimal suppression reflects that the convergence operates through incentive structures rather than direct coercion. Theater ratio (0.12): Very low. There is minimal performative content — the demographic trends are real, measurable, and consequential. The slight theater reflects policy rhetoric that acknowledges the problem while implementing measures that reinforce it (e.g., austerity that weakens institutions while praising family values). Accessibility collapse (0.92): Very high. Reversing the convergence would require coordinated intervention across fertility policy, housing, labor markets, institutional funding, and cultural norms — a degree of coordination that appears nearly inaccessible. Resistance (0.08): Very low. The trends have enormous inertia — fertility decline, urbanization, and institutional fiscal stress are self-reinforcing and resistant to policy intervention at timescales shorter than generational.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap in classification type — all five perspectives return mountain — but significant gap in perceived mutability timescale. The powerless agent sees biographical immutability (cannot change within a lifetime). The moderate agent sees the same. The institutional agent sees generational immutability (cannot reverse within a policy cycle). The analytical observer sees civilizational immutability (emergent property of modernity). The organized agent sees regional mutability (can build local alternatives) but global immutability (cannot reverse the societal trend). The gap is not in type but in scope and timescale of the perceived natural law. The false summit hypothesis suggests this gap is diagnostic: what appears as natural law from all positions may be a constructed constraint that benefits specific actors.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as mountain because the convergence is experienced as structurally immutable across all observation positions. The powerless/trapped agent (isolated elder) has no exit and no perception of alternatives. The moderate/constrained agent (sandwich generation parent) has some agency but cannot reverse the underlying trends. The institutional/constrained agent (policy planner) can manage but not prevent the convergence. The analytical/analytical observer models it as emergent inevitability. Even the organized/mobile agent (intentional community) can create local exceptions but perceives the global trend as fixed. The beneficiaries declared in base_properties (atomized consumer markets, professional service industries, state surveillance apparatus) are NOT represented as separate perspectives because they do not experience the convergence as a constraint — they experience it as an opportunity. Their presence triggers false summit detection: if the engine finds that these beneficiaries are actively reinforcing the convergence through policy influence, the mountain classification is revealed as naturalization of a contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by demonstrating that a mountain classification can be perspectivally uniform (all agents see immutability) while still being a false summit (the immutability is constructed, not natural). The mandatrophy is not 'mountain vs. snare' but 'genuine natural law vs. naturalized policy artifact.' The omega variables document the irreducible uncertainty: is the convergence truly inevitable, or is it the product of specific policy choices (pension design that discourages family care, housing policy that forces geographic dispersion, labor markets that penalize caregiving, institutional austerity) that could be reversed? The false summit detector evaluates whether the declared beneficiaries (atomized markets, professional services, surveillance apparatus) are passively filling gaps or actively reinforcing the convergence. If the latter, the mountain is revealed as a snare or tangled rope with extremely effective naturalization — so effective that even the victims perceive it as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_policy_artifact,
    'Is the convergence an inevitable consequence of modernization, or a contingent outcome of specific policy choices (pension design, housing policy, labor mobility incentives, institutional funding) that could be reversed?',
    'Cross-national comparison of societies with different policy regimes; historical analysis of pre-convergence social structures; identification of policy interventions that successfully reversed either kin-network thinning or institutional decline',
    'If natural law: mountain classification confirmed across all perspectives. If policy artifact: reclassify to tangled_rope or snare, with identifiable beneficiaries (atomized consumer markets, professional service industries replacing kin functions, surveillance apparatus benefiting from social atomization) and victims (isolated individuals, overburdened caregivers, communities losing collective capacity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_policy_artifact, empirical, 'Whether convergence is demographic inevitability or policy-driven outcome').

omega_variable(
    beneficiary_identification_threshold,
    'At what threshold of market capture does the professional service industry (elder care, childcare, therapy, financial planning) transition from filling a gap created by the convergence to actively benefiting from and reinforcing it?',
    'Analysis of industry lobbying against family-support policies; measurement of profit concentration in atomization-dependent sectors; identification of regulatory capture in family policy domains',
    'If threshold is low and already crossed: false summit confirmed — the ''natural'' convergence has identifiable beneficiaries extracting rents. If threshold is high and not yet crossed: mountain classification holds — services are genuinely filling gaps rather than creating them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_threshold, conceptual, 'When service industries transition from gap-filling to extraction').

omega_variable(
    reversibility_timescale,
    'What is the minimum timescale on which kin-network density or institutional reliability could be restored, assuming coordinated policy intervention?',
    'Historical precedent analysis (post-war family policy, New Deal institutional rebuilding); demographic modeling of fertility response to policy; trust restoration case studies',
    'If timescale < 1 generation: mountain classification weakens — the constraint is mutable at biographical timescales for organized agents. If timescale > 3 generations: mountain classification strengthens — effective immutability even at civilizational scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_timescale, empirical, 'Minimum timescale for reversing the convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_thinning_convergence, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stc_theater_1950, structural_thinning_convergence, theater_ratio, 0, 0.08).
narrative_ontology:measurement(stc_theater_1975, structural_thinning_convergence, theater_ratio, 25, 0.1).
narrative_ontology:measurement(stc_theater_2000, structural_thinning_convergence, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(stc_extract_1950, structural_thinning_convergence, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(stc_extract_1975, structural_thinning_convergence, base_extractiveness, 25, 0.06).
narrative_ontology:measurement(stc_extract_2000, structural_thinning_convergence, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_thinning_convergence, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is formulated as a single mountain claim. If decomposition is warranted, separate stories would distinguish: (1) kin-network thinning as demographic inevitability (ε ≈ 0.05, genuine mountain), (2) institutional reliability decline as policy choice (ε ≈ 0.35, tangled rope with fiscal austerity beneficiaries), and (3) the convergence itself as the interaction effect (ε ≈ 0.08, false summit). Current formulation treats the convergence as a unified phenomenon to test whether the whole can be naturalized even when components might not be.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
