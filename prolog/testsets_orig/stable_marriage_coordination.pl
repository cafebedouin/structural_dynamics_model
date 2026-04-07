% ============================================================================
% CONSTRAINT STORY: stable_marriage_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stable_marriage_coordination, []).

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
 *   constraint_id: stable_marriage_coordination
 *   human_readable: Stable Marriage Problem (Gale-Shapley Algorithm)
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Stable Marriage Problem (1962, Gale-Shapley) represents a class of
 *   matching constraints where two equally-sized groups with ranked
 *   preferences over each other require a stable allocation. The algorithm
 *   has become the canonical coordination mechanism in economic applications:
 *   medical residency matching (NRMP), school choice systems (Boston, NYC),
 *   job market clearinghouses, organ allocation, and refugee resettlement.
 *   The constraint exhibits a perspectival split between its mathematical
 *   essence (always produces stable matching — Mountain from analytical view)
 *   and its institutional deployment (solves coordination problems with low
 *   overhead — Rope from participant and institutional views). The
 *   constraint's low extractiveness (0.18) and low suppression (0.12) reflect
 *   that Gale-Shapley is fundamentally a coordination mechanism: both groups
 *   benefit from matching rather than remaining unmatched or engaging in
 *   costly decentralized search. The low theater ratio (0.25) indicates
 *   minimal performative content — the algorithm is transparent,
 *   deterministic, and produces outcomes directly from stated preferences.
 *   However, several omega variables remain unresolved: preference revelation
 *   incentives, relative outcome externalities, and proposer-side advantage.
 *   These reflect not failures of the algorithm but structural questions
 *   about its deployment context.
 *
 * KEY AGENTS:
 *   - Individual Participants: Moderate/mobile (beneficial coordination) — benefit from stable matching vs unmatched or sequential bargaining states; can opt out and pursue decentralized search
 *   - Institutional Deployers: Institutional/arbitrage (administrative benefit) — reduce transaction costs, improve system efficiency, enable objective allocation
 *   - Participant Collectives: Organized/constrained (preference aggregation) — organized groups can articulate interests and influence mechanism design
 *   - Constrained Applicants: Powerless/trapped (unequal coordination) — in allocation systems with externalities (refugee resettlement, organ allocation), forced participation creates asymmetry
 *   - Legacy Matching Institutions: Institutional/arbitrage (inertial resistance) — manual matching operators persist through credentialing and institutional routine despite algorithmic alternatives
 *   - Analytical Observer: Analytical/analytical (mathematical necessity) — views stable matching as a theorem, not a design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stable_marriage_coordination, 0.18).
domain_priors:suppression_score(stable_marriage_coordination, 0.12).
domain_priors:theater_ratio(stable_marriage_coordination, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stable_marriage_coordination, extractiveness, 0.18).
narrative_ontology:constraint_metric(stable_marriage_coordination, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stable_marriage_coordination, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stable_marriage_coordination, rope).
narrative_ontology:human_readable(stable_marriage_coordination, "Stable Marriage Problem (Gale-Shapley Algorithm)").
narrative_ontology:topic_domain(stable_marriage_coordination, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stable_marriage_coordination, matching_algorithm_users).
narrative_ontology:constraint_beneficiary(stable_marriage_coordination, both_participant_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PARTICIPANT (ROPE) — Participants in stable matching systems (job markets, university admissions, organ allocation) benefit from coordination that produces outcomes all parties prefer to unmatched states. Exit is mobile — individuals can always opt out of the matching mechanism and pursue decentralized search. The algorithm solves a genuine coordination problem with minimal coercive overhead. Experienced extraction is low because the participant receives a legitimate outcome improvement.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL DEPLOYER (ROPE) — Organizations deploying stable matching (labor market clearinghouses, medical residency matching, school choice systems) benefit from reduced transaction costs and improved system efficiency. The algorithm provides coordination infrastructure with low administrative overhead. Exit is arbitrage — institutions can license, customize, or switch implementations. Net beneficiary with low extraction because the institutional benefit is genuine coordination value, not rent extraction.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / MATHEMATICAL NECESSITY (MOUNTAIN) — From a civilizational perspective, the stable marriage problem reflects an immutable property of preference aggregation: stable matchings always exist when preferences are complete and transitive. Gale-Shapley is not a sociological constraint but a mathematical theorem. The matching mechanism emerges from logical necessity, not institutional design. Accessibility collapse is high (≥0.85) because no alternative can escape the mathematical structure — all algorithms must produce matchings from the preference set, and stable matchings must exist by the theorem.
constraint_indexing:constraint_classification(stable_marriage_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: COLLECTIVE PARTICIPANT GROUP (ROPE) — When participants organize (professional associations, residency applicant networks, student bodies), they gain agency in the matching system. Stable matching algorithms benefit organized groups by enabling group-level preference expression and preference revelation equilibrium. Exit is constrained by the institutional framework but groups can lobby for algorithm adjustments or alternative mechanisms. Low extraction experienced because organized participants can articulate and defend their interests.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CONSTRAINED APPLICANT (TANGLED ROPE) — In some applications (refugee resettlement, organ allocation, school assignment for disadvantaged students), participants face trapped exit — they cannot opt out without significant harm, and their preference ranking is constrained by limited information or external pressure. The algorithm provides coordination (ensures they receive a matching), but also enforces an assignment from a limited option set. Moderate extraction because the trapped exit and constrained preference expression create asymmetric outcomes relative to voluntary participants.
constraint_indexing:constraint_classification(stable_marriage_coordination, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY MATCHING BUREAU (PITON) — Traditional manual matching institutions (matchmakers, hiring departments conducting sequential matching, administrative clearinghouses using older procedures) view the stable matching algorithm as a replacement mechanism. The legacy institution persists through institutional inertia and professional credentialing despite algorithmic alternatives being available. Theater ratio is moderate to high because the legacy operation includes ritual and performative elements (interviews, deliberation, subjective weighting) that the algorithm bypasses. The constraint here is the institutional resistance to algorithmic replacement, not the algorithm itself.
constraint_indexing:constraint_classification(stable_marriage_coordination, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stable_marriage_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stable_marriage_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stable_marriage_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(stable_marriage_coordination, TR),
    TR >= 0.70.

:- end_tests(stable_marriage_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The Gale-Shapley algorithm solves a genuine coordination problem. Both groups benefit from stable matching relative to unmatched states or sequential bargaining with search costs. The extractiveness is not zero because institutional deployment creates asymmetries: proposer-side advantage, information asymmetry in preference ranking, and institutional control over mechanism parameters. But the base value reflects that the primary function is coordination, not extraction. The measurement shows slight increase over time (0.10 → 0.18), indicating institutional layering of extraction mechanisms (strategic preference manipulation, mechanism design gaming) onto the pure coordination function. Suppression (0.12): Low. Participants can typically opt out (mobile exit) and pursue alternatives. Preference revelation is voluntary (no coercion to truthful disclosure). Constraints on preferences come from external factors (limited job market, geographic constraints, qualification thresholds) not from the matching mechanism itself. Theater ratio (0.25): Low. Gale-Shapley is algorithmically transparent and deterministic — outcomes follow directly from stated preferences and the algorithm logic. Minimal performative ritual is required. The slight increase over time (0.20 → 0.25) reflects institutional deployment layering: interview rituals, subjective preference weighting, and bureaucratic procedures that surround the pure algorithm but are not essential to it.
 *
 * PERSPECTIVAL GAP:
 *   The Analytical Observer's Mountain perspective ('stable matching is mathematically necessary') conflicts with the Constrained Applicant's Tangled Rope perspective ('I am forced into this system with limited options'). The conflict reveals the distinction between mathematical inevitability and institutional choice. The theorem guarantees stable matchings exist, but the choice to deploy Gale-Shapley in mandatory allocation systems (school assignment, refugee resettlement, organ allocation) is institutional, not mathematical. The Legacy Matching Bureau's Piton perspective ('algorithmic replacement is degrading our profession') reflects institutional inertia: the mechanism persists because organizations have invested in existing procedures and professional credentialing, not because it functions better than alternatives. The Collective Participant Group's Rope perspective (organized actors get preference aggregation power) contrasts with the powerless Individual Participant's Tangled Rope (trappped in limited options). The gap shows that participation level and organizational capacity determine whether the mechanism functions as pure coordination (organized groups) or mixed coordination-extraction (isolated individuals).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the matching process. Institutional deployers (arbitrage exit, beneficiary status) experience low or negative d because they control deployment and benefit from administrative efficiency. Individual participants with mobile exit experience moderate d — they benefit from coordination but face proposer-side disadvantage and information asymmetry. Constrained applicants (trapped exit, victim status) experience high d — they cannot escape the mechanism and their preferences are externally limited. The algorithm itself has low d (beneficiary-favorable) because it enables coordination that all parties prefer to alternatives. The legacy institution has moderate d (constrained exit, victim status relative to replacement) — it experiences the algorithmic mechanism as a threat to institutional position. The mathematical necessity view (analytical) has zero d because it is a perspective, not an agent in the exchange.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy. The rope classification is stable across perspectives because the underlying problem is genuinely coordinative. The Gale-Shapley algorithm solves a real preference aggregation problem with minimal coercive overhead. The Tangled Rope perspective (constrained applicants) does not contradict this — it simply identifies that deployment contexts (mandatory allocation) create asymmetric participation. The Mountain perspective (mathematical necessity) is orthogonal rather than contradictory — it identifies a different constraint (logical/mathematical inevitability) that is related to but distinct from the institutional coordination mechanism. The Piton perspective (legacy institution resistance) is also orthogonal — it identifies inertial institutional behavior that uses the matching algorithm as a replacement target, not a property of the algorithm itself. The mandatrophy would arise only if the base properties created a mismatch (e.g., if extractiveness were above 0.46 while claimed_type were rope) — here they are consistently low, confirming the rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_revelation_incentive_compatibility,
    'Do participants truthfully reveal their preferences, or do they engage in strategic preference misrepresentation?',
    'Empirical comparison of revealed preferences (stated in matching system) vs inferred true preferences (from behavior post-matching, switching patterns, side agreements). Analysis of strategic behavior in specific applications (e.g., Boston School Choice before and after mechanism change).',
    'If preferences are revealed truthfully: Gale-Shapley produces optimal stable matches. If systematic strategic misrepresentation exists: the algorithm solves a different problem than preference aggregation, and extraction mechanisms emerge from incentive incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_revelation_incentive_compatibility, empirical, 'Whether participants truthfully reveal preferences or engage in strategic misrepresentation').

omega_variable(
    preference_externality_scope,
    'Do participants'' satisfaction and preference stability depend only on their own match, or on comparative outcomes with others?',
    'Analysis of stability definitions: Does the constraint consider blocking pairs based on absolute outcome improvement or relative status comparison? Survey and observational data on participant satisfaction correlated with relative outcomes.',
    'If preferences are purely personal: stable matching solves the stated problem cleanly (Rope). If preferences encode relative comparison: the algorithm can produce individually stable but collectively unstable outcomes, creating latent extraction when coordination-based fairness fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_externality_scope, conceptual, 'Whether preference stability depends on absolute or relative outcomes').

omega_variable(
    algorithmic_proposer_advantage,
    'Does the side designated as proposer (vs responder) in Gale-Shapley algorithm implementation gain structural advantage in outcome outcomes?',
    'Empirical comparison of matching outcomes when proposer/responder sides are swapped for the same preference data. Analysis of real-world applications (medical residency: do program/applicant proposer designations affect outcome distribution?). Theoretical analysis of strategy-proofness properties.',
    'If proposer advantage is systematic and significant: the algorithm encodes a hidden extraction mechanism favoring one side (Tangled Rope). If advantage is negligible: algorithm is coordinatively neutral (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_proposer_advantage, empirical, 'Whether proposer side gains structural advantage in matching outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stable_marriage_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sm_tr_t0, stable_marriage_coordination, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sm_tr_t5, stable_marriage_coordination, theater_ratio, 5, 0.23).
narrative_ontology:measurement(sm_tr_t10, stable_marriage_coordination, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(sm_be_t0, stable_marriage_coordination, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sm_be_t5, stable_marriage_coordination, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(sm_be_t10, stable_marriage_coordination, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stable_marriage_coordination, resource_allocation).
narrative_ontology:affects_constraint(stable_marriage_coordination, kidney_exchange_mechanism).
narrative_ontology:affects_constraint(stable_marriage_coordination, school_choice_system).
narrative_ontology:affects_constraint(stable_marriage_coordination, job_market_clearinghouse).

% DUAL FORMULATION NOTE:
% Stable Marriage Problem is upstream of multiple allocation mechanisms that rely on stable matching as a coordination infrastructure. Specific allocation constraints (kidney exchange, school choice, job market) inherit the low-extraction property of Gale-Shapley but add domain-specific extraction layers (organ scarcity value, school catchment politics, labor market power asymmetries). The stable matching coordination is the shared substructure across all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
