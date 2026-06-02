% ============================================================================
% CONSTRAINT STORY: institutional_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_degradation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: institutional_degradation
 *   human_readable: Institutional Degradation: Coordination Collapse and Extractive Maintenance
 *   domain: institutional_analysis/organizational_decline
 *
 * SUMMARY:
 *   Institutional degradation describes the structural trap created when an
 *   organization's primary function has atrophied or been corrupted, but the
 *   institution persists through extractive maintenance — consuming
 *   resources, enforcing compliance, and generating theater — without
 *   delivering the coordination or service benefits that justified its
 *   original existence. This constraint affects universities losing research
 *   funding but maintaining administrative hierarchies, hospitals with
 *   reduced capacity but inflated procedure counts, government agencies
 *   managing policy failure through metrics manipulation, religious
 *   institutions sustaining authority through ritual despite loss of genuine
 *   community function, and corporations maintaining organizational structure
 *   despite commodified products. The degradation is not inevitable decay; it
 *   is a specific structural equilibrium in which rent extraction becomes
 *   more profitable than function renewal. The constraint exhibits dual
 *   nature: it genuinely coordinates *something* (maintains order,
 *   distributes resources, processes cases) while extracting asymmetrically
 *   (those dependent on the institution bear disproportionate costs, while
 *   beneficiaries capture rents). The theater ratio has increased over the
 *   interval (0.52 → 0.78) while actual extractiveness has also increased
 *   (0.38 → 0.58), indicating not just performative drift but increasing
 *   asymmetry. The institution no longer pretends to serve its constituents;
 *   it serves to maintain itself.
 *
 * KEY AGENTS:
 *   - Dependent Constituents: Primary victims (powerless/trapped) — citizens, students, patients, workers with no viable exit; bear full cost of degradation
 *   - Institutional Servants: Secondary victims (powerless/identity_locked) — career employees whose professional identity is fused with the degraded institution; structurally mobile but cognitively trapped
 *   - Administrative Incumbents: Primary beneficiaries (institutional/arbitrage) — executives, board members, consultants maintaining the degraded apparatus; have exit options and benefit from arrangement
 *   - Mid-Level Managers: Mixed victims-beneficiaries (moderate/constrained) — supervise dysfunction while receiving institutional rewards; experience both coordination function and extraction
 *   - Reformers: Organized challengers (organized/constrained) — attempt institutional renewal but often absorbed into degradation cycle; reform efforts become performative
 *   - Peer Institutions: Secondary stakeholders (institutional/constrained) — face coordination challenges due to degraded neighbor's unreliability; limited exit from institutional ecosystem
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable organizational physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_degradation, 0.58).
domain_priors:suppression_score(institutional_degradation, 0.62).
domain_priors:theater_ratio(institutional_degradation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_degradation, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_degradation, tangled_rope).
narrative_ontology:human_readable(institutional_degradation, "Institutional Degradation: Coordination Collapse and Extractive Maintenance").
narrative_ontology:topic_domain(institutional_degradation, "institutional_analysis/organizational_decline").

domain_priors:requires_active_enforcement(institutional_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_degradation, administrative_incumbents).
narrative_ontology:constraint_beneficiary(institutional_degradation, rent_extractors).
narrative_ontology:constraint_victim(institutional_degradation, institutional_constituents).
narrative_ontology:constraint_victim(institutional_degradation, public_interest).
narrative_ontology:constraint_victim(institutional_degradation, institutional_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CONSTITUENT (SNARE) — Citizens, students, patients, or workers dependent on the degraded institution have no viable exit. They bear the full cost of institutional decline — reduced service quality, broken processes, perverse incentives — while trapped by geographic proximity, economic necessity, or legal obligation. No alternatives available at comparable cost. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INSTITUTIONAL SERVANT (SNARE via identity_locked) — Career administrators, long-term employees, or institutional actors whose professional identity is fused with the degraded institution. They cannot imagine themselves outside it, even when the institution ceases to serve its purported function. The lock is cognitive rather than material — they have theoretically mobile alternatives but cannot exercise them because their identity is constituted through the institutional role. Structurally mobile but functionally trapped.
constraint_indexing:constraint_classification(institutional_degradation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE INCUMBENT (ROPE) — Beneficiaries with arbitrage options: executives, board members, consultants who coordinate the degraded institution's maintenance. They experience the constraint as coordination — managing stakeholder expectations, maintaining procedural facades, and preserving their own position. They have exit options (move to other institutions, rotate to private sector) but benefit from the current arrangement. Negative directionality; low effective extraction experienced by this group.
constraint_indexing:constraint_classification(institutional_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by career investment and organizational dependency, but also receiving some institutional benefits (salary, status, procedural authority). Experiences both coordination function (processes must be maintained for any work to occur) and asymmetric extraction (required to enforce dysfunctional rules and absorb the gap between institutional rhetoric and reality). Moderate agency and significant extraction — the constraint's mixed nature.
constraint_indexing:constraint_classification(institutional_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORMER COALITION (PITON) — Organized agents attempting to reform or revitalize the institution. They perceive that institutional function has atrophied while performance theater has increased (accreditations, metrics, public messaging that masks decline). The reform effort is itself partially degraded — reform proposals become performative, new policies layer onto old dysfunction rather than replacing it. Theater ratio exceeds function. The constraint is maintained by institutional inertia despite loss of primary purpose.
constraint_indexing:constraint_classification(institutional_degradation, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: PEER INSTITUTION (TANGLED ROPE) — Other institutions in the same ecosystem (sister universities, competing hospitals, neighboring government agencies) face coordination challenges due to the degraded neighbor's unreliability. They must often compensate for or work around the failing institution, creating asymmetric extraction. But they also benefit from reduced competition and stable-but-weakened ecosystem relationships. Constrained by institutional interdependencies.
constraint_indexing:constraint_classification(institutional_degradation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, institutional decay may appear to be a natural law: all institutions eventually degrade, lose purpose, and persist through inertia. This perspective risks naturalizing what is actually a contingent product of specific incentive structures, path dependency, and extractive maintenance. The engine's false summit detector will identify this as naturalization of contingent institutional arrangement.
constraint_indexing:constraint_classification(institutional_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_degradation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_degradation, TR),
    TR >= 0.70.

:- end_tests(institutional_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The institution extracts resources (funding, labor, compliance) from constituents and converts them into administrative overhead, procedural theater, and executive rents rather than service delivery. The measurement increased from 0.38 to 0.58 over the observation interval, reflecting acceleration of rent extraction. The constraint is tangled_rope rather than pure snare because genuine coordination function persists (some services are delivered, some order is maintained, some legitimate work occurs) alongside extraction. Suppression (0.62): Moderate-high. Constituents face significant barriers to exit: material cost (relocation, finding alternatives), psychological cost (identity fusion, internalized beliefs about necessity), and institutional barriers (legal requirements, lack of alternatives, ecosystem lock-in). But suppression is not absolute — some exit pathways exist and some constituents do leave. Theater ratio (0.78): High and rising. The institution increasingly consumes resources for procedural activity, metrics generation, public relations, and accreditation management rather than delivery of stated service. The theatrical component accelerates as actual function declines — the institution substitutes visible ritual for invisible service quality. Claimed type (tangled_rope): The institution coordinates some genuine function while extracting asymmetrically. It is not pure snare (extraction without coordination) because coordination genuinely occurs. It is not pure rope (low-extraction coordination) because extraction is substantial and asymmetric. The active enforcement requirement is met: the degraded institution requires continuous effort to suppress alternatives, maintain constituent compliance, and manage the narrative gap between rhetoric and reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The dependent constituent sees pure extraction (Snare) — an institution that offers no value and provides no escape. The institutional servant sees the same constraint differently due to identity fusion (identity_locked produces Snare classification) — they are trapped not by material barriers but by inability to imagine themselves otherwise. The administrative incumbent sees coordination (Rope) — they are solving the legitimate problem of maintaining the institution's operations and managing stakeholder expectations; they experience low extraction and benefit from the arrangement. The mid-level manager sees mixed function and extraction (Tangled Rope) — the constraint both enables their work (processes, authority) and corrupts it (enforces dysfunction, demands compliance with degraded procedures). The reformer sees institutional inertia with sunset potential (Piton) — the reform effort recognizes that theater has consumed function but miscalculates the difficulty of structural change; reform proposals often become absorbed into the degradation cycle. The analytical observer risks seeing inevitable institutional physics (Mountain, false summit) — decay appears as natural law when it is actually contingent product of specific incentive structures. The perspectival gaps reveal that the institution's degradation is not uniform; it is experienced very differently depending on one's structural position and whether one benefits or bears extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Dependent constituents trapped in the institution experience maximum directionality (d ≈ 0.95 for powerless/trapped), producing high effective extraction. Identity-locked institutional servants experience moderately high directionality (d ≈ 0.85 for powerless/identity_locked) — they are structurally mobile but cognitively captured. Administrative incumbents with arbitrage options experience low or negative directionality (d ≈ 0.10 for institutional/arbitrage) — extraction flows toward them, not away. Mid-level managers experience moderate directionality (d ≈ 0.55 for moderate/constrained) — they both benefit from the institution and bear extraction costs. Reformer coalitions organized at institutional level with constrained exit experience mixed directionality (d ≈ 0.48) — they have leverage but are partially locked into the ecosystem they seek to reform. The analytical observer at civilizational scope risks naturalizing the contingent institutional arrangement (d ≈ 0.72 for analytical/analytical) — perceiving degradation as inevitable law rather than structural product.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing between genuine coordination function (institutional process, decision-making, service delivery at degraded but nonzero level) and extractive theater (metrics manipulation, procedural ritual, administrative overhead). A purely extractive constraint (Snare) with zero coordination would not be classified as tangled_rope; it would be snare regardless of who observes it. The tangled_rope classification is justified because: (1) the institution genuinely coordinates some functions — there is some actual service delivery, not just illusion; (2) beneficiaries (administrative incumbents) experience genuine coordination benefits (the institution's processes enable their work) alongside their extraction; (3) constituents experience genuine costs alongside genuine (if degraded) service delivery. If the institution provided zero coordination and only theater, it would be pure snare or piton (degraded snare). The mandatrophy is resolved by empirically measuring the coordination-to-theater ratio and confirming that genuine function persists, justifying the tangled classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    function_versus_theater_threshold,
    'At what ratio of theater to function does an institution cross from ''degraded but operational'' to ''extractive maintenance apparatus''?',
    'Longitudinal measurement of actual service output vs. procedural activity; correlation between theater_ratio and outcome metrics (graduation rates, patient recovery, case resolution, etc.)',
    'If threshold < 0.60: many institutions with high theater but genuine function misclassified as snares. If threshold > 0.85: extractive maintenance persists unchallenged by reform pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_versus_theater_threshold, empirical, 'Threshold distinguishing degraded coordination from extractive theater').

omega_variable(
    exit_cost_distribution,
    'How are exit costs distributed across constituent groups? Do some bear costs others can avoid, or are costs truly universal?',
    'Comparative exit analysis: tracking which constituencies remain in degraded institutions, which have exited, what their exit costs were, and whether they had alternatives',
    'If exit costs highly unequal: institution maintains power through differential trapping (snare from powerless perspective, rope from powerful). If universal: constraint is closer to rope or mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_distribution, empirical, 'Whether exit costs are distributed equally or differentially').

omega_variable(
    identity_lock_mechanism,
    'What portion of institutional persistence is explained by identity fusion (constituents cannot imagine themselves outside) vs. material barriers?',
    'Qualitative analysis of why constituents remain despite alternatives; post-exit trajectory (do identity-locked agents reconstruct identity after leaving?)',
    'If primarily identity-locked: constraint maintains power through cognitive capture; institutional reform requires identity disruption, not just structural change. If primarily material barriers: institutional reform can succeed by reducing exit costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Relative contribution of identity fusion vs. material barriers to institutional persistence').

omega_variable(
    reform_capture,
    'Do reform efforts themselves become absorbed into the institutional degradation cycle, becoming part of the theater rather than functional renewal?',
    'Historical analysis of reform proposals and outcomes; measurement of whether reforms produce output changes or just procedural changes; tracking reform effort expenditure vs. actual function improvement',
    'If reforms are captured: the institution exhibits even deeper extraction (constraints on the constrainers). If reforms succeed: piton classification is correct and sunset potential exists. If reforms partially succeed: institution is transitioning from tangled_rope to scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_capture, empirical, 'Whether institutional reform efforts are captured by degradation logic').

omega_variable(
    suppression_source,
    'Is suppression primarily structural (material barriers to exit) or narrative (constituents believe exit is impossible despite evidence)?',
    'Comparison of expressed vs. actual exit options; exit experiments (assistance removing barriers) and outcomes; post-exit asset recovery for those who do leave',
    'If narrative: institutional degradation is partially a shared hallucination; modest external intervention could catalyze exit. If structural: degradation requires sustained policy change to address underlying barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source, empirical, 'Whether suppression is structural or narrative-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_degradation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instdeg_tr_t0, institutional_degradation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(instdeg_tr_t5, institutional_degradation, theater_ratio, 5, 0.65).
narrative_ontology:measurement(instdeg_tr_t10, institutional_degradation, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(instdeg_be_t0, institutional_degradation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(instdeg_be_t5, institutional_degradation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(instdeg_be_t10, institutional_degradation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_degradation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_degradation, 0.12).
narrative_ontology:affects_constraint(institutional_degradation, regulatory_capture).
narrative_ontology:affects_constraint(institutional_degradation, organizational_sclerosis).
narrative_ontology:affects_constraint(institutional_degradation, path_dependency_lock).

% DUAL FORMULATION NOTE:
% Institutional degradation is a composite of three structurally distinct constraints: (1) regulatory_capture — how incumbent beneficiaries maintain extraction despite constituent pressure; (2) organizational_sclerosis — how reform efforts become captured by institutional inertia; (3) path_dependency_lock — how early institutional design choices create downstream lock-in that persists even when circumstances change. This story models the integrated phenomenon; see network constraints for component analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_degradation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
