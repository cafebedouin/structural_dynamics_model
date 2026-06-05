% ============================================================================
% CONSTRAINT STORY: ergonomic_externality_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergonomic_externality_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergonomic_externality_mountain
 *   human_readable: Ergonomic Externality Mountain: Physical Limits of Postural Deviation from Neutral Spine
 *   domain: biomechanics/occupational_health/institutional_constraints
 *
 * SUMMARY:
 *   The ergonomic externality mountain represents a genuine natural law
 *   constraint (vertebrate spine biomechanics) that industries have
 *   instrumentally naturalized to avoid retrofitting costs. The constraint is
 *   real: sustained non-neutral posture produces cumulative intervertebral
 *   disc stress that, at sufficiently high doses and durations, causes
 *   herniation, stenosis, and chronic pain. This outcome is not culturally
 *   contingent—it appears across all populations and occupational contexts.
 *   However, the magnitude of the externality and its visibility depend
 *   critically on institutional choices about workstation design, movement
 *   frequency, and postural accommodation. The key insight is that this story
 *   declares beneficiaries (industrial production systems, office furniture
 *   manufacturers, the sedentary work institution) on what claims to be a
 *   natural law. This triggers the false summit evaluation: if these
 *   institutions genuinely benefit from treating the constraint as natural
 *   (avoiding design costs), the constraint may be a Snare or Tangled Rope
 *   disguised as a Mountain. The engine's FSM detector will evaluate whether
 *   the beneficiary presence indicates institutional capture of the natural
 *   law framing.
 *
 * KEY AGENTS:
 *   - Worker Spine (Biomechanics): Universal victim (powerless/trapped) — cannot exit the constraint; bears full cost of postural deviation through cumulative disc stress. The constraint is immutable at biographical timescale.
 *   - Industrial Production System: Institutional beneficiary (institutional/arbitrage) — benefits from treating ergonomic accommodation as optional rather than required; externalizes redesign costs. May be a false summit beneficiary if the institution actively maintains the naturalized framing.
 *   - Office Furniture Manufacturers: Institutional beneficiary (institutional/arbitrage) — standardized non-ergonomic furniture is cheaper to produce than adjustable ergonomic systems; benefits from the industry-wide acceptance of non-neutral posture as inevitable rather than addressable.
 *   - Occupational Health Researchers: Analytical observer (analytical/analytical) — document the dose-response relationship between posture and disc degeneration; recognize both the genuine biomechanical limit and the institutional choice to not mitigate it.
 *   - Occupational Safety Regulators: Institutional actor (institutional/arbitrage) — maintain performative oversight (compliance theater) while allowing actual working conditions to persist unchanged; the regulation system sees its own activity as degraded (piton perspective).
 *   - Sedentary Work Institution: Systemic beneficiary (institutional/arbitrage) — the entire complex of office work, factory rationalization, and industrial management practices benefits from treating the human spine's postural limits as natural and unchangeable rather than addressable through design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergonomic_externality_mountain, 0.18).
domain_priors:suppression_score(ergonomic_externality_mountain, 0.04).
domain_priors:theater_ratio(ergonomic_externality_mountain, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergonomic_externality_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(ergonomic_externality_mountain, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(ergonomic_externality_mountain, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ergonomic_externality_mountain, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ergonomic_externality_mountain, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergonomic_externality_mountain, mountain).
narrative_ontology:human_readable(ergonomic_externality_mountain, "Ergonomic Externality Mountain: Physical Limits of Postural Deviation from Neutral Spine").
narrative_ontology:topic_domain(ergonomic_externality_mountain, "biomechanics/occupational_health/institutional_constraints").

domain_priors:emerges_naturally(ergonomic_externality_mountain).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergonomic_externality_mountain, industrial_production_systems).
narrative_ontology:constraint_beneficiary(ergonomic_externality_mountain, office_furniture_manufacturers).
narrative_ontology:constraint_beneficiary(ergonomic_externality_mountain, sedentary_work_institution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKER'S SPINE (MOUNTAIN) — The human spine has immutable biomechanical limits. Sustained postural deviation from neutral position (kyphosis, hyperlordosis, lateral flexion) produces cumulative intervertebral disc stress that accumulates toward herniation, stenosis, and chronic pain regardless of institutional accommodation. These are material constraints: disc anulus fibrosis has tensile strength limits; nucleus pulposus hydration state is governed by osmotic gradients; vertebral endplate permeability follows finite laws. No exit from these limits at biographical timescale — they are unchangeable features of vertebrate spine architecture.
constraint_indexing:constraint_classification(ergonomic_externality_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: OCCUPATIONAL HEALTH RESEARCHER (MOUNTAIN) — Across generational timescales and global epidemiology, the dose-response relationship between sustained non-neutral posture and degenerative disc disease is invariant and irreducible. No matter the institutional setting, no matter the cultural approach to work, the underlying biomechanical constraint appears: prolonged kyphosis correlates with herniation; prolonged extension correlates with facet joint arthrosis. This is not an artifact of Western office design — it appears in agricultural labor, factory work, and traditional crafts globally. The constraint is a natural law of spine physiology, not an institutional contingency.
constraint_indexing:constraint_classification(ergonomic_externality_mountain, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INDUSTRIAL PRODUCTION SYSTEM (TANGLED ROPE) — The institution benefits from treating ergonomic externality as a natural law rather than a design choice. Designing workstations to maintain neutral spine posture requires capital investment (adjustable desks, ergonomic chairs, layout redesign) and production flow redesign. The institution coordinates production—a genuine function—while simultaneously extracting from workers by externalizing postural accommodation costs. The institution declares the spine's limits 'natural' to avoid responsibility for redesign, but the decision to compress workstations, eliminate adjustability, and rationalize seated positions is extractive and active (requires_active_enforcement). The institution both benefits from coordination and extracts through design choices presented as inevitable.
constraint_indexing:constraint_classification(ergonomic_externality_mountain, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN) — At the universal/civilizational level, the ergonomic constraint is a genuine natural law. Vertebrate spine architecture is the product of evolutionary tradeoffs between upright posture and bipedal locomotion; the resulting structure has fixed biomechanical properties. Deviation from neutral spine under load produces measurable stress on disc matrix. This constraint is unchangeable—not by institutional design, not by cultural practice, not by technological intervention (short of fundamental spine redesign, which is not presently possible). The analytical observer recognizes that institutions CAN mitigate the harm through design—neutral posture is achievable with proper workstation setup—but the underlying constraint that deviation produces cost is immutable.
constraint_indexing:constraint_classification(ergonomic_externality_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: OCCUPATIONAL SAFETY REGULATION (PITON) — Regulatory standards for ergonomics (OSHA guidelines, ISO 11228, EN 527) are substantially performative. The standards reference 'neutral posture' and 'adjustable workstations' as compliance markers, but enforcement is weak, monitoring is rare, and penalties are low relative to the cost of redesign. The regulatory system performs oversight—creates safety committees, mandates ergonomic assessments—while allowing actual working conditions to degrade. The original function (protecting worker spine health) has atrophied; the ritual persists (compliance theater) because alternatives haven't fully replaced it. Theater ratio is high—the amount of regulatory activity relative to actual improvement in spinal health is substantial.
constraint_indexing:constraint_classification(ergonomic_externality_mountain, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergonomic_externality_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergonomic_externality_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergonomic_externality_mountain, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ergonomic_externality_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergonomic_externality_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ergonomic_externality_mountain, ExtMetricName, E),
    domain_priors:suppression_score(ergonomic_externality_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ergonomic_externality_mountain),
    narrative_ontology:constraint_metric(ergonomic_externality_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ergonomic_externality_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergonomic_externality_mountain, TR),
    TR >= 0.70.

:- end_tests(ergonomic_externality_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint's base extractiveness is kept low because the underlying phenomenon—disc stress from postural deviation—is a genuine biomechanical fact. There is no extraction engine per se; there is only an immutable physical limit. The extractiveness increases from 0.12 to 0.18 over the measurement interval (10 years) due to increasing institutional dependence on sedentary office work and declining average physical conditioning, which raises the average dose of non-neutral posture. Suppression (0.04): Negligible. There are no artificial barriers to understanding the constraint—biomechanics is well-documented, and ergonomic interventions are well-established. Workers can theoretically access information about neutral posture and request adjustable workstations. The low suppression reflects that the constraint is not enforced by secrecy or alternative framing; it is simply the physical world. Theater ratio (0.15): Very low. The constraint itself is not performative. However, the occupational safety regulatory system that addresses the constraint has high theater (piton perspective), which creates measurement ambiguity. The 0.15 value reflects the base constraint (nearly zero theater) plus institutional noise from the regulatory layer.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the mountain perspective (genuine natural law) and the tangled rope perspective (institutional extraction). The worker's spine experiences the constraint as immutable (mountain). The industrial production system experiences the constraint as a design choice presented as inevitable (tangled rope—they both coordinate production AND extract through non-accommodative design). The analytical observer recognizes that both are true: the constraint is immutable (you cannot design a spine that bends indefinitely without cost), but the magnitude of the externality is contingent on institutional choices about workstation design and movement frequency. The piton perspective adds a third layer: the regulatory system performs oversight of ergonomic safety while allowing non-neutral posture to persist, creating a theatrical appearance of protection without functional change.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary directionality values are computed from institutional/arbitrage combinations: industrial production and furniture manufacturers have low d values (beneficiaries with exit options) because they can exit the constraint's effects by paying for redesign, but choose not to. The worker has high d (victim/trapped) because they cannot exit—their spine is bound by the constraint, and they have no option to leave their own biomechanics. The analytical observer has d ≈ 0.72 (canonical analytical value) because they observe the constraint from outside all institutional positions. The piton regulatory system has low d (institutional/arbitrage) because they have options—they could increase enforcement, mandate adjustable workstations, set higher standards—but treat these as optional and performative.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely a mountain (immutable biomechanical limit) while simultaneously acknowledging that the institutional response—treating it as natural and unchangeable rather than addressable through design—is itself extractive. The false summit hypothesis is the key: if institutions benefit from naturalizing the constraint, the beneficiary declarations are not spurious, and the engine's FSM detector will flag this as a false summit candidate. The mountain classification would be reclassified to Tangled Rope (or possibly Snare) if the empirical evidence supports that institutions actively maintain the naturalization to avoid costs. The constraint resolves by separating the genuine natural law (disc stress from postural deviation is immutable) from the institutional contingency (the magnitude of harm is addressable through design, and institutions choose not to address it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_naturalization,
    'Is the ergonomic externality a genuine natural law of spine biomechanics, or a contingent institutional arrangement that industries have naturalized to avoid retrofit costs?',
    'Comparative analysis of workstation design across industries and cultures; measurement of herniation/stenosis rates in occupational groups with active postural accommodation (surgeons, craftspeople with adjustable setups) vs. sedentary workers; historical analysis of when the ''sedentary office'' became normalized (1950s industrial management rationalization, not inherent to computation or knowledge work)',
    'If genuine natural law: institutions cannot eliminate the externality, only mitigate. If naturalized contingency: the constraint is Snare or Tangled Rope (extractive), and institutional redesign has substantial room. The beneficiary declarations suggest the latter—industries benefit from externalizing adjustment costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_naturalization, empirical, 'Whether ergonomic constraint is natural law or naturalized institutional arrangement').

omega_variable(
    spine_adaptation_ceiling,
    'Can postural accommodation systems (active seating, core strength conditioning, movement micro-breaks) reduce the harm from non-neutral posture to negligible levels, or is there an irreducible biomechanical cost?',
    'Longitudinal studies of herniation/stenosis rates in high-exercise populations vs. sedentary populations, controlling for age/weight/genetics; RCT of intensive core training + frequent posture breaks on disc degeneration markers; historical data on prevalence in occupational groups with mandatory movement (military, dance, agriculture with frequent position changes)',
    'If adaptation can eliminate harm: the constraint is not truly a mountain—it''s Tangled Rope with a coordination solution that institutions choose not to fund. If irreducible: mountain classification holds, but institutions can still reduce magnitude through design mitigation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spine_adaptation_ceiling, empirical, 'Whether postural accommodation can eliminate ergonomic harm').

omega_variable(
    false_summit_hypothesis,
    'Are the identified beneficiaries (industrial production, furniture manufacturers, sedentary office institution) actually benefiting from treating ergonomic constraint as natural law, or is this declaration spurious?',
    'Cost-benefit analysis: redesigning workstations to support neutral posture (adjustable desks, lumbar support, frequent movement) vs. worker healthcare costs (back pain treatment, herniation surgery, worker''s compensation, lost productivity); market analysis of whether ergonomic design increases or decreases competitive advantage; historical analysis of when standardized non-ergonomic office furniture became the default',
    'If beneficiaries are real: this is an FSM candidate—institutions actively benefit from the naturalization, making the mountain a false summit. If spurious: genuine mountain with incidental institutional alignment. The presence of beneficiary declarations on a mountain requires this omega per schema.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_hypothesis, empirical, 'Whether beneficiaries genuinely exist and benefit from naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergonomic_externality_mountain, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergomon_tr_t0, ergonomic_externality_mountain, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ergomon_tr_t5, ergonomic_externality_mountain, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ergomon_tr_t10, ergonomic_externality_mountain, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(ergomon_be_t0, ergonomic_externality_mountain, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ergomon_be_t5, ergonomic_externality_mountain, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ergomon_be_t10, ergonomic_externality_mountain, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergonomic_externality_mountain, resource_allocation).
narrative_ontology:affects_constraint(ergonomic_externality_mountain, occupational_health_externality).
narrative_ontology:affects_constraint(ergonomic_externality_mountain, sedentary_work_normalization).
narrative_ontology:affects_constraint(ergonomic_externality_mountain, office_design_standardization).

% DUAL FORMULATION NOTE:
% The ergonomic externality mountain is upstream of several institutional constraints. The underlying biomechanical limit (spine stress from non-neutral posture) is a genuine natural law. The institutional response—treating it as unchangeable rather than addressing it through design—creates downstream constraints in occupational health policy, workplace standardization, and furniture manufacturing. Each downstream constraint has its own ε value reflecting the degree to which institutional choice (rather than biomechanical inevitability) drives the outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergonomic_externality_mountain, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
