% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered System (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story represents ONE READING of the contested kernel
 *   'preparedness_commitment'—specifically, the HYBRID READING, which holds
 *   that institutional preparedness operates as a genuinely layered system
 *   where memorial elements (institutional narratives, post-disaster reviews,
 *   historical case study training, continuity protocols) stabilize long-term
 *   organizational commitment while competence elements (exercised skills,
 *   updated protocols, functional testing, personnel retention) maintain
 *   actual response capacity. The hybrid reading asserts that both layers are
 *   structurally necessary: memorial commitment without competence produces
 *   the husk reading (pure performance); competence without memorial
 *   commitment produces institutional discontinuity and loss of
 *   organizational memory across generational transitions. The tension
 *   between layers creates real maintenance costs, but the hybrid reading
 *   holds that this cost is the price of genuine long-term preparedness, not
 *   a sign of system failure. The constraint exhibits tangled rope structure:
 *   genuine coordination function (both layers are required), active
 *   enforcement necessary (both memorial and competence must be
 *   institutionally maintained), and asymmetric extraction
 *   (resource-constrained agencies and field responders bear disproportionate
 *   cost of sustaining dual layers while legislative oversight and
 *   institutional legitimacy seekers benefit most).
 *
 * KEY AGENTS:
 *   - Resource-Constrained Emergency Management Agencies: Primary victims (powerless/trapped) — face irreconcilable dual mandate: maintain both memorial performance and actual competence with limited budgets
 *   - Field Responder Teams: Secondary victims (moderate/constrained) — benefit from memorial training transmission (narrative coherence, career pathways) but bear extraction cost (time in ritual exercises reduces functional capacity development)
 *   - Legislative/Oversight Authority: Primary beneficiary (institutional/arbitrage) — benefits from visible compliance (memorial elements satisfy public confidence demands) without deep competence verification; low extraction experienced
 *   - Institutional Memory Apparatus: Secondary actor (institutional/constrained) — once-functional post-disaster review processes now largely performative; maintains itself through institutional inertia
 *   - Competence-Centered Reform Coalition: Organized challenger (organized/mobile) — views hybrid system as transitional problem with sunset; building competence-prioritized alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the dual-layer structure as immutable feature of long-term institutional memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.52).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered System (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'a90d5d93-489b-42be-a9cc-8c6566e4e219').
narrative_ontology:cs_kernel_codification('a90d5d93-489b-42be-a9cc-8c6566e4e219', formalized).
narrative_ontology:cs_authority_grounding('a90d5d93-489b-42be-a9cc-8c6566e4e219', lineage).
narrative_ontology:cs_interpretation_layer_present('a90d5d93-489b-42be-a9cc-8c6566e4e219').
narrative_ontology:cs_reading_relation('a90d5d93-489b-42be-a9cc-8c6566e4e219', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a90d5d93-489b-42be-a9cc-8c6566e4e219', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_axiom('a90d5d93-489b-42be-a9cc-8c6566e4e219', foundational, memorial_elements_functionally_necessary).
narrative_ontology:cs_axiom_status(memorial_elements_functionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a90d5d93-489b-42be-a9cc-8c6566e4e219', memorial_elements_functionally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('a90d5d93-489b-42be-a9cc-8c6566e4e219', secondary, dual_layer_maintenance_cost_justified).
narrative_ontology:cs_axiom_status(dual_layer_maintenance_cost_justified, holdable).
narrative_ontology:cs_axiom_grounding('a90d5d93-489b-42be-a9cc-8c6566e4e219', dual_layer_maintenance_cost_justified, instrumental).
narrative_ontology:cs_reference_frame('a90d5d93-489b-42be-a9cc-8c6566e4e219', dual_layer_institutional_preparedness).
narrative_ontology:cs_drift_state('a90d5d93-489b-42be-a9cc-8c6566e4e219', contemporary_competence_prioritization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a90d5d93-489b-42be-a9cc-8c6566e4e219', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, institutional_legitimacy_seekers).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, competent_operational_core).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, resource_constrained_agencies).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, actual_disaster_response_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED EMERGENCY MANAGEMENT AGENCY (SNARE) — Cannot exit the dual demand: maintain both memorial performance (satisfy legislative oversight, public confidence rituals) and actual competence (train teams, test systems, update protocols). Extraction runs maximum: constrained budgets split between performative compliance and functional capacity, leaving both underfunded. No exit option — disaster response is mandatory.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD RESPONDER TEAMS (TANGLED ROPE) — Constrained by resource scarcity and institutional hierarchy, but also benefit from memorial training rituals (institutional standardization, narrative coherence of response protocols, career pathways). Genuine coordination function: memorial elements (after-action reviews, historical case study training) transmit tacit knowledge across generations. Extraction: memorial emphasis can prioritize narrative clarity over empirical effectiveness; time spent in ceremonial exercises reduces capacity for functional updates. Mixed structure.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGISLATIVE/OVERSIGHT AUTHORITY (ROPE) — Benefits from the memorial layer: visible compliance with preparedness mandates (public reporting, drill records, standardized protocols) satisfies constituent demands without requiring deep competence verification. The constraint solves a genuine coordination problem: how to maintain public confidence in disaster readiness without year-round operational overhead. From this perspective, the system is largely coordination with minimal extraction — memorial elements substitute for continuous operational verification.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETENCE-CENTERED REFORM COALITION (SCAFFOLD) — Organized actors (advanced training networks, cross-jurisdictional simulation consortia, evidence-based protocols) view the hybrid system as a transitional problem with a sunset. They are building competence-prioritized alternatives: real-time system testing, data-driven exercises, performance metrics. As these mature (estimated 10-15 years), the performative memorial layer becomes redundant — the competence layer generates its own legitimacy through demonstrated effectiveness. Low effective extraction because this perspective has agency and sees an exit path.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY APPARATUS (PITON) — The memorial layer (historical narratives, standardized post-mortems, continuity protocols) persists through institutional inertia despite degradation of its functional purpose. It was once alive — after-action reviews genuinely transmitted critical knowledge. Now largely performative: ceremonies and reports feel like retention but lack operational bite. Theater_ratio high: the apparatus maintains itself through ritual conformity rather than demonstrated necessity. Its primary function now is stabilizing institutional commitment (preventing abandonment of preparedness) rather than transmitting competence.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some separation between memorial (stabilizing commitment across generations) and competence (maintaining functional response capacity) may be intrinsic to how long-term institutional preparedness works. The constraint appears as an irreducible tension: commitment without narrative continuity dissolves; competence without memorial grounding lacks cross-generational persistence. This perspective risks naturalizing a contingent institutional arrangement. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_commitment__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid reading characterizes the memorial layer as functionally necessary for cross-generational commitment, not as pure extraction. However, the dual-layer requirement creates real overhead: maintaining both memorial narratives AND competence capacity is more resource-intensive than either alone. The measured extractiveness reflects that the memorial layer does create asymmetric cost (concentrated on resource-constrained implementers) even though it serves a genuine coordination function. Suppression (0.52): Moderate-high. The system suppresses alternatives through institutional lock-in: agencies cannot adopt competence-only approaches without political backlash (loss of visible compliance indicators); they cannot abandon preparedness systems entirely (mandatory legislative requirements). Memorial elements specifically suppress the emergence of purely data-driven, non-narrative assessment methods. Theater ratio (0.58): Moderate. Unlike the husk reading (which would score ~0.85, pure performance), the hybrid reading scores moderate because memorial elements do carry functional weight — post-disaster reviews and historical case studies genuinely transmit tacit knowledge, not just ritual performance. However, the ratio is elevated above a fully competence-driven system (~0.25) because the memorial layer emphasizes narrative coherence and institutional continuity over empirical effectiveness testing. The measurement trajectory shows increasing theater over the interval as competence-prioritized reforms (driven by the reform coalition) create visible alternatives, pushing the existing system toward greater ritual emphasis to justify its persistence.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading produces maximum perspectival divergence. Resource-constrained agencies experience snare (trapped in dual mandate with insufficient resources); field responders experience tangled rope (coordinated through memorial training, extracted through time overhead); oversight authority experiences rope (coordination without deep verification). The reform coalition sees scaffold (transitional system with sunset). The memorial apparatus sees piton (degraded ritual). The analytical observer risks seeing mountain (naturalizing layered structure as inevitable). This perspectival gap reflects genuine structural ambiguity: Is the layering a feature (necessary for institutional stability) or a bug (inefficient double-processing)? The hybrid reading answers: feature in the short term, but the competence-centered reform coalition is building alternatives that will make it obsolete.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to extraction flow. Oversight authority benefits with high exit capacity (arbitrage options) → low d (~0.12-0.18) → negative or minimal experienced extraction. Resource-constrained agencies trapped in dual mandate → high d (~0.80) → maximum experienced extraction despite moderate base_extractiveness. Field responders constrained (moderate costs to exit, some benefits) → moderate d (~0.50-0.60) → moderate experienced extraction. The memorial apparatus benefits from institutional inertia (low d) but appears as piton rather than rope because its primary function (transmitting knowledge) has degraded while its existence is maintained through ritual. The analytical observer at civilizational scope may default to neutral d (0.50) producing moderate χ, but this risks masking the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading's tangled rope classification resolves the apparent contradiction between 'preparedness is working' (memorial maintains commitment, field operates with institutional coherence) and 'preparedness is broken' (competence erodes, memorial becomes theater). The constraint genuinely coordinates (both layers necessary) while genuinely extracting (dual burden on constrained actors). The mandatrophy resolves not by choosing one characterization but by recognizing that different observers legitimately perceive different primary functions: oversight perceives coordination (rope), victims perceive extraction (snare), reformers perceive transience (scaffold), the apparatus perceives degradation (piton). The hybrid reading's key insight: the apparent 'inefficiency' of dual layers is not a bug but a necessary feature of how institutional systems maintain generational continuity without sacrificing current operational capacity. The cost is real; the function is genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_versus_competence_decoupling,
    'Are the memorial layer and competence layer genuinely decoupled, or does memorial activity actively impair competence development?',
    'Comparative analysis of preparedness outcomes in systems with high memorial:competence ratio vs. competence-prioritized systems over identical disaster classes. Track resource allocation shifts and outcome metrics.',
    'If decoupled (parallel tracks, both functional): hybrid classification holds — tangled rope with real coordination. If actively decoupled (memorial crowds out competence): reclassify toward snare — extraction hidden as coordination. If complementary (memorial improves competence retention): reclassify toward rope — genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_versus_competence_decoupling, empirical, 'Whether memorial and competence layers interact constructively or destructively').

omega_variable(
    commitment_stabilization_necessity,
    'Does the memorial layer genuinely stabilize institutional commitment to preparedness, or do competence incentives alone suffice to maintain the system?',
    'Historical analysis of preparedness system abandonment or degradation in jurisdictions that attempted competence-only approaches vs. those maintaining memorial elements. Longitudinal tracking of political support shifts.',
    'If memorial is necessary: hybrid structure justified; extraction cost is real maintenance burden for genuine institutional stability. If competence alone stabilizes commitment: memorial is pure overhead; reclassify to snare or toward competence_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_stabilization_necessity, empirical, 'Whether memorial layer is functionally necessary for maintaining institutional commitment').

omega_variable(
    inter_reading_foreclosure_risk,
    'Does the hybrid reading''s core premise (dual-layer necessity) logically foreclose the husk_reading''s premise (memorial is pure performance), or do both remain live in different institutional contexts?',
    'Structural analysis of whether a system can simultaneously hold (a) memorial elements are necessary for commitment stability, and (b) memorial elements are performative husk with zero functional purpose. Can both be true in different institutions, or does one exclude the other within rational institutional design?',
    'If they foreclose (mutually exclusive): relation should be forecloses. If both are hold-able in different contexts: relation should be coexists_with. Resolution determines the reading_relations axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inter_reading_foreclosure_risk, conceptual, 'Logical relationship between hybrid and husk readings').

omega_variable(
    competence_reading_influence_directionality,
    'Does the hybrid reading influence the competence_reading by creating structural pressure toward dual-layer systems, or does the competence_reading influence the hybrid reading by delegitimizing memorial elements?',
    'Causal chain analysis: which reading''s adoption changes the conditions under which the other reading becomes viable? Does hybrid adoption (accepting dual layers) make competence-only approaches harder to sustain (influences)? Or does competence adoption make hybrid arrangements untenable (influences in opposite direction)?',
    'If hybrid influences competence: relation is influences (hybrid → competence). If competence influences hybrid: reverse. Resolution determines reading_relations edge direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_influence_directionality, conceptual, 'Causal directionality between hybrid and competence readings').

omega_variable(
    maintenance_cost_as_extraction,
    'Is the resource cost of maintaining dual-layer systems (memorial + competence) properly characterized as extraction, or is it coordination overhead?',
    'Cost-benefit analysis: does the memorial layer''s presence increase total system cost compared to competence-only approaches? Is the increase proportional to memorial''s actual functional contribution, or is it a hidden extraction cost absorbed by constrained actors?',
    'If proportional overhead: extraction is modest, tangled rope classification holds. If disproportionate: extract rises, snare classification more accurate. Determines whether base_extractiveness (0.38) should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_cost_as_extraction, empirical, 'Whether dual-layer maintenance cost is justified by dual-layer benefit').

omega_variable(
    reading_identity_vs_kernel_identity,
    'Is ''preparedness_commitment'' a single kernel with three readings (hybrid, husk, competence), or are these three structurally distinct constraints packaged as a single kernel?',
    'Structural analysis: do all three readings share the same underlying tension (memorial vs. competence in institutional preparedness), or does each reading describe a different underlying constraint? Apply ε-invariance: if measuring preparedness under the hybrid reading produces ε=0.38, does measuring under husk reading produce same ε, or materially different value?',
    'If same kernel, three readings: current structure holds. If three distinct constraints: decompose into three separate stories with separate network edges. Determines whether reading_relations logic applies or whether these are sibling constraints rather than sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_vs_kernel_identity, conceptual, 'Whether three readings share one kernel or represent separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_hybrid_theater_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_hybrid_theater_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(prep_hybrid_theater_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(prep_hybrid_extract_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_hybrid_extract_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(prep_hybrid_extract_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_hybrid_suppress_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_hybrid_suppress_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(prep_hybrid_suppress_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, institutional_memory_transmission).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, disaster_response_capacity).

% DUAL FORMULATION NOTE:
% The 'preparedness_commitment' kernel has three readings: competence_reading (ε~0.15, rope/scaffold), husk_reading (ε~0.72, piton/snare), and hybrid_reading (ε~0.38, tangled_rope—this story). Each reading instantiates a structurally distinct constraint with different beneficiary/victim structures, different temporal dynamics, and different classification. The hybrid reading's ε (0.38) reflects the cost of dual-layer maintenance; the competence reading's ε would reflect pure functional efficiency; the husk reading's ε would reflect pure performance overhead. These are not the same constraint measured differently—they are genuinely distinct claims about what preparedness systems ARE. The network edges link them as kernel siblings (reading_relations in cs_structure), not as constraint family members (no external ε-invariance relationship).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
