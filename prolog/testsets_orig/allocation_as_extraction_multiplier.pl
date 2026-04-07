% ============================================================================
% CONSTRAINT STORY: allocation_as_extraction_multiplier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_allocation_as_extraction_multiplier, []).

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
 *   constraint_id: allocation_as_extraction_multiplier
 *   human_readable: State Resource Allocation as Extraction Multiplier in Artisan Labor Systems
 *   domain: political_economy/labor_systems/knowledge_transmission
 *
 * SUMMARY:
 *   The state resource allocation system for artisan labor operates as an
 *   extraction multiplier by compounding an immutable physiological
 *   constraint (altitude hypoxia affecting flame work quality) with a
 *   policy-contingent allocation constraint (Class I vs Class II gas pressure
 *   designation). The physiological constraint is a true mountain — oxygen
 *   partial pressure at high altitude cannot be changed at biographical
 *   timescales, and it directly affects flame temperature, combustion
 *   stability, and the artisan's cognitive and motor precision. But the
 *   allocation constraint is a tangled rope — the state planning apparatus
 *   makes classification decisions that determine which facilities receive
 *   optimal gas pressure (Class I: enables blue-core flames with minimal
 *   yellow fringe) and which receive suboptimal pressure (Class II: produces
 *   yellow-heavy flames that limit fine detail work). The system exhibits
 *   genuine coordination function (distributing scarce optimal conditions
 *   enables some high-quality production) but embeds asymmetric extraction
 *   (peripheral artisans are denied access to conditions that would enable
 *   mastery-level work, while state-aligned facilities concentrate benefits).
 *   The constraint is identity-locked for many artisans whose professional
 *   identity and kinship networks are fused with geographic location and
 *   craft tradition, making exit structurally possible but psychologically
 *   unthinkable. The theater ratio (0.48) reflects moderate performative
 *   content: facility classification reviews occur but often rubber-stamp
 *   existing designations; temporary elevation requests are processed but
 *   rarely granted; quality standards are documented but enforcement is
 *   selective.
 *
 * KEY AGENTS:
 *   - Pu-Classified Artisans: Primary victim (powerless/identity_locked) — cannot exit without abandoning professional identity; experience allocation system as pure extraction compounding physiological constraint
 *   - Mobile Artisans: Secondary victim (moderate/constrained) — have transferable skills and could relocate at high cost; experience system as mixed coordination and extraction
 *   - State Planning Apparatus: Primary beneficiary (institutional/arbitrage) — controls classification decisions and resource allocation; experiences system as pure coordination solving legitimate scarcity problem
 *   - Class I Facility Administrators: Secondary beneficiary (institutional/constrained) — receive preferential allocation but face administrative burden and reclassification risk; experience system as mixed
 *   - Reform Coalition: Organized agents (organized/mobile) — artisan guilds and technical advisors building alternative pathways through portable pressure technology and decentralized classification; see system as temporary with concrete sunset
 *   - Knowledge Transmission Integrity: Abstract victim (powerless/trapped) — intergenerational craft knowledge degrades when optimal working conditions are systematically denied; no advocate and no exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(allocation_as_extraction_multiplier, 0.58).
domain_priors:suppression_score(allocation_as_extraction_multiplier, 0.72).
domain_priors:theater_ratio(allocation_as_extraction_multiplier, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(allocation_as_extraction_multiplier, extractiveness, 0.58).
narrative_ontology:constraint_metric(allocation_as_extraction_multiplier, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(allocation_as_extraction_multiplier, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(allocation_as_extraction_multiplier, tangled_rope).
narrative_ontology:human_readable(allocation_as_extraction_multiplier, "State Resource Allocation as Extraction Multiplier in Artisan Labor Systems").
narrative_ontology:topic_domain(allocation_as_extraction_multiplier, "political_economy/labor_systems/knowledge_transmission").

domain_priors:requires_active_enforcement(allocation_as_extraction_multiplier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(allocation_as_extraction_multiplier, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(allocation_as_extraction_multiplier, class_i_facilities).
narrative_ontology:constraint_victim(allocation_as_extraction_multiplier, pu_classified_artisans).
narrative_ontology:constraint_victim(allocation_as_extraction_multiplier, knowledge_transmission_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PU-CLASSIFIED ARTISAN (SNARE) — Identity-locked to craft tradition and geographic location. Cannot exit without abandoning professional identity and kinship networks. Experiences allocation system as pure extraction: physiological constraint (altitude hypoxia) is real and unchangeable, but denial of temporary elevation access or Class I gas pressure is policy choice that compounds the natural limit. The artisan sees clearly that peers at lower altitude or in Class I facilities produce superior work, but cannot access those conditions. Maximum experienced extraction because identity fusion prevents exit despite structural mobility existing in principle.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: MOBILE ARTISAN (TANGLED ROPE) — Has skills transferable across regions and could relocate at significant cost (family disruption, loss of workshop investment, reputational reset). Experiences allocation system as mixed: genuine coordination function exists (gas distribution network, facility classification standards enable some level of craft production) but asymmetric extraction is clear (Class II designation denies access to optimal conditions that would enable mastery-level work). Can see the exit path but the cost is high. Moderate effective extraction because exit options exist but are expensive.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE PLANNING APPARATUS (ROPE) — Experiences allocation system as pure coordination: distributing scarce resources (Class I gas pressure, low-altitude facility access) according to production priorities. From this position, the system solves a legitimate collective action problem (how to allocate limited optimal conditions across competing demands). The planning apparatus has full arbitrage capacity — can reassign classifications, grant exceptions, or relocate facilities. Sees no extraction because the system runs toward this agent, not away from it. Net beneficiary of the coordination mechanism.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLASS I FACILITY ADMINISTRATOR (TANGLED ROPE) — Institutional actor that benefits from preferential allocation (receives Class I gas pressure, optimal altitude access) but is also constrained by the system (must justify classification, meet production quotas, navigate bureaucratic oversight). Experiences the allocation system as mixed coordination and extraction: genuine coordination function (enables high-quality production through resource concentration) but also extraction (administrative burden, loss of autonomy, vulnerability to reclassification). Exit options are constrained — could advocate for system reform but at career risk. Moderate effective extraction because benefits and costs are both significant.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (artisan guilds, regional administrators, technical advisors) advocating for allocation reform see the current system as temporary: portable gas pressure regulation technology, modular workshop infrastructure, and decentralized facility classification are creating pathways to bypass the extraction mechanism. The coalition has identified that the physiological constraint (altitude hypoxia) is immutable but the allocation constraint (Class I vs Class II designation) is policy-contingent and solvable through technical and institutional innovation. Estimated sunset: 15-25 years as portable pressure systems mature and regional autonomy increases. Low effective extraction because the coalition has agency and sees a concrete exit path.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the allocation system exhibits both genuine coordination (distributing scarce optimal conditions enables some high-quality production that would not occur under pure market allocation or random distribution) and asymmetric extraction (the classification system concentrates benefits on state-aligned facilities and extracts from peripheral artisans whose work quality is artificially constrained). The analytical observer sees that the physiological constraint is a true mountain (altitude hypoxia is immutable at biographical timescales) but the allocation constraint is a policy choice that multiplies the natural limit's impact. The system could coordinate without extracting — alternative allocation mechanisms exist — but the current implementation embeds extraction into the coordination function.
constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(allocation_as_extraction_multiplier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(allocation_as_extraction_multiplier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(allocation_as_extraction_multiplier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(allocation_as_extraction_multiplier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The allocation system extracts from peripheral artisans by denying access to optimal working conditions (Class I gas pressure, low-altitude facilities, temporary elevation) that would enable mastery-level work. The extraction is compounded by the physiological constraint — artisans at high altitude face both natural limits (hypoxia) AND policy limits (Class II designation), while artisans at low altitude in Class I facilities face only natural advantages. The value reflects that the extraction is significant but not total — some Class II artisans produce high-quality work through compensatory technique, and some mobility exists. Suppression (0.72): High. Barriers to exit include identity fusion with craft tradition and geographic location, kinship network dependence, workshop capital investment, bureaucratic obstacles to facility reclassification, and denial of temporary elevation requests. The suppression is structural (economic and administrative barriers) and internalized (identity lock makes exit unthinkable for many artisans). Theater ratio (0.48): Moderate. Facility classification reviews occur but often confirm existing designations without substantive evaluation. Temporary elevation requests are processed through formal channels but rarely granted. Quality standards are documented but enforcement is selective and politically influenced. The theater has increased over the interval as the gap between documented criteria and actual decision-making has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a policy-contingent allocation system can multiply the impact of an immutable natural constraint. The Pu-classified artisan sees a snare — trapped by identity fusion and systematically denied access to conditions that would enable mastery. The mobile artisan sees a tangled rope — mixed coordination and extraction with expensive exit options. The state planning apparatus sees a rope — legitimate coordination solving a scarcity problem. The Class I facility administrator sees a tangled rope from the beneficiary side — receives benefits but faces constraints. The reform coalition sees a scaffold — temporary problem with a technical and institutional sunset. The analytical observer sees a tangled rope at the civilizational level — genuine coordination function exists but the current implementation embeds extraction that could be eliminated through alternative allocation mechanisms. The perspectival gap reveals that the 'scarcity' justification (limited optimal conditions must be allocated) naturalizes a policy choice (how to allocate) as a natural constraint (what is scarce).
 *
 * DIRECTIONALITY LOGIC:
 *   The Pu-classified artisan (powerless/identity_locked/victim) experiences maximum extraction — identity fusion with craft and place prevents exit despite structural mobility existing in principle, and the allocation system compounds the physiological constraint by denying access to optimal conditions. The mobile artisan (moderate/constrained/victim) experiences moderate extraction — can see the exit path and has transferable skills, but relocation cost is high. The state planning apparatus (institutional/arbitrage/beneficiary) experiences negative extraction — the system runs toward this agent, concentrating decision-making power and enabling resource capture. The Class I facility administrator (institutional/constrained/beneficiary) experiences low extraction — benefits from preferential allocation but faces administrative constraints. The reform coalition (organized/mobile/beneficiary) experiences low extraction — has agency to build alternative pathways and sees a concrete sunset. The analytical observer sees the full structure: genuine coordination function (distributing scarce optimal conditions) embedded with asymmetric extraction (classification system concentrates benefits and denies access).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing the immutable physiological constraint (altitude hypoxia — a true mountain) from the policy-contingent allocation constraint (Class I vs Class II designation — a tangled rope). The allocation system is not pure extraction (snare) because it has genuine coordination function — distributing scarce optimal conditions enables some high-quality production that would not occur under random distribution. But it is also not pure coordination (rope) because the classification system embeds asymmetric extraction — peripheral artisans are systematically denied access while state-aligned facilities concentrate benefits. The tangled rope classification captures both structural features: coordination function (resource distribution network) and extraction mechanism (classification system that compounds natural constraint). The identity-locked perspective reveals that the extraction is particularly severe for artisans whose professional identity is fused with craft tradition and geographic location — they experience the system as a snare because exit is psychologically unthinkable, even though structural mobility exists in principle. The scaffold perspective reveals that the extraction mechanism is not permanent — portable pressure technology and decentralized classification are creating alternative pathways with a concrete sunset timeline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporary_elevation_feasibility,
    'Is temporary elevation access (seasonal migration to low-altitude facilities for critical production phases) technically and economically feasible at scale, or does it impose coordination costs that justify denial?',
    'Pilot programs testing temporary elevation access; cost-benefit analysis of transportation, housing, and production continuity vs quality gains; comparison with historical precedents of seasonal artisan migration',
    'If feasible at reasonable cost: denial of temporary elevation is pure extraction (system is snare from more perspectives). If prohibitively expensive: denial is coordination constraint (system is rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporary_elevation_feasibility, empirical, 'Whether temporary elevation access is feasible at scale').

omega_variable(
    class_designation_criteria_transparency,
    'Are Class I vs Class II facility designations based on transparent production criteria, or do they reflect political favoritism and rent-seeking?',
    'Audit of classification decisions; correlation analysis between facility classification and production metrics vs political connections; longitudinal tracking of reclassification patterns',
    'If transparent and merit-based: allocation system has genuine coordination function (tangled rope confirmed). If politically captured: allocation system is primarily extraction mechanism (snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_designation_criteria_transparency, empirical, 'Whether facility classification criteria are transparent and merit-based').

omega_variable(
    portable_pressure_technology_timeline,
    'What is the realistic timeline for portable gas pressure regulation technology to become economically accessible to Class II facilities?',
    'Technology development tracking; cost curve projections; adoption rate analysis in comparable industrial contexts',
    'If timeline < 10 years: scaffold perspective confirmed (sunset is real and near). If timeline > 30 years: scaffold perspective is aspirational rather than structural (extraction persists for another generation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portable_pressure_technology_timeline, empirical, 'Timeline for portable pressure technology to reach economic accessibility').

omega_variable(
    knowledge_transmission_degradation_rate,
    'At what rate does denial of optimal working conditions degrade intergenerational knowledge transmission in craft traditions?',
    'Longitudinal study of craft quality across generations in Class II vs Class I facilities; documentation of tacit knowledge loss; comparison of apprenticeship success rates',
    'If degradation is rapid (< 2 generations): extraction impact is severe and irreversible (victim group expands to include future generations). If degradation is slow or recoverable: extraction impact is contained to current generation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_transmission_degradation_rate, empirical, 'Rate of knowledge transmission degradation under suboptimal conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(allocation_as_extraction_multiplier, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alloc_extract_tr_t0, allocation_as_extraction_multiplier, theater_ratio, 0, 0.35).
narrative_ontology:measurement(alloc_extract_tr_t8, allocation_as_extraction_multiplier, theater_ratio, 8, 0.42).
narrative_ontology:measurement(alloc_extract_tr_t16, allocation_as_extraction_multiplier, theater_ratio, 16, 0.48).

% Extraction over time
narrative_ontology:measurement(alloc_extract_be_t0, allocation_as_extraction_multiplier, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(alloc_extract_be_t8, allocation_as_extraction_multiplier, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(alloc_extract_be_t16, allocation_as_extraction_multiplier, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(allocation_as_extraction_multiplier, resource_allocation).
narrative_ontology:affects_constraint(allocation_as_extraction_multiplier, physiological_constraint_ceiling).

% DUAL FORMULATION NOTE:
% The allocation constraint is downstream of the physiological constraint (altitude hypoxia) but represents a distinct structural mechanism. The physiological constraint is a true mountain (immutable at biographical timescales) with ε ≈ 0.08. The allocation constraint is a tangled rope (policy-contingent coordination with embedded extraction) with ε = 0.58. The allocation system multiplies the impact of the physiological constraint by denying access to compensatory mechanisms (temporary elevation, optimal gas pressure) that would mitigate the natural limit. The two constraints must be modeled separately because their ε values differ by a factor of seven — they are not the same constraint viewed from different angles, but distinct mechanisms with a causal dependency relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
