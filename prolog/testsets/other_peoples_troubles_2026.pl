% ============================================================================
% CONSTRAINT STORY: other_peoples_troubles_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_other_peoples_troubles_2026, []).

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
 *   constraint_id: other_peoples_troubles_2026
 *   human_readable: The Asymmetry of Vicarious Resilience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The asymmetry of vicarious resilience creates a structural constraint in
 *   which psychological distance enables observers to 'bear' the suffering of
 *   others with minimal emotional or material cost, while simultaneously
 *   suppressing institutional obligation to provide actual support. This
 *   constraint operates across clinical care, disaster response, mental
 *   health systems, and everyday compassion fatigue. The distance that makes
 *   others' troubles 'bearable' is not merely a cognitive feature but an
 *   institutional product: narratives celebrating 'resilience,' 'strength,'
 *   and 'coping capacity' function to justify resource scarcity and reduce
 *   caregiver burden. The constraint exhibits all six DR types from different
 *   structural positions. From the perspective of suffering individuals, it
 *   operates as pure extraction (Snare) — their distress is acknowledged as
 *   real yet acted upon as manageable without intervention. From the
 *   perspective of distant observers (including institutional actors), it
 *   functions as coordination (Rope) — the vicarious resilience narrative
 *   enables collective meaning-making and social cohesion without resource
 *   redistribution. Healthcare providers experience it as mixed
 *   coordination-extraction (Tangled Rope) — the narrative enables workload
 *   management (coordination function) while suppressing recognition of
 *   patient need (extraction). Institutional care systems maintain it as
 *   degraded ritual (Piton) — performative support structures persist while
 *   actual functional capacity is constrained. Organized peer support
 *   coalitions experience it as temporary and solvable (Scaffold) — through
 *   reciprocal vulnerability practices that rebuild empathic proximity. From
 *   the analytical/civilizational view, the constraint risks appearing as a
 *   natural law of human empathic bandwidth (Mountain) — but this naturalizes
 *   what is actually a contingent institutional choice to preserve distance.
 *
 * KEY AGENTS:
 *   - Suffering Individuals: Primary victims (powerless/trapped) — bear the actual burden of distress and institutional neglect; psychological distance suppresses recognition of need
 *   - Distant Observers: Primary beneficiaries (institutional/arbitrage) — gain psychological stability and reduced obligation through abstract engagement with others' troubles
 *   - Healthcare Providers: Secondary actors (moderate/constrained) — constrained by professional ethics and patient volume; narratives of resilience enable workload management while suppressing burnout discussion
 *   - Institutional Care Systems: Institutional beneficiary (institutional/arbitrage) — maintains cost-efficient operations through vicarious resilience narratives that justify resource scarcity
 *   - Peer Support Coalitions: Organized agents (organized/constrained) — building alternative pathways through reciprocal vulnerability and sustained witness; see exit path away from institutional abstraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as inherent cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(other_peoples_troubles_2026, 0.52).
domain_priors:suppression_score(other_peoples_troubles_2026, 0.68).
domain_priors:theater_ratio(other_peoples_troubles_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(other_peoples_troubles_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(other_peoples_troubles_2026, tangled_rope).
narrative_ontology:human_readable(other_peoples_troubles_2026, "The Asymmetry of Vicarious Resilience").
narrative_ontology:topic_domain(other_peoples_troubles_2026, "social/psychological").

domain_priors:requires_active_enforcement(other_peoples_troubles_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, distant_observers).
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, institutional_caretakers).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, suffering_individuals).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, empathy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING INDIVIDUAL (SNARE) — Trapped in immediate suffering with no exit. Bears the full cost of others' psychological distance. Their distress becomes abstract to observers, enabling vicarious resilience narratives that minimize actual support obligations. Maximum extraction through denial of urgency.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISTANT OBSERVER (ROPE) — Benefits from psychological distance. Can bear others' troubles easily through abstraction, maintaining emotional stability. Experiences the constraint as enabling coordination: narratives of universal human resilience allow collective meaning-making without immediate resource commitment. Net beneficiary.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HEALTHCARE PROVIDER (TANGLED ROPE) — Constrained by professional ethics (must maintain engagement) but also by patient volume and burnout dynamics. The vicarious resilience narrative ('patients are resilient, don't need as much support') both enables workload management (coordination) and justifies resource scarcity (extraction). Mixed structure.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL CARE SYSTEM (PITON) — Maintains performative support structures (counseling protocols, intake procedures, resource allocation meetings) while actual functional support is degraded by the vicarious resilience narrative. Theater ratio reflects that institutional language celebrates patient resilience while systemic underfunding persists. Inertial maintenance of ritual without functional capacity.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER SUPPORT COALITION (SCAFFOLD) — Organized agents (mutual aid networks, community-based organizations, peer supporters) reject the vicarious resilience narrative and create alternative pathways for bearing others' troubles through reciprocal vulnerability rather than distance. Lower extraction because participants have agency and see an exit from the institutional narrative. Sunset logic: as peer models mature, the institutional abstraction loses credibility.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a cognitive/evolutionary perspective, psychological distance is inherent to human perception — distant suffering is neurologically processed with lower intensity than immediate suffering. This perspective sees vicarious resilience as reflecting fundamental limits on empathic bandwidth. However, structural data reveals this as false naturalization: the extraction component (institutional resource denial, narrative suppression of urgency) is contingent institutional choice, not cognitive law.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(other_peoples_troubles_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(other_peoples_troubles_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(other_peoples_troubles_2026, TR),
    TR >= 0.70.

:- end_tests(other_peoples_troubles_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The primary extraction mechanism is institutional — narrative suppression of support need coupled with emotional comfort for distant observers. The constraint does not involve violent coercion, but it does involve systematic denial of recognition and resource redirection. The 0.52 value reflects that the extraction is substantial yet not totalizing; some genuine coordination occurs (peer meaning-making, actual support relationships exist), and some individuals do receive adequate care. The increase over the measurement interval (0.35 to 0.52) reflects degradation as institutional resource scarcity increases while resilience narratives intensify. Suppression (0.68): High. Multiple barriers prevent escape: psychological distance is structural to observer-victim relationships; institutional frames (professional detachment, narrative reframing) suppress urgency; career/status costs suppress caregiver advocacy for resources; social stigma suppresses explicit acknowledgment that others' troubles matter for our own well-being. Theater ratio (0.64): Moderately high and rising. Institutional care systems maintain substantial performative activity: resilience assessments, coping skill trainings, support group protocols — while actual functional support is constrained. The theater has increased over the interval as budget pressures have forced reliance on narrative substitution for material support.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reflects the core constraint structure. The suffering individual sees a Snare: their distress is real but treated as manageable through institutional narratives, suppressing obligation to provide actual support. The institutional observer sees a Rope: resilience narratives enable collective meaning-making, workload management, and cost efficiency without explicit coercion. The healthcare provider sees a Tangled Rope: professional ethics and patient need create coordination functions, but burnout and resource constraints create extraction dynamics. The peer support coalition sees a Scaffold: the vicarious resilience narrative is a temporary institutional phenomenon being replaced by reciprocal vulnerability practices. The institutional system sees a Piton: the rhetoric of resilience persists as inertial maintenance while functional capacity degrades. The civilizational observer risks a Mountain view: seeing empathic distance as inherent cognitive limit. These gaps are not measurement artifacts — they reflect real differences in structural position and exit capacity. The constraint's extractive force depends on maintaining these gaps: if distant observers recognized their extraction position as explicit, the narratives would lose legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) flows from their structural position. Suffering individuals are the canonical victims with no exit (d → 1.0, high chi). Distant observers are beneficiaries with arbitrage capacity (d → 0.0-0.15, low chi, often negative as they benefit from the arrangement). Healthcare providers are trapped between professional duty and burnout (d → 0.6-0.7, moderate-high chi). Institutional care systems extract through narrative control while avoiding direct coercion (d → 0.1-0.2, low nominal chi, but structural control remains). Peer support actors are organized with exit pathways visible (d → 0.4-0.5, moderate chi declining as alternatives mature). The analytical observer's canonical d (0.72) masks the false summit: civilizational/universal scope combined with analytical exit options produces the mountain misclassification, but the engine's false summit detector identifies it as naturalization of institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine resilience (the actual capacity of individuals to bear and recover from hardship) from vicarious resilience narratives (institutional use of resilience attribution to justify resource denial). The mandatrophy would collapse if we asked: 'Is this a coordination mechanism enabling community meaning-making, or an extraction mechanism suppressing support obligation?' The answer is both, depending on structural position. For the distant observer and institutional actor, it functions as coordination (Rope). For the suffering individual, it functions as extraction (Snare). The Tangled Rope classification (required for both beneficiaries and victims, plus enforcement) captures this dual structure: the narrative genuinely coordinates collective response to suffering, AND it systematically suppresses material support obligation. The institutional enforcement mechanism is subtle — not direct coercion but narrative gatekeeping: 'resilience' becomes the dominant frame for discussing suffering, crowding out 'need,' 'justice,' and 'obligation.' The peer support perspective (Scaffold) demonstrates that alternative coordination mechanisms exist (reciprocal vulnerability, mutual witness) with lower extraction profiles and genuine sunset potential as they mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resilience_attribution_threshold,
    'At what point does describing someone as ''resilient'' transition from accurate observation to narrative suppression of support need?',
    'Longitudinal tracking of stated resilience assessments vs. actual mental health outcomes; analysis of institutional support allocation relative to resilience attribution; qualitative interviews with care recipients about whether resilience framing improved or hindered access to support',
    'If threshold is crossed easily: resilience narrative becomes a mechanism for extracting labor/endurance from suffering individuals while avoiding institutional obligation. If threshold is high: resilience framing can function as genuine coordination mechanism enabling peer mutual aid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resilience_attribution_threshold, empirical, 'Threshold between resilience as observation versus resilience as narrative suppression').

omega_variable(
    empathic_distance_plasticity,
    'Is psychological distance to others'' suffering a fixed cognitive parameter or a practice-dependent skill that can be substantially reduced through repeated proximity and sustained engagement?',
    'Randomized intervention studies comparing cognitive empathy measures in groups with sustained exposure to suffering (clinical training, volunteer work) vs. control groups; neuroimaging of empathic distance activation before/after compassion practice',
    'If fixed: vicarious resilience reflects legitimate cognitive constraints and the constraint is closer to Mountain. If plastic: the institutional adoption of vicarious resilience is a choice that suppresses alternative practices (peer witness, sustained presence), clarifying Tangled Rope or Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empathic_distance_plasticity, empirical, 'Whether psychological distance to suffering is fixed or skill-dependent').

omega_variable(
    peer_support_sustainability,
    'Can reciprocal vulnerability models (peer witness, mutual aid) actually sustain psychological and material support functions without institutional backing, or do they degrade to isolation under resource scarcity?',
    'Longitudinal study of peer support networks with and without institutional funding; tracking of member burnout, sustainability, and actual support delivery over 3+ years; cost analysis of peer models vs. professional models',
    'If sustainable: Scaffold perspective is structural reality with genuine sunset — peer models can replace institutional abstraction. If degraded to isolation: peer support becomes theater (Piton) and the institutional system maintains latent control. Classification shifts from Scaffold to Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peer_support_sustainability, empirical, 'Sustainability of peer support models without institutional backing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(other_peoples_troubles_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opt_tr_t0, other_peoples_troubles_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(opt_tr_t10, other_peoples_troubles_2026, theater_ratio, 10, 0.55).
narrative_ontology:measurement(opt_tr_t20, other_peoples_troubles_2026, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(opt_be_t0, other_peoples_troubles_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(opt_be_t10, other_peoples_troubles_2026, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(opt_be_t20, other_peoples_troubles_2026, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(other_peoples_troubles_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, empathy_fatigue).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, caregiver_burnout_systems).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, institutional_resource_scarcity).

% DUAL FORMULATION NOTE:
% The asymmetry of vicarious resilience decomposes into two structurally distinct claims: (1) psychological distance is a feature of human empathy — distance to others' suffering is neurologically real and affects emotional intensity (base ε ≈ 0.15, closer to Mountain); (2) institutional adoption of vicarious resilience narratives to justify resource denial — the narrative is a choice that suppresses alternative practices and magnifies the extractive impact of distance (ε ≈ 0.52, Tangled Rope). This story addresses the institutional/narrative claim. The cognitive/neurological claim about empathic distance would be a separate Mountain story in a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(other_peoples_troubles_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
