% ============================================================================
% CONSTRAINT STORY: metabolic_constraint_cognition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metabolic_constraint_cognition, []).

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
 *   constraint_id: metabolic_constraint_cognition
 *   human_readable: The ATP Ceiling as an Exploitable Limit
 *   domain: biological/technological/economic
 *
 * SUMMARY:
 *   The ATP ceiling of human cognition represents a hard biological limit:
 *   the brain's oxidative metabolism cannot sustain arbitrary cognitive load
 *   indefinitely without recovery. Over the past 30 years, this constraint
 *   has shifted from a natural biological fact to an exploitable
 *   institutional frontier. Knowledge economies, attention-dependent
 *   platforms, military cognitive enhancement programs, and productivity
 *   culture have systematized extraction from this limit by: (1) escalating
 *   demand beyond sustainable ATP cycles, (2) suppressing evidence of
 *   metabolic depletion through narratives of willpower and optimization, and
 *   (3) monetizing cognitive enhancement as a coordination solution when the
 *   underlying problem is institutional overdemand. The constraint exhibits
 *   all six DR types from different perspectives, revealing how a real
 *   biological limit becomes a hybrid extraction-coordination mechanism. The
 *   extractiveness has increased from 0.28 to 0.58 over the interval as
 *   attention economy platforms and cognitive enhancement markets have
 *   scaled. Theater ratio has risen from 0.22 to 0.48 as productivity culture
 *   narratives have become more central to identity and compensation systems
 *   despite declining scientific support for infinite cognitive
 *   expandability.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victims (powerless/trapped) — face escalating cognitive demands with no exit. ATP depletion misattributed to personal failure.
 *   - Attention Economy Platforms: Primary beneficiaries (organized/mobile) — systematically extract from ATP ceiling via engagement optimization and algorithmic distraction.
 *   - Cognitive Enhancement Vendors: Primary beneficiaries (institutional/arbitrage) — monetize ATP ceiling management via nootropics, productivity software, and brain stimulation.
 *   - Sleep Science Community: Secondary actor (organized/constrained) — understand constraint structurally but face suppression in design and workplace norms.
 *   - Military Programs: Secondary beneficiary (institutional/mobile) — exploit ATP ceiling for cognitive enhancement and fatigue resistance in special operations.
 *   - Productivity Culture: Institutional narrative actor — maintains fiction that human cognition is infinitely expandable; sustains extraction through cultural reinforcement.
 *   - Individual Consciousness: Structural victim (powerless/trapped) — ATP depletion obscures metacognitive access to its own occurrence, preventing real-time exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metabolic_constraint_cognition, 0.58).
domain_priors:suppression_score(metabolic_constraint_cognition, 0.65).
domain_priors:theater_ratio(metabolic_constraint_cognition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metabolic_constraint_cognition, extractiveness, 0.58).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metabolic_constraint_cognition, tangled_rope).
narrative_ontology:human_readable(metabolic_constraint_cognition, "The ATP Ceiling as an Exploitable Limit").
narrative_ontology:topic_domain(metabolic_constraint_cognition, "biological/technological/economic").

domain_priors:requires_active_enforcement(metabolic_constraint_cognition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, cognitive_enhancement_vendors).
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, attention_economy_platforms).
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, military_programs).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, individual_cognitive_health).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, sustained_attention_capacity).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, metacognitive_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE WORKER (SNARE) — Faces escalating demands for cognitive performance (multitasking, context-switching, sustained focus) with no exit. ATP depletion from chronic overdemand produces learned helplessness: fatigue is misattributed to personal failure rather than metabolic ceiling. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ATTENTION-DEPENDENT SECTOR (TANGLED ROPE) — Knowledge workers, students, and professionals depend on cognitive performance systems (productivity software, educational platforms, workplace management) that simultaneously coordinate task execution AND extract via attention demand. Benefits from coordination; bears extraction cost through fatigue and cognitive decline. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COGNITIVE ENHANCEMENT VENDORS (ROPE) — Nootropic manufacturers, productivity platforms, and pharmaceutical companies benefit from the ATP ceiling as a coordination problem: workers need tools to manage metabolic limitations. Vendors frame this as beneficial coordination (enabling focus, managing energy). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ATTENTION ECONOMY PLATFORMS (SNARE) — Social media, streaming services, and algorithmic feeds exploit the ATP ceiling by systematizing distraction. Each platform individually poses as a coordination tool (connection, information access); collectively they extract finite attentional resources via engagement optimization. Victims can exit individual platforms but face coordination trap (remaining on platform to maintain social connection). d≈0.78, f(d)≈1.11, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, snare,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SLEEP SCIENCE COMMUNITY (TANGLED ROPE) — Organized actors (neuroscientists, sleep researchers, occupational health advocates) understand the ATP ceiling structurally but face enforced constraint: their findings are suppressed in industrial design and workplace norms. They experience the constraint as hybrid coordination (their research improves understanding) AND extraction (their evidence is ignored by systems that profit from ATP depletion). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PRODUCTIVITY CULTURE NARRATIVE (PITON) — The institutional fiction that human cognitive capacity is infinitely expandable through willpower, optimization, and better tools persists despite neuroscientific refutation. theater_ratio=0.48 is borderline, but the ritual maintenance of 'hustle culture' narratives (weekend working, always-on communication, self-quantification) despite known ATP ceiling degradation suggests piton classification. The narrative persists through institutional inertia and cultural reinforcement despite declining functional effectiveness.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / METABOLIC REALITY (MOUNTAIN-CANDIDATE) — From a civilizational perspective, the ATP ceiling is a fundamental biological constraint: the human brain's oxidative metabolism cannot sustain arbitrary cognitive load indefinitely. This appears as an immutable natural law. However, the structural data (ε=0.58, suppression=0.65, requires_active_enforcement=true) contradicts pure mountain classification — active institutional mechanisms (productivity demands, attention economy incentive structures, suppression of sleep science evidence) are required to maintain extraction. The engine will detect this as a false summit: the apparent 'law of nature' is actually a contingent institutional arrangement layered on top of a real biological limit.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_constraint_cognition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metabolic_constraint_cognition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metabolic_constraint_cognition, TR),
    TR >= 0.70.

:- end_tests(metabolic_constraint_cognition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The ATP ceiling is exploitable because institutional demands routinely exceed sustainable metabolic cycles, and the extraction is monetized via three channels: (1) attention economy platforms (engagement optimization extracts attention below ATP restoration threshold), (2) cognitive enhancement markets (monetize the gap between demand and sustainable capacity), and (3) workplace productivity norms (extract surplus cognitive effort, converting exhaustion into competitive advantage). This is not pure exploitation (ε=0.58 not 0.75+) because some institutional structures do provide genuine coordination — project management systems, attention management tools, and scientifically-informed workplace design can reduce ATP depletion. But the dominant institutional trend is toward extraction. Suppression (0.65): Moderate-high. Multiple suppression mechanisms: (1) sleep science evidence is systematically downweighted in design (notification algorithms, always-on culture), (2) fatigue is misattributed to individual failure rather than metabolic ceiling, (3) ATP depletion obscures metacognitive access (victims cannot perceive it in real-time), (4) cultural narratives of optimization and willpower override biological evidence. Theater ratio (0.48): Moderate. Productivity culture performs cognitive optimization (self-quantification apps, productivity rituals, motivational frameworks) with declining functional effectiveness — theater has increased as the underlying constraint has become more exploited. But this is not piton-level theater (≥0.70) because genuine coordination and legitimate performance optimization still occur.
 *
 * PERSPECTIVAL GAP:
 *   The knowledge worker sees a Snare: escalating demands with no exit, ATP depletion misattributed to personal inadequacy. The attention economy platform sees a Rope: they solve the coordination problem of connecting users and managing information. The sleep science community sees a Tangled Rope: their research improves understanding but faces institutional suppression. Productivity culture sees a Piton: the narrative persists through inertia despite declining evidence. The analytical observer risks seeing a Mountain: the ATP ceiling is a law of nature, therefore cognitive limits are inevitable. But the structural data reveals this as a false summit — the extraction is maintained by active institutional mechanisms, not by immutable physics. If the attention economy operated below the ATP extraction threshold, or if workplace norms incorporated sleep science findings, the constraint would be a pure Rope (coordination). The gap between the biological fact (ATP ceiling exists) and the institutional reality (systematic extraction) is the perspectival distance.
 *
 * DIRECTIONALITY LOGIC:
 *   Knowledge workers: Victims + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. ATP depletion is non-voluntary and non-escapable within contemporary knowledge economies. Attention economy platforms: Beneficiaries + mobile → d≈0.12, f(d)≈-0.05. Net beneficiary. Engagement metrics directly reward attention extraction; they experience the constraint as enabling (coordination problem solved). Cognitive enhancement vendors: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. They profit from the gap between demand and capacity; they have exit options (sell into different markets). Sleep science community: Victims + constrained → d≈0.55, f(d)≈0.75. Mixed. Their research would reduce extraction (coordination function), but they face institutional constraint that suppresses their evidence. Military programs: Beneficiaries + mobile → d≈0.15, f(d)≈-0.01. Near-net-beneficiary. They exploit ATP ceiling for enhancement but maintain high exit optionality (resource-rich, ideologically committed). Productivity culture: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. The narrative maintains extraction while appearing to solve it.
 *
 * MANDATROPHY ANALYSIS:
 *   The ATP ceiling resolves the mandatrophy through structural layering: (1) At the biological level, the ATP ceiling is a real constraint (Mountain candidate). (2) At the institutional level, systematic extraction from this limit via attention economy and productivity culture makes it a Tangled Rope or Snare from most victim perspectives. (3) The false summit (analytical observer's Mountain classification) is caught by requiring active enforcement: if the ATP ceiling were truly immutable and non-extractive, no enforcement would be needed. The fact that suppression mechanisms exist (sleep science suppression, fatigue misattribution, metacognitive obscuration) proves the constraint is contingent, not natural. (4) The mandatrophy is resolved by distinguishing the biological fact (ATP limit exists) from the institutional mechanism (systematic extraction from that limit). The constraint story models the institutional mechanism, not the biology alone. This clarifies why some knowledge economies (with sleep-protective norms, attention-friendly design) show lower extractiveness (≈0.25-0.35, Rope-Scaffold range), while attention-economy-dominated systems show high extractiveness (≈0.55-0.70, Tangled Rope-Snare range). The ATP ceiling itself does not determine the classification — the institutional response to it does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atp_restoration_threshold,
    'What recovery interval—sleep duration, cognitive rest, or metabolic recovery time—is both physiologically necessary for ATP restoration and economically viable in contemporary knowledge economies?',
    'Longitudinal neuroscience studies of ATP replenishment rates under different rest protocols; comparative cost-benefit analysis of productivity loss vs. cognitive health across sleep duration and recovery protocols',
    'If threshold < 7 hours: current scheduling norms are barely sustainable. If threshold > 9 hours: most knowledge workers operate in chronic ATP deficit. Determines whether Scaffold sunset clause is feasible (recovery-based redesign) or extraction is structural (Snare classification dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atp_restoration_threshold, empirical, 'Physiologically necessary ATP recovery interval and economic viability').

omega_variable(
    attention_economy_alternative_feasibility,
    'Can attention-allocating systems (social media, notification algorithms, feed design) be redesigned to operate below the ATP extraction threshold while maintaining coordination function?',
    'Randomized controlled trials of low-notification, algorithmic-transparency, and attention-friendly interface designs on user engagement and cognitive health; comparative analysis of platforms with and without extraction-optimized engagement metrics',
    'If feasible: platforms can be Rope (pure coordination). If infeasible: attention economy extraction is structural to profitable digital architecture. Determines whether institutional reform (Scaffold) or structural opposition (Snare) is the appropriate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_economy_alternative_feasibility, empirical, 'Whether attention systems can maintain function below ATP extraction threshold').

omega_variable(
    metabolic_consciousness_coupling,
    'Is the ATP ceiling knowable in real-time by the individual agent, or does ATP depletion inherently obscure metacognitive access to its own occurrence?',
    'Neuroscience of fatigue-induced metacognitive bias; studies of self-perception of cognitive load under ATP-depleted conditions; comparison of subjective effort reports vs. objective metabolic markers',
    'If knowable: victims can exit before catastrophic depletion (exit_options elevate to ''constrained'' rather than ''trapped''). If unknowable: suppression is structural to consciousness itself, making the constraint closer to Mountain. Determines whether exit options are truly trapped or merely appear so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metabolic_consciousness_coupling, empirical, 'Whether ATP depletion is knowable to individual consciousness in real-time').

omega_variable(
    cognitive_enhancement_substitution,
    'Can pharmacological or technological cognitive enhancement (nootropics, brain stimulation, neural interfaces) restore ATP-dependent function or merely mask depletion signals?',
    'Longitudinal studies of neurochemical and metabolic markers under enhancement regimes; comparison of task performance under enhancement vs. metabolic recovery; analysis of secondary health outcomes and tolerance development',
    'If substitutable: vendors'' Rope classification holds (coordination function). If only masking: enhancement regimes are parasitic on ATP depletion (vendors become beneficiaries of Snare). Determines whether enhancement is genuine coordination or systemic extraction theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_enhancement_substitution, empirical, 'Whether cognitive enhancement substitutes for or masks ATP depletion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metabolic_constraint_cognition, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metcog_tr_t0, metabolic_constraint_cognition, theater_ratio, 0, 0.22).
narrative_ontology:measurement(metcog_tr_t15, metabolic_constraint_cognition, theater_ratio, 15, 0.35).
narrative_ontology:measurement(metcog_tr_t30, metabolic_constraint_cognition, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(metcog_be_t0, metabolic_constraint_cognition, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(metcog_be_t15, metabolic_constraint_cognition, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(metcog_be_t30, metabolic_constraint_cognition, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metabolic_constraint_cognition, resource_allocation).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, attention_economy_rent_extraction).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, sleep_deprivation_productivity_paradox).
narrative_ontology:affects_constraint(metabolic_constraint_cognition, cognitive_enhancement_black_market).

% DUAL FORMULATION NOTE:
% The ATP ceiling is a real biological constraint (Mountain). This story models the institutional extraction mechanism layered on top of that biological fact. The upstream biological constraint (oxidative metabolism limits cognitive output) is separate from the institutional story (attention economy and productivity culture exploit that limit). The two stories are linked: the institutional extraction only works because the biological limit exists, but the institutional arrangement determines whether the limit is a pure coordination problem (Rope) or an exploitable extraction frontier (Snare/Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metabolic_constraint_cognition, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
