% ============================================================================
% CONSTRAINT STORY: regional_nuclear_proliferation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_nuclear_proliferation, []).

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
 *   constraint_id: regional_nuclear_proliferation
 *   human_readable: Regional Nuclear Proliferation Constraint
 *   domain: geopolitical/security
 *
 * SUMMARY:
 *   Regional nuclear proliferation creates a structural constraint that
 *   operates simultaneously as coordination mechanism (reducing escalation
 *   risk through deterrence stability and safeguards regimes) and as
 *   extraction mechanism (concentrating military power, denying deterrent to
 *   vulnerable states, and enforcing geopolitical hierarchy). The
 *   nonproliferation regime, established in 1968 with a disarmament sunset
 *   clause, persists past its intended termination date through institutional
 *   inertia while enforcement asymmetries deepen. Threshold states face a
 *   security dilemma: nuclear development invites sanctions and military
 *   intervention; nuclear abstinence invites conventional military coercion
 *   from nuclear-armed neighbors. The constraint's extractiveness has
 *   increased over 30 years as technical safeguards have become more
 *   performative (theater_ratio rising) and as great-power enforcement
 *   priorities have shifted from disarmament (abandoned after Cold War) to
 *   nonproliferation maintenance (enforced asymmetrically). The constraint
 *   exhibits all six DR types from different structural positions:
 *   established nuclear powers experience pure coordination; threshold states
 *   experience pure extraction; organized non-aligned movements experience
 *   mixed coordination-extraction; international institutions experience
 *   institutional degradation (piton); and the analytical observer recognizes
 *   the constraint's irreducible duality — it genuinely serves both
 *   coordination and extraction simultaneously, and the tension between these
 *   functions is not resolvable but only manageable.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Threshold States: Primary victims (powerless/trapped) — technically capable but politically prohibited; face maximum extraction through security dilemma and legal prohibition
 *   - Civilian Populations in Proliferation Zones: Secondary victims (powerless/trapped) — bear existential risk with no decision-making role; maximum suppression from security infrastructure
 *   - Established Nuclear Powers: Primary beneficiaries (institutional/arbitrage) — benefit from nonproliferation regime that locks in strategic advantage while denying rivals deterrent capability
 *   - Regional Non-Aligned States: Secondary beneficiary/victim (organized/constrained) — benefit from NPT coordination and safeguards while accepting asymmetric constraints; constrained exit through sanctions threat
 *   - International Nonproliferation Regime (IAEA, NSG): Institutional actor (institutional/mobile) — maintains performative safeguards and export controls; enforcement depends on great-power political will rather than technical capability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes constraint's structural duality as both genuine coordination and genuine extraction, with neither function eliminable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_nuclear_proliferation, 0.68).
domain_priors:suppression_score(regional_nuclear_proliferation, 0.78).
domain_priors:theater_ratio(regional_nuclear_proliferation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_nuclear_proliferation, extractiveness, 0.68).
narrative_ontology:constraint_metric(regional_nuclear_proliferation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(regional_nuclear_proliferation, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_nuclear_proliferation, tangled_rope).
narrative_ontology:human_readable(regional_nuclear_proliferation, "Regional Nuclear Proliferation Constraint").
narrative_ontology:topic_domain(regional_nuclear_proliferation, "geopolitical/security").

domain_priors:requires_active_enforcement(regional_nuclear_proliferation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_nuclear_proliferation, nuclear_armed_state).
narrative_ontology:constraint_beneficiary(regional_nuclear_proliferation, security_establishment).
narrative_ontology:constraint_victim(regional_nuclear_proliferation, non_nuclear_states).
narrative_ontology:constraint_victim(regional_nuclear_proliferation, civilian_populations).
narrative_ontology:constraint_victim(regional_nuclear_proliferation, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR THRESHOLD STATE (SNARE) — A state with technical capacity but political prohibition faces maximum extraction. Cannot develop deterrent without sanctions/military intervention; cannot exit the regional security dilemma without capitulation or external guarantee. Bears full cost of asymmetric vulnerability. Maximum suppression through legal prohibition, export controls, and conventional military superiority of nuclear-armed neighbors.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATION (SNARE) — Trapped in escalation logic; bears existential risk from nuclear use while having no decision-making role. Cannot exit the region; cannot influence state nuclear policy. Maximum suppression from security infrastructure, border controls, and conscription systems. Extraction appears as bodily risk and loss of civilian autonomy.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL NON-ALIGNED COALITION (TANGLED ROPE) — Organized states benefit from coordination mechanisms (NPT, nuclear-free zones, confidence-building measures) that reduce escalation risk; simultaneously face extraction through unequal enforcement (nuclear-armed powers exempt from restrictions). Constrained exit: leaving non-proliferation framework invites sanctions but staying accepts asymmetric constraints. Mixed experience: genuine coordination benefit plus asymmetric burden.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED NUCLEAR POWER (ROPE) — Benefits from proliferation prohibition as coordination mechanism that locks in strategic advantage. No effective exit option — maintains arsenal while prohibiting others (arbitrage position). Experiences constraint as pure coordination: nonproliferation regime stabilizes deterrence relationships and reduces regional conflicts that might escalate to great-power involvement. Net beneficiary through first-mover status.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL NON-PROLIFERATION REGIME (SCAFFOLD) — NPT (1968), safeguards systems, and export control regimes were designed as temporary coordination structures with built-in sunset logic: eliminate nuclear weapons by 2000 (Article VI), then other states gain legitimacy to develop arsenals. Regime persists past sunset through institutional inertia despite never achieving disarmament. Theater ratio high because enforcement ritual (IAEA inspections) continues performatively while actual constraint depends on geopolitical interests of nuclear powers. Sunset provisions unenforced = constraint transitions from scaffold to piton.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NPT INSTITUTIONAL FRAMEWORK (PITON) — The nonproliferation treaty has become vestigial: enforcement depends entirely on great-power preferences (applied strictly to adversaries, ignored for allies), disarmament obligations abandoned without consequence, and technical safeguards (enrichment monitoring) work only when geopolitical enforcement backs them. The ritual persists through institutional inertia — states maintain formal compliance while behavioral content has hollowed out. Theater ratio demonstrates performative character: inspection ceremonies, review conferences, and diplomatic statements maintain appearance while strategic reality diverges.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, proliferation constraint exhibits both genuine coordination (reducing escalation risk through transparency and safeguards) and asymmetric extraction (locking in power distribution, denying deterrent to vulnerable states). The constraint cannot be reduced to either function alone. Its persistence reflects both the real coordination benefit (preventing spiraling arms races) and the real extraction benefit (preserving great-power dominance). Mandatrophy resolved through recognizing that the constraint legitimately serves both functions simultaneously — the perspectival gap reveals not an error in classification but a genuine structural duality.
constraint_indexing:constraint_classification(regional_nuclear_proliferation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_nuclear_proliferation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_nuclear_proliferation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_nuclear_proliferation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_nuclear_proliferation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_nuclear_proliferation, TR),
    TR >= 0.70.

:- end_tests(regional_nuclear_proliferation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. Threshold states experience severe extraction through security dilemma (cannot develop deterrent without severe consequences; cannot remain vulnerable without risking coercion). However, extraction is not absolute because alternative strategies exist (alliance with nuclear power, conventional military buildup, hedging). The extractiveness reflects the real asymmetry of the international system — nuclear deterrent denied to some while granted to others — not a complete structural lock. Theater ratio (0.62): Moderate-high and increasing. IAEA safeguards and nonproliferation diplomacy perform strong ritual elements: inspection ceremonies, review conferences, diplomatic escalation/de-escalation cycles that follow geopolitical preferences rather than technical findings. Enrichment restrictions announced as universal nonproliferation principles are applied selectively based on alliance status (India allowed, Iran prohibited; Pakistan ignored, North Korea sanctioned). The theater has increased as the gap has widened between stated universality and actual enforcement asymmetry. Suppression (0.78): Very high. Threshold states face multilayered suppression: legal prohibition through NPT, export controls on dual-use technology, threat of military intervention, economic sanctions, and enforcement through alliance relationships. The suppression mechanisms are structural (legal, economic, military) not merely internalized, making exit-costs genuine and immediate.
 *
 * PERSPECTIVAL GAP:
 *   The seven perspectives split into three families: (1) pure extraction — threshold state and civilian population both see Snare; (2) mixed extraction-coordination — organized coalition and analytical observer both see Tangled Rope; (3) pure coordination / institutional degradation — established power sees Rope, regime sees Piton. The gap between powerless threshold state (Snare) and institutional established power (Rope) is maximum: one experiences the same structural arrangement as extraction mechanism preventing security; the other experiences it as coordination mechanism stabilizing deterrence. This gap is not resolvable by additional measurement or clarification — it reflects genuine difference in structural position and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Threshold states as victims with trapped exit face d ≈ 0.95 → f(d) ≈ 1.42 (maximum f-scaling). Established nuclear powers as beneficiaries with arbitrage options face d ≈ 0.05 → f(d) ≈ -0.12 (negative effective extraction). Organized coalitions as mixed victims/beneficiaries with constrained exit face d ≈ 0.55 → f(d) ≈ 0.75 (moderate scaling). The scope modifier σ(S) applies regional (0.9) scaling to most perspectives, raising effective extractiveness; the global scope (1.2) for established powers amplifies their extraction benefit. The chi formula χ = ε × f(d) × σ(S) produces highly differentiated effective extraction across perspectives: powerless threshold states experience χ ≈ 0.68 × 1.42 × 0.9 ≈ 0.87 (severe); institutional established power experiences χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative, subsidized); organized coalition experiences χ ≈ 0.68 × 0.75 × 0.9 ≈ 0.46 (moderate). These differentiated values drive the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL DUALITY. The constraint is not 'actually' tangled rope with piton and snare being misclassifications from limited perspectives. Rather, the constraint legitimately serves both coordination and extraction functions simultaneously, and these functions are inseparable. The coordination (reducing escalation risk, preventing destabilizing arms races) is real and valuable — international security genuinely improves if proliferation is slow and controlled. The extraction (denying deterrent to threshold states, maintaining power asymmetry) is also real and structurally necessary — any effective nonproliferation regime must enforce asymmetric constraints on some states while allowing others to retain arsenals. The mandatrophy resolves by recognizing that a constraint can simultaneously coordinate and extract without either function being reducible to the other. The perspectival gap (Rope vs Snare) persists as a feature of the system, not a bug to be fixed. The threshold state cannot exit the Snare by achieving Rope-perspective; the established power cannot maintain Rope coordination while granting threshold states equivalent deterrent. The analytical observer sees this irreducible duality and classifies as Tangled Rope — genuine coordination with genuine extraction, both structural, both necessary for the regime's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_extraction_boundary,
    'Does nuclear deterrence for threshold states constitute legitimate security (coordination) or illegitimate extraction masked as security?',
    'Comparative analysis of regional conflicts before/after proliferation; assessment of whether nuclear-armed threshold states achieve defensive stability or pursue revisionist objectives',
    'If deterrence is legitimate: threshold state has stronger claim to development rights; constraint shifts toward piton (obsolete regime). If deterrence masks revisionism: extraction logic holds; constraint remains tangled rope with higher asymmetric burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_extraction_boundary, preference, 'Whether proliferation provides legitimate deterrence or enables revisionism').

omega_variable(
    iaea_safeguards_efficacy,
    'Do IAEA inspections and technical safeguards actually prevent diversion to weapons, or are they performative theater for political reassurance?',
    'Historical audit of undeclared facilities discovered after regime change; technical analysis of detection capability vs concealment sophistication; comparison of inspection resources to proliferation threat scale',
    'If efficacious: technical safeguards constitute real coordination mechanism; constraint remains tangled rope with coordination function intact. If performative: safeguards are theater supporting piton classification; constraint shifts toward pure extraction (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iaea_safeguards_efficacy, empirical, 'Whether technical safeguards provide effective nonproliferation verification').

omega_variable(
    article_vi_enforcement_gap,
    'Why has the nonproliferation regime tolerated complete non-compliance with Article VI disarmament obligations by nuclear powers without triggering treaty renegotiation or collapse?',
    'Institutional analysis of enforcement asymmetry; documentation of threshold state withdrawal threats vs actual enforcement response; timeline of review conference outcomes',
    'If enforcement gap is negotiated coordination: regime serves real balancing function; constraint remains tangled rope. If gap reflects power asymmetry: constraint is extractive regime masquerading as universal law; shifts toward snare from threshold state perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforcement_gap, conceptual, 'Whether Article VI non-enforcement indicates structural extraction or negotiated asymmetry').

omega_variable(
    regional_nuclear_free_zone_effectiveness,
    'Do nuclear-free zone treaties (African Union, Southeast Asia, Latin America) create genuine coordination benefits or performative compliance that masks great-power dominance?',
    'Comparison of conflict escalation rates inside vs outside nuclear-free zones; assessment of whether zones reduce conventional military spending; analysis of great-power enforcement of zone provisions vs enforcement against non-signatories',
    'If zones create stability: they instantiate genuine scaffold with sunset potential (if great powers eventually join); constraint legitimacy increases. If zones are performative: they are piton classification; extraction through asymmetric constraint continues unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_nuclear_free_zone_effectiveness, empirical, 'Whether nuclear-free zones provide genuine coordination or performative compliance').

omega_variable(
    supplier_cartel_sustainability,
    'How long can nuclear supplier cartels (NSG) sustain restrictions on enrichment/reprocessing technology without collapsing from defection by supplier states seeking economic advantage or alliance rewards?',
    'Historical analysis of supplier defections (India, Pakistan, North Korea, Iran cases); modeling of defection incentives; documentation of enforcement mechanisms and punishment credibility',
    'If cartel is unstable: extraction mechanism is unsustainable; constraint transitions toward piton (inertial vestige) within generational timescale. If cartel is stable: extraction mechanism persists; constraint remains snare for threshold states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supplier_cartel_sustainability, empirical, 'Whether nuclear supplier cartel can sustain export restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_nuclear_proliferation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rnp_tr_t0, regional_nuclear_proliferation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rnp_tr_t10, regional_nuclear_proliferation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rnp_tr_t20, regional_nuclear_proliferation, theater_ratio, 20, 0.62).
narrative_ontology:measurement(rnp_tr_t30, regional_nuclear_proliferation, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(rnp_be_t0, regional_nuclear_proliferation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rnp_be_t10, regional_nuclear_proliferation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rnp_be_t20, regional_nuclear_proliferation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(rnp_be_t30, regional_nuclear_proliferation, base_extractiveness, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_nuclear_proliferation, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_nuclear_proliferation, great_power_deterrence_stability).
narrative_ontology:affects_constraint(regional_nuclear_proliferation, regional_conventional_arms_races).
narrative_ontology:affects_constraint(regional_nuclear_proliferation, nuclear_proliferation_hedging).
narrative_ontology:affects_constraint(regional_nuclear_proliferation, uranium_enrichment_supply_chain).

% DUAL FORMULATION NOTE:
% Regional nuclear proliferation constraint operates at the intersection of state security (deterrence rationale) and international order (nonproliferation regime). Two related constraints exist upstream: (1) great-power deterrence stability (ε ≈ 0.15, Mountain-adjacent) establishes the technical/strategic logic for nuclear weapons; (2) nonproliferation regime enforcement (ε ≈ 0.65, Tangled Rope) enforces asymmetric constraints. The regional proliferation constraint (ε ≈ 0.68) sits between these: it is the outcome of deterrence logic applied asymmetrically through enforcement mechanisms. Downstream constraints include conventional arms race dynamics (which spike when proliferation barriers are imposed) and uranium supply chain politics (which determine actual proliferation feasibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_nuclear_proliferation, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
