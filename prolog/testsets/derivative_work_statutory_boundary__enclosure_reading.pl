% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property_law/copyright/technology_governance
 *
 * SUMMARY:
 *   The derivative-work statutory boundary in copyright law creates a
 *   structural extraction regime disguised as coordination infrastructure.
 *   When interpreted under the enclosure reading, any use of copyrighted
 *   expression in creating new work triggers statutory liability unless the
 *   use is licensed or falls within narrow fair-use exceptions. This reading
 *   holds that the statutory boundary itself — the threshold determining what
 *   counts as transformation versus reproduction — is located at a point that
 *   maximizes incumbent gatekeeping and licensing extraction. The constraint
 *   operates across creators, downstream innovation sectors (software, music,
 *   visual media), and cross-border creative markets. From the downstream
 *   creator's perspective, the regime is a pure snare: licensing requirements
 *   foreclose creative choices made at ideation stage, and legal exposure
 *   exists before publication. From the incumbent's perspective, the regime
 *   is coordination infrastructure that enables profitable licensing markets.
 *   From organized open-source and creative-commons advocates' perspective,
 *   the regime is a tangled rope: it enables their own licensing (clear
 *   permissions) but simultaneously enforces the default enclosure structure
 *   that makes their alternative licenses necessary. The fair-use doctrine
 *   exists as a performative pressure valve: mechanically mandated but
 *   substantially ineffective due to litigation costs and outcome
 *   unpredictability. The regime shows evidence of extraction accumulation
 *   over the interval: extractiveness rising 0.42→0.68, suppression rising
 *   0.52→0.72, as digital technology multiplies transformation opportunities
 *   and enforcement infrastructure strengthens.
 *
 * KEY AGENTS:
 *   - Downstream Creator (Individual or Small Studio): Primary victim (powerless/trapped) — faces legal exposure for derivative-work preparation; bears full cost of licensing negotiation or legal risk
 *   - Innovation-Dependent Sectors (Music Sampling, Software, Visual Effects): Victim class (moderate/constrained) — must either license or relocate to fair-use jurisdictions; high suppression from licensing friction and unpredictable enforcement
 *   - Copyright Holder and Licensing Infrastructure: Primary beneficiary (institutional/arbitrage) — captures licensing revenue and gatekeeping control; experiences regime as coordination enabling profitable rights markets
 *   - Large Media Conglomerate: Secondary beneficiary (powerful/mobile) — can afford licensing negotiations and legal uncertainty; benefits from moat created by enforcement costs; can arbitrage across jurisdictions
 *   - Open-Source and Creative Commons Movement: Organized advocate (organized/constrained) — builds alternative licensing with clear permissions but cannot escape statutory enclosure default; benefits from their own licensing but constrained by broader regime
 *   - Fair-Use Doctrine and Courts: Institutional actor (institutional/arbitrage) — maintains performative relief mechanism through appeal-level doctrine; theater-ratio high because fair-use outcome is unpredictable and testing requires litigation cost
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing the boundary location as if it were the boundary concept necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.68).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.72).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/copyright/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, 'eb1cf1f9-adee-4fa3-a58b-38de9118543b').
narrative_ontology:cs_kernel_codification('eb1cf1f9-adee-4fa3-a58b-38de9118543b', formalized).
narrative_ontology:cs_authority_grounding('eb1cf1f9-adee-4fa3-a58b-38de9118543b', extraction).
narrative_ontology:cs_interpretation_layer_present('eb1cf1f9-adee-4fa3-a58b-38de9118543b').
narrative_ontology:cs_reading_relation('eb1cf1f9-adee-4fa3-a58b-38de9118543b', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb1cf1f9-adee-4fa3-a58b-38de9118543b', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('eb1cf1f9-adee-4fa3-a58b-38de9118543b', foundational, licensing_extraction_is_primary_function).
narrative_ontology:cs_axiom_status(licensing_extraction_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('eb1cf1f9-adee-4fa3-a58b-38de9118543b', licensing_extraction_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('eb1cf1f9-adee-4fa3-a58b-38de9118543b', foundational, transformation_threshold_favors_incumbent_gatekeeping).
narrative_ontology:cs_axiom_status(transformation_threshold_favors_incumbent_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('eb1cf1f9-adee-4fa3-a58b-38de9118543b', transformation_threshold_favors_incumbent_gatekeeping, empirically_contingent).
narrative_ontology:cs_reference_frame('eb1cf1f9-adee-4fa3-a58b-38de9118543b', copyright_holder_gatekeeping_authority).
narrative_ontology:cs_drift_state('eb1cf1f9-adee-4fa3-a58b-38de9118543b', digital_transformation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eb1cf1f9-adee-4fa3-a58b-38de9118543b', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_innovation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM CREATOR (SNARE) — Any use of copyrighted expression in preparation triggers derivative-work liability. The creator faces legal exposure before the work is even published; licensing requirements foreclose creative choices made at ideation stage. No exit options: cannot create without either licensing (cost barrier) or legal risk (enforcement trap). Maximum extraction — the regime predetermines the creator's input costs and output options.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INNOVATION-DEPENDENT SECTOR (SNARE) — Sectors where transformative practice requires engagement with existing expression (music sampling, video remix, software interoperability, UI patterns) face either licensing costs or legal friction. High suppression: licensing negotiations are unpredictable, licensor incentives misaligned with innovation value, administrative burden deters small producers. Constrained exit: can relocate to jurisdictions with fair-use provisions, but loses market access. Pure extraction logic: the enclosure regime monetizes gatekeeping rather than enabling new creation.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE CONGLOMERATE (TANGLED ROPE) — Powerful incumbent with capital to negotiate bulk licenses and in-house counsel to navigate ambiguity. Experiences genuine coordination function: the enclosure regime enables licensing deals and acquisition of derivative rights, creating market for rights trading. BUT also extraction: the regime's ambiguity and enforcement cost advantage creates a moat — large players can afford legal uncertainty; small competitors cannot. Mixed experience: coordination (licensing markets exist) + asymmetric extraction (enforcement advantage concentrates on large players). Mobile exit available but costly; prefers remaining inside the licensing ecosystem.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: COPYRIGHT HOLDER / LICENSING SYSTEM (ROPE) — Benefits from the enclosure regime through licensing revenue, control over derivative markets, and ability to monetize secondary uses. Experiences the constraint as coordination: the statutory framework enables licensing negotiations and creates predictable property rights. Net beneficiary position — extraction runs toward this agent. From this perspective, the derivative-work boundary is a coordination mechanism that solved a prior problem (unauthorized reproductions). The licensing ecosystem is pure benefit.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE / CREATIVE COMMONS (TANGLED ROPE) — Organized actors (FSF, Creative Commons, open-culture advocates) see the enclosure regime as a coordination problem that alternative licensing (GPL, CC-BY-SA, public domain) can solve through opt-in clarity and permission-upfront. Genuine coordination function: their licenses clarify derivative-work permissions and reduce licensing friction. BUT also extraction from their perspective: the enclosure regime's default creates asymmetry — copyright holders can enforce; creators of derivative works cannot. They must opt into alternative frameworks that are still subordinate to the statutory enclosure. Constrained exit: can build alternative licensing ecosystems but cannot escape the statutory boundary's enforcement backdrop.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIR-USE DOCTRINE (PITON) — The fair-use defense exists as a pressure-relief valve for the enclosure regime, but it is substantially performative in practice: fair-use litigation is expensive, outcomes unpredictable, and chilling effect on creation is severe. The doctrine is mechanically maintained through appellate citation and doctrine classes despite low functional impact on most creators (who cannot afford the legal testing required). Theater ratio high: courts perform fairness analysis, but the outcome depends on judge-specific interpretation and settlement power asymmetries. Piton classification: the doctrine persists through institutional inertia (it is legally mandated to exist) rather than functional prevention of extraction.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the derivative-work boundary appears as an immutable necessity of copyright law: any rule system for allocating authorial rights must draw a line between reproduction (unauthorized) and transformation (derivative or original). The boundary itself is structurally invariant — every IP regime must answer 'when does use of prior expression create a new legal work requiring permission?' This perspective risks naturalizing the LOCATION of the boundary (preponderantly protective of incumbents) as if it were the necessity of the boundary CONCEPT. The engine's false-summit detector will identify the naturalization.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(derivative_work_statutory_boundary__enclosure_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, TR),
    TR >= 0.70.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68): High, and rising over the interval (0.42→0.68). The enclosure interpretation holds that any use of copyrighted expression triggers derivative-work liability unless pre-cleared. This creates a licensing extraction point — creators must negotiate with or pay copyright holders before creation. Base extractiveness of 0.68 reflects that the regime creates bilateral control: incumbents can deny permission, demand royalties, or extract licensing costs from creators with minimal friction. The rise from 0.42 to 0.68 over 20 years reflects three factors: (1) digital technology multiplies transformation opportunities (video, remixing, interoperability), expanding the regime's extraction surface; (2) enforcement infrastructure strengthens (statutory damages increase, notice-and-takedown mechanisms proliferate, litigation cost asymmetry grows); (3) licensing intermediaries (rights-management collectives, PROs, digital platforms) accumulate more of the extraction flow. SUPPRESSION (0.72): High and rising (0.52→0.72). The regime creates substantial barriers to transformation without licensing: legal exposure (statutory damages up to $150k per infringement), litigation cost (even defensive fair-use testing is expensive), and uncertainty (transformation threshold is adjudicated case-by-case, not codified). The rise reflects that compliance burden increases with technology expansion — creators must evaluate derivative-work risk for every software dependency, music sample, UI pattern, visual quotation. THEATER RATIO (0.58): Moderate and rising (0.45→0.58). The fair-use doctrine exists as a safety valve, but its function is substantially performative: doctrine is mechanically cited in appellate opinions, fair-use factors are ritually applied in opinions, but chilling effect on actual creation is severe because litigation is expensive and outcome unpredictable. Theater increases with time because the doctrine's symbolic function (courts show fairness, doctrine exists) grows while its practical accessibility shrinks (litigation costs rise faster than creator revenue).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The downstream creator sees a pure snare (licensing requirements foreclose options, full extraction). The large conglomerate sees a tangled rope (genuine coordination through licensing markets, but also extraction advantage). The copyright holder sees pure rope (coordination enabling licensing). The organized advocates see tangled rope (their alternative licensing coordinates, but the statutory enclosure constrains). The fair-use doctrine sees itself as Mountain (immutable necessity), but the analytics detect a false summit (naturalization of arbitrary boundary location). The perspectival gap reveals that the constraint's classification depends entirely on the agent's structural position: beneficiaries experience coordination; victims experience snare; organized actors experience tangled rope. The gap is not observable-dependent (different ways of measuring the same thing) — it is position-dependent (different agents in genuinely different structural positions).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is determined by the agent's benefit/cost relationship to the licensing extraction mechanism. Copyright holders and licensing intermediaries are beneficiaries (d≈0.05-0.20): they gain from licensing revenue and gatekeeping control, experienced extractiveness is low because extraction flows toward them. Downstream creators are victims (d≈0.85-0.95): they must either pay for licenses or face legal risk, experienced extractiveness is high because costs are imposed on them. Large incumbents occupy intermediate position (d≈0.40-0.50): they benefit from licensing markets (as purchasers of licensed rights and as copyright holders) but also bear licensing costs; their mobile exit option and capital to navigate uncertainty lower their experienced extraction. Open-source advocates occupy an ambiguous position: from the regime's perspective they are nominally victims (must license or risk exposure), but their ability to opt into alternative frameworks (GPL, CC) and organize collectively (FSF infrastructure) elevates their d relative to isolated creators. The analytics perspective (d≈0.72) reflects the observer's position outside the licensing flow — they see the full structure without direct extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_indeterminacy,
    'What degree of transformation distinguishes a derivative work (requiring permission) from a new independent work (requiring no permission)?',
    'Comparative analysis of court decisions across jurisdictions; quantitative metrics of stylistic divergence, market substitutability, and functional independence; longitudinal tracking of how threshold definitions have shifted with technology (digital sampling, AI training, generative interfaces)',
    'If threshold is strict (low transformation tolerance): enclosure strengthens, snare classification confirmed across more perspectives, chi increases. If threshold is permissive (high transformation tolerance): coordination function becomes clearer, tangled-rope classifications increase, chi decreases. The reading_id instantiates strict-threshold interpretation; sibling readings may instantiate permissive interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_threshold_indeterminacy, conceptual, 'Indeterminacy of transformation threshold — kernel-level ambiguity').

omega_variable(
    intent_and_permission_asymmetry,
    'Does the enclosure regime''s requirement that creators predetermine licensing obligations (prepare-to-create permission-checking) before ideation constitute a chilling effect on legitimate transformation, or a reasonable transaction cost?',
    'Empirical studies of creator behavior pre/post copyright enforcement ratchet-ups; surveys of creators reporting avoidance due to derivative-work ambiguity; comparative analysis of innovation output by jurisdiction (high fair-use vs high enclosure); network analysis of licensing negotiation times and success rates for small-scale creators',
    'If chilling effect is substantial: suppression value increases, snare classification amplified. If transaction cost is tolerable: tangled-rope interpretation strengthens (coordination benefits offset extraction costs). Current measurements assume chilling effect is substantial; countervailing evidence would shift the baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_and_permission_asymmetry, empirical, 'Chilling effect of pre-permission licensing requirements on creative behavior').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the derivative-work statutory boundary primarily a coordination mechanism (kernel_reading: coordination_reading) enabling licensing markets and authorial control, or primarily an enclosure mechanism (kernel_reading: enclosure_reading) gatekeeping innovation and extracting from downstream creators?',
    'This is the fundamental omega that distinguishes the reading you are instantiating from its siblings. The enclosure_reading axiomatizes the extraction logic and treats the coordination function as secondary (licensing as extraction mechanism). The coordination_reading axiomatizes the licensing markets and treats extraction as contingent (extraction as side effect of coordination infrastructure). The hybrid_carveout_reading claims both are structural but proposes carved-out exceptions (fair use, research, education) that mitigate extraction. No empirical data resolves this — it is a commitment about which structural features are primary.',
    'If enclosure is primary: this reading is holdable. If coordination is primary: sibling coordination_reading is holdable. If both are equally structural: hybrid_carveout_reading is holdable. Only one reading can be the operative reading in a single legal framework at a single moment — but the other readings can coexist in other jurisdictions, legal traditions, or historical periods.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether derivative-work boundary is primarily enclosure or coordination — kernel-reading distinction').

omega_variable(
    empirical_transformation_measurement,
    'Can transformation be operationalized as a continuous metric (e.g., stylistic distance, functional substitutability, market competition) rather than a categorical yes/no boundary?',
    'Machine-learning analysis of textual, musical, and visual similarity metrics; market-impact studies measuring whether derivative works compete with original or create new markets; linguistic analysis of how courts describe transformation (metaphorical vs quantitative language)',
    'If transformation is truly categorical: the statute''s line-drawing is necessary (every legal regime needs a binary rule). If transformation is continuous: the statute''s line-drawing is arbitrary, and the enclosure effect flows from the arbitrary line placement, not the coordinate necessity of a boundary. This shifts chi downward if continuous, upward if categorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_transformation_measurement, empirical, 'Whether transformation is categorical or continuous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dw_encl_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dw_encl_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(dw_encl_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(dw_encl_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dw_encl_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dw_encl_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dw_encl_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(dw_encl_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(dw_encl_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_enforceability).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, software_interoperability_licensing).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, music_sampling_and_clearance_regimes).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, ai_training_data_licensing_bottleneck).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel has three structurally distinct readings: this enclosure_reading emphasizes extraction through licensing gatekeeping; the coordination_reading emphasizes legitimate licensing markets enabling authorial control; the hybrid_carveout_reading claims both are structural but proposes exceptions to mitigate extraction. These three readings are not the same constraint measured from different angles — they are genuinely different normative interpretations of the kernel, each producing different ε values and structural relationships. The network links connect to downstream constraints whose extractiveness and classification depend on which reading of the kernel is operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
